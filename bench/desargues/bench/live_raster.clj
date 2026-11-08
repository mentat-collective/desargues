(ns desargues.bench.live-raster
  "DESARGUES-PERF-LIVE-2 bench, warm process, 480p (low_quality, 854x480):
   Manim frames shown live in the JavaFX player (desargues.manim.raster
   play-live! into the FxHost IRaster) vs the encode path (the same scene
   rendered to an mp4, write_to_movie on).

   - arm \"live vs encode\" (desargues.bench.ab, interleaved ABAB): variants
     \"movie\" (baseline), \"live-paced\" (frames published at scene fps) and
     \"live-unpaced\" (as fast as Manim renders). Wall and process CPU-s per
     iteration; every iteration is an edit (a fresh Scene class).
     Per live sample: edit -> first pixel on screen, frames shown/dropped,
     FX-thread upload ms per frame, render-thread conversion ms per frame.
   - drops: frames dropped per second of playback at 15/30/60 fps (paced).
   Needs the Manim env and the :studio alias (JavaFX). Run: (run!)"
  (:require [desargues.bench.ab :as ab]
            [desargues.bench.sample :as smp]
            [desargues.manim.core :as mc]
            [desargues.manim.raster :as mr]
            [desargues.player.protocols :as p]
            [libpython-clj2.python :as py]))

(def media-dir
  (str (System/getProperty "java.io.tmpdir") "/desargues-bench-live-raster"))

(def scene-seconds
  "Scene time of `scene-class`: Create 1 s + Transform 1 s + wait 0.5 s."
  2.5)

(defn scene-class
  "Edit `i`: a fresh Scene class (circle radius differs per i)."
  [M i]
  (mc/create-scene-class
   (str "LiveRasterBench" i)
   (fn [self]
     (let [c (py/call-attr-kw M "Circle" [] {"radius" (+ 1.0 (* 0.01 (mod i 50))) "color" "#FFD700"})
           s (py/call-attr-kw M "Square" [] {"side_length" 2.0 "fill_opacity" 0.5})]
       (mc/play! self (py/call-attr M "Create" c))
       (mc/play! self (py/call-attr M "Transform" c s))
       (mc/wait! self 0.5)))))

(defn- cfg [M] (py/get-attr M "config"))

(defn movie-run!
  "Encode path: render edit `i` to an mp4 at 480p15 (caching off)."
  [M i]
  (let [c   (cfg M)
        Sc  (scene-class M i)
        kvs {"quality" "low_quality" "write_to_movie" true "disable_caching" true
             "media_dir" media-dir "progress_bar" "none"}
        old (into {} (map (fn [k] [k (py/get-item c k)])) (keys kvs))]
    (try
      (doseq [[k v] kvs] (py/set-item! c k v))
      (py/call-attr (Sc) "render")
      (finally (doseq [[k v] old] (py/set-item! c k v))))))

(defn- ms-stats
  "ns samples -> sample/summarize in ms, plus :max-ms."
  [xs]
  (when (seq xs)
    (let [ms (map #(/ (double %) 1e6) xs)]
      (assoc (smp/summarize ms) :max-ms (reduce max ms)))))

(defn live-run!
  "Live path: play edit `i` into `host`. The play-live! report, condensed.
   The perf profile defaults to :stock here, so each variant names exactly
   the patches it measures; pass :profile to measure a profile."
  [M host i opts]
  (let [{:keys [done]} (mr/play-live! host #((scene-class M i)) (merge {:profile :stock} opts))
        r (deref done 60000 ::timeout)]
    (when (= r ::timeout) (throw (ex-info "live run timed out" {:i i})))
    (when-let [e (:error r)] (throw e))
    (let [play-s (/ (- (:end-ns r) (:start-ns r)) 1e9)]
      {:frames         (:frames r)
       :shown          (:shown r)
       :dropped        (:overwritten r)
       :fps            (:fps r)
       :play-s         play-s
       :first-pixel-ms (/ (- (:first-shown-ns r) (:start-ns r)) 1e6)
       :fx-ms          (ms-stats (:present-ns r))
       :conv-ms        (ms-stats (:conv-ns r))})))

(defn live-arm
  "movie vs live-paced vs live-unpaced, interleaved."
  [M host extras {:keys [rounds warmup] :or {rounds 5 warmup 1}}]
  (let [i (atom 0)
        live (fn [nm opts]
               {:name nm
                :run (fn [_] (let [r (live-run! M host (swap! i inc) opts)]
                               (swap! extras update nm (fnil conj []) r)))})]
    {:id "live vs encode 480p15"
     :rounds rounds :warmup warmup
     :variants [{:name "movie" :run (fn [_] (movie-run! M (swap! i inc)))}
                (live "live-paced" {:paced? true})
                (live "live-unpaced" {:paced? false})]}))

(defn native-arm
  "DESARGUES-PERF-LIVE-3: Manim's RGBA converted on the render thread vs
   cairo's native premultiplied BGRA copied as is (:native? in play-live!),
   paced and unpaced, interleaved. Run with (ab/run-arm (native-arm M host
   extras {})); per-frame conversion ms lands in `extras`."
  [M host extras {:keys [rounds warmup] :or {rounds 9 warmup 1}}]
  (let [i (atom 0)
        live (fn [nm opts]
               {:name nm
                :run (fn [_] (let [r (live-run! M host (swap! i inc) opts)]
                               (swap! extras update nm (fnil conj []) r)))})]
    {:id "live rgba-convert vs bgra-native 480p15"
     :rounds rounds :warmup warmup
     :variants [(live "unpaced-convert" {:paced? false :native? false})
                (live "unpaced-native" {:paced? false :native? true})
                (live "paced-convert" {:paced? true :native? false})
                (live "paced-native" {:paced? true :native? true})]}))

(defn surface-reuse-arm
  "DESARGUES-PERF-LIVE-4/5: the live path with the surface-reuse patch (one
   cairo paint() background reset, the Camera buffer handed over uncopied,
   one long-lived JVM view of it), unpaced, interleaved ABAB/BABA.
   Variants: \"current\" (native BGRA, as-tensor crossing, Manim's reset and
   frame copy), \"surface-reuse\", \"+native-display\" (plus the Zig batch
   drawer, DESARGUES_PATH_BUILDER=native-display, when built) and
   \"+bg-cache\" (plus background-cache: each new Scene's Camera reuses the
   background array instead of rebuilding it). :quality defaults to
   \"low_quality\" (480p15). :variants narrows to the named ones. Run with
   (ab/run-arm (surface-reuse-arm M host extras {})); per-run reports land
   in `extras`."
  [M host extras {:keys [rounds warmup quality variants]
                  :or {rounds 15 warmup 1 quality "low_quality"}}]
  (let [i (atom 0)
        nd ((requiring-resolve 'desargues.manim.patches/path-builder-patches) "native-display")
        bc ((requiring-resolve 'desargues.manim.patches.background-cache/background-cache))
        live (fn [nm opts]
               {:name nm
                :run (fn [_] (let [r (live-run! M host (swap! i inc) (assoc opts :quality quality))]
                               (swap! extras update nm (fnil conj []) r)))})
        sr {:paced? false :native? true :surface-reuse? true}
        all [(live "current" {:paced? false :native? true})
             (live "surface-reuse" sr)
             (live "+native-display" (assoc sr :patches nd))
             (live "+bg-cache" (assoc sr :patches (conj nd bc)))]]
    {:id (str "live current vs surface-reuse " quality " unpaced")
     :rounds rounds :warmup warmup :alternate? true
     :variants (if variants (filterv (comp (set variants) :name) all) all)}))

(defn drops
  "Frames dropped per second of playback, paced, at each fps, `n` runs each.
   Per-run FX upload and conversion medians/max are in ms."
  [M host fpss n]
  (into {}
        (for [fps fpss]
          (let [rs (vec (for [k (range n)]
                          (live-run! M host (+ 100000 (* 1000 fps) k)
                                     {:paced? true :frame-rate fps})))]
            [fps {:dropped-per-s  (smp/summarize (map #(/ (:dropped %) (:play-s %)) rs))
                  :frames         (:frames (first rs))
                  :shown          (smp/summarize (map :shown rs))
                  :fx-ms-median   (smp/summarize (map (comp :median-ms :fx-ms) rs))
                  :fx-ms-max      (smp/summarize (map (comp :max-ms :fx-ms) rs))
                  :conv-ms-median (smp/summarize (map (comp :median-ms :conv-ms) rs))}]))))

(defn run!
  "Open a JavaFX host, run the arm and the drop sweep, close it."
  ([] (run! {}))
  ([opts]
   (let [M    (mc/init!)
         _    (py/call-attr (py/get-attr M "logger") "setLevel" "ERROR")
         host ((requiring-resolve 'desargues.player.javafx/host))
         _    (p/-show! host {:title "desargues live raster bench" :w 854 :h 480})
         extras (atom {})]
     (try
       (let [arm (ab/run-arm (live-arm M host extras opts))
             live-summary (fn [nm]
                            (let [rs (get @extras nm)]
                              {:first-pixel-ms (smp/summarize (map :first-pixel-ms rs))
                               :dropped        (smp/summarize (map :dropped rs))
                               :fx-ms-median   (smp/summarize (map (comp :median-ms :fx-ms) rs))
                               :fx-ms-max      (smp/summarize (map (comp :max-ms :fx-ms) rs))
                               :conv-ms-median (smp/summarize (map (comp :median-ms :conv-ms) rs))}))]
         {:arm      (select-keys arm [:arm :variants :load-start :load-end])
          :scene-s  scene-seconds
          :live     {"live-paced"   (live-summary "live-paced")
                     "live-unpaced" (live-summary "live-unpaced")}
          :drops    (drops M host (:fpss opts [15 30 60]) (:drop-runs opts 3))
          :load-end (ab/host-load)})
       (finally (p/-close! host))))))
