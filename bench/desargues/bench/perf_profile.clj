(ns desargues.bench.perf-profile
  "DESARGUES-PERF-DEFAULTS bench: the :stock perf profile (Manim as shipped)
   against :fast (desargues.perf.profile), interleaved ABAB/BABA in one warm
   process, 480p15.

   Arms (each sample is one edit iteration):
   - \"layout\": the bench layout (16 cells, hold 1) re-rendered frames-only
     through a Manim backend built with the profile.
   - \"edit-one-cell\": the same layout with one cell changed per sample (the
     same cell sequence for both variants).
   - \"anim-heavy\": desargues.bench/imperative-scene (staggered appears,
     counters, glides).
   - \"live\": the live_raster scene played into a JavaFX IRaster with
     play-live!, unpaced (needs the :studio alias and a display).
   Per sample: wall-ms, process cpu-s, :ttff-ms (run start to the first
   frame at the sink; first pixel on screen for live) and :frames.

   `sha-check` renders every arm's scene once per profile with a sink that
   hashes each frame (SHA-256) and compares the lists.

   Needs the Manim env. Run from a REPL: (report! {:out \"<dir>\"}); nothing
   runs on load and nothing is written under bench/results."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [desargues.bench :as bench]
            [desargues.bench.ab :as ab]
            [desargues.bench.live-raster :as lr]
            [desargues.bench.sample :as smp]
            [desargues.layout.core :as l]
            [desargues.manim.core :as mc]
            [desargues.manim.frame-protocols :as fp]
            [desargues.manim.frames :as fr]
            [desargues.manim.patches :as mp]
            [desargues.manim.raster :as mr]
            [desargues.perf.profile :as perf]
            [desargues.scene :as s]
            [desargues.scene.manim :as sm]
            [libpython-clj2.python :as py]
            [tech.v3.datatype :as dt])
  (:import (java.security MessageDigest)))

(def media-dir
  (str (io/file (System/getProperty "java.io.tmpdir") "desargues-bench-perf-profile")))

;; ── sinks ────────────────────────────────────────────────────────────────

(defrecord FirstFrameSink [state]
  fp/IFrameSink
  (-frame! [_ _ _ _ _]
    (swap! state (fn [{:keys [first-ns] :as st}]
                   (-> st (update :count inc)
                       (assoc :first-ns (or first-ns (System/nanoTime))))))))

(defn first-frame-sink [] (->FirstFrameSink (atom {:count 0 :first-ns nil})))

(defn sha-hex [^bytes bs]
  (apply str (map #(format "%02x" (bit-and % 0xff))
                  (.digest (MessageDigest/getInstance "SHA-256") bs))))

(defrecord HashingSink [hashes]
  fp/IFrameSink
  (-frame! [_ rgba _ _ _]
    (swap! hashes conj (sha-hex (dt/->byte-array rgba)))))

(defn hashing-sink [] (->HashingSink (atom [])))

;; ── Manim plumbing ────────────────────────────────────────────────────────

(defn- cfg [] (py/get-attr (mc/manim) "config"))

(defn ready!
  "init! Manim quietly, 480p15, frames-only, no caching."
  []
  (mc/init!)
  (py/call-attr (py/get-attr (mc/manim) "logger") "setLevel" "WARNING")
  (doseq [[k v] {"progress_bar" "none" "media_dir" media-dir "disable_caching" true
                 "write_to_movie" false}]
    (py/set-attr! (cfg) k v))
  (s/with-backend (sm/manim-backend {:perf-profile :stock})
    (s/configure! {:quality "low_quality"})))

(defn backend
  "A Manim scene backend built under `profile` (its mobject cache)."
  [profile]
  (sm/manim-backend {:perf-profile profile}))

(defn- with-sink
  "Run `f` with frame sink `sink` on SceneFileWriter.write_frame."
  [sink f]
  (fr/install! sink)
  (try (f) (finally (fr/uninstall!))))

(defn- timed
  "Measure `render!` (fn [] ...) with a FirstFrameSink: the ab measurement
   plus :ttff-ms and :frames."
  [render!]
  (let [sink (first-frame-sink)
        t0   (System/nanoTime)
        m    (ab/measure #(with-sink sink render!))
        {:keys [first-ns count]} @(:state sink)]
    {::ab/measured (-> (dissoc m :value)
                       (assoc :frames count :ttff-ms (some-> first-ns (- t0) (/ 1e6))))}))

;; ── scenes ────────────────────────────────────────────────────────────────

(defn edited
  "The bench layout with cell `k` (0..15) replaced by a text."
  [k]
  (update-in (bench/layout-tree 16)
             [:children (inc (quot k 4)) :children (mod k 4) :children 0]
             (fn [_] (l/text (str "edited " k)))))

(def anim-k
  "Circles in the anim-heavy scene."
  4)

(defn scene-thunk
  "Arm name -> (fn [backend i] render!) rendering edit `i` of its scene."
  [arm]
  (case arm
    "layout"        (fn [b _] (s/with-backend b (s/render-layout! "PerfLayout" (bench/layout-tree 16) :hold 1)))
    "edit-one-cell" (fn [b i] (s/with-backend b (s/render-layout! "PerfEdit" (edited (mod i 16)) :hold 1)))
    "anim-heavy"    (fn [b _] (s/with-backend b (s/render! "PerfAnim" (bench/imperative-scene anim-k))))))

;; ── render arms ───────────────────────────────────────────────────────────

(defn render-arm
  "Interleaved perf profiles (:variants, default [:stock :fast]) for render
   arm `arm`. Each variant keeps one backend (so a mobject cache stays warm
   across edits, as in an editing session) and installs its profile before
   every sample."
  [arm {:keys [rounds warmup variants] :or {rounds 12 warmup 2 variants [:stock :fast]}}]
  (let [render (scene-thunk arm)
        variant (fn [k]
                  (let [b (delay (backend k)) n (atom 0)]
                    {:name (name k)
                     :setup (fn [_] (perf/install! (:perf-profile @b)) nil)
                     :run (fn [_] (let [i (swap! n inc)] (timed #(render @b i))))}))]
    {:id (str arm " frames-only, " (clojure.string/join " vs " (map name variants)))
     :rounds rounds :warmup warmup :alternate? true
     :setup ready!
     :teardown #(perf/install! :stock)
     :variants (mapv variant variants)}))

;; ── live arm ──────────────────────────────────────────────────────────────

(defn live-arm
  "Interleaved perf profiles (:variants, default [:stock :fast]) through
   play-live! into `host`, unpaced; :live-opts go to every run (e.g.
   {:quality \"high_quality\" :frame-rate 15})."
  [host extras {:keys [rounds warmup variants live-opts]
                :or {rounds 15 warmup 2 variants [:stock :fast]}}]
  (let [M (mc/manim) i (atom 0)
        variant (fn [k]
                  {:name (name k)
                   :setup (fn [_] (perf/install! :stock) nil)
                   :run (fn [_]
                          (let [r (lr/live-run! M host (swap! i inc)
                                                (merge live-opts {:profile k :paced? false}))]
                            (swap! extras update (name k) (fnil conj []) r)
                            r))})]
    {:id (str "live " (:quality live-opts "low_quality") " unpaced, "
              (clojure.string/join " vs " (map name variants)))
     :rounds rounds :warmup warmup :alternate? true
     :variants (mapv variant variants)}))

;; ── SHA-256 identity ──────────────────────────────────────────────────────

(defn render-hashes
  "SHA-256 of every frame of render arm `arm`, edit `i`, under `profile`."
  [arm profile i]
  (let [b (backend profile) sink (hashing-sink)]
    (perf/install! profile)
    (try (with-sink sink #((scene-thunk arm) b i)) (finally (perf/install! :stock)))
    @(:hashes sink)))

(defn live-hashes
  "SHA-256 of every frame the live path delivers for scene edit `i` under
   `profile`, with exactly play-live!'s patches and crossing (layout
   :bgra-pre, JavaFX's)."
  [profile i]
  (let [{:keys [patches crossing quality]} (mr/live-plan :bgra-pre {:profile profile :paced? false})
        sink (hashing-sink)]
    (perf/install! :stock)
    (mp/with-patches* patches
      #(fr/render-live! (fn [] ((lr/scene-class (mc/manim) i))) sink
                        {:quality quality :crossing crossing}))
    @(:hashes sink)))

(defn sha-check
  "{arm {:frames n :identical? bool}} for every arm, stock vs fast; each
   render arm twice under :fast (cold, then warm caches)."
  []
  (ready!)
  (into {}
        (concat
         (for [arm ["layout" "edit-one-cell" "anim-heavy"]]
           (let [a  (render-hashes arm :stock 3)
                 f1 (render-hashes arm :fast 3)
                 f2 (render-hashes arm :fast 3)]
             [arm {:frames (count a) :identical? (= a f1 f2)}]))
         (for [i [7 8]]
           (let [a (live-hashes :stock i) f (live-hashes :fast i)]
             [(str "live-" i) {:frames (count a) :identical? (= a f)}])))))

;; ── lossy profiles: PSNR against the exact render ──────────────────────────

(defn psnr
  "[PSNR-dB max-abs] of frame `b` against reference frame `a` (equal-size
   RGBA byte arrays), over the colour channels; 99.0 for identical frames."
  [^bytes a ^bytes b]
  (let [n (alength a)]
    (loop [i 0 se 0.0 k 0 mx 0]
      (if (< i n)
        (if (= 3 (mod i 4))
          (recur (inc i) se k mx)
          (let [d (Math/abs (- (bit-and (aget a i) 0xff) (bit-and (aget b i) 0xff)))]
            (recur (inc i) (+ se (* d d)) (inc k) (max mx d))))
        (let [mse (/ se k)]
          [(if (zero? mse) 99.0 (* 10 (Math/log10 (/ (* 255.0 255.0) mse)))) mx])))))

(defrecord KeepingSink [frames n every]
  fp/IFrameSink
  (-frame! [_ rgba _ _ _]
    (when (zero? (mod (swap! n inc) every))
      (swap! frames conj (dt/->byte-array rgba)))))

(defn render-frames
  "Every `every`-th frame (bytes) of render arm `arm`, edit `i`, under `profile`."
  [arm profile i every]
  (let [b (backend profile) sink (->KeepingSink (atom []) (atom -1) every)]
    (perf/install! profile)
    (try (with-sink sink #((scene-thunk arm) b i)) (finally (perf/install! :stock)))
    @(:frames sink)))

(defn quality-check
  "{arm {:frames n :psnr-min :psnr-mean :max-abs}}: `profile`'s frames against
   `reference`'s (default :fast, the exact render) on the render arms."
  ([profile] (quality-check profile {}))
  ([profile {:keys [reference arms every] :or {reference :fast every 1
                                               arms ["layout" "anim-heavy"]}}]
   (ready!)
   (into {}
         (for [arm arms]
           (let [rs (mapv psnr (render-frames arm reference 3 every)
                          (render-frames arm profile 3 every))
                 ps (map first rs)]
             [arm {:frames (count rs)
                   :psnr-min (apply min ps)
                   :psnr-mean (/ (reduce + ps) (count ps))
                   :max-abs (apply max (map second rs))}])))))

;; ── report ────────────────────────────────────────────────────────────────

(defn- extra-stats
  "Median ttff-ms, frames, and cpu-ms per frame per variant of an arm result."
  [{:keys [samples]}]
  (into {}
        (for [[v ss] (group-by :variant (remove :warmup? samples))]
          [v {:ttff-ms (smp/summarize (keep :ttff-ms ss))
              :frames (:frames (first ss))
              :cpu-ms-per-frame (smp/summarize (keep #(when-let [n (:frames %)]
                                                         (when (pos? n) (/ (* 1000 (:cpu-s %)) n)))
                                                      ss))}])))

(defn report!
  "Run the arms named in :only (default all four; \"live\" opens a JavaFX
   window) and the SHA check (skip with :sha? false); write
   <out>/perf-profile.edn after every arm."
  [{:keys [only out sha?] :or {only ["layout" "edit-one-cell" "anim-heavy" "live"] sha? true}
    :as opts}]
  (assert out ":out dir required (never bench/results)")
  (let [f   (io/file out "perf-profile.edn")
        rep (atom {:system (smp/system-info) :arms []})
        save! #(do (.mkdirs (.getParentFile f)) (spit f (with-out-str (pp/pprint @rep))))]
    (when sha?
      (swap! rep assoc :sha (sha-check))
      (smp/say "sha: " (:sha @rep))
      (save!))
    (ready!)
    (doseq [arm only :when (not= arm "live")]
      (let [r (ab/run-arm (render-arm arm opts))]
        (swap! rep update :arms conj (assoc r :extra (extra-stats r)))
        (save!)))
    (when (some #{"live"} only)
      (let [host ((requiring-resolve 'desargues.player.javafx/host))
            extras (atom {})]
        ((requiring-resolve 'desargues.player.protocols/-show!) host
         {:title "desargues perf profile bench" :w 854 :h 480})
        ;; let the stage come up before the first frame is published
        (Thread/sleep 2000)
        (try
          (let [r (ab/run-arm (live-arm host extras opts))]
            (swap! rep update :arms conj
                   (assoc r :extra
                          (into {} (for [[v rs] @extras]
                                     [v {:first-pixel-ms (smp/summarize (keep :first-pixel-ms rs))
                                         :frames (:frames (first rs))}]))))
            (save!))
          (finally ((requiring-resolve 'desargues.player.protocols/-close!) host)))))
    @rep))
