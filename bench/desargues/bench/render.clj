(ns desargues.bench.render
  "Render benchmarks: interleaved A/B arms (desargues.bench.ab) over Manim.

     clojure -M:bench -m desargues.bench.render [--only warm,init] [--rounds N]
                                                [--warmup N] [--out DIR]

   Writes DIR/results/render.edn (DIR defaults to bench): the system stamp, one
   :arms entry per arm (every sample with its wall-ms, cpu-s and host load, and
   the per-variant summaries with ratios to the first variant) and :results
   rows in the shape desargues.bench.check compares.

   Arms
   - \"warm\": the reference scene (bench layout-tree 16, hold 1) rendered at
     480p15 in one warm JVM+Python, write_to_movie on (\"movie\", baseline) vs
     off (\"frames-only\": frames are still rasterized, nothing is encoded).
     Caching is off for both, so every round renders every frame.
   - \"init\": cold desargues.manim.core/init! in a fresh child JVM per sample:
     \"py-init\" (libpython up, no manim import) vs \"init!\" (py init + import
     manim). The child times itself and reports wall-ms and its own process
     CPU-seconds; JVM start and Clojure load are outside the number.

   Adding an arm: write a fn returning an arm map (see desargues.bench.ab) and
   register it in `arms`. Needs the conda Manim env (clojure -M:doctor)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [desargues.bench :as bench]
            [desargues.bench.ab :as ab]
            [desargues.bench.sample :as smp]))

(def say smp/say)

(def media-dir
  "Where the bench renders land (outside the repo)."
  (str (io/file (System/getProperty "java.io.tmpdir") "desargues-bench-render")))

;; ── Manim plumbing ───────────────────────────────────────────────────────────

(defn- manim-env? []
  (boolean (try ((requiring-resolve 'desargues.config/manim-config)) (catch Throwable _ nil))))

(defn- cfg []
  ((requiring-resolve 'desargues.manim.core/get-attr)
   ((requiring-resolve 'desargues.manim.core/manim)) "config"))

(defn- set-cfg! [k v] ((requiring-resolve 'desargues.manim.core/set-attr!) (cfg) k v))
(defn- quiet-manim! []
  (let [py-call (requiring-resolve 'libpython-clj2.python/call-attr)
        logger  ((requiring-resolve 'desargues.manim.core/get-attr)
                 ((requiring-resolve 'desargues.manim.core/manim)) "logger")]
    (py-call logger "setLevel" "WARNING")
    (set-cfg! "progress_bar" "none")))

(defn reference-scene!
  "Render the reference scene once with the current Manim config."
  [scene-name]
  (let [s-render (requiring-resolve 'desargues.scene/render-layout!)]
    (s-render scene-name (bench/layout-tree 16) :hold 1)))

;; ── arms ─────────────────────────────────────────────────────────────────────

(defn warm-render-arm
  "write_to_movie on vs off, 480p15, one warm process."
  [opts]
  (let [variant (fn [nm movie?]
                  {:name nm
                   :data {:write-to-movie movie?}
                   :setup (fn [{:keys [write-to-movie] :as d}]
                            (set-cfg! "write_to_movie" write-to-movie)
                            d)
                   :run (fn [_] (reference-scene! "BenchRef"))
                   :teardown (fn [_] (set-cfg! "write_to_movie" true))})]
    (merge {:id "warm render 480p15"
            :setup (fn []
                     ((requiring-resolve 'desargues.manim.core/init!))
                     (quiet-manim!)
                     ((requiring-resolve 'desargues.scene/configure!) {:quality "low_quality"})
                     (set-cfg! "media_dir" media-dir)
                     (set-cfg! "disable_caching" true))
            :variants [(variant "movie" true) (variant "frames-only" false)]
            :rounds 5 :warmup 1}
           opts)))

(defn svg-copy-arm
  "Frames-only warm render of the reference scene, Manim as shipped vs the
   desargues.manim.patches/svg-path-shared-copy patch (glyph copies share
   their parsed SVG path)."
  [opts]
  (let [patch   @(requiring-resolve 'desargues.manim.patches/svg-path-shared-copy)
        install (requiring-resolve 'desargues.manim.patches/install!)
        remove  (requiring-resolve 'desargues.manim.patches/uninstall!)
        variant (fn [nm patched?]
                  {:name nm
                   :setup (fn [_]
                            (set-cfg! "write_to_movie" false)
                            (if patched? (install patch) (remove patch)))
                   :run (fn [_] (reference-scene! "BenchRef"))
                   :teardown (fn [_] (remove patch) (set-cfg! "write_to_movie" true))})]
    (merge (warm-render-arm {})
           {:id "svg path shared copy 480p15 frames-only"
            :variants [(variant "manim" false) (variant "svg-shared-copy" true)]
            :rounds 5 :warmup 1}
           opts)))

(defn- sunk-render!
  "Render the reference scene frames-only with a CollectingSink installed;
   the measurement plus :ttff-ms (run start to the first frame at the sink)
   and :frames."
  []
  (let [install! (requiring-resolve 'desargues.manim.frames/install!)
        uninst!  (requiring-resolve 'desargues.manim.frames/uninstall!)
        sink     ((requiring-resolve 'desargues.manim.frames/collecting-sink))
        collect  (requiring-resolve 'desargues.manim.frames/collected)
        t0       (System/nanoTime)
        m        (ab/measure (fn []
                               (install! sink)
                               (try (reference-scene! "BenchRef") (finally (uninst!)))))
        {:keys [first-ns count]} (collect sink)]
    {::ab/measured (-> (dissoc m :value)
                       (assoc :frames count
                              :ttff-ms (some-> first-ns (- t0) (/ 1e6))))}))

(defn glyph-cache-arm
  "Frames-only warm render with svg-path-shared-copy installed in both
   variants, without vs with desargues.manim.patches/glyph-cache (Text and
   MathTex builds reused across renders). Each sample also records the time
   to the first frame at a CollectingSink (:ttff-ms)."
  [opts]
  (let [svg     @(requiring-resolve 'desargues.manim.patches/svg-path-shared-copy)
        cache   ((requiring-resolve 'desargues.manim.patches/glyph-cache))
        install (requiring-resolve 'desargues.manim.patches/install!)
        remove  (requiring-resolve 'desargues.manim.patches/uninstall!)
        variant (fn [nm cached?]
                  {:name nm
                   :setup (fn [_]
                            (set-cfg! "write_to_movie" false)
                            (install svg)
                            (if cached? (install cache) (remove cache)))
                   :run (fn [_] (sunk-render!))
                   :teardown (fn [_] (set-cfg! "write_to_movie" true))})]
    (merge (warm-render-arm {})
           {:id "glyph cache 480p15 frames-only"
            :variants [(variant "svg-copy" false) (variant "svg-copy+glyph-cache" true)]
            :teardown (fn [] (remove cache) (remove svg))
            :rounds 7 :warmup 1}
           opts)))

(defn child-cmd
  "argv that runs this namespace's child mode in a fresh JVM on our classpath.
   `jvm` is a {:jvm-opts [..] :cp-prefix [dir ..]} launch profile."
  ([mode] (child-cmd mode {}))
  ([mode {:keys [jvm-opts cp-prefix]}]
   (into [(str (io/file (System/getProperty "java.home") "bin" "java"))]
         (concat jvm-opts
                 ["--enable-native-access=ALL-UNNAMED"
                  "-cp" (str/join ":" (concat (map #(str (.getAbsoluteFile (io/file %))) cp-prefix)
                                              [(System/getProperty "java.class.path")]))
                  "clojure.main" "-m" "desargues.bench.render" "--child" mode]))))

(defn- run-child
  "Run a child, return its self-measurement as {::ab/measured {...}}, plus
   the child's whole-process wall time as :process-wall-ms. The child prints
   one line {:bench-child {:wall-ms :cpu-s}} (an unqualified key, so pr-str
   does not turn it into #:ns{} map syntax). With :whole-process? the child's
   JVM-start-to-done numbers (:uptime-ms, :proc-cpu-s) become the sample."
  ([mode] (run-child mode {}))
  ([mode {:keys [whole-process?] :as jvm}]
   (let [t0   (System/nanoTime)
         {:keys [exit out err]} (apply sh/sh (child-cmd mode jvm))
         pw   (/ (- (System/nanoTime) t0) 1e6)
         line (some->> (str/split-lines out) (filter #(str/starts-with? % "{:bench-child")) last)]
     (when (or (not (zero? exit)) (nil? line))
       (throw (ex-info "bench child failed"
                       {:mode mode :exit exit
                        :err (str/join "\n" (take-last 20 (str/split-lines err)))})))
     (let [m (assoc (:bench-child (edn/read-string line)) :process-wall-ms pw)]
       {::ab/measured (cond-> m
                        whole-process? (assoc :wall-ms (:uptime-ms m) :cpu-s (:proc-cpu-s m)
                                              :section-wall-ms (:wall-ms m)
                                              :section-cpu-s (:cpu-s m)))}))))

(defn cold-init-arm
  "Cold init cost in a fresh JVM per sample: python only vs python + import
   manim, under each desargues.manim.core/import-plan mode."
  [opts]
  (merge {:id "cold init!"
          :variants [{:name "py-init"     :run (fn [_] (run-child "py"))}
                     {:name "init! eager" :run (fn [_] (run-child "full:eager"))}
                     {:name "init! lean"  :run (fn [_] (run-child "full:lean"))}]
          :rounds 5 :warmup 1}
         opts))

(defn first-render-arm
  "The first reference-scene render right after a cold init! (fresh JVM per
   sample; init! itself is outside the number), per import mode: a deferred
   import must not just move its cost here."
  [opts]
  (merge {:id "first render after init!"
          :variants [{:name "eager" :run (fn [_] (run-child "render:eager"))}
                     {:name "lean"  :run (fn [_] (run-child "render:lean"))}]
          :rounds 5 :warmup 1}
         opts))

(def pyaot-dir
  "Classes dir `clojure -M:pyaot-build` writes (desargues.pyaot)."
  "target/pyaot")

(def launch-profiles
  "Child JVM launch profiles: name -> {:jvm-opts :cp-prefix}."
  {"source"        {}
   "c1"            {:jvm-opts ["-XX:TieredStopAtLevel=1"]}
   "aot"           {:cp-prefix [pyaot-dir]}
   "aot+c1"        {:cp-prefix [pyaot-dir] :jvm-opts ["-XX:TieredStopAtLevel=1"]}
   "aot+c1+serial" {:cp-prefix [pyaot-dir] :jvm-opts ["-XX:TieredStopAtLevel=1" "-XX:+UseSerialGC"]}
   "aot+ci2"       {:cp-prefix [pyaot-dir] :jvm-opts ["-XX:CICompilerCount=2"]}})

(defn- require-pyaot! []
  (when-not (.exists (io/file pyaot-dir "desargues" "pyaot-stamp.edn"))
    (throw (ex-info (str pyaot-dir " missing: run clojure -M:pyaot-build first") {}))))

(defn launch-arm
  "A child-JVM arm with one variant per launch profile in `profiles` (first is
   the baseline), each sample running child `mode`."
  [id mode profiles whole-process? opts]
  (merge {:id id
          :setup require-pyaot!
          :variants (mapv (fn [p] {:name p
                                   :run (fn [_] (run-child mode (assoc (launch-profiles p)
                                                                      :whole-process? whole-process?)))})
                          profiles)
          :rounds 5 :warmup 1}
         opts))

(defn pyinit-arm
  "Cold start to Python up (py-init child), whole child process from JVM
   start: Clojure loading of libpython-clj/dtype-next is inside the number."
  [opts]
  (launch-arm "cold py-init whole process" "py"
              ["source" "c1" "aot" "aot+c1" "aot+c1+serial" "aot+ci2"] true opts))

(defn init-launch-arm
  "Cold start to Manim imported (lean init!), whole child process."
  [opts]
  (launch-arm "cold init! lean whole process" "full:lean"
              ["source" "aot" "aot+c1"] true opts))

(defn init-aot-arm
  "Cold lean init! section only (Clojure load outside the number), per launch
   profile: what the prebuilt binding class saves inside init! itself."
  [opts]
  (launch-arm "cold init! lean section" "full:lean"
              ["source" "aot" "aot+c1"] false opts))

(defn c1-warm-arm
  "Warm render cost per launch profile: 3 timed frames-only renders of the
   reference scene after init! and 3 warmup renders, in a fresh child."
  [opts]
  (launch-arm "warm 3x render frames-only per launch" "warm:lean"
              ["source" "aot" "aot+c1"] false opts))

(defn native-display-arm
  "Frames-only warm render, both variants with the Zig path builder: Manim's
   per-VMobject display_vectorized (\"zig-path\", baseline) vs one Zig call
   per batch drawing path, fill and stroke
   (desargues.manim.patches.native-display; `bb native:path` builds it)."
  [opts]
  (let [path    ((requiring-resolve 'desargues.manim.patches/native-cairo-path))
        disp    ((requiring-resolve 'desargues.manim.patches.native-display/native-display))
        install (requiring-resolve 'desargues.manim.patches/install!)
        remove  (requiring-resolve 'desargues.manim.patches/uninstall!)
        need!   (fn [p] (when-not (install p)
                          (throw (ex-info "native library not built (bb native:path)"
                                          {:lib-path (:lib-path p)}))))
        variant (fn [nm display?]
                  {:name nm
                   :setup (fn [_]
                            (set-cfg! "write_to_movie" false)
                            (need! path)
                            (if display? (need! disp) (remove disp)))
                   :run (fn [_] (reference-scene! "BenchRef"))
                   :teardown (fn [_] (remove disp) (remove path) (set-cfg! "write_to_movie" true))})]
    (merge (warm-render-arm {})
           {:id "zig display 480p15 frames-only"
            :variants [(variant "zig-path" false) (variant "native-display" true)]
            :rounds 5 :warmup 1}
           opts)))

(def arms
  "Arm name -> (fn [opts] arm). --only picks a subset. pyinit, init-launch,
   init-aot and c1-warm need `clojure -M:pyaot-build` first."
  {"warm"        warm-render-arm
   "init"        cold-init-arm
   "first"       first-render-arm
   "svg-copy"    svg-copy-arm
   "glyph-cache" glyph-cache-arm
   "pyinit"      pyinit-arm
   "init-launch" init-launch-arm
   "init-aot"    init-aot-arm
   "c1-warm"     c1-warm-arm
   ;; Warm EDIT: the bench layout rendered once, then each sample changes one
   ;; cell (the same cell sequence for both variants) and renders frames-only
   ;; with svg-copy + glyph-cache, through a Manim backend without vs with a
   ;; desargues.manim.mobject-cache LRU. :ttff-ms is edit-to-first-frame.
   "edit-one-cell"
   (fn [opts]
     (let [svg     @(requiring-resolve 'desargues.manim.patches/svg-path-shared-copy)
           glyph   ((requiring-resolve 'desargues.manim.patches/glyph-cache))
           install (requiring-resolve 'desargues.manim.patches/install!)
           remove  (requiring-resolve 'desargues.manim.patches/uninstall!)
           backend (requiring-resolve 'desargues.scene.manim/manim-backend)
           lru     (requiring-resolve 'desargues.manim.mobject-cache/lru-cache)
           l-text  (requiring-resolve 'desargues.layout.core/text)
           edited  (fn [k] (update-in (bench/layout-tree 16) [:children (inc (quot k 4)) :children (mod k 4) :children 0]
                                      (fn [_] (l-text (str "edited " k)))))
           render! (fn [b tree]
                     (binding [desargues.scene/*backend* b]
                       (let [install! (requiring-resolve 'desargues.manim.frames/install!)
                             uninst!  (requiring-resolve 'desargues.manim.frames/uninstall!)
                             sink     ((requiring-resolve 'desargues.manim.frames/collecting-sink))
                             t0       (System/nanoTime)
                             m        (ab/measure #(do (install! sink)
                                                       (try ((requiring-resolve 'desargues.scene/render-layout!) "BenchEdit" tree :hold 1)
                                                            (finally (uninst!)))))
                             {:keys [first-ns count]} ((requiring-resolve 'desargues.manim.frames/collected) sink)]
                         {::ab/measured (-> (dissoc m :value)
                                            (assoc :frames count :ttff-ms (some-> first-ns (- t0) (/ 1e6))))})))
           variant (fn [nm make-cache]
                     (let [b (delay (backend {:mobject-cache (make-cache)})) n (atom 0)]
                       {:name nm
                        :setup (fn [_]
                                 (set-cfg! "write_to_movie" false) (install svg) (install glyph)
                                 (when (zero? @n) (render! @b (bench/layout-tree 16))))
                        :run (fn [_] (render! @b (edited (mod (dec (swap! n inc)) 16))))
                        :teardown (fn [_] (set-cfg! "write_to_movie" true))}))]
       (merge (warm-render-arm {})
              {:id "edit one cell 480p15 frames-only"
               :variants [(variant "rebuild" (constantly nil)) (variant "mobject-cache" #(lru))]
               :teardown (fn [] (remove glyph) (remove svg))
               :rounds 16 :warmup 2}
              opts)))
   ;; Frames-only warm render, Manim's Python path building vs the Zig one
   ;; (desargues.manim.patches/native-cairo-path; `bb native:path` builds it).
   "zig-path" (fn [opts]
                (let [patch   ((requiring-resolve 'desargues.manim.patches/native-cairo-path))
                      install (requiring-resolve 'desargues.manim.patches/install!)
                      remove  (requiring-resolve 'desargues.manim.patches/uninstall!)
                      variant (fn [nm native?]
                                {:name nm
                                 :setup (fn [_]
                                          (set-cfg! "write_to_movie" false)
                                          (if native?
                                            (when-not (install patch)
                                              (throw (ex-info "native path library not built (bb native:path)"
                                                              {:lib-path (:lib-path patch)})))
                                            (remove patch)))
                                 :run (fn [_] (reference-scene! "BenchRef"))
                                 :teardown (fn [_] (remove patch) (set-cfg! "write_to_movie" true))})]
                  (merge (warm-render-arm {})
                         {:id "zig cairo path 480p15 frames-only"
                          :variants [(variant "manim" false) (variant "zig-path" true)]
                          :rounds 5 :warmup 1}
                         opts)))
   "native-display" native-display-arm})

(def default-arms ["warm" "init"])

;; ── child mode ───────────────────────────────────────────────────────────────

(defn- child!
  "mode: \"py\" | \"full[:import-mode]\" (time init!) | \"render:import-mode\"
   (init! untimed, then time the first reference-scene render) |
   \"warm:import-mode\" (init! and 3 untimed renders, then time 3 renders).
   Also reports the whole process so far: :uptime-ms and :proc-cpu-s."
  [mode]
  (let [{:keys [python-exe library-path]} ((requiring-resolve 'desargues.config/manim-config))
        init!     (requiring-resolve 'desargues.manim.core/init!)
        py-init!  (requiring-resolve 'libpython-clj2.python/initialize!)
        prebuilt  (requiring-resolve 'desargues.manim.core/prebuilt-library)
        [kind im] (str/split mode #":" 2)
        init-opts (cond-> {} im (assoc :import-mode im))
        ready!    (fn []
                    (init! init-opts)
                    (quiet-manim!)
                    ((requiring-resolve 'desargues.scene/configure!) {:quality "low_quality"})
                    (set-cfg! "media_dir" media-dir)
                    (set-cfg! "disable_caching" true)
                    (set-cfg! "write_to_movie" false))
        f (case kind
            "py"     #(py-init! :python-executable python-exe :library-path library-path
                                :library-definition (prebuilt))
            "full"   #(init! init-opts)
            "render" (do (ready!) #(reference-scene! "BenchFirst"))
            "warm"   (do (ready!)
                         (dotimes [_ 3] (reference-scene! "BenchWarmup"))
                         #(dotimes [_ 3] (reference-scene! "BenchWarm"))))
        m (ab/measure f)]
    (println (pr-str {:bench-child (assoc (dissoc m :value)
                                          :uptime-ms (.getUptime (java.lang.management.ManagementFactory/getRuntimeMXBean))
                                          :proc-cpu-s (ab/process-cpu-s))}))
    (flush)
    (System/exit 0)))

;; ── report + main ────────────────────────────────────────────────────────────

(defn report
  "Run the named arms -> the render report map."
  [{:keys [only rounds warmup] :or {only default-arms}}]
  (let [ov      (cond-> {} rounds (assoc :rounds rounds) warmup (assoc :warmup warmup))
        results (mapv (fn [nm] (say "render arm: " nm) (ab/run-arm ((arms nm) ov))) only)]
    (assoc (smp/system-info)
           :suites (vec only)
           :load (ab/host-load)
           :arms results
           :results (into [] (mapcat ab/arm->rows) results))))

(defn- parse-args [args]
  (loop [[a & more] args opts {:out "bench"}]
    (case a
      nil opts
      "--child"  (recur (rest more) (assoc opts :child (first more)))
      "--only"   (recur (rest more) (assoc opts :only (str/split (first more) #",")))
      "--rounds" (recur (rest more) (assoc opts :rounds (parse-long (first more))))
      "--warmup" (recur (rest more) (assoc opts :warmup (parse-long (first more))))
      "--out"    (recur (rest more) (assoc opts :out (first more)))
      (recur more opts))))

(defn print-summary [{:keys [arms]}]
  (pp/print-table [:arm :variant :n :wall-median :wall-min :wall-p90 :cpu-median :ratio-wall :ratio-cpu :load1]
                  (for [{:keys [arm variants]} arms
                        {:keys [variant wall-ms cpu-s ratio load1]} variants]
                    {:arm arm :variant variant :n (:n wall-ms)
                     :wall-median (format "%.0f ms" (:median wall-ms))
                     :wall-min (format "%.0f ms" (:min wall-ms))
                     :wall-p90 (format "%.0f ms" (:p90 wall-ms))
                     :cpu-median (format "%.2f s" (:median cpu-s))
                     :ratio-wall (some->> (:wall ratio) (format "%.3f"))
                     :ratio-cpu (some->> (:cpu ratio) (format "%.3f"))
                     :load1 (some->> (:median load1) (format "%.1f"))})))

(defn -main [& args]
  (let [{:keys [child out] :as opts} (parse-args args)]
    (if child
      (child! child)
      (if-not (manim-env?)
        (do (say "no Manim env resolved (see clojure -M:doctor); nothing to run")
            (shutdown-agents) (System/exit 0))
        (let [rep (report opts)
              f   (io/file out "results" "render.edn")]
          (.mkdirs (.getParentFile f))
          (spit f (with-out-str (pp/pprint rep)))
          (print-summary rep)
          (say "results: " f)
          (shutdown-agents)
          (System/exit 0))))))
