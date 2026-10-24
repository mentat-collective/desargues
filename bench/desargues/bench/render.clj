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

(defn- child-cmd
  "argv that runs this namespace's child mode in a fresh JVM on our classpath."
  [mode]
  [(str (io/file (System/getProperty "java.home") "bin" "java"))
   "--enable-native-access=ALL-UNNAMED"
   "-cp" (System/getProperty "java.class.path")
   "clojure.main" "-m" "desargues.bench.render" "--child" mode])

(defn- run-child
  "Run a child, return its self-measurement as {::ab/measured {...}}, plus
   the child's whole-process wall time as :process-wall-ms. The child prints
   one line {:bench-child {:wall-ms :cpu-s}} (an unqualified key, so pr-str
   does not turn it into #:ns{} map syntax)."
  [mode]
  (let [t0   (System/nanoTime)
        {:keys [exit out err]} (apply sh/sh (child-cmd mode))
        pw   (/ (- (System/nanoTime) t0) 1e6)
        line (some->> (str/split-lines out) (filter #(str/starts-with? % "{:bench-child")) last)]
    (when (or (not (zero? exit)) (nil? line))
      (throw (ex-info "bench child failed"
                      {:mode mode :exit exit
                       :err (str/join "\n" (take-last 20 (str/split-lines err)))})))
    {::ab/measured (assoc (:bench-child (edn/read-string line)) :process-wall-ms pw)}))

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

(def arms
  "Arm name -> (fn [opts] arm). --only picks a subset."
  {"warm"  warm-render-arm
   "init"  cold-init-arm
   "first" first-render-arm})

(def default-arms ["warm" "init"])

;; ── child mode ───────────────────────────────────────────────────────────────

(defn- child!
  "mode: \"py\" | \"full[:import-mode]\" (time init!) | \"render:import-mode\"
   (init! untimed, then time the first reference-scene render)."
  [mode]
  (let [{:keys [python-exe library-path]} ((requiring-resolve 'desargues.config/manim-config))
        init!     (requiring-resolve 'desargues.manim.core/init!)
        py-init!  (requiring-resolve 'libpython-clj2.python/initialize!)
        [kind im] (str/split mode #":" 2)
        init-opts (cond-> {} im (assoc :import-mode im))
        f (case kind
            "py"     #(py-init! :python-executable python-exe :library-path library-path)
            "full"   #(init! init-opts)
            "render" (do (init! init-opts)
                         (quiet-manim!)
                         ((requiring-resolve 'desargues.scene/configure!) {:quality "low_quality"})
                         (set-cfg! "media_dir" media-dir)
                         (set-cfg! "disable_caching" true)
                         #(reference-scene! "BenchFirst")))
        m (ab/measure f)]
    (println (pr-str {:bench-child (dissoc m :value)}))
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
