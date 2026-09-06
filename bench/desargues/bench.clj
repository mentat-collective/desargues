(ns desargues.bench
  "Micro-benchmarks of the pure pipeline: the Emmy conveyor, the layout algebra,
   the recording backend and the trajectory solvers, plus an opt-in Manim
   render timing.

     clojure -M:bench[:dynamics] [--manim] [--charts-only] [--out DIR]

   Writes DIR/results/latest.edn (DIR defaults to bench) and the SVG charts
   under doc/bench (desargues.bench.chart). :dynamics adds the raster solvers;
   --manim renders a small layout through Manim (needs the conda env)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [desargues.bench.chart :as chart]
            [desargues.layout.core :as l]
            [desargues.layout.measure :as measure]
            [desargues.pipeline.emmy :as pipe]
            [desargues.scene :as s]
            [desargues.scene.data :as rec]
            [desargues.videos.physics :as phys]
            [emmy.env :as e])
  (:gen-class))

;; ── sampler ─────────────────────────────────────────────────────────────────

(defn- now-ms [] (/ (System/nanoTime) 1e6))

(defn- time-once
  "Wall time of one call of f, in ms."
  [f]
  (let [t0 (System/nanoTime)] (f) (/ (- (System/nanoTime) t0) 1e6)))

(defn sample
  "Time thunk f: warm up for `warmup-ms`, then take `runs` samples, each a batch
   of calls sized so one sample lasts about `target-ms`.
   Returns per-call {:mean-ms :std-ms :min-ms :runs :batch}."
  ([f] (sample f {}))
  ([f {:keys [warmup-ms runs target-ms] :or {warmup-ms 400 runs 30 target-ms 40}}]
   (let [deadline (+ (now-ms) warmup-ms)]
     (while (< (now-ms) deadline) (f)))
   (let [t1    (max 1e-6 (time-once f))
         batch (long (max 1 (Math/ceil (/ target-ms t1))))
         one   (fn [] (/ (time-once (fn [] (dotimes [_ batch] (f)))) batch))
         _     (System/gc)
         xs    (vec (repeatedly runs one))
         mean  (/ (reduce + xs) runs)
         var   (/ (reduce + (map (fn [x] (let [d (- x mean)] (* d d))) xs)) runs)]
     {:mean-ms mean :std-ms (Math/sqrt var) :min-ms (reduce min xs) :runs runs :batch batch})))

;; ── subjects ────────────────────────────────────────────────────────────────

(def emmy-functions
  "Label -> Emmy function. The conveyor differentiates and renders both to LaTeX."
  [["sin x"          (fn [x] (e/sin x))]
   ["exp(sin x)"     (fn [x] (e/exp (e/sin x)))]
   ["x⁵ + 3x³ − 2x"  (fn [x] (e/+ (e/expt x 5) (e/* 3 (e/expt x 3)) (e/* -2 x) 1))]
   ["sin(x² cos x)"  (fn [x] (e/sin (e/* (e/square x) (e/cos x))))]])

(defn layout-tree
  "A title over a 4-column grid of `n` bordered cells (text and math alternating)."
  [n]
  (let [cols 4
        rows (max 1 (quot n cols))]
    (apply l/column {:width :fill :height :fill :padding 0.5 :spacing 0.2 :align :center-x}
           (l/text "Benchmark" {:font-size 36})
           (for [r (range rows)]
             (apply l/row {:width :fill :spacing 0.2}
                    (for [c (range cols)]
                      (l/box {:width [:portion 1] :border :white}
                        (if (even? (+ r c))
                          (l/text (str "cell " r "," c))
                          (l/math "\\alpha_{ij}")))))))))

(def frame {:x 0 :y 0 :w 13.222222 :h 7.0})

(defn resolve-thunk [tree]
  (let [m (measure/make-measure (l/collect-styles tree))]
    (fn [] (l/resolve-layout tree frame m))))

(defn imperative-scene
  "A construct fn with about 3k+2 animations: k circles appear staggered, k
   counters count up together, then every circle glides to the origin."
  [k]
  (fn [stage]
    (let [cs (mapv (fn [i] (-> (s/circle :radius 0.2 :color :teal)
                               (s/move-to [(- (mod i 12) 6) (- (quot i 12) 3)])))
                   (range k))
          ds (mapv (fn [c] (-> (s/decimal 0 :font-size 24) (s/next-to c :down)))
                   cs)]
      (s/play! stage (s/stagger (map s/appear cs) :lag-ratio 0.05))
      (s/play! stage (s/together (map-indexed (fn [i d] (s/count-to d i)) ds)))
      (doseq [c cs] (s/play! stage (s/glide c [0 0])))
      (s/hold! stage 1))))

(defn record
  "Render `construct` through a fresh recording backend (pure, no Python)."
  [construct]
  (s/with-backend (rec/recording-backend)
    (s/render! "bench" construct)))

(def pendulum (phys/make-pendulum {:length 3.0 :damping 0.1 :initial-theta 0.3}))
(def solve-opts {:dt 0.01 :duration 10.0})

(defn- raster-solver [alg]
  (when-let [f (try (requiring-resolve 'desargues.infrastructure.raster-adapter/raster-solver)
                    (catch Throwable _ nil))]
    (f alg)))

(defn solvers
  "Label -> TrajectorySolver. The raster ones appear only under :dynamics."
  []
  (cond-> [["clojure euler" (phys/stepper-solver phys/euler)]
           ["clojure rk4"   (phys/stepper-solver phys/rk4)]]
    (raster-solver :rk4)
    (into [["raster euler" (raster-solver :euler)]
           ["raster rk4"   (raster-solver :rk4)]
           ["raster tsit5" (raster-solver :tsit5)]
           ["raster dp5"   (raster-solver :dp5)]])))

;; ── runs ────────────────────────────────────────────────────────────────────

(defn- say [& xs] (println (apply str xs)) (flush))

(defn- row [group id n stats] (merge {:group group :id id :n n} stats))

(defn run-pure
  "Every benchmark that needs only the JVM."
  []
  (say "emmy conveyor")
  (let [emmy (mapv (fn [[id f]]
                     (say "  " id)
                     (row :emmy id nil (sample #(pipe/derivative-spec f))))
                   emmy-functions)
        sizes [4 16 64 256 1024]
        _      (say "layout: resolve-layout")
        layout (mapv (fn [n]
                       (say "  n = " n)
                       (row :layout nil n (sample (resolve-thunk (layout-tree n)))))
                     sizes)
        _      (say "recording backend: layout->scene-graph")
        graph  (mapv (fn [n]
                       (say "  n = " n)
                       (let [tree (layout-tree n)]
                         (row :scene-graph nil n (sample #(rec/layout->scene-graph "b" tree {})))))
                     sizes)
        _      (say "recording backend: imperative render!")
        recd   (mapv (fn [k]
                       (say "  animations = " (+ 2 (* 3 k)))
                       (row :recording nil (+ 2 (* 3 k)) (sample #(record (imperative-scene k)))))
                     [4 16 64 256 1024])
        _      (say "trajectory solvers")
        solv   (mapv (fn [[id solver]]
                       (say "  " id)
                       (row :solver id nil
                            (sample #(count (phys/evolve pendulum (assoc solve-opts :solver solver)))
                                    {:runs 10 :target-ms 200})))
                     (solvers))]
    (into [] cat [emmy layout graph recd solv])))

(defn run-manim
  "Wall time of one Manim render per quality, or [] with a note when no env resolves."
  []
  (say "manim render")
  (if-not (try ((requiring-resolve 'desargues.config/manim-config)) (catch Throwable _ nil))
    (do (say "  no Manim env resolved (see clojure -M:doctor); skipped") [])
    (do (s/init!)
        (mapv (fn [[q label]]
                (say "  " label)
                (s/configure! {:quality q})
                (let [ms (time-once #(s/render-layout! (str "Bench" label) (layout-tree 16) :hold 1))]
                  (row :manim label nil {:mean-ms ms :std-ms 0.0 :min-ms ms :runs 1 :batch 1})))
              [["low_quality" "480p15"] ["medium_quality" "720p30"] ["high_quality" "1080p60"]]))))

(defn system-info []
  {:generated (str (java.time.Instant/now))
   :jvm       (System/getProperty "java.version")
   :clojure   (clojure-version)
   :os        (str (System/getProperty "os.name") " " (System/getProperty "os.arch"))
   :cpu       (str (.availableProcessors (Runtime/getRuntime)) " cores")})

(defn report
  "Run everything (Manim only when manim? is true) into one report map."
  [{:keys [manim?]}]
  (assoc (system-info)
         :results (into (run-pure) (when manim? (run-manim)))))

;; ── main ────────────────────────────────────────────────────────────────────

(defn- print-table [{:keys [results]}]
  (pp/print-table [:group :id :n :mean :std :batch]
                  (for [r results]
                    {:group (:group r) :id (:id r) :n (:n r)
                     :mean (chart/fmt-ms (:mean-ms r)) :std (chart/fmt-ms (:std-ms r))
                     :batch (:batch r)})))

(defn- parse-args [args]
  (loop [[a & more] args opts {:out "bench"}]
    (case a
      nil opts
      "--manim"       (recur more (assoc opts :manim? true))
      "--charts-only" (recur more (assoc opts :charts-only? true))
      "--out"         (recur (rest more) (assoc opts :out (first more)))
      (recur more opts))))

(defn -main [& args]
  (let [{:keys [out manim? charts-only?]} (parse-args args)
        results-file (io/file out "results" "latest.edn")
        rep (if charts-only?
              (edn/read-string (slurp results-file))
              (let [r (report {:manim? manim?})]
                (.mkdirs (.getParentFile results-file))
                (spit results-file (with-out-str (pp/pprint r)))
                r))]
    (print-table rep)
    (say "results: " results-file)
    (say "charts:  doc/bench/" (chart/write-all! rep "doc/bench"))
    (shutdown-agents)
    (System/exit 0)))
