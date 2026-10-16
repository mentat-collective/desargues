(ns desargues.bench
  "Benchmarks of desargues, one report: the pure pipeline (Emmy conveyor,
   layout algebra, recording backend, trajectory solvers), the hot paths
   (desargues.bench.hotpaths) and the geometry layer (desargues.bench.geometry),
   plus an opt-in Manim render timing.

     clojure -M:bench[:dynamics] [--only core,hotpaths,geometry] [--manim]
                                 [--charts-only] [--out DIR]

   Writes DIR/results/latest.edn (DIR defaults to bench) and the SVG charts
   under doc/bench (desargues.bench.chart). :dynamics adds the raster solvers
   and the raster geometry kernel; --manim renders a small layout through
   Manim (needs the conda env). desargues.bench.check compares a run against
   bench/results/baseline.edn."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [desargues.bench.chart :as chart]
            [desargues.bench.geometry :as geo]
            [desargues.bench.hotpaths :as hot]
            [desargues.bench.sample :as smp]
            [desargues.layout.core :as l]
            [desargues.layout.measure :as measure]
            [desargues.pipeline.emmy :as pipe]
            [desargues.scene :as s]
            [desargues.scene.data :as rec]
            [desargues.physics :as phys]
            [emmy.env :as e]
            [desargues.bench.check :as check])
  (:gen-class))

;; ── sampler ─────────────────────────────────────────────────────────────────

(def sample
  "The shared sampler (desargues.bench.sample/sample)."
  smp/sample)

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

(def ^:private say smp/say)

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
                (let [ms (smp/time-once #(s/render-layout! (str "Bench" label) (layout-tree 16) :hold 1))]
                  (row :manim label nil (assoc (smp/summarize [ms]) :batch 1))))
              [["low_quality" "480p15"] ["medium_quality" "720p30"] ["high_quality" "1080p60"]]))))

(def system-info smp/system-info)

(def suites
  "Suite name -> thunk returning rows. --only picks a subset; Manim is opt-in."
  {"core"     #'run-pure
   "hotpaths" #'hot/run
   "geometry" #'geo/run})

(def default-suites ["core" "hotpaths" "geometry"])

(defn report
  "Run the named suites (Manim only when manim? is true) into one report map."
  [{:keys [manim? only] :or {only default-suites}}]
  (assoc (system-info)
         :suites (vec only)
         :results (into (into [] (mapcat #((suites %))) only)
                        (when manim? (run-manim)))))

;; ── main ────────────────────────────────────────────────────────────────────

(defn- print-table [{:keys [results]}]
  (pp/print-table [:group :id :provider :n :median :mad :mean :batch]
                  (for [r results]
                    {:group (:group r) :id (:id r) :provider (:provider r) :n (:n r)
                     :median (some-> (:median-ms r) chart/fmt-ms)
                     :mad (some-> (:mad-ms r) chart/fmt-ms)
                     :mean (chart/fmt-ms (:mean-ms r))
                     :batch (:batch r)})))

(defn- parse-args [args]
  (loop [[a & more] args opts {:out "bench"}]
    (case a
      nil opts
      "--manim"         (recur more (assoc opts :manim? true))
      "--charts-only"   (recur more (assoc opts :charts-only? true))
      "--check"         (recur more (assoc opts :check? true))
      "--save-baseline" (recur more (assoc opts :save-baseline? true))
      "--tolerance"     (recur (rest more) (assoc opts :tolerance (parse-double (first more))))
      "--out"           (recur (rest more) (assoc opts :out (first more)))
      "--only"          (recur (rest more) (assoc opts :only (str/split (first more) #",")))
      (recur more opts))))

(defn- write-edn! [f x]
  (.mkdirs (.getParentFile (io/file f)))
  (spit f (with-out-str (pp/pprint x))))

(defn -main
  "bb bench: run, write latest.edn and the charts. --check: also compare
   against baseline.edn and exit 1 on a regression. --save-baseline: write the
   run as the new baseline.edn. --charts-only: reuse latest.edn."
  [& args]
  (let [{:keys [out manim? charts-only? only check? save-baseline? tolerance]} (parse-args args)
        results-file  (io/file out "results" "latest.edn")
        baseline-file (io/file out "results" "baseline.edn")
        rep (if charts-only?
              (edn/read-string (slurp results-file))
              (doto (report (cond-> {:manim? manim?} only (assoc :only only)))
                (->> (write-edn! results-file))))
        verdict (when check?
                  (check/compare-reports (edn/read-string (slurp baseline-file)) rep
                                         (cond-> {} tolerance (assoc :tolerance tolerance))))]
    (print-table rep)
    (say "results: " results-file)
    (say "charts:  doc/bench/" (chart/write-all! rep "doc/bench"))
    (when save-baseline?
      (write-edn! baseline-file rep)
      (say "baseline: " baseline-file))
    (when verdict (run! say (check/summary-lines verdict)))
    (shutdown-agents)
    (System/exit (if (and verdict (not (:ok? verdict))) 1 0))))
