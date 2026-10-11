(ns desargues.bench.hotpaths
  "Hot paths the core suite does not cover: the Lagrangian compile and evolve,
   the Emmy -> LaTeX -> Python conveyor, recording the landing-page scene, the
   800-step pendulum the cljw lane measured, and the raster TrajectorySolver on
   a Lagrangian system (under :dynamics only).

   Every row carries :group :id and the sampler summary; run returns rows."
  (:require [desargues.bench.sample :as smp :refer [say]]
            [desargues.dsl.math :as dmath]
            [desargues.pipeline.emmy :as pipe]
            [desargues.scene :as s]
            [desargues.scene.data :as rec]
            [desargues.videos.lagrangian :as lag]
            [desargues.videos.physics :as phys]
            [emmy.env :as e]))

(def slow
  "Sampler options for subjects that take 0.1 s or more per call."
  {:warmup-ms 2000 :runs 7 :target-ms 1})

(def medium
  "Sampler options for subjects around 10 ms per call."
  {:warmup-ms 1500 :runs 15 :target-ms 100})

(defn- row [group id stats] (merge {:group group :id id} stats))

(defn- raster-solver [alg]
  (when-let [f (try (requiring-resolve 'desargues.infrastructure.raster-adapter/raster-solver)
                    (catch Throwable _ nil))]
    (f alg)))

;; ── subjects ────────────────────────────────────────────────────────────────

(def cljw-pendulum
  "The damped pendulum doc/cljw-lane.md times: 800 RK4 steps (dt 0.01, 8 s)."
  (phys/make-pendulum {:length 3.0 :damping 0.12 :initial-theta 1.1 :top-point [0 2.2 0]}))

(def spherical-opts
  "The landing page's spherical double pendulum (site/desargues/site/pendulum3d)."
  {:l1 1.4 :l2 1.2 :theta1 1.3 :phi1 0.0 :theta2 1.6 :phi2 0.8 :phi1-dot 1.4 :phi2-dot 0.9})

(def python-functions
  "Expressions sent down the full Emmy -> LaTeX -> Python conveyor."
  [["sin x"         (e/sin 'x)]
   ["exp(sin x)"    (e/exp (e/sin 'x))]
   ["x⁵ + 3x³ − 2x" (e/+ (e/expt 'x 5) (e/* 3 (e/expt 'x 3)) (e/* -2 'x) 1)]
   ["(x²+1)/sqrt x" (e// (e/+ (e/square 'x) 1) (e/sqrt 'x))]])

(defn- landing-construct
  "The landing-page scene's construct fn, or nil when site/ is off the classpath."
  []
  (try (some-> (requiring-resolve 'desargues.site.pendulum3d/construct) deref)
       (catch Throwable _ nil)))

(defn record-landing
  "Record the landing-page scene through a fresh RecordingBackend."
  [construct]
  (s/with-backend (rec/recording-backend)
    (s/render! "desargues-double-pendulum" construct)))

;; ── runs ────────────────────────────────────────────────────────────────────

(defn run-physics []
  (say "physics: cljw-lane pendulum, 800 steps")
  (let [opts {:dt 0.01 :duration 8.0}]
    (cond-> [(row :physics "clojure rk4 x800"
                  (smp/sample #(count (phys/evolve cljw-pendulum (assoc opts :solver (phys/stepper-solver phys/rk4)))) medium))
             (row :physics "clojure euler x800"
                  (smp/sample #(count (phys/evolve cljw-pendulum (assoc opts :solver (phys/stepper-solver phys/euler)))) medium))]
      (raster-solver :rk4)
      (conj (row :physics "raster rk4 x800"
                 (smp/sample #(count (phys/evolve cljw-pendulum (assoc opts :solver (raster-solver :rk4)))) medium))))))

(defn run-lagrangian []
  (say "lagrangian: compile")
  (let [planar (lag/double-pendulum {})
        sph    (lag/spherical-double-pendulum spherical-opts)
        opts   {:dt 0.01 :duration 8.0}]
    (cond-> [(row :lagrangian "compile planar" (smp/sample #(lag/double-pendulum {}) slow))
             (row :lagrangian "compile spherical" (smp/sample #(lag/spherical-double-pendulum spherical-opts) slow))
             (do (say "lagrangian: evolve 8 s")
                 (row :lagrangian "evolve planar rk4 x800" (smp/sample #(count (phys/evolve planar opts)) medium)))
             (row :lagrangian "evolve spherical rk4 x800" (smp/sample #(count (phys/evolve sph opts)) medium))]
      (raster-solver :rk4)
      (into [(row :lagrangian "evolve planar raster-rk4 x800"
                  (smp/sample #(count (phys/evolve planar (assoc opts :solver (raster-solver :rk4)))) medium))
             (row :lagrangian "evolve spherical raster-rk4 x800"
                  (smp/sample #(count (phys/evolve sph (assoc opts :solver (raster-solver :rk4)))) medium))]))))

(defn run-pipeline []
  (say "pipeline: Emmy -> LaTeX -> Python")
  (mapv (fn [[id x]] (row :pipeline id (smp/sample #(pipe/python-code x dmath/latex->python))))
        python-functions))

(defn run-scene []
  (if-let [construct (landing-construct)]
    (do (say "scene: record the landing-page pendulum")
        (let [graph (record-landing construct)]
          [(row :scene "record landing pendulum" (smp/sample #(record-landing construct) medium))
           (row :scene "landing graph -> EDN" (smp/sample #(count (pr-str graph)) medium))]))
    (do (say "scene: site/ not on the classpath; skipped") [])))

(defn run
  "Every hot-path row."
  []
  (into [] cat [(run-physics) (run-lagrangian) (run-pipeline) (run-scene)]))
