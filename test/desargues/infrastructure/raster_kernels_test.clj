(ns desargues.infrastructure.raster-kernels-test
  "The raster kernel solver (whole RK4 inside a deftm) against the pure array
   path. The tolerance proven here is ZERO: the kernels perform the stepper's
   IEEE operations in the stepper's order, so trajectories are compared with
   `=`, times included. evolve-batch's mapping path runs without :dynamics."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.physics :as lag]
            [desargues.physics :as phys]))

(def ^:private kernel-solver
  "raster-adapter/kernel-solver, or nil when raster is not on the classpath
   (a load failure that is not about raster is rethrown)."
  (try (requiring-resolve 'desargues.infrastructure.raster-adapter/kernel-solver)
       (catch Throwable t
         (let [root (loop [e t] (if-let [c (.getCause e)] (recur c) e))]
           (when-not (re-find #"raster" (str (.getMessage root)))
             (throw t)))
         (println "[raster-kernels-test] raster not on classpath (use -A:dynamics); skipping")
         nil)))

(def opts {:dt 0.01 :duration 3.0})

(def pendulum (phys/make-pendulum {:length 3.0 :damping 0.12 :initial-theta 1.1}))

(def spherical-opts
  {:l1 1.4 :l2 1.2 :theta1 1.3 :phi1 0.0 :theta2 1.6 :phi2 0.8 :phi1-dot 1.4 :phi2-dot 0.9})

(deftest evolve-batch-maps-evolve
  (let [ps [pendulum (phys/make-pendulum {:length 2.0 :initial-theta 0.4})]]
    (is (= (mapv #(vec (phys/evolve % opts)) ps) (phys/evolve-batch ps opts)))
    (is (= [] (phys/evolve-batch [] opts)))
    (is (thrown? clojure.lang.ExceptionInfo (phys/evolve-batch ps {:dt Double/NaN})))))

(deftest kernel-solver-is-bit-identical-to-the-array-path
  (if-let [mk kernel-solver]
    (let [ks (mk)]
      (doseq [[label s] [["pendulum" pendulum]
                         ["planar double pendulum" (lag/double-pendulum {})]
                         ["spherical double pendulum (chunked kernel)" (lag/spherical-double-pendulum spherical-opts)]]]
        (testing label
          (is (= (vec (phys/evolve s opts)) (phys/evolve s (assoc opts :solver ks))))))
      (testing "a duration that is not a multiple of dt keeps the stepper's time grid"
        (is (= (vec (phys/evolve pendulum {:dt 0.03 :duration 1.0}))
               (phys/evolve pendulum {:dt 0.03 :duration 1.0 :solver ks})))))
    (is true)))

(deftest kernel-solver-batches
  (if-let [mk kernel-solver]
    (let [ks (mk)
          sweep (vec (for [i (range 6)]
                       (phys/make-pendulum {:length (+ 1.0 (* 0.5 i)) :damping (* 0.05 i)
                                            :initial-theta (+ 0.2 (* 0.1 i))})))
          planars (vec (for [i (range 4)] (lag/double-pendulum {:theta1 (+ 1.5 (* 0.1 i))})))
          mixed [pendulum (lag/double-pendulum {}) (phys/make-spring-mass {:initial-x 1.0}) (lag/double-pendulum {:theta2 1.0})]]
      (testing "a parameter sweep is one kernel call and answers what evolve does"
        (is (= (mapv #(vec (phys/evolve % opts)) sweep) (phys/evolve-batch sweep (assoc opts :solver ks)))))
      (testing "N initial conditions of one Lagrangian"
        (is (= (mapv #(vec (phys/evolve % opts)) planars) (phys/evolve-batch planars (assoc opts :solver ks)))))
      (testing "mixed kinds keep their order; a system without a kernel goes to the stepper"
        (is (= (mapv #(vec (phys/evolve % opts)) mixed) (phys/evolve-batch mixed (assoc opts :solver ks))))))
    (is true)))

(deftest kernel-rhs-feeds-raster-solvers
  (if kernel-solver
    (let [raster-solver @(requiring-resolve 'desargues.infrastructure.raster-adapter/raster-solver)
          planar (lag/double-pendulum {})
          slow (lag/double-pendulum {:compile? false})
          short {:dt 0.01 :duration 1.0}
          labels [:theta1 :theta2 :theta1-dot :theta2-dot]
          max-diff (fn [a b] (reduce max (map (fn [pa pb] (reduce max (map #(Math/abs (- (get-in pa [:state %]) (get-in pb [:state %]))) labels))) a b)))]
      (testing "raster's own RK4 on the kernel RHS tracks the stepper to rounding (1 s: the system is chaotic)"
        (let [a (phys/evolve planar short)
              b (phys/evolve planar (assoc short :solver (raster-solver :rk4)))]
          (is (= (count a) (count b)))
          (is (< (max-diff a b) 1e-12))))
      (testing "an uncompiled system integrates through the map bridge to the same values"
        (let [a (phys/evolve planar (assoc short :solver (raster-solver :rk4)))
              b (phys/evolve slow (assoc short :solver (raster-solver :rk4)))]
          (is (< (max-diff a b) 1e-12)))))
    (is true)))
