(ns desargues.infrastructure.raster-adapter-test
  "Conformance of the raster TrajectorySolver against the built-in stepper.

   The adapter is a test double's opposite: a real provider that must agree
   with the contract the stepper already satisfies. So the SAME conformance
   checks run against both providers; raster's run is skipped (loudly) when
   the :dynamics alias is not on the classpath."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.videos.physics :as phys]))

(def ^:private raster-solver
  "The raster provider, or nil when raster is not on the classpath. A load
   failure other than a missing raster is REPORTED, never swallowed: a
   provider that fails to compile must not read as 'absent'."
  (try (require 'desargues.infrastructure.raster-adapter)
       (resolve 'desargues.infrastructure.raster-adapter/raster-solver)
       (catch Throwable t
         (let [root (loop [e t] (if-let [c (.getCause e)] (recur c) e))]
           (println "[raster-adapter-test] adapter not loaded:" (.getMessage root))
           (when-not (re-find #"raster" (str (.getMessage root)))
             (throw t)))
         nil)))

(def pendulum
  (phys/make-pendulum {:length 3.0 :damping 0.1 :initial-theta 0.3}))

(defn pendulum-energy [{:keys [length gravity]} {:keys [theta omega]}]
  (+ (* 0.5 length length omega omega)
     (* gravity length (- 1 (Math/cos theta)))))

(defn conformance
  "Contract every TrajectorySolver must satisfy on a damped pendulum."
  [label solver]
  (let [traj (phys/evolve pendulum {:solver solver :dt 0.01 :duration 2.0})
        energies (map #(pendulum-energy pendulum (:state %)) traj)]
    (testing (str label ": starts at t=0 with the initial state")
      (is (= {:time 0.0 :state {:theta 0.3 :omega 0.0}} (first traj))))
    (testing (str label ": reaches the requested duration")
      (is (< (Math/abs (- 2.0 (:time (last traj)))) 1e-9)))
    (testing (str label ": time is strictly increasing")
      (is (every? (fn [[a b]] (< (:time a) (:time b))) (partition 2 1 traj))))
    (testing (str label ": every point carries every state label")
      (is (every? #(= #{:theta :omega} (set (keys (:state %)))) traj)))
    (testing (str label ": damping dissipates energy")
      (is (< (last energies) (first energies))))))

(deftest stepper-conforms
  (conformance "stepper/rk4" (phys/stepper-solver phys/rk4)))

(deftest raster-conforms
  (if raster-solver
    (do (conformance "raster/rk4" (raster-solver :rk4))
        (conformance "raster/tsit5" (raster-solver :tsit5)))
    (do (println "[raster-adapter-test] raster not on classpath (use -A:dynamics); skipping")
        (is true))))

(deftest raster-rk4-agrees-with-stepper-rk4
  (if raster-solver
    (let [a (phys/evolve pendulum {:solver (phys/stepper-solver phys/rk4) :dt 0.01 :duration 2.0})
          b (phys/evolve pendulum {:solver (raster-solver :rk4) :dt 0.01 :duration 2.0})]
      (is (= (count a) (count b)))
      (is (every? (fn [[pa pb]]
                    (< (Math/abs (- (get-in pa [:state :theta])
                                    (get-in pb [:state :theta])))
                       1e-8))
                  (map vector a b))
          "same fixed-step RK4 scheme -> same trajectory to floating precision"))
    (is true)))

(deftest raster-adaptive-tracks-stepper
  (if raster-solver
    (let [ref (phys/evolve pendulum {:solver (phys/stepper-solver phys/rk4) :dt 0.001 :duration 2.0})
          ada (phys/evolve pendulum {:solver (raster-solver :tsit5) :dt 0.01 :duration 2.0})]
      ;; tsit5 defaults to rtol 1e-3 (raster.ode/tsit5), so the agreement bound
      ;; is the solver's own relative tolerance, not a fixed-step artefact.
      ;; Measured 2026-09-06: 2.1e-4 drift at t=2 on the damped pendulum.
      (is (< (Math/abs (- (get-in (last ref) [:state :theta])
                          (get-in (last ada) [:state :theta])))
             1e-3)
          "adaptive tsit5 lands within its rtol (1e-3) of a fine-grained RK4 reference"))
    (is true)))

(deftest raster-adaptive-resamples-onto-dt-grid
  (if raster-solver
    (let [opts {:dt 0.01 :duration 2.0}
          rk4  (phys/evolve pendulum (assoc opts :solver (raster-solver :rk4)))
          ada  (phys/evolve pendulum (assoc opts :solver (raster-solver :tsit5)))
          dp5  (phys/evolve pendulum (assoc opts :solver (raster-solver :dp5)))
          raw  (phys/evolve pendulum (assoc opts :solver (raster-solver :tsit5 {:resample? false})))
          same-grid? (fn [a b]
                       (and (= (count a) (count b))
                            (every? (fn [[pa pb]] (< (Math/abs (- (:time pa) (:time pb))) 1e-9))
                                    (map vector a b))))]
      (is (same-grid? rk4 ada) "resampled tsit5 lands on the fixed-step dt grid")
      (is (same-grid? rk4 dp5) "resampled dp5 lands on the fixed-step dt grid")
      (is (< (count raw) (count ada)) "resample? false returns the solver's own accepted steps")
      (is (every? (fn [[pa pb]]
                    (< (Math/abs (- (get-in pa [:state :theta]) (get-in pb [:state :theta]))) 1e-3))
                  (map vector rk4 ada))
          "interpolated points stay within tsit5's rtol of RK4 at every grid time"))
    (is true)))
