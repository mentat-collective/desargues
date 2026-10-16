(ns desargues.physics.strata-test
  "The physics bounded context, stratum by stratum: lower strata never
   require upper ones, the Oscillator port replaces the old type switch, the
   array fast paths stay bit-identical to the generic step, and the facade
   re-exports the same values the strata define."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.physics :as phys]
            [desargues.physics.arrays :as arrays]
            [desargues.physics.forces :as forces]
            [desargues.physics.harmonic :as harmonic]
            [desargues.physics.integrators :as integrators]
            [desargues.physics.protocols :as p]
            [desargues.physics.solver :as solver]
            [desargues.physics.stepper :as stepper]
            [desargues.physics.systems :as systems]
            [desargues.physics.trajectory :as trajectory]))

(def ^:private rank
  "Stratum rank of each namespace of the context: a namespace may only
   require namespaces of strictly lower or equal rank... and never the facade."
  '{desargues.physics.protocols   0
    desargues.physics.systems     1
    desargues.physics.forces      1
    desargues.physics.specs       1
    desargues.physics.arrays      2
    desargues.physics.integrators 3
    desargues.physics.stepper     4
    desargues.physics.harmonic    2
    desargues.physics.trajectory  2
    desargues.physics.lagrangian  2
    desargues.physics.pendulums   3
    desargues.physics.solver      5
    desargues.physics.evolution   6
    desargues.physics             7})

(defn- physics-deps [ns-sym]
  (->> (ns-aliases (the-ns ns-sym)) vals (map ns-name) (filter rank) set))

(deftest lower-strata-never-require-upper-strata
  (doseq [[ns-sym r] rank
          :when (find-ns ns-sym)
          dep (physics-deps ns-sym)]
    (is (< (rank dep) r) (str ns-sym " (rank " r ") requires " dep " (rank " (rank dep) ")"))))

(deftest oscillator-port
  (testing "systems answer their linearised frequency and damping"
    (is (= (Math/sqrt (/ 9.8 2.0)) (harmonic/natural-frequency (systems/make-pendulum {:length 2.0}))))
    (is (= (Math/sqrt (/ 4.0 1.0)) (harmonic/natural-frequency (systems/make-spring-mass {:k 4.0}))))
    (is (= (/ 0.2 (* 2 (Math/sqrt (* 4.0 1.0))))
           (harmonic/damping-ratio (systems/make-spring-mass {:k 4.0 :mu 0.2}))))
    (is (= :underdamped (harmonic/damping-type (systems/make-pendulum {})))))
  (testing "anything else is an unknown system, as before"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown system type"
                          (harmonic/natural-frequency {:k 1})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown system type"
                          (harmonic/period nil)))))

(deftest analytic-solutions
  (is (= 1.0 (harmonic/harmonic-solution {:x0 1.0 :v0 0.0 :omega 2.0} 0.0)))
  (is (< (Math/abs (- (harmonic/harmonic-solution {:x0 1.0 :v0 0.0 :omega 2.0} Math/PI) 1.0)) 1e-12))
  (is (= 1.0 (harmonic/damped-harmonic-solution {:x0 1.0 :v0 0.0 :omega 2.0 :gamma 0.1} 0.0))))

(deftest rk4-array-step-equals-generic-step
  (let [sys (systems/make-pendulum {:damping 0.1 :initial-theta 1.2})
        st (p/get-state sys)
        fast (arrays/rk4-fast-step sys st 0.01)
        generic (p/get-state (p/step integrators/rk4 (systems/make-pendulum {:damping 0.1 :initial-theta 6/5}) 1/100))]
    (is (some? fast))
    (is (nil? (arrays/rk4-fast-step sys st 1/100)) "a rational dt takes the generic step")
    (is (every? number? (vals generic)))))

(deftest stepper-and-integrators
  (let [sys (systems/make-spring-mass {:initial-x 1.0})]
    (is (= "Runge-Kutta 4" (p/integrator-name integrators/rk4)))
    (is (= "Euler" (p/integrator-name integrators/euler)))
    (is (integrators/rk4? integrators/rk4))
    (is (not (integrators/rk4? integrators/euler)))
    (is (= 5 (count (p/solve-trajectory (stepper/stepper-solver) sys {:dt 0.25, :duration 1.0}))))
    (is (= (stepper/stepper-solver) (solver/solver :jvm)))
    (is (= (stepper/stepper-solver integrators/euler) (solver/->solver [:jvm {:integrator integrators/euler}])))))

(deftest forces
  (let [f (forces/combine-forces (forces/spring-force 2.0) (forces/damping-force 0.5) (forces/gravity-force 1.0))]
    (is (= (+ (- (* 2.0 1.0)) (- (* 0.5 2.0)) -1.0) (p/force-value f nil {:x 1.0 :v 2.0})))
    (is (= "Composite[Spring(k=2.0) + Damping(μ=0.5) + Gravity(g=1.0)]" (p/force-name f)))))

(deftest trajectory-transforms
  (let [sys (systems/make-pendulum {})
        traj [{:time 0.0 :state {:theta 1.0 :omega 0.0}}
              {:time 0.5 :state {:theta 0.5 :omega -1.0}}
              {:time 1.0 :state {:theta 0.0 :omega -2.0}}]]
    (is (= {:xs [0.0 0.5 1.0] :ys [1.0 0.5 0.0] :label "theta"} (trajectory/trajectory->graph-data traj :theta)))
    (is (= [[1.0 0.0] [0.5 -1.0] [0.0 -2.0]] (trajectory/phase-portrait sys traj)))
    (is (= [(first traj) (last traj)] (trajectory/sample-trajectory traj 2)))
    (is (= 3 (count (trajectory/trajectory->parametric-data sys traj systems/pendulum-position))))))

(deftest facade-re-exports-the-strata
  (is (identical? integrators/rk4 phys/rk4))
  (is (identical? integrators/euler phys/euler))
  (is (= (systems/make-pendulum {}) (phys/make-pendulum {})))
  (is (= (:arglists (meta #'systems/make-pendulum)) (:arglists (meta #'phys/make-pendulum))))
  (is (string? (:doc (meta #'phys/evolve)))))
