(ns desargues.properties.physics-test
  "Property-based tests for physics simulation systems.
   
   These tests verify physical invariants that should hold
   regardless of the specific parameter values:
   - Energy conservation (or proper dissipation with damping)
   - Period approximations for small angles
   - Integration accuracy
   - State consistency"
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [desargues.videos.physics :as phys]
            [desargues.specs.generators :as g]))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn pendulum-energy
  "Compute total mechanical energy of pendulum.
   E = (1/2)mL²ω² + mgL(1 - cos(θ))
   Using m=1 for simplicity."
  [{:keys [length gravity]} {:keys [theta omega]}]
  (let [kinetic (* 0.5 length length omega omega)
        potential (* gravity length (- 1 (Math/cos theta)))]
    (+ kinetic potential)))

(defn spring-mass-energy
  "Compute total mechanical energy of spring-mass system.
   E = (1/2)mv² + (1/2)kx²"
  [{:keys [k mass]} {:keys [x v]}]
  (let [kinetic (* 0.5 mass v v)
        potential (* 0.5 k x x)]
    (+ kinetic potential)))

(defn trajectory-energies
  "Extract energies from a trajectory."
  [system trajectory energy-fn]
  (map (fn [{:keys [state]}]
         (energy-fn system state))
       trajectory))

(defn relative-error
  "Relative error of computed against expected, with an absolute floor: below
   |expected| = 1e-9 the error is measured absolutely (scaled by 1e-9), so an
   expected value of 0 or of floating-point-noise size (a pendulum at rest,
   energy ~1e-14) does not turn round-off into an infinite error."
  [computed expected]
  (let [scale (max (Math/abs (double expected)) 1e-9)]
    (/ (Math/abs (- computed expected)) scale)))

(defn monotonic-decreasing?
  "Check if sequence is monotonically decreasing (with tolerance)."
  [xs tolerance]
  (every? (fn [[a b]] (<= b (+ a tolerance)))
          (partition 2 1 xs)))

;; =============================================================================
;; TrajectorySolver seam (DIP): evolve dispatches through the solver port
;; =============================================================================

(deftest evolve-default-is-the-rk4-stepper
  (let [sys (phys/make-pendulum {:length 2.0 :damping 0.05 :initial-theta 0.2})
        opts {:dt 0.01 :duration 1.0}]
    (is (= (phys/evolve sys opts)
           (phys/evolve sys (assoc opts :solver (phys/stepper-solver phys/rk4)))))
    (is (= (phys/evolve sys (assoc opts :integrator phys/euler))
           (phys/evolve sys (assoc opts :solver (phys/stepper-solver phys/euler)))))))

(deftest evolve-honors-an-injected-solver
  (let [sys (phys/make-pendulum {})
        recorded (atom nil)
        stub (reify phys/TrajectorySolver
               (solve-trajectory [_ system opts]
                 (reset! recorded [system opts])
                 [{:time 0.0 :state (phys/get-state system)}]))]
    (is (= [{:time 0.0 :state {:theta 0.3 :omega 0.0}}]
           (phys/evolve sys {:solver stub :dt 0.5 :duration 3.0})))
    (is (= [sys {:dt 0.5 :duration 3.0}] @recorded))))

(deftest evolve-rejects-non-finite-steps
  ;; A NaN dt never satisfies (>= t duration): before 2026-09-06 this spun the
  ;; stepper forever (and hung this suite through gen/double* NaNs).
  (let [sys (phys/make-pendulum {})]
    (is (thrown? clojure.lang.ExceptionInfo (phys/evolve sys {:dt ##NaN})))
    (is (thrown? clojure.lang.ExceptionInfo (phys/evolve sys {:dt 0.0})))
    (is (thrown? clojure.lang.ExceptionInfo (phys/evolve sys {:dt ##Inf})))
    (is (thrown? clojure.lang.ExceptionInfo (phys/evolve sys {:duration ##NaN})))
    (is (thrown? clojure.lang.ExceptionInfo (phys/evolve sys {:duration -1.0})))))

;; =============================================================================
;; Pendulum Property Tests
;; =============================================================================

(defspec pendulum-creation-always-valid 100
  (prop/for-all [opts g/gen-pendulum-opts]
                (let [pendulum (phys/make-pendulum opts)]
                  (and (instance? desargues.videos.physics.Pendulum pendulum)
                       (pos? (:length pendulum))
                       (pos? (:gravity pendulum))
                       (>= (:damping pendulum) 0)))))

(defspec undamped-pendulum-conserves-energy 50
  (prop/for-all [opts (gen/fmap #(assoc % :damping 0.0) g/gen-pendulum-opts)]
                (let [pendulum (phys/make-pendulum opts)
                      trajectory (phys/evolve pendulum {:dt 0.001 :duration 2.0 :integrator phys/rk4})
                      energies (trajectory-energies pendulum trajectory pendulum-energy)
                      initial-energy (first energies)
                      max-error (apply max (map #(relative-error % initial-energy) energies))]
      ;; Energy should be conserved within 1% for RK4 integrator
                  (< max-error 0.01))))

(defspec damped-pendulum-loses-energy 50
  (prop/for-all [opts (gen/fmap #(assoc % :damping (max 0.1 (:damping %)))
                                g/gen-pendulum-opts)]
                (let [pendulum (phys/make-pendulum opts)
                      trajectory (phys/evolve pendulum {:dt 0.01 :duration 5.0 :integrator phys/rk4})
                      energies (trajectory-energies pendulum trajectory pendulum-energy)]
      ;; Energy should decrease (with small tolerance for numerical error)
                  (monotonic-decreasing? energies 0.01))))

(defspec small-angle-period-approximation 50
  (prop/for-all [length (gen/double* {:min 0.5 :max 5.0 :NaN? false :infinite? false})
                 gravity (gen/double* {:min 5.0 :max 15.0 :NaN? false :infinite? false})]
                (let [pendulum (phys/make-pendulum {:length length
                                                    :gravity gravity
                                                    :damping 0.0
                                                    :initial-theta 0.05 ;; Small angle
                                                    :initial-omega 0.0})
                      computed-period (phys/period pendulum)
                      expected-period (* 2 Math/PI (Math/sqrt (/ length gravity)))]
      ;; For small angles, T ≈ 2π√(L/g) should be very accurate
                  (< (relative-error computed-period expected-period) 0.001))))

(defspec pendulum-position-on-circle 100
  (prop/for-all [opts g/gen-pendulum-opts
                 state g/gen-pendulum-state]
                (let [pendulum (phys/make-pendulum opts)
                      [x y z] (phys/pendulum-position pendulum state)
                      [tx ty tz] (:top-point pendulum)
                      distance (Math/sqrt (+ (Math/pow (- x tx) 2)
                                             (Math/pow (- y ty) 2)
                                             (Math/pow (- z tz) 2)))]
      ;; Bob should always be at distance L from pivot
                  (< (relative-error distance (:length pendulum)) 0.0001))))

;; =============================================================================
;; Spring-Mass Property Tests
;; =============================================================================

(defspec spring-mass-creation-always-valid 100
  (prop/for-all [opts g/gen-spring-mass-opts]
                (let [spring (phys/make-spring-mass opts)]
                  (and (instance? desargues.videos.physics.SpringMass spring)
                       (pos? (:k spring))
                       (pos? (:mass spring))
                       (>= (:mu spring) 0)))))

(defspec undamped-spring-conserves-energy 50
  (prop/for-all [opts (gen/fmap #(assoc % :mu 0.0) g/gen-spring-mass-opts)]
                (let [spring (phys/make-spring-mass opts)
                      trajectory (phys/evolve spring {:dt 0.001 :duration 2.0 :integrator phys/rk4})
                      energies (trajectory-energies spring trajectory spring-mass-energy)
                      initial-energy (first energies)
                      max-error (apply max (map #(relative-error % initial-energy) energies))]
                  (< max-error 0.01))))

(defspec damped-spring-loses-energy 50
  (prop/for-all [opts (gen/fmap #(assoc % :mu (max 0.1 (:mu %)))
                                g/gen-spring-mass-opts)]
                (let [spring (phys/make-spring-mass opts)
                      trajectory (phys/evolve spring {:dt 0.01 :duration 5.0 :integrator phys/rk4})
                      energies (trajectory-energies spring trajectory spring-mass-energy)]
                  (monotonic-decreasing? energies 0.01))))

(defspec spring-period-formula 50
  (prop/for-all [k (gen/double* {:min 0.5 :max 20.0 :NaN? false :infinite? false})
                 mass (gen/double* {:min 0.5 :max 5.0 :NaN? false :infinite? false})]
                (let [spring (phys/make-spring-mass {:k k :mass mass :mu 0.0})
                      computed-period (phys/period spring)
                      expected-period (* 2 Math/PI (Math/sqrt (/ mass k)))]
                  (< (relative-error computed-period expected-period) 0.001))))

(defspec natural-frequency-positive 100
  (prop/for-all [system (gen/one-of [g/gen-pendulum g/gen-spring-mass])]
                (pos? (phys/natural-frequency system))))

;; =============================================================================
;; Evolution Property Tests
;; =============================================================================

(defspec trajectory-starts-at-initial-state 100
  (prop/for-all [pendulum g/gen-pendulum]
                (let [trajectory (phys/evolve pendulum {:dt 0.01 :duration 1.0})
                      initial-point (first trajectory)]
                  (and (zero? (:time initial-point))
                       (= (:state initial-point) (phys/get-state pendulum))))))

(defspec trajectory-has-correct-length 50
  (prop/for-all [dt (gen/double* {:min 0.01 :max 0.1 :NaN? false :infinite? false})
                 duration (gen/double* {:min 0.5 :max 5.0 :NaN? false :infinite? false})]
                (let [pendulum (phys/make-pendulum {})
                      trajectory (phys/evolve pendulum {:dt dt :duration duration})
                      expected-steps (inc (int (/ duration dt)))]
      ;; Allow for off-by-one due to floating point
                  (<= (Math/abs (- (count trajectory) expected-steps)) 2))))

(defspec rk4-more-accurate-than-euler 30
  (prop/for-all [opts (gen/fmap #(assoc % :damping 0.0) g/gen-pendulum-opts)]
                (let [pendulum (phys/make-pendulum opts)
                      euler-traj (phys/evolve pendulum {:dt 0.01 :duration 2.0 :integrator phys/euler})
                      rk4-traj (phys/evolve pendulum {:dt 0.01 :duration 2.0 :integrator phys/rk4})
                      euler-energies (trajectory-energies pendulum euler-traj pendulum-energy)
                      rk4-energies (trajectory-energies pendulum rk4-traj pendulum-energy)
                      initial-energy (first euler-energies)
                      euler-max-error (apply max (map #(relative-error % initial-energy) euler-energies))
                      rk4-max-error (apply max (map #(relative-error % initial-energy) rk4-energies))]
      ;; RK4 should generally be more accurate than Euler
                  (<= rk4-max-error euler-max-error))))

;; =============================================================================
;; Damping Classification Property Tests
;; =============================================================================

(defspec damping-type-classification-consistent 100
  (prop/for-all [system (gen/one-of [g/gen-pendulum g/gen-spring-mass])]
                (let [zeta (phys/damping-ratio system)
                      dtype (phys/damping-type system)]
                  (cond
                    (< zeta 1) (= dtype :underdamped)
                    (= zeta 1) (= dtype :critically-damped)
                    :else (= dtype :overdamped)))))

;; =============================================================================
;; Phase Portrait Property Tests
;; =============================================================================

(defspec phase-portrait-same-length-as-trajectory 50
  (prop/for-all [opts g/gen-pendulum-opts
                 evolve-opts g/gen-short-evolve-opts]
                (let [pendulum (phys/make-pendulum opts)
                      trajectory (phys/evolve pendulum evolve-opts)
                      phase-data (phys/phase-portrait pendulum trajectory)]
                  (= (count phase-data) (count trajectory)))))

(defspec phase-portrait-points-are-pairs 50
  (prop/for-all [opts g/gen-spring-mass-opts]
                (let [spring (phys/make-spring-mass opts)
                      trajectory (phys/evolve spring {:dt 0.01 :duration 1.0})
                      phase-data (phys/phase-portrait spring trajectory)]
                  (every? #(and (vector? %) (= 2 (count %)) (every? number? %))
                          phase-data))))

;; =============================================================================
;; Analytical Solution Property Tests
;; =============================================================================

(defspec harmonic-solution-starts-at-x0 100
  (prop/for-all [params g/gen-harmonic-params]
                (let [x-at-0 (phys/harmonic-solution params 0)]
                  (< (relative-error x-at-0 (:x0 params)) 0.0001))))

(defspec harmonic-solution-periodic 50
  (prop/for-all [params (gen/fmap #(assoc % :omega (max 0.1 (:omega %)))
                                  g/gen-harmonic-params)]
                (let [omega (:omega params)
                      period (/ (* 2 Math/PI) omega)
                      x0 (phys/harmonic-solution params 0)
                      x-at-period (phys/harmonic-solution params period)]
      ;; After one period, should return to initial position
                  (< (relative-error x-at-period x0) 0.01))))

;; =============================================================================
;; Unit Tests (for edge cases)
;; =============================================================================

(deftest test-zero-initial-conditions
  (testing "System at rest stays at rest (without damping)"
    (let [pendulum (phys/make-pendulum {:initial-theta 0.0 :initial-omega 0.0 :damping 0.0})
          trajectory (phys/evolve pendulum {:dt 0.01 :duration 1.0})
          final-state (:state (last trajectory))]
      (is (< (Math/abs (:theta final-state)) 0.001))
      (is (< (Math/abs (:omega final-state)) 0.001)))))

(deftest test-sample-trajectory
  (testing "sample-trajectory reduces trajectory length"
    (let [pendulum (phys/make-pendulum {})
          trajectory (phys/evolve pendulum {:dt 0.01 :duration 1.0})
          sampled (phys/sample-trajectory trajectory 10)]
      (is (<= (count sampled) 10))
      (is (< (count sampled) (count trajectory))))))

(deftest test-force-combinators
  (testing "Forces can be combined"
    (let [spring (phys/spring-force 5.0)
          damping (phys/damping-force 0.5)
          combined (phys/combine-forces spring damping)]
      (is (satisfies? phys/Force combined))
      (is (string? (phys/force-name combined))))))
