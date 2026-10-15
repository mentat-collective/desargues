(ns desargues.specs.generators
  "Custom test.check generators for property-based testing.
   
   Provides generators for:
   - Physics system parameters (pendulum, spring-mass)
   - Geometric primitives (points, vectors)
   - Timeline/animation parameters
   - Mathematical expressions
   
   These generators are designed to produce valid, physically meaningful values
   that exercise the system without hitting edge cases like singularities."
  (:require [clojure.test.check.generators :as gen]
            [clojure.spec.alpha :as s]
            [desargues.videos.physics :as phys]))

;; =============================================================================
;; Numeric Generators
;; =============================================================================

(def gen-positive-number
  "Generate positive numbers in a reasonable range"
  (gen/double* {:min 0.001 :max 100.0 :NaN? false :infinite? false}))

(def gen-non-negative-number
  "Generate non-negative numbers"
  (gen/double* {:min 0.0 :max 100.0 :NaN? false :infinite? false}))

(def gen-unit-interval
  "Generate numbers in [0, 1]"
  (gen/double* {:min 0.0 :max 1.0 :NaN? false :infinite? false}))

(def gen-small-positive
  "Generate small positive numbers (useful for time steps, damping)"
  (gen/double* {:min 0.001 :max 0.5 :NaN? false :infinite? false}))

(def gen-bounded-number
  "Generate bounded numbers for general use"
  (gen/double* {:min -100.0 :max 100.0 :NaN? false :infinite? false}))

;; =============================================================================
;; Geometric Generators
;; =============================================================================

(def gen-coordinate
  "Generate a single coordinate value"
  (gen/double* {:min -50.0 :max 50.0 :NaN? false :infinite? false}))

(def gen-point-2d
  "Generate a 2D point [x y]"
  (gen/tuple gen-coordinate gen-coordinate))

(def gen-point-3d
  "Generate a 3D point [x y z]"
  (gen/tuple gen-coordinate gen-coordinate gen-coordinate))

(def gen-unit-vector-2d
  "Generate a unit vector in 2D"
  (gen/fmap (fn [angle]
              [(Math/cos angle) (Math/sin angle)])
            (gen/double* {:min 0 :max (* 2 Math/PI)})))

(def gen-unit-vector-3d
  "Generate a unit vector in 3D using spherical coordinates"
  (gen/fmap (fn [[theta phi]]
              [(* (Math/sin theta) (Math/cos phi))
               (* (Math/sin theta) (Math/sin phi))
               (Math/cos theta)])
            (gen/tuple
             (gen/double* {:min 0 :max Math/PI})
             (gen/double* {:min 0 :max (* 2 Math/PI)}))))

;; =============================================================================
;; Pendulum Generators
;; =============================================================================

(def gen-pendulum-length
  "Generate pendulum length (0.1m to 10m)"
  (gen/double* {:min 0.1 :max 10.0 :NaN? false :infinite? false}))

(def gen-gravity
  "Generate gravitational acceleration (reasonable range)"
  (gen/double* {:min 1.0 :max 20.0 :NaN? false :infinite? false}))

(def gen-damping
  "Generate damping coefficient (non-negative)"
  (gen/double* {:min 0.0 :max 2.0 :NaN? false :infinite? false}))

(def gen-initial-theta
  "Generate initial angle (avoiding near-vertical positions)"
  (gen/double* {:min -1.5 :max 1.5 :NaN? false :infinite? false}))

(def gen-initial-omega
  "Generate initial angular velocity"
  (gen/double* {:min -5.0 :max 5.0 :NaN? false :infinite? false}))

(def gen-pendulum-opts
  "Generate valid pendulum options"
  (gen/hash-map
   :length gen-pendulum-length
   :gravity gen-gravity
   :damping gen-damping
   :initial-theta gen-initial-theta
   :initial-omega gen-initial-omega
   :top-point gen-point-3d))

(def gen-pendulum
  "Generate a complete pendulum system"
  (gen/fmap phys/make-pendulum gen-pendulum-opts))

(def gen-underdamped-pendulum-opts
  "Generate pendulum opts that ensure underdamped behavior"
  (gen/fmap (fn [opts]
              (let [omega-n (Math/sqrt (/ (:gravity opts) (:length opts)))
                    max-damping (* 0.9 2 omega-n)] ;; Ensure zeta < 1
                (assoc opts :damping (min (:damping opts) max-damping))))
            gen-pendulum-opts))

;; =============================================================================
;; Spring-Mass Generators
;; =============================================================================

(def gen-spring-constant
  "Generate spring constant k (positive)"
  (gen/double* {:min 0.1 :max 50.0 :NaN? false :infinite? false}))

(def gen-mass
  "Generate mass (positive)"
  (gen/double* {:min 0.1 :max 10.0 :NaN? false :infinite? false}))

(def gen-spring-damping
  "Generate spring damping coefficient"
  (gen/double* {:min 0.0 :max 5.0 :NaN? false :infinite? false}))

(def gen-initial-x
  "Generate initial displacement"
  (gen/double* {:min -5.0 :max 5.0 :NaN? false :infinite? false}))

(def gen-initial-v
  "Generate initial velocity"
  (gen/double* {:min -10.0 :max 10.0 :NaN? false :infinite? false}))

(def gen-spring-mass-opts
  "Generate valid spring-mass options"
  (gen/hash-map
   :k gen-spring-constant
   :mu gen-spring-damping
   :mass gen-mass
   :initial-x gen-initial-x
   :initial-v gen-initial-v
   :equilibrium-position gen-point-3d
   :direction gen-unit-vector-3d))

(def gen-spring-mass
  "Generate a complete spring-mass system"
  (gen/fmap phys/make-spring-mass gen-spring-mass-opts))

;; =============================================================================
;; Evolution Generators
;; =============================================================================

(def gen-dt
  "Generate time step for integration"
  (gen/double* {:min 0.001 :max 0.1 :NaN? false :infinite? false}))

(def gen-duration
  "Generate simulation duration"
  (gen/double* {:min 0.5 :max 20.0 :NaN? false :infinite? false}))

(def gen-evolve-opts
  "Generate valid evolution options"
  (gen/hash-map
   :dt gen-dt
   :duration gen-duration
   :integrator (gen/elements [phys/euler phys/rk4])))

(def gen-short-evolve-opts
  "Generate evolution options for short simulations (faster tests).
   NaN/infinite are excluded: a NaN dt never satisfies (>= t duration), so the
   stepper would loop forever (this hung the suite before 2026-09-06)."
  (gen/hash-map
   :dt (gen/double* {:min 0.01 :max 0.05 :NaN? false :infinite? false})
   :duration (gen/double* {:min 0.1 :max 2.0 :NaN? false :infinite? false})
   :integrator (gen/elements [phys/euler phys/rk4])))

;; =============================================================================
;; State Generators
;; =============================================================================

(def gen-pendulum-state
  "Generate a pendulum state"
  (gen/hash-map
   :theta gen-initial-theta
   :omega gen-initial-omega))

(def gen-spring-mass-state
  "Generate a spring-mass state"
  (gen/hash-map
   :x gen-initial-x
   :v gen-initial-v))

;; =============================================================================
;; Analytical Solution Generators
;; =============================================================================

(def gen-harmonic-params
  "Generate parameters for harmonic oscillator"
  (gen/hash-map
   :x0 gen-bounded-number
   :v0 gen-bounded-number
   :omega gen-positive-number))

(def gen-damped-harmonic-params
  "Generate parameters for damped harmonic oscillator"
  (gen/fmap (fn [params]
              ;; Ensure gamma < omega for underdamped case
              (let [omega (:omega params)
                    max-gamma (* 0.9 omega)]
                (assoc params :gamma (min (:gamma params) max-gamma))))
            (gen/hash-map
             :x0 gen-bounded-number
             :v0 gen-bounded-number
             :omega gen-positive-number
             :gamma gen-non-negative-number)))

;; =============================================================================
;; Color Generators
;; =============================================================================

(def gen-color-keyword
  "Generate a Manim color keyword"
  (gen/elements [:WHITE :BLACK :RED :GREEN :BLUE :YELLOW :ORANGE :PURPLE
                 :CYAN :MAGENTA :PINK :GRAY
                 :BLUE_A :BLUE_B :BLUE_C :BLUE_D :BLUE_E
                 :GREEN_A :GREEN_B :GREEN_C :GREEN_D :GREEN_E
                 :RED_A :RED_B :RED_C :RED_D :RED_E]))

(def gen-rgb
  "Generate an RGB color tuple"
  (gen/tuple gen-unit-interval gen-unit-interval gen-unit-interval))

(def gen-color
  "Generate any color representation"
  (gen/one-of [gen-color-keyword gen-rgb]))

;; =============================================================================
;; Timeline Generators
;; =============================================================================

(def gen-animation-duration
  "Generate animation duration"
  (gen/double* {:min 0.1 :max 5.0 :NaN? false :infinite? false}))

(def gen-rate-func-keyword
  "Generate a rate function keyword"
  (gen/elements [:linear :smooth :rush-into :rush-from
                 :slow-into :double-smooth :there-and-back
                 :exponential-decay]))

;; =============================================================================
;; Register Generators with Specs
;; =============================================================================

(defn register-generators!
  "Register custom generators with their corresponding specs.
   Call this before running generative tests."
  []
  ;; Physics generators
  (s/def :desargues.specs.physics/pendulum-opts
    (s/with-gen
      (s/keys :opt-un [:desargues.specs.physics/length
                       :desargues.specs.physics/gravity
                       :desargues.specs.physics/damping
                       :desargues.specs.physics/initial-theta
                       :desargues.specs.physics/initial-omega
                       :desargues.specs.physics/top-point])
      (fn [] gen-pendulum-opts)))

  (s/def :desargues.specs.physics/spring-mass-opts
    (s/with-gen
      (s/keys :opt-un [:desargues.specs.physics/k
                       :desargues.specs.physics/mu
                       :desargues.specs.physics/mass
                       :desargues.specs.physics/initial-x
                       :desargues.specs.physics/initial-v])
      (fn [] gen-spring-mass-opts)))

  (s/def :desargues.specs.physics/evolve-opts
    (s/with-gen
      (s/keys :opt-un [:desargues.specs.physics/dt
                       :desargues.specs.physics/duration
                       :desargues.specs.physics/integrator])
      (fn [] gen-evolve-opts)))

  :generators-registered)
