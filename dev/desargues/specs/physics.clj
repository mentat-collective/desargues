(ns desargues.specs.physics
  "Spec definitions for physics simulation systems.
   
   This namespace provides comprehensive specs for:
   - Pendulum and spring-mass systems
   - Evolution/integration parameters
   - Trajectories and phase portraits
   - Force combinators
   
   Includes s/fdef function specs for instrumentation."
  (:require [clojure.spec.alpha :as s]
            [desargues.specs.common :as c]
            [desargues.videos.physics :as phys]))

;; =============================================================================
;; Pendulum System Specs
;; =============================================================================

;; Length of pendulum rod (positive number < 100m)
(s/def ::length (s/and number? pos? #(< % 100)))

;; Gravitational acceleration (positive number < 100 m/s²)
(s/def ::gravity (s/and number? pos? #(< % 100)))

;; Damping coefficient (non-negative)
(s/def ::damping (s/and number? #(>= % 0)))

;; Initial angle in radians (constrained to avoid singularities)
(s/def ::initial-theta (s/and number? #(< (abs %) Math/PI)))

;; Initial angular velocity (rad/s)
(s/def ::initial-omega number?)

;; Fixed attachment point [x y z]
(s/def ::top-point ::c/point-3d)

;; Options for creating a pendulum
(s/def ::pendulum-opts
  (s/keys :opt-un [::length ::gravity ::damping
                   ::initial-theta ::initial-omega ::top-point]))

;; A pendulum system record
(s/def ::pendulum #(instance? desargues.videos.physics.Pendulum %))

;; =============================================================================
;; Spring-Mass System Specs
;; =============================================================================

;; Spring constant (positive, N/m)
(s/def ::k (s/and number? pos?))

;; Damping coefficient for spring-mass (non-negative)
(s/def ::mu (s/and number? #(>= % 0)))

;; Mass (positive number)
(s/def ::mass (s/and number? pos?))

;; Initial displacement from equilibrium
(s/def ::initial-x number?)

;; Initial velocity
(s/def ::initial-v number?)

;; Equilibrium position [x y z]
(s/def ::equilibrium-position ::c/point-3d)

;; Direction of oscillation [dx dy dz]
(s/def ::direction ::c/vector-3d)

;; Options for creating a spring-mass system
(s/def ::spring-mass-opts
  (s/keys :opt-un [::k ::mu ::mass
                   ::initial-x ::initial-v
                   ::equilibrium-position ::direction]))

;; A spring-mass system record
(s/def ::spring-mass #(instance? desargues.videos.physics.SpringMass %))

;; =============================================================================
;; Physical System Protocol Specs
;; =============================================================================

;; Any physical system implementing the PhysicalSystem protocol
(s/def ::physical-system
  (s/or :pendulum ::pendulum
        :spring-mass ::spring-mass))

;; Pendulum angle
(s/def ::theta number?)

;; Angular velocity
(s/def ::omega number?)

;; Linear displacement
(s/def ::x number?)

;; Linear velocity
(s/def ::v number?)

;; State of a pendulum system
(s/def ::pendulum-state (s/keys :req-un [::theta ::omega]))

;; State of a spring-mass system
(s/def ::spring-mass-state (s/keys :req-un [::x ::v]))

;; State of any physical system
(s/def ::state
  (s/or :pendulum ::pendulum-state
        :spring-mass ::spring-mass-state))

;; =============================================================================
;; Evolution Specs
;; =============================================================================

;; Time step for integration (positive, typically < 0.1)
(s/def ::dt (s/and number? pos? #(< % 1.0)))

;; Total duration to simulate (positive)
(s/def ::duration (s/and number? pos?))

;; An integrator implementing the Integrator protocol
(s/def ::integrator #(satisfies? phys/Integrator %))

;; Options for evolving a system
(s/def ::evolve-opts (s/keys :opt-un [::dt ::duration ::integrator]))

;; Time value (non-negative)
(s/def ::time (s/and number? #(>= % 0)))

;; A single point in a trajectory
(s/def ::trajectory-point (s/keys :req-un [::time ::state]))

;; A sequence of trajectory points
(s/def ::trajectory (s/coll-of ::trajectory-point :kind sequential?))

;; =============================================================================
;; Phase Portrait Specs
;; =============================================================================

;; A point in phase space [var1 var2]
(s/def ::phase-point (s/tuple number? number?))

;; Phase portrait data (sequence of phase points)
(s/def ::phase-portrait (s/coll-of ::phase-point :kind sequential?))

;; Variable pair for phase portrait
(s/def ::vars (s/tuple keyword? keyword?))

;; =============================================================================
;; Harmonic Analysis Specs
;; =============================================================================

;; Natural frequency ω (positive)
(s/def ::natural-frequency ::c/positive-number)

;; Period of oscillation (positive)
(s/def ::period ::c/positive-number)

;; Damping ratio ζ (non-negative)
(s/def ::damping-ratio ::c/non-negative-number)

;; Classification of damping behavior
(s/def ::damping-type #{:underdamped :critically-damped :overdamped})

;; =============================================================================
;; Force Specs
;; =============================================================================

;; Any force implementing the Force protocol
(s/def ::force #(satisfies? phys/Force %))

;; Rest length of spring
(s/def ::rest-length number?)

;; Anchor point for spring
(s/def ::anchor ::c/point-3d)

;; Options for spring force
(s/def ::spring-force-opts (s/keys :opt-un [::rest-length ::anchor]))

;; =============================================================================
;; Analytical Solution Specs
;; =============================================================================

;; Initial position for analytical solution
(s/def ::x0 number?)

;; Initial velocity for analytical solution
(s/def ::v0 number?)

;; Damping rate γ = μ/2m
(s/def ::gamma ::c/non-negative-number)

;; Parameters for harmonic solution
(s/def ::harmonic-params (s/keys :req-un [::x0 ::v0 ::omega]))

;; Parameters for damped harmonic solution
(s/def ::damped-harmonic-params (s/keys :req-un [::x0 ::v0 ::omega ::gamma]))

;; =============================================================================
;; Visualization Specs
;; =============================================================================

;; X values for graph data
(s/def ::xs (s/coll-of number?))

;; Y values for graph data
(s/def ::ys (s/coll-of number?))

;; Label for graph data
(s/def ::label string?)

;; Data for plotting a graph
(s/def ::graph-data (s/keys :req-un [::xs ::ys ::label]))

;; Scale factor for velocity vectors
(s/def ::scale ::c/positive-number)

;; =============================================================================
;; Function Specs (s/fdef)
;; =============================================================================

(s/fdef phys/make-pendulum
  :args (s/cat :opts ::pendulum-opts)
  :ret ::pendulum)

(s/fdef phys/make-spring-mass
  :args (s/cat :opts ::spring-mass-opts)
  :ret ::spring-mass)

(s/fdef phys/pendulum-position
  :args (s/cat :pendulum ::pendulum
               :state ::pendulum-state)
  :ret ::c/point-3d)

(s/fdef phys/pendulum-arc-length
  :args (s/cat :pendulum ::pendulum
               :state ::pendulum-state)
  :ret number?)

(s/fdef phys/spring-mass-position
  :args (s/cat :spring-mass ::spring-mass
               :state ::spring-mass-state)
  :ret ::c/point-3d)

(s/fdef phys/spring-mass-force
  :args (s/cat :spring-mass ::spring-mass
               :state ::spring-mass-state)
  :ret number?)

(s/fdef phys/evolve
  :args (s/cat :system ::physical-system
               :opts ::evolve-opts)
  :ret ::trajectory)

(s/fdef phys/phase-portrait
  :args (s/cat :system ::physical-system
               :trajectory ::trajectory
               :kwargs (s/keys* :opt-un [::vars]))
  :ret ::phase-portrait)

(s/fdef phys/sample-trajectory
  :args (s/cat :trajectory ::trajectory
               :n-samples pos-int?)
  :ret ::trajectory)

(s/fdef phys/natural-frequency
  :args (s/cat :system ::physical-system)
  :ret ::natural-frequency)

(s/fdef phys/period
  :args (s/cat :system ::physical-system)
  :ret ::period)

(s/fdef phys/damping-ratio
  :args (s/cat :system ::physical-system)
  :ret ::damping-ratio)

(s/fdef phys/damping-type
  :args (s/cat :system ::physical-system)
  :ret ::damping-type)

(s/fdef phys/harmonic-solution
  :args (s/cat :params ::harmonic-params
               :t number?)
  :ret number?)

(s/fdef phys/damped-harmonic-solution
  :args (s/cat :params ::damped-harmonic-params
               :t number?)
  :ret number?)

(s/fdef phys/spring-force
  :args (s/cat :k ::k
               :kwargs (s/keys* :opt-un [::rest-length ::anchor]))
  :ret ::force)

(s/fdef phys/damping-force
  :args (s/cat :mu ::mu)
  :ret ::force)

(s/fdef phys/gravity-force
  :args (s/cat :g ::gravity)
  :ret ::force)

(s/fdef phys/combine-forces
  :args (s/cat :forces (s/* ::force))
  :ret ::force)

(s/fdef phys/trajectory->graph-data
  :args (s/cat :trajectory ::trajectory
               :var-key keyword?
               :kwargs (s/keys* :opt-un [::label]))
  :ret ::graph-data)

(s/fdef phys/velocity-vector-at
  :args (s/cat :system ::physical-system
               :state ::state
               :kwargs (s/keys* :opt-un [::scale]))
  :ret (s/tuple number? number?))
