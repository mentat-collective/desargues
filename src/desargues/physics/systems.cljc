(ns desargues.physics.systems
  "CORE stratum of the physics bounded context: the hand-written physical
   systems as value objects.

   Pendulum and SpringMass are records implementing the PhysicalSystem port
   (and the optional Oscillator capability), each with its constructor and
   the pure geometry of its state (bob / mass position). A new system is a
   new record in its own namespace implementing the same ports; nothing
   here is edited (OCP).

   Requires only desargues.physics.protocols. Host-free .cljc."
  (:require [desargues.physics.protocols :as p]))

;; ── Pendulum ─────────────────────────────────────────────────────────────────

(defrecord Pendulum [length gravity damping theta omega top-point]
  p/PhysicalSystem
  (get-state [_]
    {:theta theta :omega omega})

  (set-state [this state]
    (assoc this :theta (:theta state) :omega (:omega state)))

  (derivatives [this {:keys [theta omega]}]
    ;; θ' = ω
    ;; ω' = -(g/L)sin(θ) - μω
    {:theta omega
     :omega (- (- (* (/ gravity length) (Math/sin theta)))
               (* damping omega))})

  (state-labels [_]
    [:theta :omega])

  p/Oscillator
  (natural-frequency [_]
    (Math/sqrt (/ gravity length)))
  (damping-ratio [_]
    (/ damping (* 2 (Math/sqrt (* gravity length))))))

(defn make-pendulum
  "Create a pendulum physical system.

   Options:
   - :length - Length of pendulum rod (default 1.0)
   - :gravity - Gravitational acceleration (default 9.8)
   - :damping - Damping coefficient (default 0.0)
   - :initial-theta - Initial angle in radians (default 0.3)
   - :initial-omega - Initial angular velocity (default 0.0)
   - :top-point - Fixed point position [x y z] (default [0 2 0])

   Returns a Pendulum record implementing PhysicalSystem."
  [{:keys [length gravity damping initial-theta initial-omega top-point]
    :or {length 1.0
         gravity 9.8
         damping 0.0
         initial-theta 0.3
         initial-omega 0.0
         top-point [0 2 0]}}]
  (->Pendulum length gravity damping initial-theta initial-omega top-point))

(defn pendulum-position
  "Compute Cartesian position of pendulum bob from state.
   Returns [x y z] relative to top-point."
  [{:keys [length top-point]} {:keys [theta]}]
  (let [[tx ty tz] top-point
        x (+ tx (* length (Math/sin theta)))
        y (- ty (* length (Math/cos theta)))]
    [x y tz]))

(defn pendulum-arc-length
  "Compute arc length from equilibrium position."
  [{:keys [length]} {:keys [theta]}]
  (* length theta))

;; ── Spring-mass ──────────────────────────────────────────────────────────────

(defrecord SpringMass [k mu mass x v equilibrium-position direction]
  p/PhysicalSystem
  (get-state [_]
    {:x x :v v})

  (set-state [this state]
    (assoc this :x (:x state) :v (:v state)))

  (derivatives [_ {:keys [x v]}]
    ;; x' = v
    ;; v' = -(k/m)x - (μ/m)v
    {:x v
     :v (- (- (* (/ k mass) x))
           (* (/ mu mass) v))})

  (state-labels [_]
    [:x :v])

  p/Oscillator
  (natural-frequency [_]
    (Math/sqrt (/ k mass)))
  (damping-ratio [_]
    (/ mu (* 2 (Math/sqrt (* k mass))))))

(defn make-spring-mass
  "Create a spring-mass physical system.

   Options:
   - :k - Spring constant (default 3.0)
   - :mu - Damping coefficient (default 0.0)
   - :mass - Mass (default 1.0)
   - :initial-x - Initial displacement (default 0.0)
   - :initial-v - Initial velocity (default 0.0)
   - :equilibrium-position - [x y z] position at rest (default [0 0 0])
   - :direction - Direction of oscillation (default [1 0 0])

   Returns a SpringMass record implementing PhysicalSystem."
  [{:keys [k mu mass initial-x initial-v equilibrium-position direction]
    :or {k 3.0
         mu 0.0
         mass 1.0
         initial-x 0.0
         initial-v 0.0
         equilibrium-position [0 0 0]
         direction [1 0 0]}}]
  (->SpringMass k mu mass initial-x initial-v equilibrium-position direction))

(defn spring-mass-position
  "Compute Cartesian position of mass from state."
  [{:keys [equilibrium-position direction]} {:keys [x]}]
  (mapv + equilibrium-position (mapv #(* x %) direction)))

(defn spring-mass-force
  "Compute the spring force at current state."
  [{:keys [k mu]} {:keys [x v]}]
  (- (- (* k x)) (* mu v)))
