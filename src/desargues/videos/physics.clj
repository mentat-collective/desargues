(ns desargues.videos.physics
  "Physics simulation DSL for mathematical animations.

   This namespace provides high-level abstractions for physical systems
   commonly animated in 3Blue1Brown videos:
   - Pendulums (simple harmonic motion, damped oscillation)
   - Spring-mass systems
   - Particles with forces
   - Phase space trajectories

   ## Design Philosophy (DDD/SOLID)

   The physics layer is PURE CLOJURE - no Python dependencies.
   It computes trajectories, phase portraits, and state evolution.
   Rendering is handled separately by the renderer namespace.

   ## Key Abstractions

   - `PhysicalSystem` protocol: Any system with state evolution
   - `Force` protocol: Force functions that act on systems
   - `Integrator` protocol: ODE integration strategies

   ## Example Usage

   ```clojure
   ;; Create a damped pendulum
   (def pendulum (make-pendulum {:length 3 :damping 0.1 :initial-theta 0.3}))

   ;; Evolve the system
   (def trajectory (evolve pendulum {:dt 0.01 :duration 10}))

   ;; Get phase portrait data
   (def phase-data (phase-portrait pendulum trajectory))
   ```"
  (:require [clojure.spec.alpha :as s]))

;; =============================================================================
;; Protocols - The DNA of Physical Systems
;; =============================================================================

(defprotocol PhysicalSystem
  "Protocol for any physical system with state evolution.
   This is the core abstraction - systems have state and evolve."
  (get-state [this] "Returns current state as a map {:position ... :velocity ...}")
  (set-state [this state] "Returns new system with updated state")
  (derivatives [this state] "Returns time derivatives of state variables")
  (state-labels [this] "Returns labels for state variables [:theta :omega] etc"))

(defprotocol Force
  "Protocol for forces that can be applied to physical systems.
   Forces are composable - they can be combined additively."
  (force-value [this system state] "Computes force given system and state")
  (force-name [this] "Human-readable name for the force"))

(defprotocol Integrator
  "Protocol for ODE integration strategies.
   Different integrators trade off accuracy vs speed."
  (step [this system dt] "Advance system by dt, returns new system")
  (integrator-name [this] "Name of integration method"))

(defprotocol Constraint
  "Protocol for constraints on physical systems.
   Constraints restrict the motion (like a pendulum rod)."
  (apply-constraint [this state] "Project state onto constraint manifold")
  (constraint-name [this] "Human-readable constraint description"))

;; =============================================================================
;; Integrators - ODE Solvers
;; =============================================================================

(defrecord EulerIntegrator []
  Integrator
  (step [_ system dt]
    (let [state (get-state system)
          derivs (derivatives system state)
          new-state (merge-with + state (into {} (map (fn [[k v]] [k (* v dt)]) derivs)))]
      (set-state system new-state)))
  (integrator-name [_] "Euler"))

(defrecord RK4Integrator []
  Integrator
  (step [_ system dt]
    (let [state (get-state system)
          k1 (derivatives system state)
          state2 (merge-with + state (into {} (map (fn [[k v]] [k (* v (/ dt 2))]) k1)))
          k2 (derivatives system state2)
          state3 (merge-with + state (into {} (map (fn [[k v]] [k (* v (/ dt 2))]) k2)))
          k3 (derivatives system state3)
          state4 (merge-with + state (into {} (map (fn [[k v]] [k (* v dt)]) k3)))
          k4 (derivatives system state4)
          ;; Combine: new = old + dt/6 * (k1 + 2*k2 + 2*k3 + k4)
          combined (merge-with
                    (fn [& vs] (reduce + vs))
                    k1
                    (into {} (map (fn [[k v]] [k (* 2 v)]) k2))
                    (into {} (map (fn [[k v]] [k (* 2 v)]) k3))
                    k4)
          new-state (merge-with + state (into {} (map (fn [[k v]] [k (* v (/ dt 6))]) combined)))]
      (set-state system new-state)))
  (integrator-name [_] "Runge-Kutta 4"))

(def euler (->EulerIntegrator))
(def rk4 (->RK4Integrator))

(defprotocol TrajectorySolver
  "Contract for anything that turns a PhysicalSystem into a Trajectory
   (a seq of {:time t :state state}) over a time span. Providers: the
   built-in fixed-step Integrator loop (StepperSolver) and external ODE
   engines (see desargues.infrastructure.raster-adapter)."
  (solve-trajectory [this system opts]
    "Evolve `system` per opts {:dt :duration}. Returns a Trajectory whose
     first point is {:time 0.0 :state (get-state system)}."))

(defrecord StepperSolver [integrator]
  TrajectorySolver
  (solve-trajectory [_ system {:keys [dt duration] :or {dt 0.01 duration 10.0}}]
    (loop [sys system
           t 0.0
           trajectory [{:time 0.0 :state (get-state system)}]]
      (if (>= t duration)
        trajectory
        (let [new-sys (step integrator sys dt)
              new-t (+ t dt)]
          (recur new-sys new-t (conj trajectory {:time new-t :state (get-state new-sys)})))))))

(defn stepper-solver
  "A TrajectorySolver that drives a step-based Integrator (default rk4)."
  ([] (stepper-solver rk4))
  ([integrator] (->StepperSolver integrator)))

(defn step-euler
  "Single Euler integration step for state evolution.
   state: current state map
   deriv-fn: function that computes derivatives from state
   dt: time step"
  [state deriv-fn dt]
  (let [derivs (deriv-fn state)]
    (reduce-kv (fn [s k v]
                 (if-let [dv (get derivs k)]
                   (update s k + (* dv dt))
                   s))
               state
               state)))

;; =============================================================================
;; Pendulum - The Classic Physical System
;; =============================================================================

(defrecord Pendulum [length gravity damping theta omega top-point]
  PhysicalSystem
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
    [:theta :omega]))

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

;; =============================================================================
;; Spring-Mass System
;; =============================================================================

(defrecord SpringMass [k mu mass x v equilibrium-position direction]
  PhysicalSystem
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
    [:x :v]))

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

;; =============================================================================
;; Generic Evolution Functions
;; =============================================================================

(defn evolve
  "Evolve a physical system over time.

   Arguments:
   - system: Any PhysicalSystem
   - opts: Map with :dt (time step), :duration (total time), and either
     :integrator (a step-based Integrator, wrapped in a StepperSolver; default
     rk4) or :solver (any TrajectorySolver, which takes precedence).

   :dt must be a finite positive number and :duration a finite non-negative
   number; anything else (NaN in particular) is rejected up front, because a
   NaN step never reaches the duration and would spin forever.

   Returns a sequence of {:time t :state state} maps."
  [system {:keys [dt duration integrator solver]
           :or {dt 0.01 duration 10.0 integrator rk4}}]
  (when-not (and (number? dt) (Double/isFinite (double dt)) (pos? dt))
    (throw (ex-info "evolve: :dt must be a finite positive number" {:dt dt})))
  (when-not (and (number? duration) (Double/isFinite (double duration)) (not (neg? duration)))
    (throw (ex-info "evolve: :duration must be a finite non-negative number" {:duration duration})))
  (solve-trajectory (or solver (stepper-solver integrator))
                    system
                    {:dt dt :duration duration}))

(defn phase-portrait
  "Extract phase portrait data from trajectory.
   Returns sequence of [var1 var2] pairs for plotting."
  [system trajectory & {:keys [vars] :or {vars nil}}]
  (let [labels (state-labels system)
        [v1 v2] (or vars (take 2 labels))]
    (map (fn [{:keys [state]}]
           [(get state v1) (get state v2)])
         trajectory)))

(defn sample-trajectory
  "Sample trajectory at regular intervals, returning AT MOST n-samples points
   (the first point always included). Useful for discrete animation keyframes."
  [trajectory n-samples]
  (let [total (count trajectory)
        step (max 1 (long (Math/ceil (/ total (double (max 1 n-samples))))))]
    (take-nth step trajectory)))

;; =============================================================================
;; Harmonic Analysis
;; =============================================================================

(defn natural-frequency
  "Compute natural frequency of a system.

   For pendulum: ω = sqrt(g/L)
   For spring-mass: ω = sqrt(k/m)"
  [system]
  (cond
    (instance? Pendulum system)
    (Math/sqrt (/ (:gravity system) (:length system)))

    (instance? SpringMass system)
    (Math/sqrt (/ (:k system) (:mass system)))

    :else
    (throw (ex-info "Unknown system type" {:system system}))))

(defn period
  "Compute period of oscillation."
  [system]
  (/ (* 2 Math/PI) (natural-frequency system)))

(defn damping-ratio
  "Compute damping ratio ζ = μ / (2 * sqrt(k*m)) for spring-mass
   or ζ = μ / (2 * sqrt(g/L)) for pendulum."
  [system]
  (cond
    (instance? Pendulum system)
    (/ (:damping system)
       (* 2 (Math/sqrt (* (:gravity system) (:length system)))))

    (instance? SpringMass system)
    (/ (:mu system)
       (* 2 (Math/sqrt (* (:k system) (:mass system)))))

    :else
    (throw (ex-info "Unknown system type" {:system system}))))

(defn damping-type
  "Classify damping: :underdamped, :critically-damped, :overdamped"
  [system]
  (let [zeta (damping-ratio system)]
    (cond
      (< zeta 1) :underdamped
      (= zeta 1) :critically-damped
      :else :overdamped)))

;; =============================================================================
;; Analytical Solutions (for comparison/validation)
;; =============================================================================

(defn harmonic-solution
  "Analytical solution for undamped harmonic motion.
   x(t) = A*cos(ωt) + B*sin(ωt)
   where A = x0, B = v0/ω"
  [{:keys [x0 v0 omega]} t]
  (+ (* x0 (Math/cos (* omega t)))
     (* (/ v0 omega) (Math/sin (* omega t)))))

(defn damped-harmonic-solution
  "Analytical solution for damped harmonic motion (underdamped case).
   x(t) = e^(-γt) * (A*cos(ω't) + B*sin(ω't))
   where γ = μ/2m, ω' = sqrt(ω² - γ²)"
  [{:keys [x0 v0 omega gamma]} t]
  (let [omega-prime (Math/sqrt (- (* omega omega) (* gamma gamma)))
        A x0
        B (/ (+ v0 (* gamma x0)) omega-prime)]
    (* (Math/exp (- (* gamma t)))
       (+ (* A (Math/cos (* omega-prime t)))
          (* B (Math/sin (* omega-prime t)))))))

;; =============================================================================
;; Force Combinators
;; =============================================================================

(defrecord SpringForce [k rest-length anchor]
  Force
  (force-value [_ _ {:keys [x]}]
    (- (* k (- x rest-length))))
  (force-name [_] (str "Spring(k=" k ")")))

(defrecord DampingForce [mu]
  Force
  (force-value [_ _ {:keys [v]}]
    (- (* mu v)))
  (force-name [_] (str "Damping(μ=" mu ")")))

(defrecord GravityForce [g]
  Force
  (force-value [_ _ _]
    (- g))
  (force-name [_] (str "Gravity(g=" g ")")))

(defrecord CompositeForce [forces]
  Force
  (force-value [_ system state]
    (reduce + (map #(force-value % system state) forces)))
  (force-name [_]
    (str "Composite[" (clojure.string/join " + " (map force-name forces)) "]")))

(defn spring-force
  "Create a spring force F = -k(x - rest-length)"
  [k & {:keys [rest-length anchor] :or {rest-length 0 anchor [0 0 0]}}]
  (->SpringForce k rest-length anchor))

(defn damping-force
  "Create a damping force F = -μv"
  [mu]
  (->DampingForce mu))

(defn gravity-force
  "Create a gravity force F = -g"
  [g]
  (->GravityForce g))

(defn combine-forces
  "Combine multiple forces into a composite force."
  [& forces]
  (->CompositeForce (vec forces)))

;; =============================================================================
;; Visualization Helpers
;; =============================================================================

(defn trajectory->graph-data
  "Convert trajectory to data suitable for plotting x(t) graph.
   Returns {:xs [times] :ys [values] :label label}"
  [trajectory var-key & {:keys [label] :or {label (name var-key)}}]
  {:xs (mapv :time trajectory)
   :ys (mapv #(get-in % [:state var-key]) trajectory)
   :label label})

(defn trajectory->parametric-data
  "Convert trajectory to parametric curve data.
   Returns sequence of [x y] points."
  [system trajectory position-fn]
  (mapv (fn [{:keys [state]}]
          (position-fn system state))
        trajectory))

(defn velocity-vector-at
  "Compute velocity vector at a given state.
   Useful for drawing velocity arrows on animations."
  [system state & {:keys [scale] :or {scale 0.5}}]
  (let [derivs (derivatives system state)
        [v1 v2] (take 2 (vals derivs))]
    [(* scale v1) (* scale v2)]))

;; =============================================================================
;; Specs
;; =============================================================================

(s/def ::length (s/and number? pos?))
(s/def ::gravity (s/and number? pos?))
(s/def ::damping (s/and number? #(>= % 0)))
(s/def ::initial-theta number?)
(s/def ::initial-omega number?)
(s/def ::top-point (s/coll-of number? :count 3))

(s/def ::pendulum-opts
  (s/keys :opt-un [::length ::gravity ::damping ::initial-theta ::initial-omega ::top-point]))

(s/def ::k (s/and number? pos?))
(s/def ::mu (s/and number? #(>= % 0)))
(s/def ::mass (s/and number? pos?))
(s/def ::initial-x number?)
(s/def ::initial-v number?)

(s/def ::spring-mass-opts
  (s/keys :opt-un [::k ::mu ::mass ::initial-x ::initial-v]))

(s/def ::dt (s/and number? pos?))
(s/def ::duration (s/and number? pos?))
(s/def ::integrator #(satisfies? Integrator %))

(s/def ::evolve-opts
  (s/keys :opt-un [::dt ::duration ::integrator]))
