(ns desargues.physics.protocols
  "PROTOCOL stratum of the physics bounded context: the ports.

   Every other stratum speaks through these. Each port is small and
   segregated (ISP): a system that can only answer its derivative implements
   PhysicalSystem and nothing else; ArraySystem, Oscillator and
   BatchTrajectorySolver are optional capabilities a caller probes with
   satisfies?. Any implementation of a port is substitutable for any other
   (LSP): the stepper, the raster, wasm and gpu solvers all answer the same
   Trajectory for the same system.

   Requires nothing. Host-free .cljc.")

;; ── systems ──────────────────────────────────────────────────────────────────

(defprotocol PhysicalSystem
  "Protocol for any physical system with state evolution.
   This is the core abstraction - systems have state and evolve."
  (get-state [this] "Returns current state as a map {:position ... :velocity ...}")
  (set-state [this state] "Returns new system with updated state")
  (derivatives [this state] "Returns time derivatives of state variables")
  (state-labels [this] "Returns labels for state variables [:theta :omega] etc"))

(defprotocol ArraySystem
  "Optional fast path for a PhysicalSystem whose derivative is a primitive
   kernel. Arrays are flat doubles in (state-labels this) order."
  (array-kernel [this]
    "nil, or a fresh (not thread-shared) map {:state ^doubles current state,
     :deriv! (fn [^doubles s ^doubles ds]) filling ds with the derivative at s,
     :->state (fn [^doubles s]) the state map get-state would answer at s}.
     deriv! must perform the same IEEE operations as `derivatives`."))

(defprotocol Oscillator
  "Optional for a PhysicalSystem with a linearised harmonic motion. A system
   that does not implement it answers ex-info \"Unknown system type\"."
  (natural-frequency [system]
    "ω: sqrt(g/L) for a pendulum, sqrt(k/m) for a spring-mass.")
  (damping-ratio [system]
    "ζ = μ / (2 sqrt(k m)) for a spring-mass, μ / (2 sqrt(g L)) for a pendulum."))

(defn- unknown-system [system]
  (throw (ex-info "Unknown system type" {:system system})))

(extend-protocol Oscillator
  #?(:cljs default :default Object)
  (natural-frequency [system] (unknown-system system))
  (damping-ratio [system] (unknown-system system))
  nil
  (natural-frequency [system] (unknown-system system))
  (damping-ratio [system] (unknown-system system)))

;; ── forces and constraints ───────────────────────────────────────────────────

(defprotocol Force
  "Protocol for forces that can be applied to physical systems.
   Forces are composable - they can be combined additively."
  (force-value [this system state] "Computes force given system and state")
  (force-name [this] "Human-readable name for the force"))

(defprotocol Constraint
  "Protocol for constraints on physical systems.
   Constraints restrict the motion (like a pendulum rod)."
  (apply-constraint [this state] "Project state onto constraint manifold")
  (constraint-name [this] "Human-readable constraint description"))

;; ── integration ──────────────────────────────────────────────────────────────

(defprotocol Integrator
  "Protocol for ODE integration strategies.
   Different integrators trade off accuracy vs speed."
  (step [this system dt] "Advance system by dt, returns new system")
  (integrator-name [this] "Name of integration method"))

(defprotocol TrajectorySolver
  "Contract for anything that turns a PhysicalSystem into a Trajectory
   (a seq of {:time t :state state}) over a time span. Providers: the
   built-in fixed-step Integrator loop (desargues.physics.stepper) and
   external ODE engines (see desargues.infrastructure.raster-adapter)."
  (solve-trajectory [this system opts]
    "Evolve `system` per opts {:dt :duration}. Returns a Trajectory whose
     first point is {:time 0.0 :state (get-state system)}."))

(defprotocol BatchTrajectorySolver
  "Optional for a TrajectorySolver: evolve many systems in one call (N initial
   conditions, a parameter sweep). Must answer what mapping solve-trajectory
   over the systems would."
  (solve-batch [this systems opts]
    "A vector of Trajectories, one per system, per opts {:dt :duration}."))
