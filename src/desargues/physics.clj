(ns desargues.physics
  "FACADE of the physics bounded context: the public surface in one require.

   The context is stratified; lower strata never require upper ones:

     desargues.physics.protocols     Protocol  ports (PhysicalSystem, Force,
                                               Constraint, Integrator,
                                               TrajectorySolver,
                                               BatchTrajectorySolver,
                                               ArraySystem, Oscillator)
     desargues.physics.systems       Core      Pendulum, SpringMass records
     desargues.physics.forces        Core      force records, combinators
     desargues.physics.specs         Core      clojure.spec option shapes
     desargues.physics.arrays        Pure/JVM  double[] RK4 fast paths
     desargues.physics.integrators   Pure      Euler, RK4
     desargues.physics.stepper       Pure      StepperSolver
     desargues.physics.harmonic      Pure      frequencies, analytic solutions
     desargues.physics.trajectory    Pure      graph/parametric/phase data
     desargues.physics.solver        Open      `solver` multimethod by key
     desargues.physics.evolution     App       evolve, evolve-batch
     desargues.physics.lagrangian    Core/Pure LagrangianSystem (Emmy)
     desargues.physics.pendulums     Core      double pendulums

   This namespace only re-exports values; it defines no behaviour. Code that
   IMPLEMENTS a port (defrecord, reify, extend) or adds a backend (defmethod
   on solver) requires the stratum namespace directly.

   ```clojure
   (require '[desargues.physics :as phys])
   (def pendulum (phys/make-pendulum {:length 3 :damping 0.1 :initial-theta 0.3}))
   (def trajectory (phys/evolve pendulum {:dt 0.01 :duration 10}))
   (phys/phase-portrait pendulum trajectory)
   ```"
  (:refer-clojure :exclude [step])
  (:require [desargues.physics.evolution :as evolution]
            [desargues.physics.forces :as forces]
            [desargues.physics.harmonic :as harmonic]
            [desargues.physics.integrators :as integrators]
            [desargues.physics.lagrangian :as lagrangian]
            [desargues.physics.pendulums :as pendulums]
            [desargues.physics.protocols :as p]
            [desargues.physics.solver :as solver-port]
            [desargues.physics.stepper :as stepper]
            [desargues.physics.systems :as systems]
            [desargues.physics.trajectory :as trajectory]))

(defmacro ^:private re-export
  "(re-export alias/name ...) defines name here as the value of alias/name,
   carrying its docstring and arglists (quoted: def evaluates its metadata)."
  [& syms]
  `(do ~@(for [s syms
               :let [m (select-keys (meta (resolve s)) [:doc :arglists])
                     n (symbol (name s))]]
           `(do (def ~n ~s)
                (alter-meta! (var ~n) merge '~m)))))

;; ── ports (call sites; implementers require desargues.physics.protocols) ────
(re-export p/get-state p/set-state p/derivatives p/state-labels
           p/force-value p/force-name p/step p/integrator-name
           p/solve-trajectory p/solve-batch p/array-kernel)

;; ── systems ──────────────────────────────────────────────────────────────────
(re-export systems/make-pendulum systems/pendulum-position systems/pendulum-arc-length
           systems/make-spring-mass systems/spring-mass-position systems/spring-mass-force)

;; ── forces ───────────────────────────────────────────────────────────────────
(re-export forces/spring-force forces/damping-force forces/gravity-force forces/combine-forces)

;; ── integration ──────────────────────────────────────────────────────────────
(re-export integrators/euler integrators/rk4 integrators/step-euler
           stepper/stepper-solver solver-port/solver
           evolution/evolve evolution/evolve-batch)

;; ── analysis ─────────────────────────────────────────────────────────────────
(re-export harmonic/natural-frequency harmonic/period harmonic/damping-ratio
           harmonic/damping-type harmonic/harmonic-solution harmonic/damped-harmonic-solution
           trajectory/phase-portrait trajectory/sample-trajectory
           trajectory/trajectory->graph-data trajectory/trajectory->parametric-data
           trajectory/velocity-vector-at)

;; ── Lagrangian systems ───────────────────────────────────────────────────────
(re-export lagrangian/lagrangian-from-positions lagrangian/lagrangian-system
           lagrangian/positions-at lagrangian/energy
           pendulums/double-pendulum pendulums/spherical-double-pendulum)
