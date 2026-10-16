# desargues.physics — the physics bounded context

Physical systems, their integration and the data a renderer plots. Pure
Clojure; no rendering, no Python. The presentation layers (scene DSL, site
decks, the live player) consume it through the facade `desargues.physics`.

```clojure
(require '[desargues.physics :as phys])
(def p (phys/make-pendulum {:length 3 :damping 0.1 :initial-theta 0.3}))
(def traj (phys/evolve p {:dt 0.01 :duration 10}))            ; RK4 on the JVM
(phys/evolve p {:solver [:raster {:kernel? true}] :dt 0.01 :duration 10}) ; -M:dynamics
(phys/phase-portrait p traj)
```

## Strata

Lower strata never require upper ones (checked by
`test/desargues/physics/strata_test.clj`).

| ns | stratum | holds |
|----|---------|-------|
| `protocols` (.cljc) | Protocol | PhysicalSystem, ArraySystem, Oscillator, Force, Constraint, Integrator, TrajectorySolver, BatchTrajectorySolver |
| `systems` (.cljc) | Core | Pendulum, SpringMass records + constructors, positions |
| `forces` (.cljc) | Core | Spring/Damping/Gravity/Composite force records |
| `specs` (.cljc) | Core | clojure.spec option shapes |
| `arrays` (.clj, JVM) | Pure | the double[] RK4 fast paths (same IEEE ops, bit-identical) |
| `integrators` (.cljc) | Pure | Euler, RK4 |
| `stepper` (.cljc) | Pure | StepperSolver, the built-in TrajectorySolver |
| `harmonic` (.cljc) | Pure | natural frequency, period, damping, analytic solutions |
| `trajectory` (.cljc) | Pure | graph / parametric / phase data, sampling |
| `lagrangian` (.clj) | Core + Pure | LagrangianSystem from an Emmy Lagrangian |
| `pendulums` (.clj) | Core | planar and spherical double pendulums |
| `solver` (.clj) | Open | the `solver` multimethod, backend key -> TrajectorySolver |
| `evolution` (.clj) | Application | evolve, evolve-batch |
| `desargues.physics` | Facade | re-exports the public names |

## Known quirk: the stepper's time grid

`StepperSolver` accumulates time in floating point (`t <- t + dt`, starting
at 0.0) and stops once `t >= duration`. Because `dt` is rarely exact in
binary, the accumulated `t` can land just below `duration` and take one more
step. With `dt 0.1` over `duration 1.0`, ten additions give
0.9999999999999999, so the trajectory has 12 points (t = 0.0 ... ~1.1), not
11. The array fast path (`arrays/rk4-trajectory-arrays`) reproduces the same
grid bit for bit, and the `:wasm` backend answers the same time grid.

This is documented, not changed: whether to switch to an integer step count
(`t = i * dt`, `n = round(duration / dt)`) alters every recorded trajectory
and golden value, and is an open decision for the maintainer.

## Extending

- A new engine: one `defmethod desargues.physics.solver/solver` in its own
  namespace plus `desargues.backend/register-backend!` (see
  `desargues.infrastructure.{raster-adapter,wasm-runtime,gpu-solver}`).
- A new system: a record implementing `PhysicalSystem` (and `Oscillator`,
  `ArraySystem` if it can); nothing existing is edited.
- Code that implements a port requires `desargues.physics.protocols`
  directly; call sites can use the facade.
