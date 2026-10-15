# Changelog

All notable changes to desargues. The format follows
[keepachangelog.com](https://keepachangelog.com/); a version is a git tag
(`vX.Y.Z`) that consumers pin as a tools.deps git coordinate.

## [Unreleased]

### Added
- `:wasm` backend (`desargues.infrastructure.wasm-runtime`): a compiled
  Lagrangian system's batched RK4 compiled to WebAssembly by raster
  (`raster-kernels/wasm-kernel`, `rk4-wasm-form`) and run on the JVM by
  Chicory (`com.dylibso.chicory/runtime` + `compiler`, under `:dynamics`).
  `(phys/evolve dp {:solver :wasm})` answers the stepper's time grid exactly
  and its values within ~1e-13; `trajectory-module` hands the same module to
  a browser or any wasm engine. Systems without an Emmy kernel fall back to
  the stepper.
- `desargues.kernel.libm`: fdlibm sin/cos (3-part Cody-Waite reduction,
  `__kernel_sin`/`__kernel_cos`) as pure kernel forms, within 2^-53 of
  `java.lang.Math` for |x| < 1e5; `with-precise-trig` rewrites a form's
  `Math/sin`/`Math/cos`. The wasm kernels use it: raster's own wasm sin/cos
  (a 5e-6 minimax) moved a 3 s chaotic double-pendulum trajectory by 3e-3.

### Fixed
- `raster-kernels/lower-emmy-kernel` indexed the state with doubles
  (`(aget s 0.0)`): its prewalk revisited the emitted `aget` and turned the
  index into a double. The JVM coerced it silently; wasm rejects it. It now
  lowers bottom-up.
- `backend/load!` reports a provider whose imported Java class is missing as
  `:absent` (it reported only a missing namespace file).

## [0.3.0] - 2026-10-12

### Added
- `desargues.backend`: numeric backends, open by key. A backend (`:jvm`,
  `:raster`, or one another library registers with `register-backend!`) names
  who runs the numbers behind a port, and every port dispatches on that key
  through a multimethod: `desargues.videos.physics/solver`,
  `desargues.geometry.kernel/provider`,
  `desargues.board.compiler/kernel-compiler` and `desargues.kernel/realize`.
  A backend's provider namespaces load the first time it is named, so raster
  stays off the classpath until `:raster` is asked for; an unknown or absent
  backend fails at the port with what it needs. raster is one backend among
  others: a new engine is a registration and `defmethod`s in its own
  namespace, with no edit to a port.
- `evolve` and `evolve-batch` take a backend key as `:solver`:
  `{:solver :raster}`, `{:solver [:raster {:alg :tsit5}]}`,
  `{:solver [:raster {:kernel? true}]}`, `{:solver [:jvm {:integrator euler}]}`.
- `desargues.kernel`: kernels as data. `realize` (target, Emmy expression ->
  form; `:sexp`, `:tex`, `:raster`) moved here from `desargues.board.algebra`,
  which keeps the same multimethod under its old name. Emmy `:primitive`
  kernel sources are read once (`source->ir`, `stores`) and split by one
  chunker (`chunk`) that the JVM kernel (`compile-jvm`) and the raster
  kernels both use.
- `desargues.kernel.emmy`: desargues kernels as Emmy compilation modes
  (`:desargues/jvm`), on an Emmy whose compiler is open
  ([mentat-collective/emmy#199](https://github.com/mentat-collective/emmy/pull/199));
  on Emmy v0.32.0 it registers nothing.
- `desargues.board`: interactive boards. A `BoardSpec` (a function of a
  variable and draggable params) compiles to a WebAssembly kernel plus the
  data plato renders: Emmy does the algebra (`desargues.board.algebra`), a
  `KernelCompiler` runs every number. Kinds are open (`desargues.board.kernel/
  plan`): `:calculus` (f, f' and the running integral) and `:construction`
  (points, lines, circles, curves, conics, groups, projective and Euclidean
  operations, 3D figures, reference checks).
- Raster kernels: `(solver :raster {:kernel? true})` runs the whole RK4
  integration inside one raster `deftm` (compiled Lagrangian systems and
  all-double pendulums), bit-identical to the stepper; `evolve-batch`
  integrates N systems that share a kernel in one call. Pendulum 2.2-3.9 ms ->
  0.19-0.37 ms; a 64-trajectory sweep 145 ms -> 13 ms.
- Benchmarks: `bb bench` runs the core, hot-path and geometry suites;
  `bb bench:check` compares a fresh run to `bench/results/baseline.edn` and
  exits 1 on a regression; `bb bench:baseline` saves one. See
  `doc/benchmarks.md`.
- CI: the pure suites, the raster dynamics suite and the Typed Clojure gate
  run on every push and pull request.

### Changed
- Performance, bit-identical results: Nelder-Mead on double arrays with a
  primitive objective (disposition solver, `fit-homography`, `fit-keyframes`),
  a primitive RK4 step for all-double states, Emmy `:primitive` `double[]`
  kernels for the Lagrangian state derivative and positions, and compiled
  state derivatives cached by physical parameters. 3-15x on the hot paths.
- `desargues.board.compiler` no longer requires raster: the raster
  `KernelCompiler` is `desargues.infrastructure.raster-board`, answered by
  `(kernel-compiler :raster)`. `raster-compiler` still works.
- `desargues.geometry.kernel/select-kernel` accepts any backend key with a
  `provider` method, besides `:array`, `:auto` and `:raster`.
- CI and the site build run on JDK 25.

## [0.2.0] - 2026-10-11

### Added
- `desargues.geometry`: a projective layer on Emmy generics (`projective`, with
  symbolic Desargues and Pappus proofs), a disposition solver (clp(FD) logic
  tier, Nelder-Mead polish, trajectory checks), display-homography fitting
  (`fit`), and emission of solved configurations as facade scenes.
- `desargues.geometry.kernel`: a `GeometryKernel` port with batched
  double-array cost and trajectory-sampling kernels; homography fits run about
  30x faster than on Emmy matrices, bit-identical results. A raster provider
  (`:dynamics`) is selectable; the pure-Clojure array kernel is the default.
  Benchmarks: `clojure -M:bench:dynamics -m desargues.bench.geometry`.
- `doc/cljw-lane.md` and `dev/cljw/`: measurements of desargues' pure half on
  ClojureWasm and a Zig wasm RK4 kernel (no-go for now, revisit conditions listed).
- `desargues.scene/path` and `desargues.scene/reshape`: one connected stroke
  through a list of points (`:color :width`, `:closed?`) and the animation that
  moves its points, the count free to change (a trail growing a sample at a
  time). Manim backend: a `VMobject` with `set_points_as_corners`, animated via
  `animate.set_points_as_corners` (`desargues.manim.paths`); recording backend:
  `:path` node, `:reshape` anim; plato (`path-mobject` branch) draws polylines
  and polygons and tweens their points.
- Landing page: the sin curve is a path and its tangent a line that follows the
  slope; the 3D double pendulum's trail is a growing path.
- Landing page, 3D double pendulum (`desargues.site.pendulum3d`): the camera
  orbits the vertical axis slowly, re-projecting the trail and a floor ring
  every sample, and each bob is a disc whose radius scales with its depth.
- `desargues.scene/line` and `desargues.scene/connect`: a straight segment
  mobject and the animation that moves its endpoints (a rod following a bob),
  implemented by the Manim backend (`Line`, `animate.put_start_and_end_on`) and
  the recording backend (`:line` node, `:connect` anim); plato plays them.
- `desargues.videos.lagrangian`: `PhysicalSystem`s whose equations of motion
  Emmy derives from a Lagrangian built from the masses' Cartesian positions,
  compiled with Emmy's compiler (simplification off); the planar and the
  spherical (3D) double pendulum ship as constructors.
- Landing page: the pendulum draws its rod, a projected 3D double pendulum with
  a trail, an Emmy-derived equations-of-motion slide.
- `desargues.pipeline.emmy`: a pure Collect/Promote/Pipeline/Boundary conveyor
  Emmy → LaTeX → Python code; `desargues.emmy-manim` delegates to it.
- `TrajectorySolver` port in `desargues.videos.physics` with a raster adapter
  behind the `:dynamics` alias: RK4 and Euler on a fixed grid, Tsit5 and DP5
  adaptive, both resampled onto the `dt` grid by default.
- `desargues.videos.interpret`: timeline → scene interpreter (it existed but was
  hidden by an unanchored `videos/` ignore rule).
- `:test` alias (cognitect test-runner), `:dynamics` alias (raster), `:doctor`
  alias (`desargues.doctor` environment check), `:bench` alias
  (`desargues.bench`, EDN report + SVG charts), `:site` alias (plato landing page).
- Python-guarded layout realize smoke test; `manim-test` skips without a Manim env.
- `bb.edn` tasks: `doctor`, `test`, `test:dynamics`, `typecheck`, `bench`, `site`.

### Changed
- `MathExpression` hash is consistent with `=`; ids are content-derived.
- `desargues.config/add-project-to-syspath!` defaults to `<root>/py`; the Python
  scene files live under `py/`.
- Verified against Manim Community 0.21.0.
- `desargues.core` usage strings name `clojure -M:run` (they still said `lein run`).

### Removed
- The legacy Leiningen `project.clj`; tools.deps is the only build.
- Stale setup/status documents that still referred to the pre-rename `varcalc`
  namespaces (`QUICKSTART`, `SETUP_COMPLETE`, `SUCCESS`, `EVALUATION_SUMMARY`,
  `README_MANIM`, `MCP_SETUP`, `intro`).

## [0.1.1] - 2026-07-09

### Added
- Backend-neutral scene facade `desargues.scene` with a Manim backend and a pure
  recording backend (EDN scene graphs, no Python).
- Layout algebra `desargues.layout.*` (elm-ui style) with a pure extent estimator.
- Pure domain services; the Python barrier factored out of the domain.

### Changed
- Manim paths derived from the environment (`DESARGUES_*`, `CONDA_PREFIX`); no
  machine paths in source.
- Typed Clojure checker moved to the `:dev` alias.

## [0.1.0] - 2026-07-09

### Added
- First tools.deps release: git coordinates for hive-dsl and hive-di, `VERSION`,
  `bb.edn`.
- Emmy → LaTeX → Manim pipeline, the DevX hot-reload segment DAG, the
  brachistochrone derivation and number-theory demos (work from 2025-12).

[Unreleased]: https://github.com/mentat-collective/desargues/compare/v0.2.0...HEAD
[0.2.0]: https://github.com/mentat-collective/desargues/compare/v0.1.1...v0.2.0
[0.1.1]: https://github.com/mentat-collective/desargues/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/mentat-collective/desargues/releases/tag/v0.1.0
