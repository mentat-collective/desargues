# Changelog

All notable changes to desargues. The format follows
[keepachangelog.com](https://keepachangelog.com/); a version is a git tag
(`vX.Y.Z`) that consumers pin as a tools.deps git coordinate.

## [Unreleased]

### Added
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

[Unreleased]: https://github.com/mentat-collective/desargues/compare/v0.1.1...HEAD
[0.1.1]: https://github.com/mentat-collective/desargues/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/mentat-collective/desargues/releases/tag/v0.1.0
