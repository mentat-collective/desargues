# desargues

**Emmy + Manim — backend-neutral mathematical animation in Clojure**

desargues bridges [Emmy](https://github.com/mentat-collective/emmy) (symbolic
mathematics) and [Manim Community Edition](https://www.manim.community/)
(mathematical animation) through [libpython-clj](https://github.com/clj-python/libpython-clj).
It computes derivatives symbolically, converts them to LaTeX, and renders them
behind a **backend-neutral scene facade**: the same scene code writes an mp4
under Manim, or records a pure EDN scene graph with no Python at all.

**Landing page:** <https://mentat.org/desargues/> — a
[plato](https://github.com/BuddhiLW/plato) deck built by desargues itself, with
live scenes and the benchmark charts below.

- 🧮 **Symbolic mathematics** — define functions, differentiate, simplify via Emmy
- 🎬 **Animation** — the Emmy → LaTeX → Python → Manim pipeline
- 🔌 **Backend-neutral facade** — `desargues.scene` programs to a protocol; a pure
  `RecordingBackend` renders the same scene to EDN
- 📐 **Layout algebra** — an elm-ui-inspired layout DSL with a pure extent
  estimator (measure → resolve → realize)
- 🧭 **Numeric backends, open by key**: every numeric port (trajectory solvers,
  geometry kernels, board compilers) dispatches on a backend key: `:jvm` is
  built in, `:raster` is one alias away, and a new engine is a registration
  (`desargues.backend`)
- 📐 **Projective geometry layer** — `desargues.geometry.*`: emmy-generic
  projective primitives (join/meet duality, cross ratio, homographies, conics,
  symbolic Desargues/Pappus proofs), a clp(FD) + Nelder-Mead disposition
  solver that chooses scene layouts, display-homography fitting, and emission
  of solved configurations as facade scenes
- ⚙️ **Environment-driven config** — no machine paths baked into source

## Install

### As a library

```clojure
;; deps.edn
{:deps {io.github.mentat-collective/desargues
        {:git/tag "v0.3.0" :git/sha "1cd856f"}}}
```

`main` carries more than the last tag (see [CHANGELOG](doc/CHANGELOG.md)); pin a
sha from `main` to get it.

### From a clone

```bash
git clone https://github.com/mentat-collective/desargues
cd desargues
clojure -P            # pre-fetch dependencies (about 10 s)
bb doctor             # or: clojure -M:doctor
```

Requirements: **JDK 11+** and the **Clojure CLI**. [babashka](https://babashka.org)
is optional; every `bb` task below is one `clojure` command. That is all the pure
half needs: the recording backend, the layout algebra, the Emmy conveyor and the
physics run with no Python.

### The Manim backend

Rendering video needs one conda env with Manim (and its LaTeX + ffmpeg):

```bash
conda create -n manim -c conda-forge python=3.12 manim
conda activate manim      # sets CONDA_PREFIX; desargues.config picks it up
bb doctor                 # every row should read ok
```

`clojure -M:doctor` reports each requirement as `ok` / `missing` with the fix:
the JVM, the recording backend, the resolved conda prefix, `import manim`,
`latex`, `ffmpeg`. It exits 0 when the pure path works; the Manim rows are advisory.

Paths are **derived from the environment** by `desargues.config`: it reads
`DESARGUES_CONDA_PREFIX`, else `CONDA_PREFIX`, and probes `bin/python`,
`lib/libpython3.x.so` and `site-packages` under it. To target an env that is not
the active one, or to override a single path:

| Variable | Purpose |
|---|---|
| `DESARGUES_CONDA_PREFIX` | conda env root (wins over `CONDA_PREFIX`) |
| `DESARGUES_MANIM_PYTHON` | python executable |
| `DESARGUES_MANIM_LIBPYTHON` | `libpython*.so` |
| `DESARGUES_MANIM_SITEPACKAGES` | site-packages dir |
| `DESARGUES_PROJECT_ROOT` | root whose `py/` goes on Python `sys.path` |
| `DESARGUES_QUALITY` | render quality (default `medium_quality`) |

`(desargues.config/manim-config)` returns the resolved map, or throws a message
naming these variables. Verified against Manim Community 0.21.0.

## Sixty seconds, no Python

```clojure
(require '[desargues.scene :as s]
         '[desargues.scene.data :as rec]
         '[desargues.layout.core :as l])

;; A declarative layout, realized to an EDN scene graph
(rec/layout->scene-graph "hello"
  (l/column {:padding 0.6 :spacing 0.4 :align :center-x}
    (l/text "Reserves")
    (l/math "\\Delta M = 1000"))
  {})
;; => {:scene "hello" :nodes {1 {:node :text ...} 2 {:node :math ...}} :steps [...] ...}

;; An imperative scene, recorded instead of rendered
(defn construct [stage]
  (let [t (s/text "Hello")]
    (s/play! stage (s/appear t))
    (s/hold! stage 2)))

(s/with-backend (rec/recording-backend)
  (s/render! "demo" construct))
;; => {:scene "demo" :nodes {...} :steps [{:step :play ...} {:step :hold :seconds 2}]}
```

That data is what [plato](https://github.com/BuddhiLW/plato) plays in the browser.

## Quick start with Manim

```clojure
(require '[desargues.api :as v])
(require '[emmy.env :as e])

(v/init!)                          ; bring up Python/Manim (via desargues.config)

(def f  (v/func "f" #(e/sin %)))   ; a MathFunction
(def df (v/derivative f))          ; Emmy differentiates functions

(v/to-latex f)                     ; => "\\sin\\left(x\\right)"
(v/to-latex df)                    ; => "\\cos\\left(x\\right)"
(v/animate-derivative f)           ; render f and f' side by side
```

The same `construct` fn from above renders for real with the default (Manim)
backend, `(s/render! "demo" construct)`, and a layout renders with

```clojure
(v/render-layout! "balance"
  (v/column {:padding 0.6 :spacing 0.4 :align :center-x}
    (v/text "Reserves")
    (v/math "\\Delta M = 1000")))
```

Videos land under `media/videos/<quality>/`.

## Commands

| Task | Command | Notes |
|---|---|---|
| Environment check | `bb doctor` · `clojure -M:doctor` | pure path + Manim env |
| Tests | `bb test` · `clojure -M:test` | Manim suites skip without the env |
| Tests + raster | `bb test:dynamics` · `clojure -M:test:dynamics` | solver conformance |
| Type check | `bb typecheck` · `clojure -M:dev -m desargues.typecheck` | Typed Clojure gate |
| Benchmarks | `bb bench` · `clojure -M:bench:dynamics` | add `--manim` for render timings |
| Landing page | `bb site` | plato deck → `dist/site` |
| Demos | `clojure -M:run [intro\|step N\|quickstart\|derivative]` | needs the Manim env |
| Dev REPL | `clojure -A:dev` | `dev/` sources, orchestra, the checker |

## Benchmarks

`bb bench` measures the pure pipeline with a warm-up and batched samples, writes
`bench/results/latest.edn`, and regenerates the charts below (`doc/bench/`).
`--manim` adds one Manim render per quality. Numbers are from one machine and
change with the JVM; run it on yours.

![Pure backend scaling](doc/bench/scaling.svg)

![Trajectory solvers](doc/bench/solvers.svg)

![Emmy conveyor](doc/bench/emmy-conveyor.svg)

What they say: the recording backend and the layout algebra are linear in the
number of elements (a thousand-element layout resolves in milliseconds), a
10-second pendulum at `dt = 0.01` costs a few milliseconds with any solver, and
differentiating a function plus rendering both to LaTeX is tens of microseconds.

## Architecture

Layered, with the dependency arrow pointing inward toward the pure domain:

```
api            desargues.api            high-level facade (symbolic → animation)
scene facade   desargues.scene          backend-neutral animation API  ── DIP seam
  ├ backend    desargues.scene.manim    ManimBackend  (draws via libpython-clj)
  └ backend    desargues.scene.data     RecordingBackend (records EDN, no Python)
layout         desargues.layout.*       elm-ui layout algebra + pure extent measure
pipeline       desargues.pipeline.emmy  Emmy → LaTeX → Python, pure CPPB conveyor
backends       desargues.backend        backend key → provider namespaces, loaded on demand
kernels        desargues.kernel         realize (by target), Emmy kernels as data, JVM kernel
physics        desargues.videos.physics TrajectorySolver port; (solver :jvm | :raster | :wasm | :gpu)
boards         desargues.board          BoardSpec → wasm kernel + data for plato
lagrangian     desargues.videos.lagrangian  Emmy derives q'' from L = T − V (double pendulums)
manim          desargues.manim.*        low-level Manim mobject/animation bindings
domain         desargues.domain.*       pure Clojure + Emmy, Typed-Clojure-annotated
config         desargues.config         env-derived settings (the Collect layer)
```

The seam is `desargues.scene.protocols`: both backends implement the same
protocols, so consumers depend on the abstraction, never on Manim (Dependency
Inversion). Because the pure `RecordingBackend` and the Manim backend are
interchangeable behind that facade (Liskov substitution), the neutrality is
*checkable*: run any scene through the recording backend and assert on the data.

```
desargues/
├── src/desargues/        api, scene (+ manim, data backends), layout, pipeline,
│                         domain, infrastructure (raster adapter), devx, videos, config
├── py/                   Python scene definitions importable via desargues.config
├── test/desargues/       pure suites + Python-guarded suites (skip without the env)
├── bench/                desargues.bench + charts        (clojure -M:bench)
├── site/                 the plato landing page deck     (bb site)
├── dev/                  REPL helpers, typecheck gate
└── doc/                  guides, ADRs, changelog
```

Typed Clojure is split across the two classpaths: the annotation runtime is a
normal dependency because `desargues.domain.*` carry `t/ann` annotations, while the
checker lives in the `:dev` alias. The raster solvers live in `:dynamics` because
raster pins a newer typed runtime.

### Backends

A backend is a keyword that names who runs the numbers. Each port owns a
multimethod on that key, and `desargues.backend` loads a backend's provider
namespaces the first time it is named:

```clojure
(require '[desargues.videos.physics :as phys])

(phys/evolve pendulum {:dt 0.01 :duration 10})                        ; :jvm, the default
(phys/evolve pendulum {:solver [:raster {:alg :tsit5}] :dt 0.01 :duration 10})
(phys/evolve-batch systems {:solver [:raster {:kernel? true}] :dt 0.01 :duration 10})
```

`:raster` needs the `:dynamics` alias and JDK 24 or newer; without it the port
says so.

#### Trajectories as WebAssembly

A compiled Lagrangian system (`desargues.videos.lagrangian`) carries the Emmy
source of its state derivative. raster compiles that source, with the whole
batched RK4 loop around it, to a self-contained wasm module, and the `:wasm`
backend runs the module on the JVM through Chicory:

```clojure
(require '[desargues.videos.lagrangian :as lag])
(def dp (lag/double-pendulum {}))

(phys/evolve dp {:solver :wasm :dt 0.01 :duration 10})   ; same grid, ~1e-13 from :jvm

(require '[desargues.infrastructure.wasm-runtime :as wasm])
(:bytes (wasm/trajectory-module dp))   ; the module a browser runs: export rk4, ABI in
                                       ; raster-kernels/wasm-kernel
```

raster's wasm backend has no sin/cos opcode and lowers them to a 5e-6
polynomial, which moved a 3 s chaotic trajectory by 3e-3. desargues rewrites
them first with fdlibm's reduction and kernels (`desargues.kernel.libm`,
within 2^-53 of `java.lang.Math`), so the module tracks the JVM stepper to
~1e-13. Measured on the planar double pendulum, 300 steps: V8 (node 22) runs
it in 0.42 ms per trajectory and 6.5 ms for 64 in one call, close to the JVM
raster kernel. Chicory takes about 10 ms; it is there to prove the module, not
to race it.

#### Trajectories on a GPU

The same batched RK4 is also emitted as one `raster.par/map-void!` over
trajectories, which raster compiles to an OpenCL (or Level Zero) kernel through
Panama, no JNI. The `:gpu` backend runs compiled Lagrangian systems and
all-Double pendulums (per-trajectory g/L and damping, so a parameter sweep is
one launch); anything else goes to the JVM stepper:

```clojure
(with-open [gs (phys/solver :gpu)]            ; or [:gpu {:device :ze:0}]
  (phys/evolve-batch sweep {:solver gs :dt 0.01 :duration 10}))
```

A solver owns one device session and compiles each kernel once; close it to
release the device. Pendulums × 800 steps, CPU kernel vs RTX 4070 Laptop:
64 → 3.5 vs 3.6 ms, 1024 → 67 vs 18 ms, 8192 → 456 vs 79 ms. Values track the
CPU to ~1e-14 per pendulum and ~5e-13 over 3 s of a chaotic double pendulum
(the device's sin and fused multiply-adds round differently). It needs
`:dynamics` and an OpenCL ICD; `gpu-solver/device-available?` asks first.

A new engine registers a key and adds methods in its own namespace:

```clojure
(backend/register-backend! :my-engine {:namespaces '[my.engine.desargues]})

;; in my.engine.desargues
(defmethod phys/solver :my-engine [_ & [opts]] (->MySolver opts))
```

## Documentation

- [ARCHITECTURE.md](doc/ARCHITECTURE.md) — SOLID/DDD design and the layer contracts
- [BOUNDED_CONTEXTS.md](doc/BOUNDED_CONTEXTS.md) — the domain / rendering / devx contexts
- [ADR_CODE_QUALITY.md](doc/ADR_CODE_QUALITY.md) — code quality standards
- [CLEAN_API_GUIDE.md](doc/CLEAN_API_GUIDE.md) — the `desargues.api` facade
- [EMMY_MANIM_GUIDE.md](doc/EMMY_MANIM_GUIDE.md) · [EMMY_MANIM_QUICKREF.md](doc/EMMY_MANIM_QUICKREF.md) — the Emmy → LaTeX → Manim pipeline
- [EQUATION_EVALUATION_GUIDE.md](doc/EQUATION_EVALUATION_GUIDE.md) — evaluating functions and tables
- [MANIM_SETUP.md](doc/MANIM_SETUP.md) — Manim via libpython-clj, troubleshooting
- [BRACHISTOCHRONE.md](BRACHISTOCHRONE.md) — the brachistochrone derivation videos
- [ROADMAP.md](doc/ROADMAP.md) — the DevX hot-reload roadmap
- [CHANGELOG.md](doc/CHANGELOG.md)

DevX (Figwheel-style hot reload of animation segments) is documented in
[CLAUDE.md](CLAUDE.md#devx-module-hot-reload--smart-preview-system).

## Resources

- Emmy — https://github.com/mentat-collective/emmy
- Manim Community — https://www.manim.community/
- libpython-clj — https://github.com/clj-python/libpython-clj
- plato — https://github.com/BuddhiLW/plato

## License

This program and the accompanying materials are made available under the terms of
the Eclipse Public License 2.0 (https://www.eclipse.org/legal/epl-2.0/), or (at
your option) the GNU General Public License, version 2 or later, with the GNU
Classpath Exception (https://www.gnu.org/software/classpath/license.html).
