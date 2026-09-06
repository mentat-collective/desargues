# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**Desargues** is a Clojure library that bridges Emmy (symbolic mathematics) with Manim Community Edition (mathematical animation) via libpython-clj. The system computes derivatives symbolically, converts them to LaTeX, and renders beautiful mathematical animations.

**Technology Stack:**
- Clojure with tools.deps (`deps.edn`); babashka tasks via `bb.edn`. A legacy Leiningen `project.clj` is retained but is not the primary build.
- Emmy for symbolic mathematics
- Manim Community (Python) for mathematical animations
- libpython-clj for Clojure-Python interop
- Conda environment for Python dependencies

## Essential Commands

### Build & Run (tools.deps)
```bash
# Run main animation demo (sin(x) derivative)
clojure -M:run

# Dev REPL (dev/ sources + orchestra + the Typed Clojure checker)
clojure -A:dev

# Static Typed Clojure gate
clojure -M:dev -m desargues.typecheck

# Pre-fetch dependencies
clojure -P
```

### Python Environment
```bash
# Activate conda environment (required before running)
conda activate manim

# Verify Manim installation
manim --version
```

### Generated Output
Videos are saved to `media/videos/1080p60/` after rendering. Quality can be changed by editing `:quality` parameter in `src/desargues/manim_quickstart.clj` (`"low_quality"`, `"medium_quality"`, or `"high_quality"`).

## Architecture: Layered DDD/SOLID Design

This codebase follows **Domain-Driven Design** with **SOLID principles**. Understanding the layer boundaries is critical:

### Layer Structure (Dependency Direction: Top → Down)

```
API Layer (Facade)
    ↓ depends on
Application/Domain Layer (Core Logic)
    ↓ depends on
Infrastructure Layer (External Systems)
```

### Layer Responsibilities

**1. API Layer** (`src/desargues/api.clj`)
- Facade pattern providing clean, simple interface
- Factory methods: `expr`, `func`, `point`
- Fluent builders: `scene`, `show`, `transform`, `wait`, `render!`
- High-level workflows: `animate-expression`, `animate-derivative`
- **Never contains business logic**

**2. Domain Layer** (`src/desargues/domain/`)
- `protocols.clj`: Protocol definitions (interfaces)
  - `IMathematicalObject`, `IEvaluable`, `IDifferentiable`
  - `IRenderable`, `IAnimatable`, `IScene`
- `math_expression.clj`: Domain entities and value objects
  - Entities: `MathExpression`, `MathFunction` (have identity)
  - Value Objects: `LaTeX`, `PythonCode`, `Point` (immutable, value-based equality)
- `services.clj`: Domain services
  - `differentiate-expression`, `evaluate-expression`, `simplify-expression`
  - `tabulate-function`, `compare-at-points`
- **Pure Clojure + Emmy, no external dependencies**
- **No I/O, no rendering concerns**

**3. Infrastructure Layer** (`src/desargues/infrastructure/`)
- `manim_adapter.clj`: Manim integration
  - Implements domain protocols for Python interop
  - `ManimMobject`, `ManimAnimation`, `ManimSceneBuilder`
  - Adapter pattern wrapping Manim Python objects
- **All Python/Manim code isolated here**
- **Depends on domain protocols, not vice versa**

### Key Design Patterns Used

- **Facade**: `api.clj` hides complexity of lower layers
- **Builder**: `ManimSceneBuilder` for fluent scene construction
- **Adapter**: `ManimMobject` adapts Python objects to Clojure protocols
- **Strategy**: Protocol-based polymorphism for different object types
- **Factory**: Factory functions for creating domain objects
- **Repository**: Protocols defined (not yet implemented) for persistence

## Critical Implementation Details

### Python Initialization

**ALWAYS initialize Python/Manim before any Manim operations.** Paths are resolved
by `desargues.config` from the environment (see the Configuration section of
`README.md`) — never hardcoded in source.

```clojure
(require '[desargues.scene :as s])
(s/init!)                    ; brings the Manim backend up (idempotent)
;; or, for the high-level API:
(require '[desargues.api :as v])
(v/init!)
```

Configuration is derived from `CONDA_PREFIX` / `DESARGUES_CONDA_PREFIX` plus the
`DESARGUES_MANIM_*` overrides. Inspect the resolved values with
`(desargues.config/manim-config)`. If Manim isn't found, activate the conda env or
set `DESARGUES_CONDA_PREFIX` — do **not** edit source paths.

### Emmy → LaTeX → Python Pipeline

The conversion pipeline is critical to understand:

1. **Emmy expression** → `(e/D f)` computes derivative
2. **Emmy → LaTeX** → `(e/->TeX expr)` generates LaTeX string
3. **LaTeX → Python** → `latex2py` converts to Python code
4. **Python → Manim** → Manim renders the LaTeX

**Key functions:**
- `src/desargues/emmy_manim.clj`: `emmy->latex`, `latex->python`, `emmy->python`
- Pipeline in `src/desargues/emmy_manim_examples.clj:create-derivative-animation`

### Python Class Instantiation

When creating Python scene classes with keyword arguments:

```clojure
;; CORRECT: Direct function call with keyword args
(FunctionAndDerivative :func_latex "..." :deriv_latex "...")

;; INCORRECT: Do not use py/call-attr-kw
(py/call-attr-kw FunctionAndDerivative [] {:func_latex "..." ...})
```

### Protocol Method Naming

**WARNING**: `wait` is a reserved method name (conflicts with `Object.wait()`). Use `wait-duration` in protocols:

```clojure
(defprotocol IScene
  (wait-duration [this duration])  ;; NOT 'wait'
  ...)
```

### Emmy Function vs Expression

Emmy works with **functions** (callable) not raw expressions:

```clojure
;; CORRECT: Define as function
(defn f [x] (e/sin x))
(def df (e/D f))

;; Then evaluate symbolically
(f 'x)   ;; Returns symbolic expression
(df 'x)  ;; Returns derivative expression

;; INCORRECT: Using raw symbols
(def expr '(sin x))  ;; This is just a list, not an Emmy expression
```

### Forward References

When defining records that reference each other, use `declare`:

```clojure
;; Forward declaration
(declare render-scene-impl)

(defrecord ManimSceneBuilder [...]
  p/IScene
  (render-scene [this]
    (render-scene-impl this)))  ;; Can use before definition

;; Define later
(defn render-scene-impl [builder] ...)
```

## File Organization

### Entry Points
- `src/desargues/core.clj`: Main `-main` function (invoked by `clojure -M:run`)
- `src/desargues/api.clj`: Primary API for library users

### Integration Layers
- `src/desargues/manim_quickstart.clj`: Basic Manim setup and initialization
- `src/desargues/emmy_manim.clj`: Emmy ↔ Manim conversion utilities
- `src/desargues/emmy_manim_examples.clj`: Complete example workflows

### Python Scene Definitions
- `manim_examples.py`: Basic Manim scenes (CreateCircle, SquareToCircle)
- `emmy_manim_scenes.py`: Emmy-driven scenes (FunctionAndDerivative, ChainRule, TaylorSeries)
- `equation_evaluation_scenes.py`: Evaluation and table scenes

Python files live in `py/` under the project root (root resolved by `desargues.config`; override with `DESARGUES_PROJECT_ROOT`) so Clojure can import them via `py/import-module`. Call `(desargues.config/add-project-to-syspath!)` to put `<root>/py` on `sys.path` (pass a subdir, or `nil` for the bare root, to add another directory).

### Tests
- Pure suites (no Python): `test/desargues/domain/pure_test.clj`, `test/desargues/pipeline/emmy_test.clj`, `test/desargues/layout/core_test.clj`, `test/desargues/properties/physics_test.clj`, the `devx` suites
- Python-guarded suites: `test/desargues/manim_test.clj` (Manim integration), `test/desargues/layout/realize_test.clj` (layout backend smoke + low-quality render); both need the conda env (`CONDA_PREFIX` / `DESARGUES_CONDA_PREFIX`), realize_test skips loudly without it
- `test/desargues/infrastructure/raster_adapter_test.clj`: solver conformance; the raster provider runs only under `-A:dynamics`, otherwise it skips loudly
- Run with `clojure -M:test` (cognitect test-runner); add `:dynamics` for raster

## Common Development Workflows

### Creating a New Mathematical Animation

**Option 1: Using High-Level API**
```clojure
(require '[desargues.api :as v])
(v/init!)

;; Define function
(defn my-func [x] (e/exp (e/sin x)))

;; Create and animate derivative
(def e (v/expr (my-func 'x)))
(v/animate-derivative e)
```

**Option 2: Using Builder Pattern**
```clojure
(require '[desargues.api :as v])
(v/init!)

(def e1 (v/expr '(sin x)))
(def e2 (v/derivative e1))

(-> (v/scene)
    (v/show e1)
    (v/wait 1)
    (v/transform e1 e2)
    (v/wait 2)
    (v/render!))
```

**Option 3: Using Python Scene Classes**
```clojure
(require '[desargues.emmy-manim-examples :as ex])
(ex/render-emmy-scene "ChainRule")
```

### Adding a New Python Scene

1. Create scene class in `emmy_manim_scenes.py`:
```python
class MyScene(Scene):
    def __init__(self, **kwargs):
        self.param = kwargs.get('param', 'default')
        super().__init__()

    def construct(self):
        # Animation code
        pass
```

2. Import and use from Clojure:
```clojure
(require '[desargues.config :as config])
(config/add-project-to-syspath!)   ; prepends the resolved project root onto sys.path

(let [scenes (py/import-module "emmy_manim_scenes")
      MyScene (py/get-attr scenes "MyScene")
      scene (MyScene :param "value")]
  (mq/render-scene! scene))
```

### Extending Domain with New Protocols

To add new mathematical capabilities:

1. Define protocol in `src/desargues/domain/protocols.clj`:
```clojure
(defprotocol IIntegrable
  (integrate [this] "Compute indefinite integral"))
```

2. Implement for domain entities in `src/desargues/domain/math_expression.clj`:
```clojure
(extend-type MathExpression
  p/IIntegrable
  (integrate [this]
    (let [result (e/integrate (:expr this))]
      (create-expression result))))
```

3. Add domain service in `src/desargues/domain/services.clj`:
```clojure
(defn integrate-expression [math-expr]
  (p/integrate math-expr))
```

4. Expose in API (`src/desargues/api.clj`):
```clojure
(defn integral [math-obj]
  (svc/integrate-expression math-obj))
```

## Debugging Tips

### Python Import Errors
If you see `ModuleNotFoundError: No module named 'manim'`:
- Check the conda environment is activated: `conda activate manim` (sets `CONDA_PREFIX`)
- Inspect the resolved config: `(desargues.config/manim-config)`
- If the manim env isn't the active one, set `DESARGUES_CONDA_PREFIX` (or the `DESARGUES_MANIM_*` overrides)

### LaTeX Rendering Errors
If LaTeX fails to render:
- Install texlive packages: `sudo apt-get install texlive texlive-latex-extra texlive-fonts-extra`
- Check Manim works standalone: `manim --version`

### Emmy Evaluation Issues
Remember Emmy distinguishes between:
- Symbolic evaluation: `(f 'x)` → returns expression
- Numeric evaluation: `(f 3.14)` → returns number
- Use `e/simplify` to simplify symbolic results

### Video Not Generated
- Check console output for errors
- Try `low_quality` rendering first (faster)
- Verify `media/videos/` directory exists

## Testing Strategy

Tests are integration tests that verify:
- Python initialization
- Manim module imports
- Scene class creation
- Rendering pipeline

Run tests before committing:
```bash
clojure -M:test                      # every suite (Manim suites need the conda env)
clojure -M:test:dynamics             # plus the raster TrajectorySolver conformance
clojure -M:test -n desargues.domain.pure-test   # one namespace
```

Pure suites must be green everywhere; the Python-guarded suites must be green with the
conda env active (verified against Manim CE 0.21.0). A namespace that fails to LOAD
contributes zero assertions, so read the runner's load output, not only the summary.

## Important Configuration Files

- `project.clj`: Leiningen dependencies and build config
- `.gitignore`: Excludes `media/`, `target/`, Python `__pycache__`
- Python scene files must be in project root for `py/import-module` to work

## Video Quality Settings

Edit `src/desargues/manim_quickstart.clj:render-scene!`:
```clojure
(py/call-attr scene "render"
              :quality "low_quality"    ; 480p15 - fast
              :quality "medium_quality" ; 720p30 - balanced
              :quality "high_quality"   ; 1080p60 - best (default)
              :preview false)
```

---

## DevX Module: Hot-Reload & Smart Preview System

The DevX module (`desargues.devx.*`) provides Figwheel-style hot-reload for Manim animations. This section documents how to use it.

### Core Concepts

| Concept | Description |
|---------|-------------|
| **Segment** | A discrete, cacheable portion of an animation with unique ID, hash, dependencies, and render state |
| **Scene Graph** | DAG of segments with dependency tracking and topological sorting |
| **Hot Reload** | File watcher detects changes → reloads namespace → marks affected segments dirty → re-renders |
| **Preview** | Last-frame image preview or OpenGL live preview window |

### Segment States

```
:pending → :dirty → :rendering → :cached
                         ↓
                      :error
```

- `○` pending - Never rendered
- `◐` dirty - Needs re-render (code changed)
- `◔` rendering - Currently rendering
- `●` cached - Rendered and up-to-date
- `✗` error - Render failed

### Quick Start

```clojure
;; 1. Load devx modules
(require '[desargues.devx.core :as devx])
(require '[desargues.devx.repl :as repl])

;; 2. Initialize Manim
(devx/init!)

;; 3. Define segments
(def intro
  (devx/segment :intro
    (fn [scene]
      (let [title (mob/text "Hello!")]
        (manim/play! scene (anim/write title))))))

(def main
  (devx/segment :main
    :deps #{:intro}  ; Declares dependency
    (fn [scene]
      (let [circle (mob/circle)]
        (manim/play! scene (anim/create circle))))))

;; 4. Build scene graph and load into REPL
(def my-scene (devx/scene [intro main]))
(repl/set-graph! my-scene)

;; 5. Start hot-reload
(repl/w!)  ; Watch with auto-render
```

### REPL Command Reference

#### State Management
| Command | Description |
|---------|-------------|
| `(repl/set-graph! g)` | Set current working graph |
| `(repl/get-graph)` | Get current graph |
| `(repl/clear-graph!)` | Clear current graph |

#### Status & Info
| Command | Shortcut | Description |
|---------|----------|-------------|
| `(repl/status)` | `(repl/s!)` | Print full status |
| `(repl/quick-status)` | - | One-line summary |
| `(repl/show-graph)` | - | ASCII dependency graph |
| `(repl/segments)` | - | List all segment IDs |
| `(repl/dirty-segments)` | - | List dirty segment IDs |
| `(repl/stats)` | - | Statistics map |

#### Rendering
| Command | Shortcut | Description |
|---------|----------|-------------|
| `(repl/render!)` | `(repl/r!)` | Render dirty segments |
| `(repl/render-all!)` | - | Force re-render all |
| `(repl/render-segment! :id)` | - | Render specific segment |

#### Preview (Phase 3)
| Command | Shortcut | Description |
|---------|----------|-------------|
| `(repl/preview! :id)` | `(repl/p! :id)` | Last-frame preview (opens image) |
| `(repl/live! :id)` | `(repl/L! :id)` | OpenGL live preview window |
| `(repl/live! :id :interactive? true)` | - | Interactive with IPython prompt |
| `(repl/select!)` | `(repl/sel!)` | Interactive segment menu |
| `(repl/select-and-preview!)` | - | Select then preview |
| `(repl/select-and-live!)` | - | Select then live preview |

#### Change Management
| Command | Shortcut | Description |
|---------|----------|-------------|
| `(repl/dirty! :id)` | `(repl/d! :id)` | Mark segment dirty |
| `(repl/dirty-all!)` | - | Mark all dirty |

#### Export
| Command | Shortcut | Description |
|---------|----------|-------------|
| `(repl/combine! "out.mp4")` | `(repl/c! "out.mp4")` | Combine cached segments |
| `(repl/export! "out.mp4")` | - | Render + combine |

#### Hot Reload
| Command | Shortcut | Description |
|---------|----------|-------------|
| `(repl/watch!)` | - | Start watcher (manual render) |
| `(repl/watch! :auto-render? true)` | `(repl/w!)` | Watch + auto-render |
| `(repl/watch! :auto-render? true :auto-combine? true)` | `(repl/W!)` | Watch + auto-render + auto-combine |
| `(repl/unwatch!)` | `(repl/uw!)` | Stop watcher |
| `(repl/watching?)` | - | Check if watching |
| `(repl/watch-status)` | - | Watcher statistics |
| `(repl/reload!)` | - | Manual reload |
| `(repl/tracking-report)` | - | Namespace tracking report |

#### Help
| Command | Description |
|---------|-------------|
| `(repl/help)` | Print full command reference |

### Quality Settings

```clojure
;; Use :quality option on render commands
(repl/render! :quality :high)
(repl/preview! :intro :quality :medium)

;; Presets:
;; :low    = 480p @ 15fps (fast iteration)
;; :medium = 720p @ 30fps (balanced)
;; :high   = 1080p @ 60fps (production)

;; Auto-quality based on context
(require '[desargues.devx.quality :as quality])
(quality/with-context :export
  (repl/render!))  ; Uses :high automatically
```

### Typical Development Workflow

```clojure
;; 1. Define your segments in a source file
;; src/myproject/scenes.clj

;; 2. In REPL, load and start watching
(require '[myproject.scenes :as scenes])
(repl/set-graph! scenes/my-scene)
(repl/w!)  ; Start hot-reload with auto-render

;; 3. Edit your source file...
;; Console shows:
;;   [change] src/myproject/scenes.clj
;;   [reload] Reloading: myproject.scenes
;;   [dirty] 2 segment(s) marked dirty: #{:main :outro}
;;   [render] Rendering segment: main...
;;   [render] Completed in 2.3s

;; 4. Preview specific segment
(repl/p! :main)   ; Opens image viewer with last frame
(repl/L! :main)   ; Opens OpenGL preview window

;; 5. Interactive selection
(repl/sel!)
;; Segments (3)
;; ───────────────
;;   1. ● intro     [abc123]
;;   2. ◐ main      [def456] → intro
;;   3. ○ outro     [ghi789] → main
;;
;; Select segment (number, q=quit, g=graph, d=dirty): 

;; 6. Export final video
(repl/export! "my-animation.mp4" :quality :high)
```

### DevX File Structure

```
src/desargues/devx/
├── core.clj        # Pure API facade (stateless)
├── repl.clj        # Stateful REPL workflow (hot-reload commands)
├── segment.clj     # Segment record, states, hashing
├── graph.clj       # Scene graph DAG, topological sort
├── renderer.clj    # Manim rendering, partial files
├── watcher.clj     # File watching (Beholder)
├── ns_tracker.clj  # Namespace dependency tracking
├── reload.clj      # Hot-reload coordination (clj-reload)
├── preview.clj     # Last-frame preview, image viewers
├── opengl.clj      # OpenGL live preview
├── selector.clj    # Interactive segment selector
├── quality.clj     # Quality presets, auto-quality
├── events.clj      # Observer pattern for events
├── specs.clj       # Specification pattern for queries
├── repository.clj  # Persistence (memory/file)
├── backend.clj     # Strategy pattern interface
└── backend/
    └── manim.clj   # Manim backend implementation
```

### Key Protocols

| Protocol | Location | Purpose |
|----------|----------|---------|
| `ISegment` | segment.clj | Segment identity and state |
| `IHashable` | segment.clj | Content-addressable hashing |
| `IRenderBackend` | backend.clj | Strategy for rendering |
| `ISpecification` | specs.clj | Composable query predicates |
| `IGraphRepository` | repository.clj | Persistence abstraction |
| `IQualityProvider` | quality.clj | Quality settings provider |

### Important Implementation Details

#### Segment Metadata for Hot-Reload

The `defsegment` macro captures `:source-ns` metadata automatically. When a namespace changes, the tracker finds all segments with matching `:source-ns` and marks them dirty.

```clojure
;; Segment stores source namespace
(:source-ns (meta my-segment))  ; => 'myproject.scenes
```

#### Content-Addressable Hashing

Segments use SHA-256 hashing for change detection:
- Hash includes: construct-fn source, dependencies, metadata
- Changed hash = segment needs re-render

```clojure
(seg/compute-segment-hash segment dep-hashes)
;; => "abc123..." (SHA-256 base64, first 12 chars)
```

#### Partial Movie Files

Each segment renders to a partial file in `media/partial_movie_files/`:
```
media/partial_movie_files/
├── intro_abc123.mp4
├── main_def456.mp4
└── outro_ghi789.mp4
```

Combine with ffmpeg:
```clojure
(renderer/combine-partials! graph "final.mp4")
```

### Error Handling

| Error Type | Behavior |
|------------|----------|
| Namespace reload error | Caught, logged, watcher continues |
| Render error | Segment marked `:error`, can retry |
| File watcher error | Caught, watcher continues |
| Python init error | Thrown, must fix before continuing |

### Testing DevX

```bash
# Run all devx tests
clojure -M:test -n desargues.devx.segment-test -n desargues.devx.graph-test \
                -n desargues.devx.renderer-test -n desargues.devx.hot-reload-test
```

### Extension Points

```clojure
;; Custom quality preset
(quality/register-preset! :4k {:quality "fourk_quality" :fps 60 :height 2160})

;; Custom event observer
(events/register-observer! :my-logger
  (fn [event] (println event))
  :filter #(= (:type %) :segment-rendered))

;; Custom rendering backend
(backend/register-backend! :custom my-backend-impl)

;; Custom specification
(specs/find-segments graph (specs/and-spec (specs/dirty?) (specs/independent?)))
```

### Verification Status (2025-12-13)

The hot-reload system has been **verified end-to-end** with the following test results:

| Test | Status | Details |
|------|--------|---------|
| Python/Manim Init | ✅ PASS | Initializes correctly via `devx/init!` |
| Scene Graph Construction | ✅ PASS | 3 segments with `source-ns` metadata, correct dependency chain |
| File Watcher | ✅ PASS | Beholder detects file changes within 500ms |
| Reload Cycle | ✅ PASS | Namespace reloads, hashes recomputed, dirty segments marked |
| Hot-Reload Integration | ✅ PASS | Full `repl/watch!` workflow operational |

**Test files:**
- `dev/hot_reload_test.clj` - Test segments with dependencies
- `dev/verify_hot_reload.clj` - Verification test suite

**Run verification:**
```clojure
(require 'verify-hot-reload)
(verify-hot-reload/run-all-tests!)  ; All 5 tests should pass
```

### Development Roadmap Status

| Phase | Name | Status |
|-------|------|--------|
| 1 | Foundation: Scene Graph & Segment | ✅ Complete |
| 2 | Architectural Refinement (SOLID/DDD/GoF) | ✅ Complete |
| 3 | Hot Reload Infrastructure | ✅ **Verified** |
| 4 | Smart Preview (Figwheel-style) | ✅ Complete (needs Manim render testing) |
| 5 | Parallel Execution | 🔄 Partial (basic structure exists) |
| 6 | DSL Completion & Integration | ⏳ Pending |
| 7 | Developer Experience Polish | 🔄 Partial |

### Known Limitations

1. **Deps must be keywords**: Use `:deps #{:intro}` not `:deps #{intro}` in `defsegment`
2. **Source-ns tracking**: Only segments defined with `defsegment` macro get automatic `source-ns` metadata
3. **clj-reload protection**: Namespaces `desargues.manim.core`, `desargues.manim-quickstart`, and `user` are protected from unloading
