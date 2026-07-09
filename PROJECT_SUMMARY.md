# Desargues - Project Summary

## Overview

**Desargues** is a Clojure library that bridges **Emmy** (symbolic mathematics) with **Manim** (mathematical animation) via **libpython-clj**. The system computes derivatives symbolically, converts them to LaTeX, and renders beautiful mathematical animations.

The project provides multiple API layers:
1. **Original DDD/SOLID API** (`desargues.api`) - Domain-driven design with protocols
2. **DSL Layer** (`desargues.dsl.*`) - High-level declarative DSL inspired by 3Blue1Brown videos
3. **DevX Layer** (`desargues.devx.*`) - Hot-reload and incremental rendering system

## Technology Stack

| Dependency | Version | Purpose |
|------------|---------|---------|
| Clojure | 1.12.3 | Core language |
| Emmy | 0.32.0 | Symbolic mathematics, automatic differentiation |
| libpython-clj | 2.026 | Clojure-Python interop for Manim |
| test.check | 1.1.1 | Property-based testing |
| typed.clj | 1.3.0 | Optional static type checking |
| orchestra | 2021.01.01-1 | Spec instrumentation (dev) |
| beholder | 1.0.2 | File system watching for hot-reload |
| clj-reload | 0.7.1 | Namespace reloading (preserves defonce) |
| Manim Community | (Python) | Mathematical animation rendering |
| Conda | manim env | Python environment management |

## Project Structure

```
src/desargues/
├── core.clj                    # Main entry point (-main)
├── api.clj                     # High-level facade API
│
├── domain/                     # Domain Layer (DDD)
│   ├── protocols.clj           # Protocol definitions (interfaces)
│   ├── math_expression.clj     # Domain entities (MathExpression, MathFunction)
│   └── services.clj            # Domain services
│
├── infrastructure/             # Infrastructure Layer
│   └── manim_adapter.clj       # Manim integration adapters
│
├── devx/                       # Developer Experience Layer
│   ├── core.clj                # Pure API facade (SRP - no state)
│   ├── repl.clj                # Stateful REPL workflow (hot-reload, watching)
│   ├── segment.clj             # Segment record and protocols
│   ├── graph.clj               # Scene graph DAG with dependency tracking
│   ├── renderer.clj            # Segment rendering with partial files
│   ├── parallel.clj            # Wave-based parallel rendering (Phase 5)
│   ├── reload.clj              # Hot-reload infrastructure (Phase 2)
│   ├── watcher.clj             # File watching with Beholder (Phase 2)
│   ├── ns_tracker.clj          # Namespace dependency tracking (Phase 2)
│   ├── preview.clj             # Enhanced last-frame preview (Phase 3)
│   ├── opengl.clj              # OpenGL live preview (Phase 3)
│   ├── selector.clj            # Interactive segment selector (Phase 3)
│   ├── quality.clj             # Extensible quality presets + auto-quality (OCP)
│   ├── events.clj              # Observer pattern for segment events
│   ├── specs.clj               # Specification pattern for queries
│   ├── repository.clj          # Repository pattern for persistence
│   ├── backend.clj             # Strategy pattern interface
│   └── backend/
│       └── manim.clj           # Manim backend implementation
│
├── dsl/                        # High-level Animation DSL
│   ├── core.clj                # Core abstractions (Reactive, Temporal, Physics)
│   ├── math.clj                # Math expression DSL with Emmy
│   ├── renderer.clj            # DSL-to-Manim rendering bridge
│   └── examples.clj            # Example scene rewrites
│
├── specs/                      # Type Safety Infrastructure
│   ├── common.clj              # Shared specs (numbers, points, colors, time)
│   ├── physics.clj             # Physics simulation specs with fdef
│   ├── api.clj                 # API function specs
│   ├── validation.clj          # Validation helpers
│   ├── generators.clj          # test.check generators
│   └── devx_generators.clj     # Generators for devx module
│
├── videos/                     # Video Rendering System
│   ├── render.clj              # Main video renderer
│   ├── physics.clj             # Physics simulation engine
│   ├── timeline.clj            # Timeline management
│   ├── typography.clj          # Text and LaTeX rendering
│   ├── math_objects.clj        # Mathematical mobject creation
│   ├── characters.clj          # Character animations
│   └── scenes/                 # Individual scene definitions
│       ├── max_rand.clj
│       ├── pendulum.clj
│       └── spring_mass.clj
│
├── manim/                      # Low-level Manim Bindings
│   ├── core.clj                # Python initialization, module access
│   ├── mobjects.clj            # All visual objects (~100 constructors)
│   ├── animations.clj          # All animation types (~70 types)
│   ├── scenes.clj              # Scene types, cameras, rendering
│   ├── constants.clj           # Colors, directions, math constants
│   ├── all.clj                 # Convenience re-exports
│   └── examples.clj            # Working demonstrations
│
├── manim_quickstart.clj        # Basic Manim setup
├── emmy_manim.clj              # Emmy ↔ Manim conversion utilities
├── emmy_manim_examples.clj     # Complete example workflows
├── brachistochrone.clj         # Physics example (brachistochrone curve)
└── brachistochrone_pure.clj    # Pure Clojure physics simulation

docs/
├── BOUNDED_CONTEXTS.md         # DDD bounded context documentation
└── ADR_CODE_QUALITY.md         # Architecture Decision Record for code standards

dev/
└── user.clj                    # REPL helpers for specs and type checking

test/desargues/
├── devx/                       # DevX module tests
│   ├── segment_test.clj
│   ├── graph_test.clj
│   ├── renderer_test.clj
│   ├── parallel_test.clj       # Wave-based parallel execution tests (22 tests)
│   ├── quality_test.clj
│   ├── events_test.clj
│   ├── specs_test.clj
│   ├── backend_test.clj
│   └── repository_test.clj
└── ...                         # Other test files

dev/
├── user.clj                    # REPL helpers for specs and type checking
├── hot_reload_test.clj         # Test segments for hot-reload verification
├── verify_hot_reload.clj       # Hot-reload verification suite
└── verify_parallel.clj         # Parallel execution verification suite
```

## Architecture Decision Record (ADR)

**See `docs/ADR_CODE_QUALITY.md`** for comprehensive code quality standards.

This codebase follows **SOLID principles** and **GoF design patterns**. Key patterns:

### Design Patterns Used

| Pattern | Implementation | Location |
|---------|---------------|----------|
| **Strategy** | `IRenderBackend` protocol | `devx/backend.clj` |
| **Observer** | Event registry | `devx/events.clj` |
| **Repository** | `IGraphRepository` protocol | `devx/repository.clj` |
| **Specification** | `ISpecification` protocol | `devx/specs.clj` |
| **Facade** | Public API | `devx/core.clj` |
| **Registry** | Quality presets | `devx/quality.clj` |

### Naming Conventions

**CRITICAL: Bang (`!`) Convention**
```clojure
;; Side effects MUST use bang suffix
(defn register-preset! [k v] (swap! registry assoc k v))
(defn emit! [event] ...)
(defn init-backend! [backend] ...)

;; Pure functions NEVER use bang
(defn resolve-quality [q] ...)
(defn find-segments [g spec] ...)
(defn create-segment [id expr] ...)
```

## Key APIs and Usage

### 1. DevX API - Hot Reload System

The devx module enables Figwheel-style hot-reload for Manim animations:

```clojure
(require '[desargues.devx.core :as devx])
(require '[desargues.devx.repl :as repl])

;; Initialize
(devx/init!)

;; Define segments (discrete, cacheable scene portions)
(def intro 
  (devx/segment :intro
    (fn [scene]
      (let [title (create-text "Hello")]
        (play! scene (write title))))))

(def main-content
  (devx/segment :main
    :deps #{:intro}  ; Depends on intro segment
    (fn [scene]
      ;; ... content
      )))

;; Build scene graph
(def my-scene (devx/scene [intro main-content]))

;; Preview a segment (quick low-quality render)
(devx/preview! my-scene :intro)

;; Render only dirty (changed) segments
(devx/render! my-scene)

;; Render with options
(devx/render! my-scene :quality :high :parallel? true)

;; Combine partial files into final video
(devx/export! my-scene "output.mp4")

;; REPL shortcuts (operate on current-scene)
(repl/use-scene! my-scene)
(repl/r!)           ; render dirty
(repl/p! :intro)    ; preview segment
(repl/s!)           ; status
(repl/d! :main)     ; mark dirty
(repl/e! "out.mp4") ; export

;; Preview commands (Phase 3)
(repl/preview! :intro)          ; Last-frame preview (opens image)
(repl/live! :intro)             ; OpenGL live preview window
(repl/live! :intro :interactive? true)  ; With IPython prompt
(repl/select!)                  ; Interactive segment menu
(repl/show-graph)               ; ASCII dependency graph

;; Shortcuts
(repl/L! :intro)                ; Alias for live!
(repl/sel!)                     ; Alias for select!

;; Hot-reload (in repl.clj)
(repl/watch!)       ; Start watching source files
(repl/unwatch!)     ; Stop watching
(repl/reload!)      ; Manual reload
```

#### Quality Presets (Extensible via OCP)

```clojure
(require '[desargues.devx.quality :as quality])

;; Built-in presets
(quality/get-preset :low)     ; {:quality "low_quality" :fps 15 :height 480}
(quality/get-preset :medium)  ; {:quality "medium_quality" :fps 30 :height 720}
(quality/get-preset :high)    ; {:quality "high_quality" :fps 60 :height 1080}

;; Register custom preset (OCP - extend without modification)
(quality/register-preset! :4k {:quality "fourk_quality" :fps 60 :height 2160})

;; Resolve quality from various inputs
(quality/resolve-quality :low)               ; Keyword lookup
(quality/resolve-quality {:fps 24 :height 720})  ; Custom map

;; Auto-quality mode (Phase 3) - context-based selection
(quality/with-context :development ...)  ; Uses :low
(quality/with-context :preview ...)      ; Uses :low
(quality/with-context :export ...)       ; Uses :high

;; Per-segment quality override
(quality/with-segment-quality segment :high)

;; Check current quality info
(quality/print-quality-info)
(quality/print-auto-rules)
```

#### Event System (Observer Pattern)

```clojure
(require '[desargues.devx.events :as events])

;; Register custom observer
(events/register-observer! :my-logger
  (fn [event]
    (println "Event:" (:type event) (:segment-id event)))
  :filter (fn [e] (= (:type e) :segment-rendered)))

;; Built-in observers
(events/logging-observer println)  ; Logs all events
(events/metrics-observer)          ; Tracks render metrics

;; Emit events (done automatically by renderer)
(events/emit! {:type :segment-rendered :segment-id :intro :duration-ms 1500})

;; Unregister
(events/unregister-observer! :my-logger)
```

#### Specification Pattern (Composable Queries)

```clojure
(require '[desargues.devx.specs :as specs])

;; Basic specifications
(specs/dirty?)       ; Matches dirty segments
(specs/cached?)      ; Matches cached segments
(specs/independent?) ; Matches segments with no deps

;; Composable with and/or/not
(specs/and-spec (specs/dirty?) (specs/independent?))
(specs/or-spec (specs/has-state? :dirty) (specs/has-state? :error))
(specs/not-spec (specs/cached?))

;; Query segments
(specs/find-segments graph (specs/dirty?))
(specs/find-first graph (specs/has-id? :intro))
(specs/count-matching graph (specs/cached?))
```

#### Rendering Backends (Strategy Pattern)

```clojure
(require '[desargues.devx.backend :as backend])

;; Set active backend
(backend/set-backend! :manim)
(backend/set-backend! :mock)  ; For testing

;; Scoped backend switching
(backend/with-backend :mock
  (devx/render! scene))

;; Register custom backend
(backend/register-backend!
  (reify backend/IRenderBackend
    (backend-name [_] :custom)
    (render-segment [_ seg opts] ...)))
```

#### Repository Pattern (Persistence)

```clojure
(require '[desargues.devx.repository :as repo])

;; In-memory repository (development/testing)
(def mem-repo (repo/create-memory-repository))
(repo/save-graph mem-repo :my-scene graph)
(repo/load-graph mem-repo :my-scene)

;; File-based repository (persistence)
(def file-repo (repo/create-file-repository "/path/to/storage"))
(repo/save-graph file-repo :my-scene graph)
```

### 2. Original API (`desargues.api`)

```clojure
(require '[desargues.api :as v])

;; Initialize Python/Manim
(v/init!)

;; Create mathematical expressions
(def e (v/expr '(sin x)))
(def f (v/func [x] (e/sin x)))

;; Compute derivatives
(def df (v/derivative e))

;; Animate
(v/animate-derivative e)

;; Builder pattern for scenes
(-> (v/scene)
    (v/show e)
    (v/wait 1)
    (v/transform e df)
    (v/render!))
```

### 3. DSL API (`desargues.dsl.*`)

#### Reactive Values (replaces ValueTracker)
```clojure
(require '[desargues.dsl.core :as dsl])

;; Create reactive values
(def x (dsl/reactive 0.5))
(def y (dsl/reactive 0.3))

;; Derived values auto-update (no manual updaters!)
(def sum (dsl/derive-reactive [x y] +))

(dsl/get-value sum)  ; => 0.8
(dsl/set-value! x 1.0)
(dsl/get-value sum)  ; => 1.3 (automatically updated)
```

#### Animation Combinators
```clojure
;; Sequential: >>
(dsl/>> (dsl/animation :create circle)
        (dsl/animation :fade-out circle))

;; Parallel: ||
(dsl/|| (dsl/animation :rotate circle :angle Math/PI)
        (dsl/animation :scale circle :factor 2))

;; Staggered: |>
(dsl/|> 0.1  ; 100ms lag between starts
        (dsl/animation :fade-in obj1)
        (dsl/animation :fade-in obj2)
        (dsl/animation :fade-in obj3))
```

#### Timeline DSL
```clojure
(-> (dsl/timeline)
    (dsl/at 0 (dsl/animation :create circle))
    (dsl/wait 1)
    (dsl/then (dsl/animation :transform circle square))
    (dsl/at 3 (dsl/animation :fade-out square)))
```

#### Physics Simulation
```clojure
;; Create particle
(def ball (dsl/particle {:position [0 5 0]
                         :velocity [1 0 0]
                         :mass 1.0}))

;; Apply forces
(dsl/apply-force! ball (dsl/spring-force [0 0 0] 10.0 :damping 0.5))
(dsl/apply-force! ball (dsl/gravity-force :g 9.8))

;; Step simulation
(dsl/step! ball 0.016)  ; ~60fps
```

### 4. Type Safety with Specs

```clojure
;; In REPL (dev profile)
(require '[user :refer :all])

;; Load all specs
(require-all!)

;; Instrument functions with specs
(instrument-all!)

;; Run generative tests on a function
(check-fn 'desargues.videos.physics/make-pendulum :num-tests 100)

;; Check all functions in a namespace
(check-ns 'desargues.videos.physics :num-tests 50)

;; Explore specs
(list-specs "desargues.specs.physics")
(describe-spec ::phys/pendulum)

;; Manual validation
(valid? ::phys/pendulum {:length 1.0 :gravity 9.8})
(explain ::phys/angle 400)  ; Shows why invalid

;; Type checking (optional)
(typecheck!)  ; Check instrumented namespaces
```

### 5. Video Rendering (`desargues.videos.render`)

```bash
# Full brachistochrone derivation (12 steps, high quality)
clojure -M -m desargues.videos.render

# Just the intro scene
clojure -M -m desargues.videos.render intro

# Specific derivation step (1-12)
clojure -M -m desargues.videos.render step 7

# Low quality for fast iteration (~4x faster)
clojure -M -m desargues.videos.render intro --low

# All steps as separate files (parallelizable)
clojure -M -m desargues.videos.render all-steps --low
```

### 6. Low-level Manim Bindings (`desargues.manim.*`)

```clojure
(require '[desargues.manim.core :as manim])
(require '[desargues.manim.mobjects :as mob])
(require '[desargues.manim.animations :as anim])

;; Initialize
(manim/init!)

;; Create mobjects
(def c (mob/circle :radius 2 :color @mob/BLUE))
(def t (mob/tex "x^2 + y^2 = r^2"))

;; Create animations
(def fade (anim/fade-in c))
(def write (anim/write t))

;; Render scene
(manim/defscene MyScene []
  (manim/play! self fade)
  (manim/wait! self 1)
  (manim/play! self write))
```

## Architecture

### Layer Dependencies (Top → Down)

```
┌─────────────────────────────────────┐
│          API Layer                  │
│  desargues.api, desargues.dsl.*     │
│  desargues.devx.core                │
│  (Facade pattern, DSL)              │
└─────────────────┬───────────────────┘
                  │ depends on
┌─────────────────▼───────────────────┐
│      Domain/Application Layer       │
│  desargues.domain.*, dsl.core/math  │
│  desargues.devx.segment/graph       │
│  (Protocols, Entities, Services)    │
└─────────────────┬───────────────────┘
                  │ depends on
┌─────────────────▼───────────────────┐
│       Infrastructure Layer          │
│  desargues.manim.*, dsl.renderer    │
│  desargues.devx.renderer            │
│  (Python interop, Manim adapters)   │
└─────────────────────────────────────┘
```

### DevX Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                     devx/core.clj                            │
│  Pure API Facade (SRP): segment, scene, render!, preview!    │
└───────────────────────────┬──────────────────────────────────┘
                            │
     ┌──────────────────────┼──────────────────────┐
     ▼                      ▼                      ▼
┌─────────────┐    ┌──────────────┐    ┌───────────────────┐
│  segment.clj│    │   graph.clj  │    │     repl.clj      │
│  - Segment  │    │  - SceneGraph│    │  - watch!/unwatch!│
│  - IHashable│    │  - DAG       │    │  - reload!        │
│  - States   │    │  - Topo sort │    │  - Stateful REPL  │
└─────────────┘    └──────────────┘    └───────────────────┘
     │                      │
     └──────────┬───────────┘
                ▼
┌──────────────────────────────────────────────────────────────┐
│                    Supporting Modules                         │
├──────────────┬──────────────┬──────────────┬────────────────┤
│ quality.clj  │  events.clj  │   specs.clj  │ repository.clj │
│ - Presets    │  - Observer  │  - Query     │ - Persistence  │
│ - Registry   │  - emit!     │  - Compose   │ - Memory/File  │
└──────────────┴──────────────┴──────────────┴────────────────┘
                            │
                            ▼
                   ┌──────────────┐
                   │ renderer.clj │
                   │ - Render to  │
                   │   partial    │
                   │   files      │
                   └──────────────┘
                            │
                            ▼
              ┌─────────────────────────┐
              │ backend.clj + backend/* │
              │ - IRenderBackend        │
              │ - Strategy pattern      │
              │ - Manim, Mock backends  │
              └─────────────────────────┘
```

### Key Protocols

| Protocol | Location | Purpose |
|----------|----------|---------|
| `IMathematicalObject` | domain/protocols | Base for all math objects |
| `IEvaluable` | domain/protocols | Objects that can be evaluated |
| `IDifferentiable` | domain/protocols | Objects supporting differentiation |
| `IRenderable` | domain/protocols | Objects that can render to Manim |
| `IAnimatable` | domain/protocols | Objects that can be animated |
| `ISegment` | devx/segment | Segment identity and state |
| `IHashable` | devx/segment | Content-addressable hashing |
| `IRenderBackend` | devx/backend | Strategy for rendering backends |
| `IGraphRepository` | devx/repository | Repository for persistence |
| `ISpecification` | devx/specs | Composable query predicates |
| `IQualityProvider` | devx/quality | Quality settings provider |
| `Reactive` | dsl/core | Reactive state containers |
| `Temporal` | dsl/core | Time-varying entities |
| `Physical` | dsl/core | Physics simulation objects |

## Critical Implementation Details

### Python Class Methods (IMPORTANT!)

When creating Python Scene subclasses from Clojure, you **MUST** wrap construct functions with `py-class/make-tuple-instance-fn` to properly receive the `self` argument:

```clojure
(require '[libpython-clj2.python :as py])
(require '[libpython-clj2.python.class :as py-class])

;; CORRECT: Wrap with make-tuple-instance-fn
(defn my-construct [self]
  (let [circle (mob/circle)]
    (py/call-attr self "play" (anim/create circle))))

(let [Scene (py/get-attr (manim/manim) "Scene")
      wrapped-construct (py-class/make-tuple-instance-fn my-construct)]
  (py/create-class "MyScene" [Scene] {"construct" wrapped-construct}))

;; WRONG: Without wrapping, self is not passed!
```

### Segment Hashing

Segments use SHA-256 hashing for change detection:
- Hash includes: construct-fn source, dependencies, metadata
- Dependent segment hashes are combined
- Changed hash = segment needs re-render

```clojure
;; Hash computation
(seg/compute-segment-hash segment dep-hashes)
;; => "abc123..." (SHA-256 base64)
```

### Partial Movie Files

Manim generates partial movie files that can be combined:
```
media/partial_movie_files/
├── intro_abc123.mp4
├── main_def456.mp4
└── outro_ghi789.mp4
```

The renderer tracks which partial files are valid and combines them:
```clojure
(renderer/combine-partials! graph "final.mp4")
```

## Implementation Patterns

### 1. Registry Pattern (OCP)
```clojure
;; Extensible without modification
(defonce preset-registry (atom {}))

(defn register-preset! [key settings]
  {:pre [(keyword? key) (map? settings)]}
  (swap! preset-registry assoc key settings))

;; Extend by registration, not code changes
(register-preset! :custom {:fps 24 :height 720})
```

### 2. Observer Pattern
```clojure
;; Decouple producers from consumers
(defn emit! [event]
  (doseq [[_id {:keys [handler filter]}] @observer-registry]
    (when (filter event)
      (try (handler event)
           (catch Exception e
             (log-error e))))))  ; Error isolation
```

### 3. Specification Pattern
```clojure
;; Composable predicates
(defprotocol ISpecification
  (satisfied? [this segment])
  (spec-description [this]))

(defrecord AndSpec [specs]
  ISpecification
  (satisfied? [_ segment]
    (every? #(satisfied? % segment) specs)))

;; Usage
(find-segments graph (and-spec (dirty?) (independent?)))
```

### 4. Strategy Pattern
```clojure
;; Swappable implementations
(defprotocol IRenderBackend
  (render-segment [this segment opts]))

;; Switch at runtime
(set-backend! :mock)
(with-backend :manim body...)
```

### 5. Reactive Programming (DSL)
```clojure
;; Instead of manual updaters (Python style):
;; tracker.add_updater(lambda m: m.set_value(get_max()))

;; Use derived reactive values:
(def max-val (dsl/derive-reactive [x1 x2] max))
;; Automatically updates when x1 or x2 change
```

### 6. Data as Code
```clojure
;; Animations are data structures, not imperative commands
{:type :sequence
 :children [{:type :fade-in :target :circle :duration 1}
            {:type :wait :duration 0.5}
            {:type :transform :from :circle :to :square}]}
```

### 7. Spec-Driven Development
```clojure
;; Define specs for functions
(s/fdef make-pendulum
  :args (s/cat :opts ::pendulum-opts)
  :ret ::pendulum)

;; Instrument during development
(stest/instrument `make-pendulum)

;; Generative testing
(stest/check `make-pendulum {:num-tests 100})
```

## Python Environment Setup

```bash
# Create conda environment
conda create -n manim python=3.12
conda activate manim

# Install Manim
pip install manim

# Verify
manim --version
```

Paths are **derived from the environment** — no source edits needed. With the
conda env active, `desargues.config` reads `CONDA_PREFIX` and probes
`<prefix>/bin/python`, `<prefix>/lib/libpython<abi>.so`, and the env's
`site-packages`. To target a manim env other than the active one, set
`DESARGUES_CONDA_PREFIX`; to override a single path, set `DESARGUES_MANIM_PYTHON`,
`DESARGUES_MANIM_LIBPYTHON`, or `DESARGUES_MANIM_SITEPACKAGES`.

## Development Workflow

### Running the Main Demo
```bash
clojure -M:run
```

### REPL Development
```bash
clojure -A:dev

;; In REPL:
(require '[desargues.api :as v])
(v/init!)
(v/animate-derivative (v/expr '(sin x)))

;; With type safety (checker via the :dev alias):
(require '[user :refer :all])
(require-all!)
(instrument-all!)
(help)  ; Show all available commands

;; With hot-reload (devx):
(require '[desargues.devx.repl :as repl])
(repl/watch!)   ; Start watching files
(repl/s!)       ; Check status
```

### Running Tests
```bash
clojure -M:dev -m desargues.typecheck   # Typed Clojure gate (static types)
# clojure.test namespaces (desargues.*-test) run from your editor/REPL
```

### Checking Compilation
```bash
clojure -M:dev -m desargues.typecheck   # Static type checking
```

## Extension Points

### Adding New Quality Presets
```clojure
(require '[desargues.devx.quality :as quality])
(quality/register-preset! :4k {:quality "fourk_quality" :fps 60 :height 2160})
```

### Adding New Event Observers
```clojure
(require '[desargues.devx.events :as events])
(events/register-observer! :my-observer
  (fn [event] (process event))
  :filter (fn [e] (= (:type e) :segment-rendered)))
```

### Adding New Specifications
```clojure
(require '[desargues.devx.specs :as specs])
(defrecord MySpec [param]
  specs/ISpecification
  (satisfied? [_ segment] (check param segment))
  (spec-description [_] (str "my-spec:" param)))
```

### Adding New Rendering Backends
```clojure
(require '[desargues.devx.backend :as backend])
(defrecord MyBackend []
  backend/IRenderBackend
  (backend-name [_] :my-backend)
  (render-segment [_ seg opts] ...))
(backend/register-backend! (->MyBackend))
```

### Adding New Mobject Types
1. Add constructor in `src/desargues/manim/mobjects.clj`
2. Add render method in `src/desargues/dsl/renderer.clj`

### Adding New Animation Types
1. Add constructor in `src/desargues/manim/animations.clj`
2. Add render method in `src/desargues/dsl/renderer.clj`

### Adding New Physics Forces
```clojure
(defn my-force [params]
  (fn [particle]
    (let [pos (dsl/get-position particle)]
      ;; Return [fx fy fz] force vector
      [0 0 0])))
```

## Development Roadmap

The project follows a phased development plan:

| Phase | Name | Priority | Status |
|-------|------|----------|--------|
| 1 | Foundation: Scene Graph & Segment Model | HIGH | ✅ Complete |
| 2 | Architectural Refinement (SOLID/DDD/GoF) | HIGH | ✅ Complete |
| 3 | Hot Reload Infrastructure | HIGH | ✅ **Verified** (2025-12-13) |
| 4 | Smart Preview (Figwheel-style) | MEDIUM | ✅ Complete |
| 5 | Parallel Execution | MEDIUM | ✅ **Complete** (2025-12-13) |
| 6 | DSL Completion & Integration | MEDIUM | ⏳ Pending |
| 7 | Developer Experience Polish | LOW | 🔄 Partial |

### Phase 3 Verification Results

The hot-reload system has been **verified end-to-end**:

| Test | Result |
|------|--------|
| Python/Manim Initialization | ✅ Pass |
| Scene Graph Construction | ✅ Pass |
| File Watcher (Beholder) | ✅ Pass |
| Reload Cycle (clj-reload) | ✅ Pass |
| Hot-Reload Integration | ✅ Pass |

Run verification: `(require 'verify-hot-reload) (verify-hot-reload/run-all-tests!)`

### Phase 5 Parallel Execution

The parallel execution system has been implemented with wave-based rendering:

| Component | File | Description |
|-----------|------|-------------|
| `parallel.clj` | `devx/parallel.clj` | Wave-based parallel rendering module |
| Wave Analysis | `analyze-waves` | Partitions segments into parallelizable waves |
| Wave Statistics | `wave-stats` | Calculates parallelization metrics |
| Time Estimation | `estimate-parallel-time` | Predicts speedup from parallel execution |
| Thread Pool | `render-wave!` | Manages concurrent segment rendering |

**REPL Commands:**
```clojure
(require '[desargues.devx.repl :as repl])

(repl/rp!)                    ; Render dirty segments in parallel
(repl/waves!)                 ; Print wave analysis for current graph
(repl/estimate-speedup 10.0)  ; Estimate speedup with 10s avg segment time
```

**Direct API:**
```clojure
(require '[desargues.devx.parallel :as parallel])

;; Analyze waves
(parallel/print-wave-analysis graph)

;; Render with wave-based execution
(parallel/render-parallel! graph :max-workers 4 :quality :low)

;; With progress callback
(parallel/render-parallel! graph
  :on-progress (fn [event] (println event)))

;; Estimate parallel time
(parallel/estimate-parallel-time graph 10.0 4)
;; => {:sequential-estimate-s 50.0
;;     :parallel-estimate-s 30.0
;;     :speedup-factor 1.67}
```

**Verification Results (22 tests, 72 assertions):**

| Test Category | Tests | Status |
|---------------|-------|--------|
| Wave Analysis | 7 | ✅ Pass |
| Wave Statistics | 4 | ✅ Pass |
| Time Estimation | 4 | ✅ Pass |
| Integration | 3 | ✅ Pass |
| Edge Cases | 4 | ✅ Pass |

Run verification: `(load-file "dev/verify_parallel.clj") (verify-parallel/run-all-tests!)`

**Note:** Due to Python's GIL, true CPU-parallel rendering within a single process is limited. Benefits include wave analysis for understanding parallelization potential, concurrent I/O operations, and future support for subprocess-based true parallelism.

## Output

Rendered videos are saved to `media/videos/` directory:
- **High quality**: `media/videos/1080p60/` - 1080p at 60fps
- **Medium quality**: `media/videos/720p30/` - 720p at 30fps  
- **Low quality**: `media/videos/480p15/` - 480p at 15fps (fast dev iteration)

Use `--low` flag or `:quality :low` for fast iteration during development.

## Test Statistics

| Category | Tests | Assertions |
|----------|-------|------------|
| DevX Core | 31 | 103 |
| Quality | 10 | 15 |
| Events | 16 | 24 |
| Specs | 23 | 35 |
| Backend | 14 | 21 |
| Repository | 12 | 18 |
| Parallel | 22 | 72 |
| **Total** | **128+** | **288+** |

## Known Issues

- Emmy reflection warnings during compilation (harmless)
- SLF4J logger binding warning (harmless)
- Python must be initialized before any Manim operations
- Segment rendering requires Python environment to be active

## Related Documentation

- **`CLAUDE.md`** - Instructions for Claude Code AI assistant
- **`docs/BOUNDED_CONTEXTS.md`** - DDD bounded context documentation
- **`docs/ADR_CODE_QUALITY.md`** - Architecture Decision Record for code standards
