# desargues

**Emmy + Manim — backend-neutral mathematical animation in Clojure**

desargues bridges [Emmy](https://github.com/mentat-collective/emmy) (symbolic
mathematics) and [Manim Community Edition](https://www.manim.community/)
(mathematical animation) through [libpython-clj](https://github.com/clj-python/libpython-clj).
It computes derivatives symbolically, converts them to LaTeX, and renders them —
and it does so behind a **backend-neutral scene facade**, so the same animation
code runs under Manim *or* under a pure recording backend that emits plain data.

## Features

- 🧮 **Symbolic mathematics** — define functions, differentiate, simplify via Emmy
- 🎬 **Animation** — the Emmy → LaTeX → Python → Manim pipeline
- 🔌 **Backend-neutral facade** — `desargues.scene` programs to a protocol, not to
  Manim; a pure `RecordingBackend` renders the same scene to EDN with no Python
- 📐 **Layout algebra** — an elm-ui-inspired declarative layout DSL with a pure
  extent estimator (measure → resolve → realize)
- 🏗️ **Layered DDD/SOLID design** — pure, Typed-Clojure-annotated domain; the
  Dependency-Inversion seam sits at `desargues.scene.protocols`
- ⚙️ **Environment-driven config** — no machine paths baked into source

## Prerequisites

- **Java** — JDK 11+
- **Clojure CLI** — the `clojure` / `clj` tools.deps command
- **Anaconda/Miniconda** — to provide a Python env with Manim
- **LaTeX** — for rendering math (usually installed alongside Manim)

## Installation

```bash
git clone https://github.com/mentat-collective/desargues
cd desargues
```

Create a conda environment with Manim Community Edition:

```bash
conda create -n manim python=3.12
conda activate manim
conda install -c conda-forge manim
manim --version          # e.g. Manim Community v0.18.x
```

Clojure dependencies resolve on first use (`clojure -P` to pre-fetch).

## Configuration

Python/Manim paths are **derived from the environment** — nothing is hardcoded in
source. `desargues.config` resolves them once, at the boundary:

1. It reads `DESARGUES_CONDA_PREFIX`, else `CONDA_PREFIX`, as the conda env root.
2. From that root it probes `<prefix>/bin/python`,
   `<prefix>/lib/libpython<abi>.so`, and the env's `site-packages`
   (abi probed newest-first: 3.13 → 3.10).

So activating the env is usually all you need:

```bash
conda activate manim     # sets CONDA_PREFIX; desargues.config picks it up
```

To target a manim env that is *not* the active one, or to override any single
path, set the corresponding variable:

| Variable | Purpose |
|---|---|
| `DESARGUES_CONDA_PREFIX` | conda env root (wins over `CONDA_PREFIX`) |
| `DESARGUES_MANIM_PYTHON` | python executable |
| `DESARGUES_MANIM_LIBPYTHON` | `libpython*.so` |
| `DESARGUES_MANIM_SITEPACKAGES` | site-packages dir |
| `DESARGUES_PROJECT_ROOT` | root prepended to Python `sys.path` for `.py` scenes |
| `DESARGUES_QUALITY` | render quality (default `medium_quality`) |

`(desargues.config/manim-config)` returns the resolved map and throws a clear
error if no env is detected and no overrides are given.

## Quick start

### High-level API — symbolic → animation

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

```
desargues/
├── src/desargues/
│   ├── core.clj                          # Main entry point (lein run)
│   ├── manim_quickstart.clj              # Basic Manim setup
│   ├── emmy_manim.clj                    # Emmy → LaTeX → Python
│   ├── emmy_manim_examples.clj           # Complete examples & workflows
│   ├── emmy_evaluation.clj               # Equation evaluation
│   ├── api.clj                           # Clean Facade API
│   │
│   ├── domain/                           # Domain Layer (DDD)
│   │   ├── protocols.clj                 # Core protocols/interfaces
│   │   ├── math_expression.clj           # Domain entities
│   │   └── services.clj                  # Domain services
│   │
│   ├── infrastructure/                   # Infrastructure Layer
│   │   └── manim_adapter.clj             # Manim integration
│   │
│   └── emmy_python/                      # Emmy-Python bridge
│       └── equations.clj                 # Sample equations
│
├── test/desargues/
│   └── manim_test.clj                    # Integration tests
│
├── *.py                                  # Python scene definitions
│   ├── manim_examples.py                 # Basic scenes
│   ├── emmy_manim_scenes.py              # Emmy-driven scenes
│   └── equation_evaluation_scenes.py     # Evaluation scenes
│
├── media/videos/                         # Generated videos (gitignored)
│
├── doc/                                  # Documentation
│   ├── ARCHITECTURE.md                   # SOLID/DDD architecture guide
│   ├── CLEAN_API_GUIDE.md                # Clean API usage guide
│   ├── EMMY_MANIM_GUIDE.md               # Emmy integration guide
│   └── ...                               # Additional guides
│
├── README.md                             # This file
└── project.clj                           # Leiningen project file
```

### Backend-neutral scene facade — the DIP seam

The *same* construct function renders under Manim or records to pure EDN:

```clojure
(require '[desargues.scene :as s])
(require '[desargues.scene.data :as rec])

(defn construct [stage]
  (let [t (s/text "Hello")]
    (s/play! stage (s/appear t))
    (s/hold! stage 2)))

;; Pure — no Python, returns an EDN scene graph you can assert on
(s/with-backend (rec/recording-backend)
  (s/render! "demo" construct {}))

;; The real animation (the Manim backend is the lazy default)
(s/render! "demo" construct {})
```

### Layout algebra

```clojure
(require '[desargues.api :as v])

(v/render-layout!
  (v/column {:padding 0.6 :spacing 0.4 :align :center-x}
    (v/text "Reserves")
    (v/math "\\Delta M = 1000")))
```

## Architecture

Layered, with the dependency arrow pointing inward toward the pure domain:

```
api            desargues.api            high-level facade (symbolic → animation)
scene facade   desargues.scene          backend-neutral animation API  ── DIP seam
  ├ backend    desargues.scene.manim    ManimBackend  (draws via libpython-clj)
  └ backend    desargues.scene.data     RecordingBackend (records EDN, no Python)
layout         desargues.layout.*       elm-ui layout algebra + pure extent measure
manim          desargues.manim.*        low-level Manim mobject/animation bindings
domain         desargues.domain.*       pure Clojure + Emmy, Typed-Clojure-annotated
config         desargues.config         env-derived settings (the Collect layer)
```

The seam is `desargues.scene.protocols`: both backends implement the same
protocols, so consumers depend on the abstraction, never on Manim
(Dependency Inversion). Because the pure `RecordingBackend` and the Manim backend
are interchangeable behind that facade (Liskov substitution), the neutrality is
*checkable* — run any scene through the recording backend and assert on the data.

## Build & development (tools.deps)

desargues builds with the Clojure CLI (`deps.edn`); a `bb.edn` is provided for
babashka. Aliases: `:run`, `:dev`, `:test`.

```bash
clojure -M:run                                  # render the demo (needs the manim env)
clojure -A:dev                                  # dev REPL: dev/ sources + orchestra + the checker
clojure -M:dev -m desargues.typecheck           # static Typed Clojure gate
clojure -M -m desargues.videos.render intro --low   # render a specific scene, low quality
```

Typed Clojure is split across the two classpaths: the annotation runtime
(`typed.clj.runtime`) is a normal dependency because `desargues.domain.*` carry
`t/ann`/`t/defalias` annotations, while the checker (`typed.clj.checker`) lives in
the `:dev` alias — only `desargues.typecheck` needs it.

## Documentation

- **[ARCHITECTURE.md](doc/ARCHITECTURE.md)**: SOLID/DDD design patterns used in the project
- **[CLEAN_API_GUIDE.md](doc/CLEAN_API_GUIDE.md)**: Guide to the clean API facade
- **[EMMY_MANIM_GUIDE.md](doc/EMMY_MANIM_GUIDE.md)**: Emmy integration and LaTeX conversion
- **[EQUATION_EVALUATION_GUIDE.md](doc/EQUATION_EVALUATION_GUIDE.md)**: Evaluating functions and creating tables

## Resources

- Emmy — https://github.com/mentat-collective/emmy
- Manim Community — https://www.manim.community/
- libpython-clj — https://github.com/clj-python/libpython-clj

## License

This program and the accompanying materials are made available under the terms of
the Eclipse Public License 2.0 (https://www.eclipse.org/legal/epl-2.0/), or (at
your option) the GNU General Public License, version 2 or later, with the GNU
Classpath Exception (https://www.gnu.org/software/classpath/license.html).
