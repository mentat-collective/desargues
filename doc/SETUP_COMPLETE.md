# Manim + Clojure Setup Complete! ✅

## What Has Been Set Up

Your project now has a complete integration between **Manim (Mathematical Animation Engine)** and **Clojure** using **libpython-clj**.

### Test Results

The manim integration suite (`desargues.manim-test`) verifies Python init, the Manim
import, object creation, colors/constants, and animations. Run it from a REPL:

```clojure
(require '[desargues.manim-test :as mt])
(mt/run-all-tests)
```

The full `clojure.test` suite lives under the `:test` alias (`clojure -M:test` adds the
`test/` classpath and spec-check assertions).

## Quick Start Guide

### Run the Quickstart Example

```bash
conda activate manim
clojure -M:dev
```

(Legacy: the old `lein repl` is no longer the supported build.)

Then in the REPL:

```clojure
(require '[desargues.manim-quickstart :as mq])
(mq/quickstart!)
```

This will create the classic Manim "pink circle" animation and save it to `media/videos/`.

## Files

### 1. Test Files (`test/desargues/`)

- **`manim_test.clj`** - `clojure.test` integration suite
  - Tests Python initialization
  - Tests Manim import
  - Tests object creation (Circle, Square, etc.)
  - Tests colors and constants
  - Tests animations
  - Runs under the `:test` alias

### 2. Source Files (`src/desargues/`)

- **`manim_quickstart.clj`** ⭐ **START HERE**
  - Complete quickstart example from Manim tutorial
  - Functions:
    - `(init!)` - Initialize Python environment (paths from `desargues.config`)
    - `(quickstart!)` - Run complete example in one command
    - `(make-create-circle-scene)` - Create the CreateCircle scene
    - `(make-square-to-circle-scene)` - Create transformation example
    - `(render-scene! scene)` - Render any scene
  - Includes extensive examples in the `comment` block

- **`manim_renderer.clj`**
  - Advanced rendering utilities
  - Quality settings configuration
  - Scene rendering with options

- **`manim.clj`**
  - Basic Manim integration helpers

- **`config.clj`**
  - Environment-driven Python/Manim configuration (no hardcoded paths)

### 3. Python Files (`py/`)

- **`manim_examples.py`**
  - Traditional Python examples that work with both:
    - Manim CLI: `manim -pql py/manim_examples.py CreateCircle`
    - Clojure (can be imported as a module)
  - Includes 5 example scenes:
    - `CreateCircle` - Basic quickstart
    - `SquareToCircle` - Transformation
    - `SquareAndCircle` - Multiple objects
    - `AnimatedSquareToCircle` - Advanced animation
    - `DifferentRotations` - Rotation methods

### 4. Documentation

- **`README_MANIM.md`** - Main user guide
- **`MANIM_SETUP.md`** - Detailed setup and troubleshooting
- **`SETUP_COMPLETE.md`** - This file

## Configuration Details

### Environment

Python/Manim paths are **derived from the environment** by `desargues.config`; nothing is
hardcoded to one machine. With the conda env active (`CONDA_PREFIX` set) a typical setup is:

- **Conda environment**: `manim` (activate it, or set `DESARGUES_CONDA_PREFIX`)
- **Python version**: 3.12+
- **Manim version**: 0.19+
- **Interpreter**: `$CONDA_PREFIX/bin/python`
- **libpython**: `$CONDA_PREFIX/lib/libpython3.x.so`

Override any of these with `DESARGUES_MANIM_PYTHON`, `DESARGUES_MANIM_LIBPYTHON`,
`DESARGUES_MANIM_SITEPACKAGES`, `DESARGUES_PROJECT_ROOT`, or `DESARGUES_QUALITY`.

### Project Dependencies (in `deps.edn`)

```clojure
:deps {org.clojure/clojure {:mvn/version "1.12.3"}
       io.github.mentat-collective/emmy {:git/tag "v0.32.0" :git/sha "53fd990"}
       clj-python/libpython-clj {:mvn/version "2.026"}}
```

## Example Usage

### Example 1: Quickstart (One Command)

```bash
conda activate manim
clojure -M:dev
```

```clojure
(require '[desargues.manim-quickstart :as mq])
(mq/quickstart!)
```

### Example 2: Step-by-Step

```clojure
(require '[desargues.manim-quickstart :as mq])
(require '[libpython-clj2.python :as py])

;; Initialize Python (once per REPL session)
(mq/init!)

;; Create the CreateCircle scene
(let [CreateCircle (mq/make-create-circle-scene)
      scene (CreateCircle)]
  (mq/render-scene! scene))
```

### Example 3: Custom Animation

```clojure
(require '[desargues.manim-quickstart :as mq])
(require '[libpython-clj2.python :as py])

(mq/init!)

(let [manim (mq/get-manim-module)
      Circle (py/get-attr manim "Circle")
      Square (py/get-attr manim "Square")
      Create (py/get-attr manim "Create")
      RED (py/get-attr manim "RED")
      BLUE (py/get-attr manim "BLUE")

      MyScene
      (mq/create-scene-class
       "MyScene"
       (fn [self]
         (let [circle (Circle)
               square (Square)]
           ;; Style
           (py/call-attr-kw circle "set_fill" [RED] {:opacity 0.7})
           (py/call-attr-kw square "set_fill" [BLUE] {:opacity 0.7})

           ;; Position
           (py/call-attr-kw square "next_to"
                            [circle (py/get-attr manim "RIGHT")]
                            {:buff 0.5})

           ;; Animate
           (py/call-attr self "play" (Create circle) (Create square)))))]

  (mq/render-scene! (MyScene)))
```

### Example 4: Using Python CLI

```bash
conda activate manim
manim -pql py/manim_examples.py CreateCircle
```

Flags:
- `-p` - Preview video after rendering
- `-q` - Quality: `l` (low), `m` (medium), `h` (high)
- Examples: `-pql` (preview + quality low)

## Important Notes

### Keyword Arguments in Python Calls

When calling Python functions with keyword arguments, use `call-attr-kw`:

```clojure
;; CORRECT:
(py/call-attr-kw circle "set_fill" [PINK] {:opacity 0.5})

;; INCORRECT (will fail):
(py/call-attr circle "set_fill" PINK :opacity 0.5)
```

### Python Initialization

Python must be initialized once per REPL session:

```clojure
(require '[desargues.manim-quickstart :as mq])
(mq/init!)  ;; Do this once
```

## Output Location

Videos are saved to:
```
media/videos/
```

The exact path depends on quality settings and scene name.

## Next Steps

1. ✅ **Try the quickstart**: `(mq/quickstart!)`
2. Browse the [Manim Example Gallery](https://docs.manim.community/en/stable/examples.html)
3. Combine with Emmy (already in `deps.edn`) for symbolic math visualizations
4. Create your own mathematical animations!

## Resources

- [Manim Documentation](https://docs.manim.community/)
- [Manim Quickstart Tutorial](https://docs.manim.community/en/stable/tutorials/quickstart.html)
- [libpython-clj Guide](https://clj-python.github.io/libpython-clj/)
- [Emmy (Scientific Computing)](https://github.com/mentat-collective/emmy)

## Troubleshooting

See `MANIM_SETUP.md` for detailed troubleshooting.

Common issues:
- **Library not found**: Set `DESARGUES_MANIM_LIBPYTHON` to the correct `libpython3.x.so`
- **Import errors**: Verify the conda environment: `conda activate manim && python -c "import manim"`
- **Render failures**: Check `media/` directory permissions

---

**Everything is ready to go! Happy animating!** 🎬✨
