# Manim + Clojure Integration

This project integrates [Manim (Community Edition)](https://www.manim.community/) with Clojure using [libpython-clj](https://github.com/clj-python/libpython-clj), allowing you to create mathematical animations using Clojure!

## 🚀 Quick Start

### Option 1: One-Command Quickstart

Activate your manim conda env and start a REPL with the tools.deps build:

```bash
conda activate manim
clojure -M:dev
```

(Legacy: the old `lein repl` is no longer the supported build — use `deps.edn` aliases.)

Then:

```clojure
(require '[desargues.manim-quickstart :as mq])
(mq/quickstart!)
```

This will render the classic Manim quickstart example (a pink circle being created).

### Option 2: Python CLI (Traditional Way)

```bash
conda activate manim
manim -pql py/manim_examples.py CreateCircle
```

## 📁 What's Been Set Up

### Clojure Namespaces

1. **`desargues.manim-test`** - Test suite to verify integration
   ```clojure
   (require '[desargues.manim-test :as mt])
   (mt/run-all-tests)  ; Run all tests
   ```

2. **`desargues.manim-quickstart`** - Main quickstart example (⭐ START HERE)
   ```clojure
   (require '[desargues.manim-quickstart :as mq])
   (mq/quickstart!)  ; Create and render the quickstart scene
   ```

3. **`desargues.manim`** - Basic manim integration helpers

4. **`desargues.manim-renderer`** - Advanced rendering utilities

### Python Files

- **`py/manim_examples.py`** - Traditional Python examples you can render with the CLI

### Documentation

- **`MANIM_SETUP.md`** - Detailed setup and troubleshooting guide
- **`README_MANIM.md`** - This file

## 📖 Examples

### Example 1: Quickstart (CreateCircle)

From the [official Manim tutorial](https://docs.manim.community/en/stable/tutorials/quickstart.html):

```clojure
(require '[desargues.manim-quickstart :as mq])

;; Initialize (once per REPL session)
(mq/init!)

;; Create and render
(let [CreateCircle (mq/make-create-circle-scene)
      scene (CreateCircle)]
  (mq/render-scene! scene))
```

### Example 2: Square to Circle Transformation

```clojure
(require '[desargues.manim-quickstart :as mq])

(mq/init!)

(let [SquareToCircle (mq/make-square-to-circle-scene)
      scene (SquareToCircle)]
  (mq/render-scene! scene))
```

### Example 3: Custom Scene

```clojure
(require '[desargues.manim-quickstart :as mq])
(require '[libpython-clj2.python :as py])

(mq/init!)

(let [manim (mq/get-manim-module)
      Scene (py/get-attr manim "Scene")
      Circle (py/get-attr manim "Circle")
      Square (py/get-attr manim "Square")
      Create (py/get-attr manim "Create")
      RED (py/get-attr manim "RED")
      BLUE (py/get-attr manim "BLUE")

      ;; Define custom scene
      MyScene
      (py/python-type
       "MyScene"
       [Scene]
       {"construct"
        (fn [self]
          (let [circle (Circle)
                square (Square)]
            ;; Style objects
            (py/call-attr circle "set_fill" RED :opacity 0.7)
            (py/call-attr square "set_fill" BLUE :opacity 0.7)

            ;; Position square next to circle
            (py/call-attr square "next_to" circle (py/get-attr manim "RIGHT") :buff 0.5)

            ;; Animate both
            (py/call-attr self "play" (Create circle) (Create square))))})]

  ;; Render it
  (mq/render-scene! (MyScene)))
```

## 🎬 Output

Videos are saved to: `media/videos/`

The exact path depends on the quality settings and scene name.

## 🔧 Configuration

### Environment Details

Python/Manim paths are **derived from your environment** by `desargues.config` — they are
not hardcoded. With your conda env active (`CONDA_PREFIX` set), the code resolves the
interpreter, `libpython3.x.so`, and `site-packages` automatically. A typical env:

- **Conda environment**: `manim` (activate it, or set `DESARGUES_CONDA_PREFIX`)
- **Python version**: 3.12+
- **Manim version**: 0.21 (verified; 0.19+ works)
- **Interpreter**: `$CONDA_PREFIX/bin/python`
- **libpython**: `$CONDA_PREFIX/lib/libpython3.x.so`

### Changing Paths

There is nothing to edit in source. Override the derived defaults with environment
variables (they win over auto-detection):

- `DESARGUES_CONDA_PREFIX` — target a specific conda env root
- `DESARGUES_MANIM_PYTHON` — interpreter path
- `DESARGUES_MANIM_LIBPYTHON` — `libpython3.x.so` path
- `DESARGUES_MANIM_SITEPACKAGES` — env `site-packages` path
- `DESARGUES_PROJECT_ROOT` — project root (for locating `py/` scene files)

## 🧪 Testing

Verify everything works from the REPL:

```bash
conda activate manim
clojure -M:dev
```

```clojure
(require '[desargues.manim-test :as mt])
(mt/run-all-tests)
```

Expected output:
```
=== Testing Manim Integration ===

✓ Python initialized successfully
✓ Manim imported successfully
✓ Created Circle object successfully

=== Test Results ===
Python Init: ✓ PASS
Manim Import: ✓ PASS
Object Creation: ✓ PASS

All tests passed! 🎉
```

The full `clojure.test` suite lives under the `:test` alias (`clojure -M:test` adds the
`test/` classpath and spec-check assertions).

## 🎨 Available Manim Objects

Some commonly used objects and functions:

**Shapes**: Circle, Square, Triangle, Rectangle, Polygon, Arc, etc.

**Animations**: Create, Transform, FadeIn, FadeOut, Write, DrawBorderThenFill, etc.

**Colors**: RED, BLUE, GREEN, PINK, YELLOW, ORANGE, PURPLE, etc.

**Math**: Tex, MathTex, Axes, NumberPlane, Vector, etc.

Access them like this:
```clojure
(let [manim (mq/get-manim-module)
      Circle (py/get-attr manim "Circle")
      RED (py/get-attr manim "RED")]
  ;; Use them...
  )
```

## 📚 Resources

- [Manim Documentation](https://docs.manim.community/)
- [Manim Examples Gallery](https://docs.manim.community/en/stable/examples.html)
- [libpython-clj Guide](https://clj-python.github.io/libpython-clj/)
- [Manim Tutorial](https://docs.manim.community/en/stable/tutorials/quickstart.html)

## 🤝 Integration with Emmy

This project also includes [Emmy](https://github.com/mentat-collective/emmy) (a Clojure library for scientific computing). You can combine Emmy's symbolic math capabilities with Manim's visualization! See `EMMY_MANIM_GUIDE.md`.

## 🎯 Next Steps

1. Run the quickstart example: `(mq/quickstart!)`
2. Explore the examples in `py/manim_examples.py`
3. Browse the [Manim example gallery](https://docs.manim.community/en/stable/examples.html)
4. Create your own mathematical animations!
5. Combine with Emmy for symbolic mathematics + visualization

## 🐛 Troubleshooting

See `MANIM_SETUP.md` for detailed troubleshooting steps.

Common issues:
- **Library not found**: Auto-detection failed — set `DESARGUES_MANIM_LIBPYTHON` to the
  correct `libpython3.x.so`
- **Import errors**: Ensure the conda environment is active (`CONDA_PREFIX` set) or point
  `DESARGUES_CONDA_PREFIX` at it
- **Render failures**: Check `media/` directory permissions

---

**Happy animating!** 🎬✨
