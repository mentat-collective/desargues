# Manim with libpython-clj Setup

This guide explains how to use Manim (Mathematical Animation Engine) from Clojure using libpython-clj.

## Prerequisites

You need:
- A conda environment providing Manim (this guide calls it `manim`), created with e.g.
  `conda create -n manim -c conda-forge manim`
- `libpython-clj` on the classpath (already declared in `deps.edn`)

## Project Structure

```
desargues/
├── deps.edn                     # tools.deps build (deps + aliases)
├── bb.edn                       # Babashka entry
├── src/desargues/
│   ├── config.clj               # Environment-driven Python/Manim config
│   ├── manim.clj                # Basic manim integration
│   └── manim_renderer.clj       # Complete rendering examples
├── py/
│   └── manim_examples.py        # Python scenes (usable from CLI or Clojure)
└── doc/MANIM_SETUP.md           # This file
```

## Environment Configuration

Python/Manim paths are **derived from your environment** by `desargues.config` — nothing
is hardcoded. When your conda env is active (`CONDA_PREFIX` is set), the code probes
`$CONDA_PREFIX/bin/python`, `$CONDA_PREFIX/lib/libpython3.x.so`, and the matching
`site-packages`. Resolution order and overrides:

- `DESARGUES_CONDA_PREFIX` (else `CONDA_PREFIX`) — root of the conda env to use.
- Individual overrides that win over the derived defaults:
  - `DESARGUES_MANIM_PYTHON` — path to the interpreter
  - `DESARGUES_MANIM_LIBPYTHON` — path to `libpython3.x.so`
  - `DESARGUES_MANIM_SITEPACKAGES` — path to the env's `site-packages`
  - `DESARGUES_PROJECT_ROOT` — project root for locating the `.py` scene files
  - `DESARGUES_QUALITY` — render quality (default `medium_quality`)

To activate the env, either `conda activate manim` (so `CONDA_PREFIX` is set) or point
`DESARGUES_CONDA_PREFIX` at the env. `(desargues.config/manim-config)` throws a clear
error if no env is detected and no overrides are supplied.

## Quick Start

### Option 1: Using the Clojure REPL (Recommended for Interactive Development)

1. Activate your manim conda env, then start a REPL:
   ```bash
   conda activate manim
   clojure -M:dev
   ```
   (Legacy: the old Leiningen `lein repl` is no longer the supported build.)

2. Load the manim renderer namespace:
   ```clojure
   (require '[desargues.manim-renderer :as mr])
   ```

3. Initialize Python with your manim environment (paths come from `desargues.config`):
   ```clojure
   (mr/init!)
   ```

4. Setup manim:
   ```clojure
   (mr/setup-manim!)
   ```

5. Create and render the quickstart example:
   ```clojure
   (mr/create-and-render-circle!)
   ```

   The video will be saved in `media/videos/` folder!

### Option 2: Using Python CLI (Traditional Manim Way)

Activate your conda environment and use manim's CLI:

```bash
conda activate manim
manim -pql py/manim_examples.py CreateCircle
```

Flags:
- `-p`: Preview the video after rendering
- `-q`: Quality (l=low, m=medium, h=high)
- `-ql`: Quick low quality render

## Available Examples

### In Python file (`py/manim_examples.py`):
- `CreateCircle` - Basic quickstart example
- `SquareToCircle` - Transformation animation
- `SquareAndCircle` - Multiple objects
- `AnimatedSquareToCircle` - Advanced animation
- `DifferentRotations` - Rotation methods

Render any of them:
```bash
manim -pql py/manim_examples.py <SceneName>
```

### From Clojure REPL:

After initialization, you can create custom animations:

```clojure
(let [manim (py/import-module "manim")
      Scene (py/get-attr manim "Scene")
      Circle (py/get-attr manim "Circle")
      Square (py/get-attr manim "Square")
      Create (py/get-attr manim "Create")
      Transform (py/get-attr manim "Transform")
      BLUE (py/get-attr manim "BLUE")
      PINK (py/get-attr manim "PINK")

      ;; Create a custom scene
      MyScene
      (py/python-type
       "MyScene"
       [Scene]
       {"construct"
        (fn [self]
          (let [circle (Circle)
                square (Square)]
            (py/call-attr circle "set_fill" PINK :opacity 0.7)
            (py/call-attr square "set_fill" BLUE :opacity 0.7)
            (py/call-attr self "play" (Create square))
            (py/call-attr self "play" (Transform square circle))))})]

  ;; Render it
  (let [scene (MyScene)]
    (py/call-attr scene "render")))
```

## Troubleshooting

### Python library not found
If you get an error about libpython not found, check your Python shared library:

```bash
conda activate manim
find $CONDA_PREFIX -name "libpython*.so"
```

If auto-detection picks the wrong file, export `DESARGUES_MANIM_LIBPYTHON` to point at
the correct `libpython3.x.so` (no code edits needed — `desargues.config` reads it).

### Import errors
Make sure manim is properly installed:

```bash
conda activate manim
python -c "import manim; print(manim.__version__)"
```

### Video output location
By default, manim saves videos to:
```
media/videos/<python_file_name>/<quality>/<scene_name>.mp4
```

For Clojure-rendered scenes, check:
```
media/videos/
```

## Next Steps

- Read the [Manim documentation](https://docs.manim.community/)
- Explore more examples in the [Manim example gallery](https://docs.manim.community/en/stable/examples.html)
- Create your own scenes in Clojure!
- Combine with Emmy (already in `deps.edn`) for mathematical visualizations

## Resources

- [Manim Documentation](https://docs.manim.community/)
- [libpython-clj Documentation](https://clj-python.github.io/libpython-clj/)
- [Manim Quickstart](https://docs.manim.community/en/stable/tutorials/quickstart.html)
