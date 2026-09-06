# Manim Quickstart - Ready to Use!

## ✅ All Fixed and Working!

All syntax errors have been resolved. The integration is ready to use.

## 🚀 Try It Now

### Option 1: Run the Complete Quickstart Example

```bash
lein repl
```

Then in the REPL:

```clojure
(require '[varcalc.manim-quickstart :as mq])
(mq/quickstart!)
```

This will:
1. Initialize Python with your manim conda environment
2. Create the CreateCircle scene (pink circle)
3. Render the animation
4. Save to `media/videos/`

### Option 2: Step-by-Step

```clojure
(require '[varcalc.manim-quickstart :as mq])
(require '[libpython-clj2.python :as py])

;; Step 1: Initialize Python (do this once per REPL session)
(mq/init!)

;; Step 2: Create and render the CreateCircle scene
(let [CreateCircle (mq/make-create-circle-scene)
      scene (CreateCircle)]
  (mq/render-scene! scene))
```

### Option 3: Custom Scene

```clojure
(require '[varcalc.manim-quickstart :as mq])
(require '[libpython-clj2.python :as py])

(mq/init!)

;; Get manim module and objects
(let [manim (mq/get-manim-module)
      Circle (py/get-attr manim "Circle")
      Create (py/get-attr manim "Create")
      RED (py/get-attr manim "RED")

      ;; Create a custom scene
      MyScene
      (mq/create-scene-class
       "MyRedCircle"
       (fn [self]
         (let [circle (Circle)]
           (py/call-attr-kw circle "set_fill" [RED] {:opacity 0.8})
           (py/call-attr self "play" (Create circle)))))]

  ;; Render it
  (mq/render-scene! (MyScene)))
```

### Option 4: Square to Circle Transformation

```clojure
(require '[varcalc.manim-quickstart :as mq])

(mq/init!)

(let [SquareToCircle (mq/make-square-to-circle-scene)
      scene (SquareToCircle)]
  (mq/render-scene! scene))
```

## 📖 More Examples

Check the `comment` block at the end of `src/varcalc/manim_quickstart.clj` for many more examples!

## 🐍 Traditional Python Way

You can also use the traditional manim CLI:

```bash
conda activate manim
manim -pql manim_examples.py CreateCircle
```

Available scenes in `manim_examples.py`:
- `CreateCircle` - Pink circle (quickstart example)
- `SquareToCircle` - Transformation animation
- `SquareAndCircle` - Multiple objects
- `AnimatedSquareToCircle` - Advanced animation
- `DifferentRotations` - Rotation comparison

## 💡 Important Notes

### Keyword Arguments

When calling Python functions with keyword arguments, always use `call-attr-kw`:

```clojure
;; ✅ CORRECT:
(py/call-attr-kw circle "set_fill" [PINK] {:opacity 0.5})

;; ❌ WRONG (will fail):
(py/call-attr circle "set_fill" PINK :opacity 0.5)
```

### Initialization

- Initialize Python **once per REPL session**
- `(mq/init!)` or `(mq/quickstart!)` will handle this
- If already initialized, it will skip gracefully

### Output Location

Videos are saved to `media/videos/`

## 🧪 Verify Everything Works

Run the test suite:

```bash
lein test varcalc.manim-test
```

Expected output:
```
Ran 14 tests containing 40 assertions.
0 failures, 0 errors.
```

## 📚 Resources

- **Full documentation**: See `README_MANIM.md`
- **Detailed setup**: See `MANIM_SETUP.md`
- **Setup summary**: See `SETUP_COMPLETE.md`

- [Manim Documentation](https://docs.manim.community/)
- [Manim Example Gallery](https://docs.manim.community/en/stable/examples.html)
- [libpython-clj Guide](https://clj-python.github.io/libpython-clj/)

## 🎬 What Happens When You Run

When you run `(mq/quickstart!)`:

1. **Initialize Python**: Sets up Python 3.12 with manim environment
2. **Create Scene Class**: Defines the CreateCircle scene
3. **Instantiate Scene**: Creates an instance of the scene
4. **Render**: Executes the animation and saves to video

The output will be in `media/videos/` directory.

## 🔧 Troubleshooting

### "ModuleNotFoundError: No module named 'manim'"

Make sure you initialized Python first:
```clojure
(mq/init!)
```

### "No such var: py/python-initialized?"

This has been fixed! Just pull the latest changes.

### Conda environment issues

Verify manim is installed:
```bash
conda activate manim
python -c "import manim; print(manim.__version__)"
```

Should output: `0.21.0` (desargues is verified against Manim CE 0.21; 0.19+ also works)

---

**Ready to create beautiful mathematical animations!** 🎨

Start with: `(mq/quickstart!)`
