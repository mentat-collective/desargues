# Emmy + Manim Integration Guide

Combine Emmy's symbolic mathematics with Manim's beautiful animations!

## Pipeline

```
Emmy Expression → LaTeX → Python Code → Manim Animation
```

## Quick Start

```clojure
(require '[desargues.emmy-manim-examples :as ex])

;; Example 1: Your original example - sin²(x + 3)
(ex/example-chain-rule)

;; Example 2: sin(x + π) and its derivative
(ex/example-sin-derivative)

;; Example 3: Taylor series expansion
(ex/example-taylor-series)
```

## Components

### 1. Emmy → LaTeX

```clojure
(require '[desargues.emmy-manim :as em])
(require '[emmy.env :refer [sin cos square D]])

;; Your example
(em/emmy->latex (square (sin (+ 'a 3))))
;; => "\\sin^{2}\\left(a + 3\\right)"

;; Derivative
(em/emmy->latex ((D sin) 'x))
;; => "\\cos\\left(x\\right)"
```

### 2. LaTeX → Python (using latex2py)

```clojure
(em/latex->python "\\frac{1}{2} + \\frac{3}{4}")
;; => "((1 / 2) + (3 / 4))"

(em/latex->python "\\sin(x)")
;; => "sin(x)"
```

### 3. Combined: Emmy → Python

```clojure
(em/emmy->python (square (sin (+ 'a 3))))
;; Emmy → LaTeX → Python in one step
```

### 4. Render in Manim

```clojure
(require '[desargues.manim-quickstart :as mq])

;; Initialize (Python/Manim paths come from desargues.config)
(mq/init!)

;; Render a LaTeX equation
(let [tex-obj (em/render-emmy-equation (square (sin (+ 'a 3))))]
  tex-obj)
```

## Examples

### Example 1: Your Original Use Case

```clojure
(require '[desargues.emmy-manim-examples :as ex])

;; Show sin²(x + 3) with chain rule derivation
(ex/example-chain-rule)
```

This will:
1. Calculate the derivative using Emmy
2. Generate LaTeX for both
3. Convert to Python code
4. Render a Manim animation showing the chain rule

### Example 2: Explore Any Function

```clojure
(require '[emmy.env :refer [sin cos exp square D]])

;; Define a function
(defn my-func [x]
  (* (exp x) (sin x)))

;; Explore it
(ex/explore-function my-func)
;; Returns:
;; {:function {:expr (...)
;;             :latex "..."
;;             :python "..."}
;;  :derivative {:expr (...)
;;               :latex "..."
;;               :python "..."}}

;; Animate it
(ex/create-derivative-animation my-func)
```

### Example 3: Multiple Derivatives

```clojure
;; Function and its first 3 derivatives
(let [f #(sin %)
      df1 (D f)
      df2 (D df1)
      df3 (D df2)]
  (map #(em/emmy->latex (% 'x)) [f df1 df2 df3]))
;; => ["\\sin\\left(x\\right)"
;;     "\\cos\\left(x\\right)"
;;     "-\\sin\\left(x\\right)"
;;     "-\\cos\\left(x\\right)"]
```

### Example 4: From Your Code

```clojure
(require '[desargues.emmy-python.equations :as eq])

;; Use the functions you defined
(em/emmy->latex (eq/expt1 'x))
;; => "\\sin\\left(x + \\pi\\right)"

(em/emmy->latex ((D eq/expt1) 'x))
;; => "\\cos\\left(x + \\pi\\right)"

;; Animate them
(ex/create-derivative-animation eq/expt1)
```

## Available Manim Scenes

All in `py/emmy_manim_scenes.py`:

### FunctionAndDerivative
Shows a function and its derivative side by side

```clojure
(ex/create-derivative-animation #(sin %))
```

### DerivativeSteps
Step-by-step derivative calculation for sin²(x + 3)

```clojure
(ex/render-emmy-scene "DerivativeSteps")
```

### ChainRule
Your original example with chain rule

```clojure
(ex/example-chain-rule)
```

### TaylorSeries
Taylor series expansion of sin(x)

```clojure
(ex/example-taylor-series)
```

### ProductRule
Product rule demonstration

```clojure
(ex/example-product-rule)
```

### EmmyShowcase
Multiple Emmy equations showcased

```clojure
(ex/render-emmy-scene "EmmyShowcase")
```

### QuadraticFormula
Quadratic formula animation

```clojure
(ex/render-emmy-scene "QuadraticFormula")
```

## Creating Custom Scenes

### Option 1: Add to Python file

Add your scene to `py/emmy_manim_scenes.py`:

```python
class MyCustom(Scene):
    def __init__(self, equation_latex=None, **kwargs):
        self.equation_latex = equation_latex or r"f(x) = x^2"
        super().__init__(**kwargs)

    def construct(self):
        eq = MathTex(self.equation_latex)
        self.play(Write(eq))
        self.wait(2)
```

Use from Clojure:

```clojure
(let [latex (em/emmy->latex (square 'x))]
  (ex/render-emmy-scene "MyCustom" :equation_latex latex))
```

### Option 2: Use existing scenes

Pass Emmy-generated LaTeX to existing scenes:

```clojure
;; Add the project's scene dir to Python's sys.path.
;; Replaces the old hardcoded (sys.path.insert 0 "/abs/path") snippet.
(desargues.config/add-project-to-syspath!)

;; Import and use
(let [scenes (py/import-module "emmy_manim_scenes")
      FunctionAndDerivative (py/get-attr scenes "FunctionAndDerivative")
      f-latex (em/emmy->latex (sin 'x))
      df-latex (em/emmy->latex (cos 'x))
      scene (py/call-attr-kw FunctionAndDerivative []
                             {:func_latex f-latex
                              :deriv_latex df-latex})]
  (mq/render-scene! scene))
```

## Complete Workflow Example

```clojure
(require '[desargues.emmy-manim-examples :as ex])
(require '[desargues.emmy-manim :as em])
(require '[emmy.env :refer [sin cos square exp D pi]])

;; 1. Define a function
(defn f [x]
  (square (sin (+ x 'pi))))

;; 2. Calculate derivative
(def df (D f))

;; 3. Generate LaTeX
(def f-latex (em/emmy->latex (f 'x)))
(def df-latex (em/emmy->latex (df 'x)))

;; 4. Convert to Python
(def f-python (em/emmy->python (f 'x)))
(def df-python (em/emmy->python (df 'x)))

;; 5. Print everything
(println "Function:")
(println "  LaTeX:" f-latex)
(println "  Python:" f-python)
(println "\nDerivative:")
(println "  LaTeX:" df-latex)
(println "  Python:" df-python)

;; 6. Render animation
(ex/create-derivative-animation f)
```

## Integration with Your Code

Your `desargues.emmy-python.equations` namespace:

```clojure
(ns desargues.emmy-python.equations
  (:require [emmy.env :as e :refer :all]))

(defn expt1 [x]
  (e/sin (+ x pi)))
```

Use it:

```clojure
(require '[desargues.emmy-python.equations :as eq])
(require '[desargues.emmy-manim-examples :as ex])

;; Animate expt1 and its derivative
(ex/create-derivative-animation eq/expt1)

;; Explore it
(ex/explore-function eq/expt1)

;; Get LaTeX
(em/emmy->latex (eq/expt1 'x))
;; => "\\sin\\left(x + \\pi\\right)"

;; Get Python
(em/emmy->python (eq/expt1 'x))
```

## Tips

### Use Symbolic Pi

Instead of:
```clojure
(->TeX (sin (+ 'x 3.141592653589793)))
;; => "\\sin\\left(x + 3.141592653589793\\right)"
```

Use:
```clojure
(->TeX (sin (+ 'x 'pi)))
;; => "\\sin\\left(x + \\pi\\right)"
```

### Simplify Before Rendering

```clojure
(require '[emmy.env :refer [simplify]])

(let [expr '(+ (square (sin x)) (square (cos x)))
      simplified (simplify expr)]
  {:original (em/emmy->latex expr)
   :simplified (em/emmy->latex simplified)})
;; simplified => 1 (Pythagorean identity!)
```

## Output Location

All videos are saved to:
```
media/videos/1080p60/<SceneName>.mp4
```

## Resources

- **Emmy Documentation:** https://github.com/mentat-collective/emmy
- **Manim Documentation:** https://docs.manim.community/
- **LaTeX2Py:** https://github.com/OrangeX4/latex2py

## Next Steps

1. Try the examples: `(ex/example-chain-rule)`
2. Explore your functions: `(ex/explore-function eq/expt1)`
3. Create custom animations by adding scenes to `py/emmy_manim_scenes.py`
4. Combine with symbolic simplification for equation transformations

Happy mathematical animating! 📐✨
