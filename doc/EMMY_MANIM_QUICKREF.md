# Emmy + Manim Quick Reference

## Setup

```clojure
(require '[desargues.emmy-manim-examples :as ex])
(require '[desargues.emmy-manim :as em])
(require '[emmy.env :refer [sin cos square exp D pi ->TeX]])
(require '[desargues.manim-quickstart :as mq])

(mq/init!)  ;; Do this once
```

## Your Original Example

```clojure
;; Generate LaTeX for sin²(a + 3)
(->TeX '(square (sin (+ a 3))))
;; => "\\sin^{2}\\left(a + 3\\right)"

;; Full pipeline: Emmy → LaTeX → Python
(em/emmy->python '(square (sin (+ a 3))))
```

**Important:** Use quoted forms `'(...)` for symbolic expressions!

## Working with Functions

```clojure
;; Define a function
(defn f [x]
  (sin (+ x pi)))

;; Get LaTeX for f(x)
(->TeX (f 'x))
;; => "\\sin\\left(x + \\pi\\right)"

;; Get LaTeX for derivative
(->TeX ((D f) 'x))
;; => "\\cos\\left(x + \\pi\\right)"

;; Explore function and derivative
(ex/explore-function f)

;; Animate it!
(ex/create-derivative-animation f)
```

## Common Patterns

### Pattern 1: Symbolic Expression

```clojure
;; Quoted symbolic form
(->TeX '(+ (* a (square x)) (* b x) c))
;; => "c + b\\,x + a\\,{x}^{2}"
```

### Pattern 2: Function Application

```clojure
;; Define function, apply to symbol
(defn my-func [x]
  (* (exp x) (sin x)))

(->TeX (my-func 'x))
```

### Pattern 3: Derivatives

```clojure
;; Any function
(defn f [x]
  (square (sin x)))

;; Its derivative
(->TeX ((D f) 'x))
```

## LaTeX to Python

```clojure
(em/latex->python "\\frac{1}{2} + \\frac{3}{4}")
;; => "((1 / 2) + (3 / 4))"

(em/latex->python "\\sin(x)")
;; => "sin(x)"
```

## Emmy → Python Direct

```clojure
;; Combine both steps
(em/emmy->python '(square (sin (+ a 3))))
```

## Render Animations

### Example 1: Pre-made Scenes

```clojure
;; Chain rule example (your sin²(x + 3))
(ex/example-chain-rule)

;; Taylor series
(ex/example-taylor-series)

;; Product rule
(ex/example-product-rule)
```

### Example 2: Custom Function

```clojure
;; Define your function
(defn my-func [x]
  (exp (- (square x))))  ;; Gaussian

;; Animate it and its derivative
(ex/create-derivative-animation my-func)
```

### Example 3: From Your Code

```clojure
(require '[desargues.emmy-python.equations :as eq])

;; Use your expt1 function
(ex/create-derivative-animation eq/expt1)
```

## Output

All videos saved to: `media/videos/1080p60/`

## Key Functions

| Function | Purpose | Example |
|----------|---------|---------|
| `->TeX` | Emmy → LaTeX | `(->TeX '(sin x))` |
| `em/latex->python` | LaTeX → Python | `(em/latex->python "\\sin(x)")` |
| `em/emmy->python` | Emmy → Python | `(em/emmy->python '(sin x))` |
| `ex/explore-function` | Analyze function | `(ex/explore-function #(sin %))` |
| `ex/create-derivative-animation` | Animate f and f' | `(ex/create-derivative-animation f)` |
| `ex/render-emmy-scene` | Render scene | `(ex/render-emmy-scene "ChainRule")` |

## Available Scenes

- `"DerivativeSteps"` - Step-by-step derivative
- `"ChainRule"` - Chain rule for sin²(x + 3)
- `"TaylorSeries"` - Taylor series expansion
- `"ProductRule"` - Product rule demo
- `"QuadraticFormula"` - Quadratic formula
- `"EmmyShowcase"` - Multiple equations

## Examples

### Example 1: Your Use Case

```clojure
(require '[desargues.emmy-python.equations :as eq])

;; Your expt1: sin(x + π)
(->TeX (eq/expt1 'x))
;; => "\\sin\\left(x + 3.141592653589793\\right)"

;; Better: use symbolic pi
(defn expt1-symbolic [x]
  (sin (+ x 'pi)))

(->TeX (expt1-symbolic 'x))
;; => "\\sin\\left(x + \\pi\\right)"

(->TeX ((D expt1-symbolic) 'x))
;; => "\\cos\\left(x + \\pi\\right)"
```

### Example 2: Multiple Derivatives

```clojure
;; Function and first 3 derivatives of sin
(let [f #(sin %)
      df1 (D f)
      df2 (D df1)
      df3 (D df2)]
  (map #(->TeX (% 'x)) [f df1 df2 df3]))
;; => ["\\sin\\left(x\\right)"
;;     "\\cos\\left(x\\right)"
;;     "-\\sin\\left(x\\right)"
;;     "-\\cos\\left(x\\right)"]
```

### Example 3: Simplification

```clojure
(require '[emmy.env :refer [simplify]])

;; sin²(x) + cos²(x) = 1
(let [expr '(+ (square (sin x)) (square (cos x)))]
  {:before (->TeX expr)
   :after (->TeX (simplify expr))})
```

## Troubleshooting

**ClassCastException:** Use quoted forms for symbolic expressions
```clojure
;; ❌ Wrong
(->TeX (square (sin (+ 'a 3))))

;; ✅ Correct
(->TeX '(square (sin (+ a 3))))
```

**Numeric π instead of symbol:** Use symbolic pi
```clojure
;; ❌ Numeric
(sin (+ x pi))  ;; => sin(x + 3.14159...)

;; ✅ Symbolic
(sin (+ x 'pi))  ;; => sin(x + π)
```

## More Info

See `EMMY_MANIM_GUIDE.md` for complete guide.
