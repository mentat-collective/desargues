# Clean API Guide - SOLID/DDD Architecture

Simple, powerful API for mathematical animations.

## Quick Start

```clojure
(require '[desargues.api :as api])

;; 1. Initialize
(api/init!)

;; 2. Create expression
(def e (api/expr '(sin x)))

;; 3. Animate it!
(api/animate-expression e)
```

## Core Concepts

### Expressions

Mathematical expressions with identity:

```clojure
;; Create expression
(def quadratic (api/expr '(+ (* a (square x)) (* b x) c)))

;; With metadata
(def named-expr (api/expr '(sin x) {:name "sine" :domain "reals"}))
```

### Functions

Mathematical functions:

```clojure
;; Create function
(def f (api/func "f" #(emmy.env/sin %)))

;; With domain/codomain
(def g (api/func "g"
                 #(emmy.env/square %)
                 [:reals]
                 [:non-negative-reals]))
```

### Points

Evaluation points:

```clojure
(def p1 (api/point 'x 0))
(def p2 (api/point 'x (/ pi 2)))
```

## Operations

### Derivatives

```clojure
(def e (api/expr '(square (sin x))))
(def de (api/derivative e))

;; LaTeX
(api/to-latex de)
;; => "2\sin(x)\cos(x)"
```

### Evaluation

```clojure
(def e (api/expr '(square x)))
(def result (api/evaluate-at e (api/point 'x 3)))

;; result => {:input {:variable x, :value 3}
;;            :output 9
;;            :expression (square x)}
```

### Simplification

```clojure
(def e (api/expr '(+ (square (sin x)) (square (cos x)))))
(def simplified (api/simplify e))

(api/to-latex simplified)
;; => "1" (Pythagorean identity!)
```

## Animations

### Fluent API (Recommended)

Build scenes step-by-step:

```clojure
(-> (api/scene)
    (api/show expr1)
    (api/wait 1)
    (api/show expr2)
    (api/wait 1)
    (api/transform expr1 expr2)
    (api/wait 2)
    (api/render!))
```

### High-Level Workflows

Pre-built workflows for common cases:

```clojure
;; Show expression
(api/animate-expression e)

;; Show derivative
(api/animate-derivative e)

;; Show simplification
(api/animate-simplification e)

;; Transform one to another
(api/animate-transformation e1 e2)
```

## Examples

### Example 1: Basic Animation

```clojure
(require '[desargues.api :as api])

(api/init!)

;; Create expression
(def e (api/expr '(square (sin (+ x 3)))))

;; Animate it
(api/animate-expression e)
```

### Example 2: Derivative Animation

```clojure
(api/init!)

(def f (api/expr '(sin x)))

;; Show function and derivative
(api/animate-derivative f)
```

### Example 3: Transformation

```clojure
(api/init!)

(def e1 (api/expr '(+ (square (sin x)) (square (cos x)))))
(def e2 (api/simplify e1))  ;; Becomes 1

;; Show transformation
(api/animate-transformation e1 e2)
```

### Example 4: Custom Scene

```clojure
(api/init!)

(def e1 (api/expr '(sin x)))
(def e2 (api/derivative e1))  ;; cos x
(def e3 (api/derivative e2))  ;; -sin x

(-> (api/scene)
    ;; Show function
    (api/show e1)
    (api/wait 1)

    ;; Show first derivative
    (api/show e2)
    (api/wait 1)

    ;; Show second derivative
    (api/show e3)
    (api/wait 1)

    ;; Transform original to second derivative
    (api/transform e1 e3)
    (api/wait 2)

    (api/render!))
```

### Example 5: Your Use Case

From your `equations.clj`:

```clojure
(require '[desargues.emmy-python.equations :as eq])
(require '[desargues.api :as api])

(api/init!)

;; Convert your function to expression
(def expt1-expr (api/expr (eq/expt1 'x)))

;; Animate it
(api/animate-expression expt1-expr)

;; Show its derivative
(api/animate-derivative expt1-expr)

;; Evaluate at a point
(def result (api/evaluate-at expt1-expr (api/point 'x 0)))
```

## Macros

### defexpr - Define Named Expression

```clojure
(api/defexpr pythagorean
  (+ (square (sin x)) (square (cos x))))

;; Use it
(api/animate-simplification pythagorean)
;; Shows: sin²(x) + cos²(x) → 1
```

### deffunc - Define Named Function

```clojure
(require '[emmy.env :as e])

(api/deffunc gaussian [x]
  (e/exp (- (e/square x))))

;; Use it
(def gauss-expr (api/expr (gaussian 'x)))
(api/animate-expression gauss-expr)
```

## Tables and Comparisons

### Tabulate Function

```clojure
(def f (api/func "f" #(emmy.env/sin %)))

(api/tabulate f
  [(api/point 'x 0)
   (api/point 'x (/ pi 2))
   (api/point 'x pi)])

;; Returns:
;; {:function f
;;  :points [...]
;;  :values [{:point ... :value ...} ...]}
```

### Compare Functions

```clojure
(def f1 (api/func "sin" #(emmy.env/sin %)))
(def f2 (api/func "cos" #(emmy.env/cos %)))

(api/compare-functions
  [f1 f2]
  [(api/point 'x 0) (api/point 'x (/ pi 4))])

;; Returns comparison data structure
```

## Architecture Layers

### User-Facing API

```clojure
(require '[desargues.api :as api])  ;; ← You use this
```

### Domain Layer (Advanced)

Direct access to domain models and services:

```clojure
(require '[desargues.domain.math-expression :as expr])
(require '[desargues.domain.protocols :as p])
(require '[desargues.domain.services :as svc])

;; Create expression
(def e (expr/create-expression '(sin x)))

;; Use protocols
(p/to-latex e)
(p/simplify e)

;; Use domain services
(svc/differentiate-expression e)
```

### Infrastructure (Expert)

Direct Manim access:

```clojure
(require '[desargues.infrastructure.manim-adapter :as manim])

;; Build scene manually
(def builder (manim/create-scene-builder))
(def builder2 (manim/with-object builder expr))
(manim/build-and-render builder2)
```

## Complete Workflow

```clojure
(require '[desargues.api :as api])
(require '[emmy.env :refer [sin cos square pi D]])

;; Initialize
(api/init!)

;; 1. Create expressions
(def f (api/expr '(square (sin x))))
(def df (api/derivative f))

;; 2. Simplify
(def f-simp (api/simplify f))

;; 3. Evaluate at point
(def result (api/evaluate-at f (api/point 'x (/ pi 4))))

;; 4. Get LaTeX
(println "f(x) =" (api/to-latex f))
(println "f'(x) =" (api/to-latex df))

;; 5. Animate
(-> (api/scene)
    ;; Show original
    (api/show f)
    (api/wait 1)

    ;; Show derivative
    (api/show df)
    (api/wait 1)

    ;; Show simplified form
    (api/transform f f-simp)
    (api/wait 2)

    (api/render!))
```

## Benefits

### Simple for Beginners

```clojure
;; Just 3 lines!
(api/init!)
(def e (api/expr '(sin x)))
(api/animate-expression e)
```

### Powerful for Experts

```clojure
;; Full control with fluent API
(-> (api/scene)
    (api/show expr1)
    (configure-color :blue)
    (configure-position [0 2 0])
    (api/transform expr1 expr2)
    (with-custom-animation ...)
    (api/render!))
```

### Clean Architecture

- **Domain** - Pure math logic
- **Infrastructure** - Manim integration
- **API** - Simple interface

You only interact with the API layer!

## Migration from Old Code

### Before (Direct Emmy + Manim)

```clojure
(require '[emmy.env :refer [->TeX sin]])
(require '[desargues.manim-quickstart :as mq])

(mq/init!)
(let [latex (->TeX (sin 'x))]
  ;; Manually create Python scene...
  )
```

### After (Clean API)

```clojure
(require '[desargues.api :as api])

(api/init!)
(api/animate-expression (api/expr '(sin x)))
```

**Much cleaner!**

## Summary

The new API provides:

✅ **Simple** - Easy to use
✅ **Powerful** - Full control when needed
✅ **Clean** - Follows SOLID/DDD
✅ **Testable** - Well-structured
✅ **Extensible** - Easy to add features

Start simple:
```clojure
(api/init!)
(api/animate-expression (api/expr '(sin x)))
```

Go advanced when ready:
```clojure
(-> (api/scene)
    (api/show ...)
    (api/transform ...)
    (api/render!))
```

See `ARCHITECTURE.md` for design details!
