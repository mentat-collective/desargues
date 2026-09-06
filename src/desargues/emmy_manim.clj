(ns desargues.emmy-manim
  "Bridge between Emmy (symbolic math) and Manim (animations)"
  (:require [emmy.env :as e :refer [->TeX D simplify sin cos square exp log]]
            [libpython-clj2.python :as py]
            [desargues.manim-quickstart :as mq]
            [desargues.pipeline.emmy :as conv]))

;; ============================================================================
;; Emmy to LaTeX
;; ============================================================================

(defn emmy->latex
  "Convert an Emmy expression to a LaTeX string (the conveyor's Promote stage;
   kept here as the historical entry point)."
  [expr]
  (:content (conv/promote-latex expr)))

;; ============================================================================
;; LaTeX to Python (using latex2py)
;; ============================================================================

(defn latex->python
  "Convert LaTeX math string to Python code using latex2py"
  [latex-str]
  (let [latex2py (py/import-module "latex2py.parser")
        parse-latex (py/get-attr latex2py "parse_latex")]
    (parse-latex latex-str)))

;; ============================================================================
;; Combined Pipeline
;; ============================================================================

(defn emmy->python
  "Convert an Emmy expression to Python code via LaTeX: the pure conveyor
   with this namespace's Python-backed latex->python injected as the
   boundary converter."
  [expr]
  (conv/python-code expr latex->python))

;; ============================================================================
;; Manim Integration
;; ============================================================================

(defn render-latex-equation
  "Render a LaTeX equation in Manim and return the Tex object"
  [latex-str]
  (let [manim (mq/get-manim-module)
        Tex (py/get-attr manim "Tex")]
    (Tex latex-str)))

(defn render-emmy-equation
  "Render an Emmy expression as LaTeX in Manim"
  [expr]
  (render-latex-equation (emmy->latex expr)))

;; ============================================================================
;; Example Expressions
;; ============================================================================

(defn example-expressions
  "Collection of interesting mathematical expressions"
  []
  {:quadratic '(+ (* a (square x)) (* b x) c)
   :derivative '(D (fn [x] (sin x)))
   :integral '(integral (fn [x] (exp x)))
   :sin-squared '(square (sin (+ a 3)))
   :chain-rule '(D (fn [x] (square (sin x))))
   :taylor-sin '(+ x (/ (- (expt x 3)) 6) (/ (expt x 5) 120))})

;; ============================================================================
;; REPL Examples
;; ============================================================================

(comment
  ;; Initialize Python/Manim
  (mq/init!)

  ;; === Emmy to LaTeX ===

  ;; Simple expression
  (emmy->latex '(+ a b))
  ;; => "a + b"

  ;; Your example - use symbolic evaluation
  (emmy->latex '(square (sin (+ a 3))))
  ;; => "\\sin^{2}\\left(a + 3\\right)"

  ;; Or build it up programmatically (need to quote or use symbols)
  (emmy->latex (list 'square (list 'sin (list '+ 'a 3))))

  ;; Derivative
  (emmy->latex ((D sin) 'x))
  ;; => "\\cos\\left(x\\right)"

  ;; === LaTeX to Python ===

  (latex->python "\\frac{1}{2} + \\frac{3}{4}")
  ;; => "((1 / 2) + (3 / 4))"

  (latex->python "\\sin(x)")
  ;; => "sin(x)"

  ;; === Combined: Emmy to Python ===

  (emmy->python (square (sin (+ 'a 3))))
  ;; => LaTeX => Python code

  (emmy->python '(+ (* a (square x)) (* b x) c))
  ;; => Quadratic formula in Python

  ;; === Render in Manim ===

  ;; Create a Tex object for an Emmy expression
  (let [tex-obj (render-emmy-equation (square (sin (+ 'a 3))))]
    tex-obj)

  ;; Get LaTeX for derivative
  (emmy->latex ((D #(sin %)) 'x))

  ;; Compare function and its derivative
  (let [f #(sin %)
        df (D f)]
    {:function (emmy->latex (f 'x))
     :derivative (emmy->latex (df 'x))}))
