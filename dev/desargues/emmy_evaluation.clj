(ns desargues.emmy-evaluation
  "Evaluate Emmy expressions at specific values and animate with Manim"
  (:require [emmy.env :as e :refer [->TeX simplify]]
            [libpython-clj2.python :as py]
            [desargues.config :as config]
            [desargues.manim-quickstart :as mq]
            [desargues.emmy-manim :as em]))

;; ============================================================================
;; Evaluation Helpers
;; ============================================================================

(defn evaluate-at
  "Evaluate an Emmy function at a specific numeric value"
  [f x-val]
  (f x-val))

(defn evaluate-symbolic
  "Evaluate an Emmy expression symbolically at a value.
   Uses function application instead of substitution."
  [expr var value]
  ;; For Emmy, we typically work with functions, not raw expressions
  ;; If expr is a function, apply it; otherwise return as-is
  (if (fn? expr)
    (expr value)
    expr))

;; ============================================================================
;; Scene Creation
;; ============================================================================

(defn render-evaluation-scene
  "Render an equation evaluation scene"
  [scene-name & args]
  ;; Add project directory to path
  (config/add-project-to-syspath!)

  ;; Import and render
  (let [scenes (py/import-module "equation_evaluation_scenes")
        scene-class (py/get-attr scenes scene-name)
        scene (if (empty? args)
                (scene-class)
                (apply scene-class args))]
    (mq/render-scene! scene)))

(defn create-single-evaluation
  "Create an animation showing f(x) evaluated at a specific value"
  [f x-val]
  (let [;; Get symbolic expression
        f-expr (f 'x)
        f-latex (em/emmy->latex f-expr)

        ;; Evaluate at the value
        result (f x-val)

        ;; Create substituted expression
        subst-expr (f x-val)
        subst-latex (em/emmy->latex subst-expr)

        ;; Simplify result
        result-simplified (simplify result)
        result-latex (em/emmy->latex result-simplified)]

    (println "Function:" f-latex)
    (println "At x =" x-val)
    (println "Substituted:" subst-latex)
    (println "Result:" result-latex)

    ;; Add project directory to path
    (config/add-project-to-syspath!)

    ;; Create scene
    (let [scenes (py/import-module "equation_evaluation_scenes")
          EquationEvaluation (py/get-attr scenes "EquationEvaluation")
          scene (py/call-attr-kw EquationEvaluation []
                                 {:equation_latex (str "f(x) = " f-latex)
                                  :var_name "x"
                                  :var_value (str x-val)
                                  :substituted_latex subst-latex
                                  :result result-latex})]
      (mq/render-scene! scene))))

(defn create-evaluation-table
  "Create a table showing a function evaluated at multiple points"
  [f x-values]
  (let [f-expr (f 'x)
        f-latex (em/emmy->latex f-expr)

        ;; Evaluate at all points
        evaluations (map (fn [x-val]
                           (let [result (simplify (f x-val))
                                 result-latex (em/emmy->latex result)]
                             [(str x-val)
                              (em/emmy->latex (f x-val))
                              result-latex]))
                         x-values)]

    (println "Creating table for:" f-latex)
    (println "Values:" x-values)
    (doseq [[x expr result] evaluations]
      (println (str "  f(" x ") = " result)))

    ;; Add project directory to path
    (config/add-project-to-syspath!)

    ;; Create scene
    (let [scenes (py/import-module "equation_evaluation_scenes")
          EvaluationTable (py/get-attr scenes "EvaluationTable")
          rows (py/->py-list (map py/->py-list evaluations))
          scene (py/call-attr-kw EvaluationTable []
                                 {:title_text (str "Evaluating " f-latex)
                                  :headers (py/->py-list ["x" "f(x)" "Value"])
                                  :rows rows})]
      (mq/render-scene! scene))))

(defn create-multiple-evaluations
  "Show the same function evaluated at multiple points"
  [f x-values]
  (let [f-expr (f 'x)
        f-latex (em/emmy->latex f-expr)

        ;; Evaluate at all points
        evaluations (map (fn [x-val]
                           (let [result (simplify (f x-val))]
                             [(str x-val) (em/emmy->latex result)]))
                         x-values)]

    ;; Add project directory to path
    (config/add-project-to-syspath!)

    ;; Create scene
    (let [scenes (py/import-module "equation_evaluation_scenes")
          MultipleEvaluations (py/get-attr scenes "MultipleEvaluations")
          evals-py (py/->py-list (map py/->py-tuple evaluations))
          scene (py/call-attr-kw MultipleEvaluations []
                                 {:equation_latex (str "f(x) = " f-latex)
                                  :var_name "x"
                                  :evaluations evals-py})]
      (mq/render-scene! scene))))

(defn create-derivative-evaluation
  "Show function and derivative evaluated at a point"
  [f x-val]
  (let [df (e/D f)

        ;; Get expressions
        f-expr (f 'x)
        df-expr (df 'x)

        ;; Get LaTeX
        f-latex (em/emmy->latex f-expr)
        df-latex (em/emmy->latex df-expr)

        ;; Evaluate at point
        f-val (simplify (f x-val))
        df-val (simplify (df x-val))

        ;; Get result LaTeX
        f-val-latex (em/emmy->latex f-val)
        df-val-latex (em/emmy->latex df-val)]

    (println "Function:" f-latex "→" f-val-latex "at x =" x-val)
    (println "Derivative:" df-latex "→" df-val-latex "at x =" x-val)

    ;; Add project directory to path
    (config/add-project-to-syspath!)

    ;; Create scene
    (let [scenes (py/import-module "equation_evaluation_scenes")
          DerivativeAtPoint (py/get-attr scenes "DerivativeAtPoint")
          scene (py/call-attr-kw DerivativeAtPoint []
                                 {:func_latex (str "f(x) = " f-latex)
                                  :deriv_latex (str "f'(x) = " df-latex)
                                  :point (str x-val)
                                  :func_value f-val-latex
                                  :deriv_value df-val-latex})]
      (mq/render-scene! scene))))

(defn create-comparison-table
  "Compare multiple functions at the same points"
  [functions x-values]
  (let [;; Get function expressions and LaTeX
        func-data (map (fn [f]
                         (let [expr (f 'x)
                               latex (em/emmy->latex expr)]
                           {:f f :expr expr :latex latex}))
                       functions)

        ;; Evaluate each function at each point
        results (map (fn [{:keys [f]}]
                       (map (fn [x-val]
                              (em/emmy->latex (simplify (f x-val))))
                            x-values))
                     func-data)

        ;; Create function names for display
        func-names (map-indexed (fn [i {:keys [latex]}]
                                  (str (char (+ 102 i)) "(x) = " latex))
                                func-data)]

    ;; Add project directory to path
    (config/add-project-to-syspath!)

    ;; Create scene
    (let [scenes (py/import-module "equation_evaluation_scenes")
          ComparisonTable (py/get-attr scenes "ComparisonTable")
          x-vals-py (py/->py-list (map str x-values))
          results-py (py/->py-list (map py/->py-list results))
          func-names-py (py/->py-list func-names)
          scene (py/call-attr-kw ComparisonTable []
                                 {:title_text "Function Comparison"
                                  :function_names func-names-py
                                  :x_values x-vals-py
                                  :results results-py})]
      (mq/render-scene! scene))))

;; ============================================================================
;; REPL Examples
;; ============================================================================

(comment
  ;; Setup
  (require '[emmy.env :refer [sin cos exp square pi D]])
  (mq/init!)

  ;; === Single Evaluation ===

  ;; Example 1: sin(x) at x = π/2
  (create-single-evaluation #(sin %) (/ e/pi 2))

  ;; Example 2: x² at x = 3
  (create-single-evaluation #(square %) 3)

  ;; Example 3: sin(x + π) at x = 0
  (create-single-evaluation #(sin (+ % 'pi)) 0)

  ;; === Evaluation Table ===

  ;; Example 1: sin(x) at multiple points
  (create-evaluation-table
   #(sin %)
   [0 (/ e/pi 4) (/ e/pi 2) (* 3 (/ e/pi 4)) e/pi])

  ;; Example 2: Quadratic at integer points
  (create-evaluation-table
   #(+ (square %) (* 2 %) 1)
   [0 1 2 3 4])

  ;; Example 3: Exponential decay
  (create-evaluation-table
   #(exp (- %))
   [0 1 2 3])

  ;; === Multiple Evaluations (List Style) ===

  (create-multiple-evaluations
   #(square %)
   [1 2 3 4 5])

  ;; === Derivative at a Point ===

  ;; sin(x) and cos(x) at π/4
  (create-derivative-evaluation
   #(sin %)
   (/ e/pi 4))

  ;; x³ and 3x² at x = 2
  (create-derivative-evaluation
   #(* % % %)
   2)

  ;; === Compare Multiple Functions ===

  ;; Compare sin and cos at same points
  (create-comparison-table
   [#(sin %) #(cos %)]
   [0 (/ e/pi 2) e/pi])

  ;; Compare x, x², x³
  (create-comparison-table
   [identity #(square %) #(* % % %)]
   [0 1 2 3])

  ;; === Pre-made Scenes ===

  ;; Default examples
  (render-evaluation-scene "EquationEvaluation")
  (render-evaluation-scene "EvaluationTable")
  (render-evaluation-scene "MultipleEvaluations")
  (render-evaluation-scene "TabulateFunction"))
