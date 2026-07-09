(ns desargues.emmy-manim-examples
  "Complete examples of Emmy + Manim integration"
  (:require [emmy.env :as e :refer [->TeX D simplify sin cos square exp log pi]]
            [libpython-clj2.python :as py]
            [desargues.config :as config]
            [desargues.manim-quickstart :as mq]
            [desargues.emmy-manim :as em]
            [desargues.emmy-python.equations :as eq]))

;; ============================================================================
;; Helper Functions
;; ============================================================================

(defn render-emmy-scene
  "Render a scene from emmy_manim_scenes.py"
  [scene-name & args]
  ;; Add project directory to path
  (config/add-project-to-syspath!)

  ;; Import and instantiate the scene
  (let [scenes (py/import-module "emmy_manim_scenes")
        scene-class (py/get-attr scenes scene-name)
        scene (if (empty? args)
                (scene-class)
                (apply scene-class args))]
    (mq/render-scene! scene)))

(defn create-derivative-animation
  "Create an animation showing a function and its derivative"
  [f]
  (let [df (D f)
        f-latex (em/emmy->latex (f 'x))
        df-latex (em/emmy->latex (df 'x))]

    (println "Function:" f-latex)
    (println "Derivative:" df-latex)

    ;; Add project directory to path
    (config/add-project-to-syspath!)

    ;; Create and render scene
    (let [scenes (py/import-module "emmy_manim_scenes")
          FunctionAndDerivative (py/get-attr scenes "FunctionAndDerivative")
          scene (FunctionAndDerivative :func_latex (str "f(x) = " f-latex)
                                       :deriv_latex (str "f'(x) = " df-latex))]
      (mq/render-scene! scene))))

;; ============================================================================
;; Example Workflows
;; ============================================================================

(defn example-sin-derivative
  "Example: Show sin(x + π) and its derivative"
  []
  (mq/init!)
  (create-derivative-animation #(e/sin (+ % 'pi))))

(defn example-quadratic
  "Example: Show a quadratic and its derivative"
  []
  (mq/init!)
  (create-derivative-animation eq/expt2))

(defn example-chain-rule
  "Example: Your original example - sin²(x + 3)"
  []
  (mq/init!)
  (let [f #(square (sin (+ % 3)))
        df (D f)
        f-latex (em/emmy->latex (f 'x))
        df-latex (em/emmy->latex (df 'x))]

    (println "\nOriginal function:")
    (println "  LaTeX:" f-latex)
    (println "  Python:" (em/emmy->python (f 'x)))

    (println "\nDerivative:")
    (println "  LaTeX:" df-latex)
    (println "  Python:" (em/emmy->python (df 'x)))

    ;; Render the ChainRule scene
    (render-emmy-scene "ChainRule")))

(defn example-taylor-series
  "Example: Taylor series animation"
  []
  (mq/init!)
  (render-emmy-scene "TaylorSeries"))

(defn example-product-rule
  "Example: Product rule demonstration"
  []
  (mq/init!)
  (render-emmy-scene "ProductRule"))

;; ============================================================================
;; Interactive Exploration
;; ============================================================================

(defn explore-function
  "Explore a function: show it, its derivative, and Python code"
  [f]
  (let [df (D f)
        f-expr (f 'x)
        df-expr (df 'x)
        f-latex (em/emmy->latex f-expr)
        df-latex (em/emmy->latex df-expr)]

    {:function {:expr f-expr
                :latex f-latex
                :python (em/emmy->python f-expr)}
     :derivative {:expr df-expr
                  :latex df-latex
                  :python (em/emmy->python df-expr)}}))

;; ============================================================================
;; REPL Examples
;; ============================================================================

(comment
  ;; === Setup ===
  (mq/init!)

  ;; === Explore Functions ===

  ;; Your original example
  (explore-function #(square (sin (+ % 3))))

  ;; sin(x + π)
  (explore-function #(e/sin (+ % 'pi)))

  ;; Quadratic
  (explore-function eq/expt2)

  ;; Gaussian
  (explore-function eq/expt4)

  ;; === Emmy → LaTeX → Python ===

  ;; Generate LaTeX
  (em/emmy->latex (square (sin (+ 'a 3))))
  ;; => "\\sin^{2}\\left(a + 3\\right)"

  ;; Convert to Python
  (em/emmy->python (square (sin (+ 'a 3))))
  ;; => Python code

  ;; Full pipeline
  (let [expr (square (sin (+ 'x 3)))]
    {:latex (em/emmy->latex expr)
     :python (em/emmy->python expr)})

  ;; === Render Animations ===

  ;; Example 1: sin(x + π) and its derivative
  (example-sin-derivative)
  ;; Video saved to media/videos/

  ;; Example 2: Quadratic and its derivative
  (example-quadratic)

  ;; Example 3: Your chain rule example
  (example-chain-rule)

  ;; Example 4: Taylor series
  (example-taylor-series)

  ;; Example 5: Product rule
  (example-product-rule)

  ;; === Pre-made Scenes ===

  ;; Derivative steps
  (render-emmy-scene "DerivativeSteps")

  ;; Emmy showcase
  (render-emmy-scene "EmmyShowcase")

  ;; Quadratic formula
  (render-emmy-scene "QuadraticFormula")

  ;; === Custom Function and Derivative ===

  ;; Define your own function
  (defn my-func [x]
    (* (exp x) (sin x)))

  ;; Explore it
  (explore-function my-func)

  ;; Animate it
  (create-derivative-animation my-func)

  ;; === Compare Multiple Derivatives ===

  ;; Function and its first 3 derivatives
  (let [f #(e/sin %)
        df1 (D f)
        df2 (D df1)
        df3 (D df2)]
    (map #(em/emmy->latex (% 'x)) [f df1 df2 df3]))
  ;; => ["\\sin\\left(x\\right)"
  ;;     "\\cos\\left(x\\right)"
  ;;     "-\\sin\\left(x\\right)"
  ;;     "-\\cos\\left(x\\right)"]

  ;; === Emmy Simplification → Manim ===

  ;; Simplify then render
  (let [expr '(+ (square (sin x)) (square (cos x)))
        simplified (simplify expr)]
    {:original (em/emmy->latex expr)
     :simplified (em/emmy->latex simplified)})
  ;; Can create a Transform animation between them!
  )
