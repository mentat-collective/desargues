(ns desargues.pipeline.emmy
  "The Emmy -> LaTeX -> Python -> Manim conveyor, stratified CPPB:

     Collect  : fix the symbolic facts about a domain function or expression
                (its image at a free variable, its derivative). Pure.
     Promote  : lift ONE symbolic fact into ONE rendering value object
                (LaTeX / PythonCode). Pure; each promoter is independent of
                the others and of the boundary.
     Pipeline : compose promoters into the render SPEC a scene needs. Pure
                data in, pure data out.
     Boundary : hand a spec to Python/Manim. Lives in desargues.emmy-manim
                (latex->python, render-*) and the infrastructure adapter,
                never here.

   Nothing in this namespace performs an effect, so every stage is testable
   without Python. Conversions that need Python (LaTeX -> Python via latex2py)
   are INJECTED as plain string functions, keeping the promoter pure and the
   boundary swappable."
  (:require [desargues.domain.math :as m]
            [desargues.domain.math-expression :as expr]))

;; ============================================================================
;; Collect
;; ============================================================================

(defn collect-expression
  "Collect a bare symbolic expression."
  [x]
  {:expr x})

(defn collect-function
  "Collect the symbolic facts of function f at free variable var (default 'x):
   its image and the image of its derivative."
  ([f] (collect-function f 'x))
  ([f var]
   {:f f
    :var var
    :expr (f var)
    :derivative-expr ((m/differentiate f) var)}))

;; ============================================================================
;; Promote (one fact -> one value object)
;; ============================================================================

(defn promote-latex
  "Symbolic expression -> LaTeX value object."
  [x]
  (expr/->LaTeX (m/->latex x)))

(defn promote-python
  "LaTeX value object -> PythonCode value object through an injected
   converter (LaTeX string -> Python string). The converter is the boundary's
   concern (latex2py runs in Python); injecting it keeps this stage pure."
  [latex converter]
  (expr/->PythonCode (converter (:content latex))))

;; ============================================================================
;; Pipeline (compose promoters into render specs)
;; ============================================================================

(defn expression-spec
  "Spec for rendering one expression: {:expr x :latex \"...\"}."
  [x]
  (-> (collect-expression x)
      (assoc :latex (:content (promote-latex x)))))

(defn derivative-spec
  "Spec for a function-and-derivative scene:
   {:func-latex \"...\" :deriv-latex \"...\"} (plus the collected facts)."
  ([f] (derivative-spec f 'x))
  ([f var]
   (let [{:keys [expr derivative-expr] :as facts} (collect-function f var)]
     (assoc facts
            :func-latex  (:content (promote-latex expr))
            :deriv-latex (:content (promote-latex derivative-expr))))))

(defn python-code
  "Expression -> Python source string via LaTeX, with the LaTeX->Python
   converter injected."
  [x converter]
  (-> x promote-latex (promote-python converter) :content))
