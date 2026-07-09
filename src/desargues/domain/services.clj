(ns desargues.domain.services
  "Domain services (operations not belonging to specific entities)"
  (:require [desargues.domain.protocols :as p]
            [desargues.domain.math-expression :as expr]
            [typed.clojure :as t]))

;; ============================================================================
;; Mathematical Operations Service
;; ============================================================================

(defn differentiate-expression
  "Service: derivative of a differentiable object. Emmy differentiates
   functions, so pass a MathFunction; a bare symbolic expression is not
   differentiable on its own."
  [math-obj]
  (p/derivative math-obj))

(defn evaluate-expression
  "Service: Evaluate a math object at a point"
  [math-obj point]
  (expr/evaluation-result point (p/evaluate math-obj point) math-obj []))

(defn simplify-expression
  "Service: Simplify an expression"
  [math-expr]
  (p/simplify math-expr))

;; ============================================================================
;; Conversion Service (Bridge between Emmy and representations)
;; ============================================================================

(defn expression-to-latex
  "Service: Convert expression to a LaTeX value object"
  [math-expr]
  (expr/->LaTeX (p/to-latex math-expr)))

(defn expression-to-python
  "Service: Convert expression to Python (requires latex2py integration)"
  [math-expr latex-converter]
  ;; Inject dependency on latex converter (Dependency Inversion)
  (let [latex (expression-to-latex math-expr)]
    (expr/->PythonCode (latex-converter (:content latex)))))

;; ============================================================================
;; Function Operations Service
;; ============================================================================

(defn apply-function
  "Service: Apply a mathematical function to an argument"
  [math-func arg]
  (let [result ((:f math-func) arg)]
    (expr/create-expression
     result
     {:function (:name math-func)
      :argument arg})))

(defn compose-functions
  "Service: Compose two mathematical functions (f ∘ g)"
  [f g]
  (let [composed (comp (:f f) (:f g))
        name (str (:name f) "∘" (:name g))]
    (expr/create-function name composed)))

;; ============================================================================
;; Comparison Service
;; ============================================================================

(defn compare-at-points
  "Service: Evaluate multiple functions at same points"
  [functions points]
  (for [point points]
    {:point point
     :results (for [func functions]
                {:function (:name func)
                 :value (apply-function func (:value point))})}))

;; ============================================================================
;; Tabulation Service
;; ============================================================================

(defn tabulate-function
  "Service: Create a table of function values"
  [math-func points]
  {:function math-func
   :points points
   :values (map (fn [point]
                  {:point point
                   :value (apply-function math-func (:value point))})
                points)})

;; ============================================================================
;; Transformation Service
;; ============================================================================

(defn transform-expression
  "Service: Apply a transformation to an expression"
  [math-expr transformation]
  (let [transformed (transformation (:expr math-expr))]
    (expr/create-expression
     transformed
     (assoc (:metadata math-expr)
            :transformation transformation
            :source (:id math-expr)))))

(t/ann ^:no-check differentiate-expression [t/Any :-> t/Any])

(t/ann ^:no-check evaluate-expression [t/Any t/Any :-> t/Any])

(t/ann ^:no-check simplify-expression [t/Any :-> t/Any])

(t/ann ^:no-check expression-to-latex [t/Any :-> t/Any])

(t/ann ^:no-check expression-to-python [t/Any t/AnyFunction :-> t/Any])

(t/ann ^:no-check apply-function [t/Any t/Any :-> t/Any])

(t/ann ^:no-check compose-functions [t/Any t/Any :-> t/Any])

(t/ann ^:no-check compare-at-points [t/Any t/Any :-> t/Any])

(t/ann ^:no-check tabulate-function [t/Any t/Any :-> t/Any])

(t/ann ^:no-check transform-expression [t/Any t/Any :-> t/Any])
