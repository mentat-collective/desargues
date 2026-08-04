(ns desargues.specs.api
  "Spec definitions for the desargues public API.
   
   This namespace provides comprehensive specs for the facade API:
   - Expression and function creation
   - Mathematical operations
   - Scene construction
   - Animation workflows
   
   Use with orchestra for instrumentation during development."
  (:require [clojure.spec.alpha :as s]
            [desargues.specs.common :as c]
            [desargues.api :as api]
            [desargues.domain.math-expression :as expr]))

;; =============================================================================
;; Expression/Function Specs
;; =============================================================================

;; Any valid Emmy symbolic form
(s/def ::emmy-form any?)

;; Optional metadata for expressions
(s/def ::metadata (s/map-of keyword? any?))

;; A MathExpression domain entity
(s/def ::math-expression
  #(instance? desargues.domain.math_expression.MathExpression %))

;; A MathFunction domain entity
(s/def ::math-function
  #(instance? desargues.domain.math_expression.MathFunction %))

;; An evaluation Point
(s/def ::point
  #(instance? desargues.domain.math_expression.Point %))

;; Any mathematical object (expression or function)
(s/def ::math-object
  (s/or :expression ::math-expression
        :function ::math-function))

;; Name for a function (symbol or string)
(s/def ::function-name
  (s/or :symbol symbol?
        :string string?))

;; An Emmy-compatible function
(s/def ::emmy-function fn?)

;; Function domain specification
(s/def ::domain (s/nilable (s/coll-of any?)))

;; Function codomain specification
(s/def ::codomain (s/nilable (s/coll-of any?)))

;; =============================================================================
;; Scene Builder Specs
;; =============================================================================

;; A scene builder record
(s/def ::scene-builder map?)

;; A Manim animation object
(s/def ::animation any?)

;; A Manim mobject
(s/def ::mobject any?)

;; =============================================================================
;; LaTeX/Rendering Specs
;; =============================================================================

;; A LaTeX-formatted string
(s/def ::latex-string ::c/latex-string)

;; =============================================================================
;; Evaluation Specs
;; =============================================================================

;; Result of evaluating an expression
(s/def ::evaluation-result
  #(instance? desargues.domain.math_expression.EvaluationResult %))

;; A symbolic variable
(s/def ::variable symbol?)

;; A numeric value for evaluation
(s/def ::numeric-value number?)

;; =============================================================================
;; Table Specs
;; =============================================================================

;; A collection of evaluation points
(s/def ::points-collection (s/coll-of ::point))

;; A collection of mathematical functions
(s/def ::functions-collection (s/coll-of ::math-function))

;; =============================================================================
;; Function Specs (s/fdef)
;; =============================================================================

;; Initialization
(s/fdef api/init!
  :args (s/cat)
  :ret any?)

;; Expression Creation
(s/fdef api/expr
  :args (s/alt :simple (s/cat :form ::emmy-form)
               :with-meta (s/cat :form ::emmy-form
                                 :metadata ::metadata))
  :ret ::math-expression)

(s/fdef api/func
  :args (s/alt :simple (s/cat :name ::function-name
                              :f ::emmy-function)
               :with-domain (s/cat :name ::function-name
                                   :f ::emmy-function
                                   :domain ::domain
                                   :codomain ::codomain))
  :ret ::math-function)

(s/fdef api/point
  :args (s/cat :var ::variable
               :val (s/or :numeric ::numeric-value
                          :symbolic ::emmy-form))
  :ret ::point)

;; Mathematical Operations
(s/fdef api/derivative
  :args (s/cat :math-fn ::math-function)
  :ret ::math-function)

(s/fdef api/evaluate-at
  :args (s/cat :math-obj ::math-object
               :point ::point)
  :ret ::evaluation-result)

(s/fdef api/simplify
  :args (s/cat :math-expr ::math-expression)
  :ret ::math-expression)

(s/fdef api/to-latex
  :args (s/cat :math-obj ::math-object)
  :ret ::latex-string)

;; Scene Construction
(s/fdef api/scene
  :args (s/cat)
  :ret ::scene-builder)

(s/fdef api/show
  :args (s/cat :builder ::scene-builder
               :obj any?)
  :ret ::scene-builder)

(s/fdef api/transform
  :args (s/cat :builder ::scene-builder
               :from any?
               :to any?)
  :ret ::scene-builder)

(s/fdef api/wait
  :args (s/cat :builder ::scene-builder
               :duration ::c/duration)
  :ret ::scene-builder)

(s/fdef api/render!
  :args (s/cat :builder ::scene-builder)
  :ret any?)

;; High-Level Workflows
(s/fdef api/animate-expression
  :args (s/cat :math-expr ::math-expression)
  :ret any?)

(s/fdef api/animate-derivative
  :args (s/cat :math-fn ::math-function)
  :ret any?)

(s/fdef api/animate-transformation
  :args (s/cat :from ::math-expression
               :to ::math-expression)
  :ret any?)

(s/fdef api/animate-simplification
  :args (s/cat :math-expr ::math-expression)
  :ret any?)

;; Evaluation and Tables
(s/fdef api/tabulate
  :args (s/cat :func ::math-function
               :points ::points-collection)
  :ret (s/coll-of ::evaluation-result))

(s/fdef api/compare-functions
  :args (s/cat :functions ::functions-collection
               :points ::points-collection)
  :ret (s/map-of ::function-name (s/coll-of ::evaluation-result)))
