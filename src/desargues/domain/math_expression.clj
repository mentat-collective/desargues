(ns desargues.domain.math-expression
  "Domain model for mathematical expressions (DDD Entity/Value Object)
   
   This namespace contains the core domain entities and value objects:
   - Value Objects: LaTeX, PythonCode, NumericValue, Point
   - Entities: MathExpression, MathFunction
   
   Type annotations are provided for compile-time checking with Typed Clojure."
  (:require [typed.clojure :as t]
            [desargues.types :as types]
            [desargues.domain.protocols :as p]
            [desargues.domain.math :as m]))

;; ============================================================================
;; Value Objects (Immutable mathematical values)
;; ============================================================================

(t/ann-record LaTeX [content :- types/LaTeXString])

(defrecord LaTeX [content]
  Object
  (toString [_] content))

(t/ann-record PythonCode [content :- types/PythonCodeString])

(defrecord PythonCode [content]
  Object
  (toString [_] content))

(t/ann-record NumericValue [value :- t/Num])

(defrecord NumericValue [value]
  Object
  (toString [_] (str value)))

;; ============================================================================
;; Entity: MathExpression (has identity and behavior)
;; ============================================================================

(t/ann-record MathExpression
              [id :- types/ExpressionId
               expr :- types/EmmyExpr
               latex :- (t/U nil types/LaTeXString)
               python-code :- (t/U nil types/PythonCodeString)
               metadata :- (t/Map t/Kw t/Any)])

(defrecord MathExpression [id expr latex python-code metadata]
  Object
  (toString [_] (str "MathExpression[" id "]: " latex)))

(t/ann create-expression
       (t/IFn [types/EmmyExpr :-> MathExpression]
              [types/EmmyExpr (t/Map t/Kw t/Any) :-> MathExpression]))

(defn create-expression
  "Factory: Create a mathematical expression from Emmy form.
   Id is derived from the expression's canonical printed form so that
   value-equal expressions get value-equal ids (Emmy Literals are = but
   violate the hashCode contract, so we hash the stable string form)."
  ([expr]
   (create-expression expr {}))
  ([expr metadata]
   (->MathExpression
    (symbol (str "expr-" (Integer/toHexString (hash (str expr)))))
    expr
    nil
    nil
    metadata)))

;; ============================================================================
;; Entity: Function (mathematical function with domain)
;; ============================================================================

(t/ann-record MathFunction
              [id :- types/FunctionId
               name :- t/Sym
               f :- t/Any
               domain :- (t/U nil (t/Vec t/Num))
               codomain :- (t/U nil (t/Vec t/Num))
               metadata :- (t/Map t/Kw t/Any)])

(defrecord MathFunction [id name f domain codomain metadata]
  Object
  (toString [_] (str "MathFunction[" name "]")))

(t/ann create-function
       (t/IFn [t/Sym t/Any :-> MathFunction]
              [t/Sym t/Any (t/U nil (t/Vec t/Num)) (t/U nil (t/Vec t/Num)) (t/Map t/Kw t/Any) :-> MathFunction]))

(defn create-function
  "Factory: Create a mathematical function"
  ([name f]
   (create-function name f nil nil {}))
  ([name f domain codomain metadata]
   (->MathFunction
    (symbol (str "func-" (Integer/toHexString (hash [name domain codomain]))))
    name
    f
    domain
    codomain
    metadata)))

;; ============================================================================
;; Value Object: Point (for evaluation)
;; ============================================================================

(t/ann-record Point
              [variable :- types/SymbolicVar
               value :- (t/U t/Num types/EmmyExpr)])

(defrecord Point [variable value]
  Object
  (toString [_] (str variable " = " value)))

(t/ann point [types/SymbolicVar (t/U t/Num types/EmmyExpr) :-> Point])

(defn point [var val]
  (->Point var val))

;; ============================================================================
;; Value Object: Evaluation Result
;; ============================================================================

(t/ann-record EvaluationResult
              [input :- t/Any
               output :- t/Any
               expression :- (t/U nil MathExpression)
               steps :- (t/Vec t/Any)])

(defrecord EvaluationResult [input output expression steps]
  Object
  (toString [_] (str input " → " output)))

(t/ann evaluation-result
       (t/IFn [t/Any t/Any :-> EvaluationResult]
              [t/Any t/Any (t/U nil MathExpression) (t/Vec t/Any) :-> EvaluationResult]))

(defn evaluation-result
  ([input output]
   (evaluation-result input output nil []))
  ([input output expr steps]
   (->EvaluationResult input output expr steps)))

(extend-protocol p/ILatexConvertible
  MathExpression
  (to-latex [this]
    (m/->latex (:expr this))))

(extend-protocol p/ISimplifiable
  MathExpression
  (simplify [this]
    (create-expression
     (m/simplify (:expr this))
     (assoc (:metadata this) :simplified-from (:id this)))))

(extend-protocol p/IEvaluable
  MathExpression
  (evaluate [this point]
    (m/evaluate (:expr this) (:variable point) (:value point)))
  (evaluate-symbolic [this point]
    (m/substitute (:expr this) (:variable point) (:value point)))
  MathFunction
  (evaluate [this point]
    ((:f this) (:value point)))
  (evaluate-symbolic [this point]
    ((:f this) (:value point))))

(extend-protocol p/IDifferentiable
  MathFunction
  (derivative [this]
    (create-function
     (symbol (str (:name this) "'"))
     (m/differentiate (:f this))
     (:domain this) (:codomain this)
     (assoc (:metadata this) :derived-from (:id this))))
  (derivative-at [this point]
    ((m/differentiate (:f this)) (:value point)))
  (nth-derivative [this n]
    (create-function
     (symbol (str (:name this) "^(" n ")"))
     (reduce (fn [g _] (m/differentiate g)) (:f this) (range n))
     (:domain this) (:codomain this)
     (assoc (:metadata this) :derived-from (:id this)))))
