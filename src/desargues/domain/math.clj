(ns desargues.domain.math
  "Abstraction barrier over the symbolic-math engine (Emmy).

   The only domain namespace that requires the engine directly. Callers above
   this barrier depend on these operations, never on emmy.env, so the engine
   can be swapped by rewriting this namespace alone (Stratified Design:
   abstraction barrier + minimal interface)."
  (:require [emmy.env :as e]
            [emmy.expression :as ex]
            [emmy.value :as v]
            [typed.clojure :as t]))

(defn content-key
  "Stable, hashable content key for a symbolic expression: the engine kind
   (:literal for a wrapped engine expression, :form for a bare s-expression)
   paired with its printed form. Two expressions get the same key iff they
   are the same kind and print identically."
  [x]
  [(if (ex/literal? x) :literal :form) (str x)])

(defn numeric?
  "True when x is a plain numeric value in the engine's sense: a JVM number,
   a ratio, or an engine complex number. Symbolic forms are not numeric."
  [x]
  (boolean (v/numerical? x)))

(defn differentiate
  "Derivative of a function (fn -> fn). Emmy differentiates functions."
  [f]
  (e/D f))

(defn simplify
  "Simplify a symbolic expression."
  [x]
  (e/simplify x))

(defn ->latex
  "Render a symbolic expression to a LaTeX string."
  [x]
  (e/->TeX x))

(defn substitute
  "Replace old with new inside symbolic expression expr."
  [expr old new]
  (ex/substitute expr old new))

(defn evaluate
  "Evaluate expr at var = val. Returns the numeric value (integer, double,
   ratio or complex) when the simplified result is numeric, otherwise the
   simplified symbolic expression. (evaluate-symbolic keeps the symbolic
   form; this concrete path coerces numeric results.)"
  [expr var val]
  (let [result (simplify (substitute expr var val))
        inner  (if (ex/literal? result) (ex/expression-of result) result)]
    (if (numeric? inner) inner result)))

(t/ann ^:no-check content-key [t/Any :-> (t/HVec [t/Kw t/Str])])

(t/ann ^:no-check numeric? [t/Any :-> t/Bool])

(t/ann ^:no-check differentiate [t/AnyFunction :-> t/AnyFunction])

(t/ann ^:no-check simplify [t/Any :-> t/Any])

(t/ann ^:no-check ->latex [t/Any :-> t/Str])

(t/ann ^:no-check substitute [t/Any t/Any t/Any :-> t/Any])

(t/ann ^:no-check evaluate [t/Any t/Any t/Any :-> t/Any])
