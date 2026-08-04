(ns desargues.domain.math
  "Abstraction barrier over the symbolic-math engine (Emmy).

   The only domain namespace that requires the engine directly. Callers above
   this barrier depend on these operations, never on emmy.env, so the engine
   can be swapped by rewriting this namespace alone (Stratified Design:
   abstraction barrier + minimal interface)."
  (:require [emmy.env :as e]
            [emmy.expression :as ex]
            [typed.clojure :as t]))

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
  "Evaluate expr at var = val. Returns a plain number when the result is
   numeric, otherwise the simplified symbolic expression. (evaluate-symbolic
   keeps the symbolic form; this concrete path coerces numeric results.)"
  [expr var val]
  (let [result (simplify (substitute expr var val))
        frozen (e/freeze result)]
    (if (number? frozen) frozen result)))

(t/ann ^:no-check differentiate [t/AnyFunction :-> t/AnyFunction])

(t/ann ^:no-check simplify [t/Any :-> t/Any])

(t/ann ^:no-check ->latex [t/Any :-> t/Str])

(t/ann ^:no-check substitute [t/Any t/Any t/Any :-> t/Any])

(t/ann ^:no-check evaluate [t/Any t/Any t/Any :-> t/Any])
