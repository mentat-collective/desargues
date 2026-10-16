(ns desargues.logic.formula
  "Propositional formulas as s-expressions.

   A formula is a connective application (op & args) whose op has a
   desargues.logic.connective spec, the constants true and false, or an ATOM:
   anything else, a sentence letter P or a whole term such as (in x A). Atoms
   are compared by value.

   The SUBFORMULAS of a formula, in post-order without repeats, are the
   columns of its truth table: each one's value depends only on columns
   before it."
  (:require [desargues.logic.connective :as c]))

(defn compound?
  "True when f is a connective application."
  [f]
  (and (seq? f) (c/connective? (first f))))

(defn constant? [f] (boolean? f))

(defn atom?
  "True when f is neither a connective application nor a truth constant."
  [f]
  (not (or (compound? f) (constant? f))))

(defn op [f] (when (compound? f) (first f)))

(defn args [f] (when (compound? f) (vec (rest f))))

(defn subterm
  "The subterm of form at path, a vector of argument indices from the root."
  [form path]
  (reduce (fn [g i] (nth (rest g) i)) form path))

(defn subformulas
  "Every subformula of f, children before parents, each once."
  [f]
  (letfn [(walk [acc g]
            (let [acc (if (compound? g) (reduce walk acc (args g)) acc)]
              (if (some #{g} acc) acc (conj acc g))))]
    (walk [] f)))

(defn atoms
  "The atoms of f in order of first appearance."
  [f]
  (filterv atom? (subformulas f)))

(defn compounds
  "The compound subformulas of f, children before parents."
  [f]
  (filterv compound? (subformulas f)))

(defn evaluate
  "The truth value of f under env, a map atom -> boolean."
  [f env]
  (cond (constant? f) f
        (atom? f) (let [v (get env f ::missing)]
                    (if (= ::missing v)
                      (throw (ex-info "Atom without a value" {:atom f :env env}))
                      v))
        :else (c/value (op f) (mapv #(evaluate % env) (args f)))))

(defn valuations
  "Every env over atoms, in textbook row order."
  [atoms]
  (mapv #(zipmap atoms %) (c/rows (count atoms))))

(defn tautology?
  "True when f holds under every valuation of its atoms."
  [f]
  (every? #(evaluate f %) (valuations (atoms f))))
