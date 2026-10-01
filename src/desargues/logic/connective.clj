(ns desargues.logic.connective
  "Truth-functional connectives, each defined by its ESSENTIAL CASE: the one
   pattern of argument values that decides the connective.

     and      true  only when every part is true
     or       false only when every part is false
     implies  false only when T => F
     iff      true  only when both sides agree
     not      flips its part

   `spec` is OPEN on the connective symbol. A spec is

     {:arity        1 | 2 | :n
      :essential?   (fn [arg-values]) -> true in the essential case
      :essential    the connective's value in the essential case
      :value        (optional) (fn [arg-values]) when no essential case exists
      :shortcut     the essential case in words}

   `value` is derived from the essential case, `rows` from `value`; nothing
   else in desargues.logic evaluates a connective.")

(defmulti spec
  "Connective symbol -> its spec map, or nil when the symbol is not a connective."
  identity)

(defmethod spec :default [_] nil)

(defmethod spec 'not [_]
  {:arity 1
   :value (fn [[a]] (not a))
   :shortcut "¬ flips the value"})

(defmethod spec 'and [_]
  {:arity :n
   :essential? (fn [vs] (every? true? vs))
   :essential true
   :shortcut "∧ is true only when every part is true"})

(defmethod spec 'or [_]
  {:arity :n
   :essential? (fn [vs] (every? false? vs))
   :essential false
   :shortcut "∨ is false only when every part is false"})

(defmethod spec 'implies [_]
  {:arity 2
   :essential? (fn [[a b]] (and (true? a) (false? b)))
   :essential false
   :shortcut "⇒ is false only when T ⇒ F"})

(defmethod spec 'iff [_]
  {:arity 2
   :essential? (fn [[a b]] (= a b))
   :essential true
   :shortcut "⇔ is true only when both sides agree"})

(defn connective?
  "True when symbol op names a registered connective."
  [op]
  (some? (spec op)))

(defn connectives
  "Every registered connective symbol."
  []
  (sort-by str (remove #{:default} (keys (methods spec)))))

(defn essential?
  "True when arg-values are op's essential case (false for a connective that
   has none)."
  [op arg-values]
  (if-let [e? (:essential? (spec op))]
    (boolean (e? arg-values))
    false))

(defn value
  "The truth value of connective op at arg-values."
  [op arg-values]
  (let [{f :value e? :essential? v :essential} (spec op)]
    (cond f (boolean (f arg-values))
          e? (if (e? arg-values) v (not v))
          :else (throw (ex-info (str "Not a connective: " op) {:op op})))))

(defn rows
  "Every assignment of n truth values, in textbook order: the first position
   changes slowest and each position starts at true."
  [n]
  (if (zero? n)
    [[]]
    (vec (for [a [true false] more (rows (dec n))] (into [a] more)))))

(defn rows-giving
  "The argument rows of an n-place op whose value is v."
  [op n v]
  (filterv #(= v (value op %)) (rows n)))
