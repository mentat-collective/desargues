(ns desargues.logic.relational
  "Truth tables as a relation, with core.logic.

   Each connective becomes the relation between its argument values and its
   value, read off its rows (desargues.logic.connective). A formula is then a
   conjunction of those relations over one logic variable per subformula, and
   the same program runs in any direction:

     (solutions f true)    every valuation of f's atoms making f true
     (solutions f false)   every counterexample
     (solutions f nil)     the whole table"
  (:require [clojure.core.logic :as l]
            [desargues.logic.connective :as c]
            [desargues.logic.formula :as f]))

(defn connectiveo
  "Goal: v is the value of connective op at arg-vars."
  [op arg-vars v]
  (let [n (count arg-vars)
        rows (for [b [true false] row (c/rows-giving op n b)] (conj row b))]
    (l/membero (conj (vec arg-vars) v) rows)))

(defn- formulao
  "Goal relating every subformula of formula to its variable in vars."
  [formula vars]
  (l/and*
   (for [g (f/compounds formula)]
     (connectiveo (f/op g)
                  (mapv #(if (f/constant? %) % (vars %)) (f/args g))
                  (vars g)))))

(defn solutions
  "Every valuation of formula's atoms (as envs) under which formula has value
   v; v nil leaves the value free."
  [formula v]
  (let [subs (filterv (complement f/constant?) (f/subformulas formula))
        vars (zipmap subs (repeatedly l/lvar))
        atoms (f/atoms formula)]
    (vec (distinct
          (l/run* [q]
            (formulao formula vars)
            (if (some? v) (l/== (vars formula) v) l/succeed)
            (l/== q (mapv vars atoms))
            (l/everyg #(l/membero % [true false]) (mapv vars atoms)))))))

(defn counterexamples
  "Every env falsifying formula."
  [formula]
  (let [atoms (f/atoms formula)]
    (mapv #(zipmap atoms %) (solutions formula false))))
