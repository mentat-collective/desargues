(ns desargues.logic.table
  "Truth tables as dynamic programming.

   The columns are the formula's atoms, then its compound subformulas children
   first, so each cell is computed from cells already filled in its row by one
   question: is this the connective's essential case?

   `table` is the whole filled table; `cells` is the fill, column by column,
   each cell carrying the child values it read and whether they were the
   essential case."
  (:require [desargues.logic.connective :as c]
            [desargues.logic.formula :as f]))

(defn- read-arg [values a] (if (f/constant? a) a (get values a)))

(defn- fill-row
  "Row env extended with every compound column's value, children first."
  [columns env]
  (reduce (fn [vals g]
            (assoc vals g (c/value (f/op g) (mapv #(read-arg vals %) (f/args g)))))
          env
          columns))

(defn table
  "The truth table of formula:
     {:formula :atoms :columns :rows [{:env :values}] :verdict}
   where :columns are the compound subformulas and :verdict is :tautology,
   :contradiction or :contingent."
  [formula]
  (let [atoms (f/atoms formula)
        columns (f/compounds formula)
        rows (mapv (fn [env] {:env env :values (fill-row columns env)})
                   (f/valuations atoms))
        final (mapv #(f/evaluate formula (:env %)) rows)]
    {:formula formula
     :atoms atoms
     :columns columns
     :rows rows
     :verdict (cond (every? true? final) :tautology
                    (every? false? final) :contradiction
                    :else :contingent)}))

(defn cells
  "The compound cells of table in fill order (column by column, rows top to
   bottom): {:column :col :row :value :args :essential?}. :col is the index
   among compound columns, :args the child values read."
  [{:keys [columns rows]}]
  (vec (for [[j g] (map-indexed vector columns)
             [i {:keys [values]}] (map-indexed vector rows)]
         (let [vs (mapv #(read-arg values %) (f/args g))]
           {:column g :col j :row i
            :value (get values g)
            :args vs
            :essential? (c/essential? (f/op g) vs)}))))

(defn counterexamples
  "The rows (as envs) where formula is false."
  [{:keys [formula rows]}]
  (vec (for [{:keys [env]} rows :when (false? (f/evaluate formula env))] env)))
