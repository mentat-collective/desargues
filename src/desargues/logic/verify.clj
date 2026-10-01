(ns desargues.logic.verify
  "Checking a statement, behind a port.

   A Checker answers one question about a formula (a propositional formula or
   a class statement, read through desargues.logic.classes): is it valid?

     {:verdict :proved | :refuted | :unknown
      :by      the checker's id
      :counterexample env      ; when :refuted
      :certificate  data}      ; checker-specific evidence

   `truth-table` is the decision procedure in this library. The Lean kernel
   (ansatz) adapter lives under the :ansatz alias, in
   desargues.logic.verify.ansatz, so the library itself never needs it."
  (:require [desargues.logic.classes :as cl]
            [desargues.logic.formula :as f]
            [desargues.logic.refute :as rf]
            [desargues.logic.table :as t]))

(defprotocol Checker
  (-id [this] "A keyword naming the checker.")
  (-check [this formula] "The verdict map for a propositional formula."))

(defn propositional
  "statement -> the propositional formula it means (class statements are read
   at one element)."
  [statement]
  (cl/membership statement))

(defn check
  "The verdict of checker on statement."
  [checker statement]
  (assoc (-check checker (propositional statement)) :by (-id checker)))

(def truth-table
  "Decides a formula by its truth table; the certificate is the refutation
   tree (every branch closed)."
  (reify Checker
    (-id [_] :truth-table)
    (-check [_ formula]
      (if (= :tautology (:verdict (t/table formula)))
        {:verdict :proved :certificate (rf/refute formula)}
        {:verdict :refuted :counterexample (first (t/counterexamples (t/table formula)))}))))

(defn agree
  "Check statement with every checker; :verdict is shared when they all
   agree and :disagreement otherwise. :results holds each checker's answer."
  [checkers statement]
  (let [results (mapv #(check % statement) checkers)
        verdicts (set (map :verdict (remove #(= :unknown (:verdict %)) results)))]
    {:verdict (case (count verdicts) 0 :unknown 1 (first verdicts) :disagreement)
     :results results}))

(defn decidable?
  "True when statement reduces to a propositional formula all of whose atoms
   are sentence letters or memberships of an element in a class letter."
  [statement]
  (let [g (propositional statement)]
    (every? (fn [a] (or (symbol? a)
                        (and (seq? a) (= 'in (first a)) (= 3 (count a))
                             (symbol? (nth a 1)) (symbol? (nth a 2)))))
            (f/atoms g))))
