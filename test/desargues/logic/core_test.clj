(ns desargues.logic.core-test
  "The logic core: connectives by their essential case, formulas, tables,
   refutation, the relational table, class statements and TeX; plus
   properties tying the three deciders together over random formulas."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.logic.classes :as cl]
            [desargues.logic.connective :as c]
            [desargues.logic.formula :as f]
            [desargues.logic.refute :as rf]
            [desargues.logic.relational :as rel]
            [desargues.logic.table :as tb]
            [desargues.logic.tex :as lt]))

(def ^:private T true)
(def ^:private F false)

(deftest essential-cases
  (testing "and is true only when every part is true"
    (is (c/essential? 'and [T T]))
    (is (= [[T T]] (c/rows-giving 'and 2 true)))
    (is (= [[T T T]] (c/rows-giving 'and 3 true))))
  (testing "or is false only when every part is false"
    (is (c/essential? 'or [F F]))
    (is (= [[F F]] (c/rows-giving 'or 2 false))))
  (testing "implies is false only at T => F"
    (is (c/essential? 'implies [T F]))
    (is (= [[T F]] (c/rows-giving 'implies 2 false))))
  (testing "iff is true only when both sides agree"
    (is (= [[T T] [F F]] (c/rows-giving 'iff 2 true))))
  (testing "not flips and has no essential case"
    (is (= [F T] (mapv #(c/value 'not [%]) [T F])))
    (is (not-any? #(c/essential? 'not %) (c/rows 1))))
  (testing "value is derived: the essential value exactly in the essential case"
    (doseq [op '[and or implies iff]
            row (c/rows 2)
            :let [{v :essential} (c/spec op)]]
      (is (= (c/value op row) (if (c/essential? op row) v (not v)))
          (str op " " row))))
  (testing "every connective carries a shortcut for narration"
    (is (= '#{and iff implies not or} (set (c/connectives))))
    (is (every? (comp string? :shortcut c/spec) (c/connectives))))
  (testing "rows are in textbook order"
    (is (= [[T T] [T F] [F T] [F F]] (c/rows 2)))))

(deftest formulas
  (let [g '(implies (and P (implies P Q)) Q)]
    (is (= '[P Q (implies P Q) (and P (implies P Q)) (implies (and P (implies P Q)) Q)]
           (f/subformulas g)))
    (is (= '[P Q] (f/atoms g)))
    (is (= '[(implies P Q) (and P (implies P Q)) (implies (and P (implies P Q)) Q)]
           (f/compounds g)))
    (is (= 'Q (f/subterm g [1])))
    (is (= '(implies P Q) (f/subterm g [0 1]))))
  (testing "membership terms are atoms compared by value"
    (is (= '[(in x A) (in x B)] (f/atoms '(or (in x A) (not (in x B)) (in x A))))))
  (is (false? (f/evaluate '(implies P Q) '{P true Q false})))
  (is (thrown? clojure.lang.ExceptionInfo (f/evaluate 'P {}))))

(deftest table-verdicts
  (is (= :tautology (:verdict (tb/table '(or P (not P))))))
  (is (= :contradiction (:verdict (tb/table '(and P (not P))))))
  (is (= :contingent (:verdict (tb/table '(implies P Q)))))
  (is (= :tautology (:verdict (tb/table '(implies (and P (implies P Q)) Q)))))
  (testing "cells: essential exactly where the connective's essential case is read"
    (let [cells (tb/cells (tb/table '(implies P Q)))]
      (is (= [false true false false] (mapv :essential? cells)))
      (is (= [true false true true] (mapv :value cells)))))
  (is (= '[{P false Q true}] (tb/counterexamples (tb/table '(implies (or P Q) P))))))

(deftest refutation
  (testing "modus ponens closes without branching"
    (let [tree (rf/refute '(implies (and P (implies P Q)) Q))]
      (is (= :closed (:status tree)))
      (is (= :assume (:kind (first (:steps tree)))))
      (is (= :clash (:kind (peek (:steps tree)))))
      (is (rf/tautology? '(implies (and P (implies P Q)) Q)))))
  (testing "(P or Q) => P has the counterexample P = F, Q = T"
    (is (= '{P false Q true} (rf/counterexample '(implies (or P Q) P))))
    (is (not (rf/tautology? '(implies (or P Q) P))))))

(deftest relational-table
  (doseq [g '[(implies (or P Q) P) (iff P Q) (or P (not P)) (and P Q R)]]
    (is (= (set (tb/counterexamples (tb/table g))) (set (rel/counterexamples g))) (str g)))
  (testing "the same program runs forward to the whole table"
    (is (= 4 (count (rel/solutions '(iff P Q) nil))))
    (is (= 2 (count (rel/solutions '(iff P Q) true))))))

(defn- identity? [s] (f/tautology? (cl/membership s)))

(deftest class-statements
  (is (= '(iff (not (or (in x A) (in x B))) (and (not (in x A)) (not (in x B))))
         (cl/membership '(= (compl (union A B)) (inter (compl A) (compl B))))))
  (is (identity? '(= (compl (union A B)) (inter (compl A) (compl B)))) "De Morgan")
  (is (identity? '(= (inter A (union B C)) (union (inter A B) (inter A C)))) "distributive")
  (is (identity? '(= (sym-diff (sym-diff A B) C) (sym-diff A (sym-diff B C))))
      "symmetric difference is associative")
  (is (identity? '(= (sym-diff A empty) A)) "A + empty = A")
  (is (not (identity? '(= (diff A B) (diff B A)))) "A - B differs from B - A")
  (is (identity? '(subset (inter A B) A)))
  (testing "unfold-steps record the operator and the path rewritten"
    (let [steps (cl/unfold-steps (cl/statement '(= (union A B) (union B A))))]
      (is (= '[union union nil] (mapv :rule steps)))
      (is (= [[0] [1] nil] (mapv :path steps))))))

(deftest tex-strings
  (is (= "P \\Rightarrow Q" (lt/->TeX '(implies P Q))))
  (is (= "\\lnot P" (lt/->TeX '(not P))))
  (is (= "\\lnot \\left(P \\lor Q\\right) \\Leftrightarrow \\lnot P \\land \\lnot Q"
         (lt/->TeX '(iff (not (or P Q)) (and (not P) (not Q))))))
  (is (= "\\textcolor{gold}{P \\land Q} \\lor R"
         (lt/->TeX (lt/highlight '(or (and P Q) R) [0] :gold))))
  (is (= "x \\in A \\cup B" (lt/->TeX '(in x (union A B)))))
  (is (= "x \\notin A" (lt/->TeX '(not (in x A))))))

(def ^:private gen-formula
  "Random formulas over P Q R and the five connectives, depth at most 4."
  (letfn [(g [depth]
            (if (zero? depth)
              (gen/elements '[P Q R])
              (let [sub (g (dec depth))]
                (gen/frequency
                 [[1 (gen/elements '[P Q R])]
                  [1 (gen/fmap #(list 'not %) sub)]
                  [3 (gen/fmap (fn [[op a b]] (list op a b))
                               (gen/tuple (gen/elements '[and or implies iff]) sub sub))]]))))]
    (g 4)))

(defspec deciders-agree 200
  (prop/for-all [g gen-formula]
    (let [v (= :tautology (:verdict (tb/table g)))]
      (= v (f/tautology? g) (rf/tautology? g)))))

(defspec relational-counterexamples-are-the-table's 200
  (prop/for-all [g gen-formula]
    (= (set (tb/counterexamples (tb/table g))) (set (rel/counterexamples g)))))

(defspec refute-counterexample-falsifies 200
  (prop/for-all [g gen-formula]
    (if-let [env (rf/counterexample g)]
      (false? (f/evaluate g env))
      (f/tautology? g))))
