(ns desargues.logic.proof-test
  (:require [clojure.test.check.generators :as gen]
            [desargues.logic.formula :as f]
            [desargues.logic.proof :as p]
            [desargues.logic.proof.library :as lib]
            [hive-dsl.result :as r]
            [hive-test.trifecta :refer [deftrifecta]]))

;; ---------------------------------------------------------------------------
;; Fixtures

(def library
  (lib/library '{"1.1.5a" (implies (and (implies P Q) (implies Q R)) (implies P R))}))

(def transitivity
  "A <= B and B <= C give A <= C, by element chasing (Pinter 1.11 v)."
  '{:goal (implies (and (subset A B) (subset B C)) (subset A C))
    :lines [{:claim (subset A B) :by :hyp}
            {:claim (subset B C) :by :hyp}
            {:claim (forall [x] (implies (in x A) (in x B))) :by :def :from 0}
            {:claim (forall [y] (implies (in y B) (in y C))) :by :def :from 1}
            {:claim (implies (in x A) (in x B)) :by :inst :from 2 :with {x x}}
            {:claim (implies (in x B) (in x C)) :by :inst :from 3 :with {y x}}
            {:claim (implies (in x A) (in x C)) :by :use :result "1.1.5a"
             :with {P (in x A) Q (in x B) R (in x C)} :from [4 5]}
            {:claim (forall [x] (implies (in x A) (in x C))) :by :gen :from 6 :vars [x]}
            {:claim (subset A C) :by :def :from 7}
            {:claim (implies (and (subset A B) (subset B C)) (subset A C)) :by :taut :from [8]}]})

(def cases
  {:transitivity transitivity
   :wrong-substitution (assoc-in transitivity [:lines 6 :with] '{P (in x A) Q (in x C) R (in x B)})
   :bad-taut (assoc-in transitivity [:lines 6] '{:claim (implies (in x C) (in x A)) :by :taut :from [4 5]})
   :wrong-definition (assoc-in transitivity [:lines 2 :claim] '(forall [x] (implies (in x B) (in x A))))
   :captured-gen '{:goal (implies (in x A) (forall [x] (in x A)))
                   :lines [{:claim (in x A) :by :hyp}
                           {:claim (forall [x] (in x A)) :by :gen :from 0 :vars [x]}
                           {:claim (implies (in x A) (forall [x] (in x A))) :by :taut :from [1]}]}
   :not-the-goal (update transitivity :lines pop)
   :malformed '{:goal P :lines []}})

;; ---------------------------------------------------------------------------
;; Subjects

(defn verdict
  "The checkable summary of checking proof against the test library."
  [proof]
  (let [res (p/check proof library)]
    (if (r/ok? res)
      (let [{:keys [ok? concludes? weakest lines]} (:ok res)]
        {:ok? ok? :concludes? concludes? :weakest weakest
         :failing (vec (keep #(when-not (:ok? %) (:index %)) lines))})
      {:malformed (:error res)})))

(def formula-gen
  (gen/recursive-gen
   (fn [inner]
     (gen/one-of [(gen/fmap #(list 'not %) inner)
                  (gen/fmap #(apply list %) (gen/tuple (gen/elements '[and or implies iff]) inner inner))]))
   (gen/elements '[P Q R])))

(defn taut-step
  "Check the two-line proof phi |- psi by :taut; return the checker's answer
   next to the truth table's."
  [[phi psi]]
  (let [goal (list 'implies phi psi)
        proof {:goal goal
               :lines [{:claim phi :by :hyp}
                       {:claim psi :by :taut :from [0]}
                       {:claim goal :by :taut :from [1]}]}
        line (get-in (p/check proof) [:ok :lines 1])]
    {:accepted? (:ok? line) :tautology? (f/tautology? goal)}))

;; ---------------------------------------------------------------------------
;; Trifectas

(deftrifecta proof-verdicts
  desargues.logic.proof-test/verdict
  {:golden-path "test/golden/desargues/logic/proof-verdicts.edn"
   :cases cases
   :gen (gen/elements (vals cases))
   :pred map?
   :mutations [["always-accepts" (fn [_] {:ok? true :concludes? true :weakest :evidence/given :failing []})]
               ["ignores-library" (fn [proof] (let [{:keys [ok?] :as v} (verdict proof)]
                                                (if (some? ok?) (assoc v :ok? (:concludes? v) :failing []) v)))]
               ["accepts-any-gen" (fn [proof] (let [v (verdict proof)]
                                                (cond-> v (= [1] (:failing v)) (assoc :ok? true :failing []))))]]})

(deftrifecta taut-step-agrees-with-truth-table
  desargues.logic.proof-test/taut-step
  {:gen (gen/tuple formula-gen formula-gen)
   :pred #(= (:accepted? %) (:tautology? %))
   :num-tests 300})
