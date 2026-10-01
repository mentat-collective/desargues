(ns desargues.logic.scene.venn-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.logic.scene.venn :as v]
            [desargues.scene.data :as rec]
            [desargues.tex.label :as label]
            [desargues.logic.scene.beats :as b]))

(def de-morgan '(= (compl (union A B)) (inter (compl A) (compl B))))
(def distributive '(= (inter A (union B C)) (union (inter A B) (inter A C))))
(def diff-swap '(= (diff A B) (diff B A)))

(deftest regions-are-letter-subsets
  (is (= #{#{} #{'A} #{'B} #{'A 'B}} (set (v/regions '[A B]))))
  (is (= 8 (count (set (v/regions '[A B C]))))))

(deftest de-morgan-sides-denote-the-same-regions
  (let [{:keys [letters lhs rhs verdict]} (v/analyse de-morgan)]
    (is (= '[A B] letters))
    (is (= #{#{}} lhs rhs))
    (is (:holds? verdict))
    (is (empty? (:offending verdict)))))

(deftest distributive-law-holds-on-three-circles
  (let [{:keys [lhs rhs verdict]} (v/analyse distributive)]
    (is (= #{#{'A 'B} #{'A 'C} #{'A 'B 'C}} lhs rhs))
    (is (:holds? verdict))))

(deftest symmetric-difference
  (is (= #{#{'A} #{'B}} (v/denotes '(sym-diff A B) '[A B]))))

(deftest a-minus-b-differs-from-b-minus-a
  (let [{:keys [lhs rhs verdict]} (v/analyse diff-swap)]
    (is (= #{#{'A}} lhs))
    (is (= #{#{'B}} rhs))
    (is (not (:holds? verdict)))
    (is (= #{#{'A} #{'B}} (:offending verdict)))))

(deftest inclusion-verdict
  (is (:holds? (:verdict (v/analyse '(subset (inter A B) A)))))
  (is (= #{#{'B}} (:offending (:verdict (v/analyse '(subset B (inter A B))))))))

(deftest shading-dots-lie-in-their-region
  (let [d (v/diagram '[A B C] (:main v/frames))
        sh (v/shading d #{#{'A} #{'A 'B 'C}})]
    (is (every? seq (vals sh)))
    (doseq [[r ps] sh p ps]
      (is (= r (v/region-at d p))))))

(deftest every-position-inside-the-world
  (doseq [st [de-morgan distributive diff-swap '(= (compl (compl A)) A)
              '(= (compl universe) empty)]]
    (testing (pr-str st)
      (is (every? b/inside? (b/footprints (v/beats st)))))))

(deftest beats-are-data-in-flow-order
  (let [bs (v/beats diff-swap)]
    (is (= :diagram (:beat (first bs))))
    (is (some #(= :recolor (:beat %)) bs))
    (is (not-any? #(= :recolor (:beat %)) (v/beats de-morgan)))
    (is (= "the regions differ: the identity fails"
           (:text (first (filter #(= :verdict (:id %)) (b/items bs))))))))

(deftest recording-works
  (let [g (v/record :de-morgan de-morgan)]
    (is (= :de-morgan (:scene g)))
    (is (pos? (rec/animation-count g)))
    (is (some #(= :circle (:node %)) (vals (:nodes g))))
    (is (some label/tex-node? (vals (:nodes g))))
    (is (contains? (rec/colors-used g) :gold))
    (is (contains? (rec/colors-used g) :teal)))
  (let [g (v/record :diff-swap diff-swap)
        anims (mapcat #(cons % (:children %)) (mapcat :anims (rec/play-steps g)))]
    (is (some #(and (= :recolor (:anim %)) (= :red (:color %))) anims))))
