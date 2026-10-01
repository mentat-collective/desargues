(ns desargues.logic.scene.derive-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [desargues.logic.classes :as cl]
            [desargues.logic.refute :as r]
            [desargues.logic.scene.beats :as b]
            [desargues.logic.scene.derive :as sd]
            [desargues.tex.label :as tl]))

(def de-morgan '(= (compl (union A B)) (inter (compl A) (compl B))))
(def sym-diff '(= (sym-diff A B) (diff (union A B) (inter A B))))

(defn- flat [beats] (mapcat #(if (= :par (:beat %)) (flat (:beats %)) [%]) beats))

(defn- line? [id] (and (vector? id) (#{:line :line-hl} (first id))))

(defn- max-lines-on-screen
  [beats]
  (:max (reduce (fn [{:keys [on] :as acc} bt]
                  (let [on (-> (apply disj on (:ids bt))
                               (into (filter line?) (map :id (:items bt))))]
                    (-> acc (assoc :on on) (update :max max (count on)))))
                {:on #{} :max 0}
                (flat beats))))

(deftest de-morgan-chain
  (let [ch (sd/chain de-morgan)]
    (is (= de-morgan (:formula (first ch))))
    (is (= (cl/membership de-morgan) (:formula (peek ch))))
    (is (r/tautology? (:formula (peek ch))))
    (is (= [nil "\\text{extent}" "\\text{def. of }X'" "\\text{def. of }X \\cup Y"
            "\\text{def. of }X \\cap Y" "\\text{def. of }X'" "\\text{def. of }X'"]
           (map :label ch)))))

(deftest de-morgan-beats
  (let [bs (sd/beats de-morgan)
        its (b/items bs)
        hls (filter #(= :line-hl (first (:id %))) (filter #(vector? (:id %)) its))]
    (testing "every step highlights the subterm about to change"
      (is (= 6 (count hls)))
      (is (every? #(str/includes? (:latex %) "\\textcolor{gold}") hls)))
    (testing "the derivation names its definitions at the right margin"
      (is (= 6 (count (filter #(= :rule (first (:id %))) (filter #(vector? (:id %)) its)))))
      (is (every? #(< 4 (first (:at %))) (filter #(= :rule (first (:id %))) (filter #(vector? (:id %)) its)))))
    (is (<= (max-lines-on-screen bs) 6))
    (is (= "a propositional formula: decide it by a truth table"
           (:text (last (filter #(= :caption (:id %)) its)))))
    (is (every? b/inside? (b/footprints bs)))))

(deftest long-chains-scroll
  (let [bs (sd/beats sym-diff :handoff :refutation)]
    (is (< 6 (count (sd/chain sym-diff))))
    (is (<= (max-lines-on-screen bs) 6))
    (is (some #(= :move (:beat %)) (flat bs)))
    (is (every? b/inside? (b/footprints bs)))
    (is (str/ends-with? (:text (last (filter #(= :caption (:id %)) (b/items bs)))) "a refutation"))))

(deftest recording-works
  (let [g (sd/record "de-morgan" de-morgan)
        texs (filter tl/tex-node? (vals (:nodes g)))]
    (is (= "de-morgan" (:scene g)))
    (is (seq (:steps g)))
    (is (some #(str/includes? (tl/node-latex %) "\\textcolor{gold}") texs))))
