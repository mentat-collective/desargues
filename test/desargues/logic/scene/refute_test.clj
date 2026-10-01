(ns desargues.logic.scene.refute-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [desargues.logic.scene.beats :as b]
            [desargues.logic.scene.refute :as sr]
            [desargues.tex.label :as tl]))

(def modus-ponens '(implies (and P (implies P Q)) Q))
(def affirming '(implies (or P Q) P))

(defn- flat [beats] (mapcat #(if (= :par (:beat %)) (flat (:beats %)) [%]) beats))

(defn- captions [beats]
  (keep (fn [it] (when (= :caption (:id it)) (or (:text it) (:latex it)))) (b/items beats)))

(defn- crosses [beats] (filter #(= "\\times" (:latex %)) (b/items beats)))

(defn- links [beats] (filter #(= :line (:kind %)) (b/items beats)))

(defn- references-known?
  "Every id a beat acts on was shown by an earlier beat."
  [beats]
  (:ok (reduce (fn [{:keys [shown] :as acc} bt]
                 (let [acted (concat (:ids bt) (keys (:to bt)))
                       acc (if (every? #(or (shown %) (= :caption %)) acted) acc (assoc acc :ok false))]
                   (update acc :shown into (map :id (:items bt)))))
               {:shown #{} :ok true}
               (flat beats))))

(deftest modus-ponens-closes-in-one-branch
  (let [bs (sr/beats modus-ponens)]
    (is (empty? (links bs)) "no split")
    (is (= 1 (count (crosses bs))) "one closed column")
    (is (some #{"assume it is F"} (captions bs)))
    (is (some #{"⇒ is false only when T ⇒ F, so"} (captions bs)))
    (is (some #{"clash: no row of ⇒ gives T"} (captions bs)))
    (is (= "every branch closes: tautology" (last (captions bs))))
    (testing "the clashing line turns red"
      (is (some #(and (= :recolor (:beat %)) (= :red (:color %))) (flat bs))))
    (is (references-known? bs))))

(deftest open-branch-shows-the-counterexample
  (let [bs (sr/beats affirming)
        gold (filter #(and (= :gold (:color %)) (vector? (:id %))) (b/items bs))]
    (is (empty? (crosses bs)))
    (is (= ["P = \\mathrm{F},\\ Q = \\mathrm{T}"] (map :latex gold)))
    (is (= "\\text{counterexample: }P = \\mathrm{F},\\ Q = \\mathrm{T}" (last (captions bs))))
    (is (some #{"∨ is false only when every part is false, so"} (captions bs)))
    (is (references-known? bs))))

(deftest branches-split-into-columns
  (let [bs (sr/beats '(iff (iff P Q) (iff Q P)))
        rows (filter #(and (vector? (:id %)) (= :row (first (:id %)))) (b/items bs))]
    (is (= 6 (count (links bs))))
    (is (= 4 (count (crosses bs))))
    (is (= 7 (count (distinct (map (comp first :at) rows))))
        "root column, two case columns, four leaf columns")
    (is (some #(str/starts-with? % "case 1 of 2") (captions bs)))
    (is (references-known? bs))))

(deftest every-footprint-inside-the-world
  (doseq [fm [modus-ponens affirming '(iff (iff P Q) (iff Q P))
              '(iff (not (or P Q)) (and (not P) (not Q)))
              '(implies (and (or P Q) (or (not P) R)) (or Q R))
              '(iff (or (and (in x A) (not (in x B))) (and (in x B) (not (in x A))))
                    (and (or (in x A) (in x B)) (not (and (in x A) (in x B)))))]]
    (is (every? b/inside? (b/footprints (sr/beats fm))) (str fm))))

(deftest recording-works
  (let [g (sr/record "modus-ponens" modus-ponens)
        texs (filter tl/tex-node? (vals (:nodes g)))]
    (is (= "modus-ponens" (:scene g)))
    (is (seq (:steps g)))
    (is (some #(= "\\times" (tl/node-latex %)) texs))
    (is (some #(= :red (tl/node-color %)) texs))))
