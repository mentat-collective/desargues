(ns desargues.logic.scene.table-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.logic.formula :as f]
            [desargues.logic.scene.table :as st]
            [desargues.logic.table :as t]
            [desargues.scene.data :as rec]
            [desargues.tex.label :as label]
            [desargues.tex.typeset :as ty]
            [desargues.logic.tex :as lt]))

(def de-morgan '(iff (not (or P Q)) (and (not P) (not Q))))
(def modus-ponens '(implies (and P (implies P Q)) Q))

(def by-atoms
  {1 ['(or P (not P)) '(and P (not P))]
   2 [de-morgan modus-ponens '(or P Q)]
   3 ['(iff (and P (or Q R)) (or (and P Q) (and P R)))
      '(implies (and (implies P Q) (implies Q R)) (implies P R))
      '(iff (in x A) (and (in x B) (not (in x C))))]
   4 ['(implies (and (implies P Q) (implies R S) (or P R)) (or Q S))
      '(iff (not (and (or P Q) (or R S))) (or (and (not P) (not Q)) (and (not R) (not S))))]})

(defn- inside? [{:keys [kind at from to] :as item}]
  (let [{:keys [half-w half-h]} st/world
        in? (fn [[x y] [hw hh]] (and (<= (+ (Math/abs (double x)) hw) half-w)
                                     (<= (+ (Math/abs (double y)) hh) half-h)))]
    (if (= kind :line)
      (and (in? from [0 0]) (in? to [0 0]))
      (in? at (st/item-extent item)))))

(deftest fill-is-column-major-children-first
  (doseq [fm (mapcat val by-atoms)]
    (let [bs (st/beats fm)
          cols (vec (f/compounds fm))
          fills (filter #(= :fill (:phase %)) bs)]
      (testing (pr-str fm)
        (is (= (range (count cols)) (map :col fills)))
        (is (every? (fn [{:keys [items]}] (= (range (count items)) (map :row items))) fills))
        (doseq [[j g] (map-indexed vector cols)
                a (f/args g)
                :when (f/compound? a)]
          (is (< (.indexOf cols a) j) "a child column fills before its parent"))
        (is (= [:intro :headers :frame :atoms] (take 4 (map :phase bs))))
        (is (= [:outline :verdict :final] (take-last 3 (map :phase bs))))
        (is (= (count cols) (count (filter #(= :shortcut (:phase %)) bs))) "one shortcut per column")))))

(deftest essential-marks-match-table-cells
  (doseq [fm (mapcat val by-atoms)]
    (let [bs (st/beats fm)
          expected (set (for [{:keys [row col essential?]} (t/cells (t/table fm)) :when essential?]
                          {:row row :col col}))
          marked (set (mapcat :cells (filter #(= :mark (:beat %)) bs)))
          filled (set (for [b bs :when (= :fill (:phase b)) it (:items b) :when (:essential? it)]
                        {:row (:row it) :col (:col b)}))]
      (is (= expected marked) (pr-str fm))
      (is (= expected filled) (pr-str fm))
      (is (every? #(= :gold (:color %)) (filter #(= :mark (:beat %)) bs))))))

(deftest cell-values-match-table
  (let [tbl (t/table modus-ponens)
        bs (st/beats modus-ponens)
        values (for [b bs :when (= :fill (:phase b)) it (:items b)] (:value it))]
    (is (= (map :value (t/cells tbl)) values))))

(deftest verdict-and-final-colour
  (let [final (fn [fm] (last (st/beats fm)))]
    (is (= {:verdict :tautology :color :green} (select-keys (final de-morgan) [:verdict :color])))
    (is (= 4 (count (:ids (final de-morgan)))))
    (let [fm '(or P Q) fb (final fm)]
      (is (= [:contingent :red] [(:verdict fb) (:color fb)]))
      (is (= (count (t/counterexamples (t/table fm))) (count (:ids fb)))))
    (is (= 2 (count (:ids (final '(and P (not P)))))))))

(deftest every-item-inside-the-world
  (doseq [[n fms] by-atoms fm fms]
    (let [bs (st/beats fm)
          items (st/beat-items bs)
          lay (st/layout (t/table fm))]
      (testing (str n " atoms " (pr-str fm))
        (is (= n (count (f/atoms fm))))
        (is (seq items))
        (is (every? inside? items) (pr-str (mapv :id (remove inside? items))))
        (is (= (:legible? lay) (<= 12.0 (:header-size lay))))
        (when (<= (count (:columns lay)) 12)
          (is (:legible? lay) "up to 12 columns the headers stay legible"))))))

(deftest recording-yields-a-scene-graph
  (let [g (st/record-table "de-morgan" de-morgan)
        items (st/beat-items (st/beats de-morgan))]
    (is (= "de-morgan" (:scene g)))
    (is (= (count items) (:node-count g)))
    (is (seq (rec/play-steps g)))
    (is (every? #{:white :grey :gold :green :red :orange} (rec/colors-used g)))
    (is (= (count (filter #(= :tex (:kind %)) items))
           (count (filter label/tex-node? (vals (:nodes g))))))
    (is (every? (fn [nd] (or (= :line (:node nd)) (:at nd))) (vals (:nodes g))))))

(deftest tex-width-tracks-the-typesetter
  (let [ts (ty/latex-typesetter)]
    (if-not (ty/available? ts)
      (is true "latex/dvisvgm absent: skipped")
      (doseq [fm (concat (mapcat val by-atoms) '[P (in x A) (not (or P Q))])]
        (let [l (lt/->TeX fm)
              w (/ (:w (ty/typeset ts l :white)) 10.0)
              est (st/tex-width l 60)]
          (is (< 0.8 (/ est w) 1.25) (str l " real " w " est " est)))))))
