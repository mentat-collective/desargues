(ns desargues.logic.scene.grid-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [desargues.logic.scene.figure :as fig]
            [desargues.logic.scene.grid :as gr]
            [desargues.scene.data :as rec]
            [hive-test.trifecta :refer [deftrifecta]]))

;; ---------------------------------------------------------------------------
;; Fixtures

(def mod-2 {:relation (set (for [x (range 1 5) y (range 1 5) :when (= (mod x 2) (mod y 2))] [x y]))
            :carrier #{1 2 3 4} :name 'R})

(def cases
  {:mod-2 mod-2
   :chain {:relation #{[1 2] [2 3] [1 1]} :carrier #{1 2 3} :name 'S}
   :empty {:relation #{} :carrier #{1 2} :name 'R}})

;; ---------------------------------------------------------------------------
;; Generators

(def relation-gen
  (gen/let [n (gen/choose 1 6)
            bits (gen/vector gen/boolean (* n n))]
    {:relation (set (for [[i b] (map-indexed vector bits) :when b] [(quot i n) (rem i n)]))
     :carrier (set (range n))}))

(def equivalence-gen
  (gen/let [n (gen/choose 1 6)
            labels (gen/vector (gen/choose 0 2) n)]
    {:relation (set (for [x (range n) y (range n) :when (= (labels x) (labels y))] [x y]))
     :carrier (set (range n))}))

(defn- red? [b] (= :red (:color b)))

;; ---------------------------------------------------------------------------
;; Trifecta

(deftrifecta grid-flow
  desargues.logic.scene.grid/beats
  {:golden-path "test/golden/desargues/logic/scene/grid-flow.edn"
   :cases cases
   :gen (gen/one-of [relation-gen equivalence-gen])
   :pred fig/inside-world?
   :num-tests 120
   :mutations [["no-red-marks" (fn [spec] (mapv (fn [b] (if (:items b) (update b :items #(filterv (complement red?) %)) b))
                                                (gr/beats spec)))]
               ["never-reorders" (fn [spec] (filterv #(not= :move (:beat %)) (gr/beats spec)))]
               ["transposed" (fn [spec] (gr/beats (update spec :relation #(set (map (comp vec reverse) %)))))]]})

;; ---------------------------------------------------------------------------
;; Facts

(deftest facts-from-the-model
  (let [a (gr/analyse mod-2)]
    (is (:equivalence? a))
    (is (= [#{1 3} #{2 4}] (:classes a))))
  (let [a (gr/analyse (:chain cases))]
    (is (= [2 3] (:missing-diagonal a)))
    (is (= [[1 2 3]] (:failures a)))
    (is (not (:equivalence? a)))))

(deftest recording-works
  (let [g (gr/record :mod-2 mod-2)]
    (is (pos? (rec/animation-count g)))
    (is (every? (rec/colors-used g) [:gold :teal :blue]))))
