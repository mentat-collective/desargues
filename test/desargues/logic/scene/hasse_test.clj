(ns desargues.logic.scene.hasse-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [desargues.logic.scene.figure :as fig]
            [desargues.logic.scene.hasse :as h]
            [desargues.scene.data :as rec]
            [hive-test.trifecta :refer [deftrifecta]]))

;; ---------------------------------------------------------------------------
;; Fixtures

(def divisors-of-12
  (let [ds [1 2 3 4 6 12]]
    {:carrier (set ds) :order (set (for [x ds y ds :when (zero? (mod y x))] [x y]))}))

(def bowtie {:carrier '#{a b c d} :order '#{[a c] [a d] [b c] [b d]}})

(def cases
  {:divisors-of-12 {:spec divisors-of-12 :opts {:subset #{4 6}}}
   :bowtie {:spec bowtie :opts {:subset '#{a b}}}
   :antichain {:spec {:carrier #{1 2 3} :order #{}} :opts {}}})

;; ---------------------------------------------------------------------------
;; Subject and generators

(defn flow
  "The Hasse beats of a case {:spec :opts}."
  [{:keys [spec opts]}]
  (h/beats spec opts))

(defn- closure [n edges]
  (loop [r (into (set edges) (map (fn [x] [x x])) (range n))]
    (let [r' (into r (for [[x y] r [y' z] r :when (= y y')] [x z]))]
      (if (= r r') r (recur r')))))

(def poset-gen
  (gen/let [n (gen/choose 1 6)
            bits (gen/vector gen/boolean (* n n))
            sub (gen/vector gen/boolean n)]
    {:spec {:carrier (set (range n))
            :order (closure n (for [i (range n) j (range n)
                                    :when (and (< i j) (nth bits (+ (* i n) j)))]
                                [i j]))}
     :opts {:subset (set (keep-indexed #(when %2 %1) sub))}}))

;; ---------------------------------------------------------------------------
;; Trifecta

(deftrifecta hasse-flow
  desargues.logic.scene.hasse-test/flow
  {:golden-path "test/golden/desargues/logic/scene/hasse-flow.edn"
   :cases cases
   :gen poset-gen
   :pred fig/inside-world?
   :num-tests 120
   :mutations [["draws-every-comparable-pair"
                (fn [{:keys [spec] :as in}]
                  (let [r (h/reflexive-closure spec)]
                    (mapv (fn [b] (if (and (= :show (:beat b)) (some #(= :cover (first (:id %))) (:items b)))
                                    (assoc b :items (vec (for [[x y] (sort-by pr-str r) :when (not= x y)]
                                                           (fig/line [:cover x y] [0 0] [0 0] :grey 2))))
                                    b))
                          (flow in))))]
               ["calls-everything-a-lattice"
                (fn [in] (filterv #(not= [:note 1] (first (:ids %))) (flow in)))]
               ["upside-down"
                (fn [{:keys [spec] :as in}]
                  (flow (assoc in :spec (update spec :order #(set (map (comp vec reverse) %))))))]]})

;; ---------------------------------------------------------------------------
;; Facts

(deftest facts-from-the-model
  (is (= [[1 2] [1 3] [2 4] [2 6] [3 6] [4 12] [6 12]] (h/covers divisors-of-12)))
  (is (:lattice? (h/analyse divisors-of-12)))
  (is (= 12 (get (h/op-table divisors-of-12 'join) [4 6])))
  (is (= 2 (get (h/op-table divisors-of-12 'meet) [4 6])))
  (is (= {:pair '[a b] :missing :join :culprits '[c d]} (:failure (h/analyse bowtie))))
  (is (= {:sup 12 :inf 2} (select-keys (h/bounds divisors-of-12 #{4 6}) [:sup :inf]))))

(deftest recording-works
  (let [g (h/record :bowtie bowtie {:subset '#{a b}})]
    (is (pos? (rec/animation-count g)))
    (is (contains? (rec/colors-used g) :red)))
  (let [g (h/record :d12 divisors-of-12)]
    (is (some #(= :dot (:node %)) (vals (:nodes g))))))
