(ns desargues.logic.scene.arrow-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [desargues.logic.scene.arrow :as ar]
            [desargues.logic.scene.figure :as fig]
            [desargues.scene.data :as rec]
            [hive-test.trifecta :refer [deftrifecta]]))

;; ---------------------------------------------------------------------------
;; Fixtures

(def not-injective {:graph '#{[1 a] [2 a] [3 b]} :dom #{1 2 3} :cod '#{a b c} :name 'f})
(def bijection {:graph '#{[1 b] [2 a]} :dom #{1 2} :cod '#{a b} :name 'g})

(def cases
  {:not-injective {:spec not-injective :opts {:image #{1 3} :preimage '#{a}}}
   :bijection {:spec bijection :opts {}}
   :not-a-function {:spec {:graph '#{[1 a] [1 b]} :dom #{1 2} :cod '#{a b} :name 'h} :opts {}}
   :relation-ring {:spec {:graph #{[1 2] [2 1] [3 3]} :dom #{1 2 3} :name 'R} :opts {}}
   :composition {:compose {:f '#{[1 a] [2 b] [3 b]} :g '#{[a x] [b y]} :A #{1 2 3} :B '#{a b} :C '#{x y}}}})

;; ---------------------------------------------------------------------------
;; Subject

(defn flow
  "The beats of an arrow-flow case: {:spec :opts} or {:compose spec}."
  [{:keys [spec opts compose]}]
  (if compose (ar/compose-beats compose) (ar/beats spec opts)))

(def letters '[a b c d e f])

(def function-gen
  (gen/let [n (gen/choose 1 6) k (gen/choose 1 6)
            ys (gen/vector (gen/choose 0 (dec k)) n)
            img (gen/vector gen/boolean n)
            pre (gen/vector gen/boolean k)]
    {:spec {:graph (set (map (fn [x y] [x (letters y)]) (range n) ys))
            :dom (set (range n)) :cod (set (take k letters)) :name 'f}
     :opts {:image (set (keep-indexed #(when %2 %1) img))
            :preimage (set (keep-indexed #(when %2 (letters %1)) pre))}}))

(def relation-gen
  (gen/let [n (gen/choose 1 6)
            bits (gen/vector gen/boolean (* n n))]
    {:spec {:graph (set (for [[i b] (map-indexed vector bits) :when b] [(quot i n) (rem i n)]))
            :dom (set (range n)) :name 'R}
     :opts {}}))

(def compose-gen
  (gen/let [n (gen/choose 1 6) k (gen/choose 1 6) l (gen/choose 1 6)
            fs (gen/vector (gen/choose 0 (dec k)) n)
            gs (gen/vector (gen/choose 0 (dec l)) k)]
    {:compose {:f (set (map vector (range n) (map letters fs)))
               :g (set (map (fn [y z] [y (symbol (str "z" z))]) (take k letters) gs))
               :A (set (range n)) :B (set (take k letters)) :C (set (map #(symbol (str "z" %)) (range l)))}}))

(defn- drop-beats [pred bs] (vec (remove pred bs)))

(defn- red? [b] (and (= :recolor (:beat b)) (= :red (:color b))))

;; ---------------------------------------------------------------------------
;; Trifecta

(deftrifecta arrow-flow
  desargues.logic.scene.arrow-test/flow
  {:golden-path "test/golden/desargues/logic/scene/arrow-flow.edn"
   :cases cases
   :gen (gen/one-of [function-gen relation-gen compose-gen])
   :pred fig/inside-world?
   :num-tests 120
   :mutations [["never-red" (fn [in] (drop-beats #(or (red? %)
                                                      (and (= :par (:beat %)) (some red? (:beats %))))
                                                 (flow in)))]
               ["headless-arrows" (fn [in] (mapv (fn [b] (if (:items b)
                                                           (update b :items #(filterv (fn [it] (not= :head (get (:id it) 1))) %))
                                                           b))
                                                 (flow in)))]
               ["swapped-columns" (fn [in] (if-let [s (:spec in)]
                                             (ar/beats (cond-> s (:cod s) (assoc :dom (:cod s) :cod (:dom s)
                                                                                 :graph (set (map (comp vec reverse) (:graph s)))))
                                                       (:opts in))
                                             (flow in)))]]})

;; ---------------------------------------------------------------------------
;; Facts

(deftest facts-from-the-model
  (is (= {:function? true :lonely [] :forked [] :collisions {'a [1 2]} :gaps ['c]
          :injective? false :surjective? false}
         (ar/analyse not-injective)))
  (is (= [true true] ((juxt :injective? :surjective?) (ar/analyse bijection)))))

(deftest recording-works
  (let [g (ar/record :f not-injective {:image #{1 3}})]
    (is (pos? (rec/animation-count g)))
    (is (some #(= :dot (:node %)) (vals (:nodes g))))
    (is (every? (rec/colors-used g) [:red :gold])))
  (let [g (ar/record-compose :gf (:compose (:composition cases)))]
    (is (contains? (rec/colors-used g) :teal))))
