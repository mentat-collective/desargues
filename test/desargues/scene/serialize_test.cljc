(ns desargues.scene.serialize-test
  "Round-trip tests for canonical scene-graph EDN (ROADMAP 5.4): record a
   small scene through the facade, save, load, re-save; the text is
   byte-identical and the data equal."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [desargues.scene :as s]
            [desargues.scene.record :as rec]
            [desargues.scene.data :as data]
            [desargues.scene.serialize :as ser]))

(defn- small-scene []
  (rec/record "Serialize"
              (fn [stage]
                (let [title (-> (s/text "Hello" :font-size 30 :color :white) (s/move-to [0 3]))
                      dot   (-> (s/dot :color :gold) (s/move-to [-1 0]))
                      rod   (s/line [0 2] [1 0] :color :teal :width 4)
                      n     (s/decimal 0 :num-decimal-places 2)]
                  (s/play! stage (s/appear title) (s/draw rod))
                  (s/play! stage (s/together [(s/glide dot [1 0] :run-time 0.5)
                                              (s/count-to n 3.25)]))
                  (s/hold! stage 1.5)))))

(defn- tmp-file [name]
  #?(:clj (let [d (java.nio.file.Files/createTempDirectory "desargues-serialize"
                                                           (make-array java.nio.file.attribute.FileAttribute 0))]
            (str (.resolve d name)))))

(deftest string-round-trip
  (let [g  (small-scene)
        s1 (ser/->canonical-edn g)
        g2 (ser/<-edn s1)]
    (is (= g g2) "data survives the round trip")
    (is (= s1 (ser/->canonical-edn g2)) "re-serializing yields identical text")
    (is (str/ends-with? s1 "\n"))
    (is (= s1 (ser/->canonical-edn (small-scene))) "recording again yields identical text")))

#?(:clj
   (deftest file-round-trip
     (let [g  (small-scene)
           p1 (tmp-file "a/scene.edn")
           p2 (tmp-file "b/scene.edn")]
       (ser/save-scene-graph! g p1)
       (let [g2 (ser/load-scene-graph! p1)]
         (is (= g g2))
         (ser/save-scene-graph! g2 p2)
         (is (= (slurp p1) (slurp p2)) "save -> load -> save is byte-identical")))))

(deftest canonical-key-order
  (testing "insertion order of maps and sets never reaches the text"
    (let [a (into {} (map (fn [i] [(keyword (str "k" i)) i])) (range 40))
          b (into {} (map (fn [i] [(keyword (str "k" i)) i])) (reverse (range 40)))]
      (is (= (ser/->canonical-edn {:m a :s (set (keys a))})
             (ser/->canonical-edn {:s (set (keys b)) :m b})))))
  (testing "keys sorted at every depth; mixed key kinds have a total order"
    (is (= "{:a {:x 1, :y 2}, :b [{:p 1, :q 2}]}\n"
           (ser/->canonical-edn {:b [{:q 2 :p 1}] :a {:y 2 :x 1}})))
    (is (= "{nil 0, 1 :n, \"s\" :str, :k :kw, sym :sym}\n"
           (ser/->canonical-edn {'sym :sym :k :kw "s" :str 1 :n nil 0}))))
  (testing "long collections break deterministically and still read back"
    (let [g (small-scene)
          s (ser/->canonical-edn g)]
      (is (str/includes? s "\n"))
      (is (every? #(<= (count %) 140) (str/split-lines s))))))

(deftest layout-graph-round-trips
  (let [g (data/layout->scene-graph "layout" {:type :text :content "Hi" :attrs {}} {})
        t (ser/->canonical-edn g)]
    (is (= g (ser/<-edn t)))
    (is (= t (ser/->canonical-edn (ser/<-edn t))))))

(defn- not-data-info [x]
  (try (ser/->canonical-edn x) nil
       (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) e
         (assoc (ex-data e) :message (ex-message e)))))

(defrecord R [a])

(deftest rejects-non-data
  (testing "a function inside the graph"
    (let [info (not-data-info {:scene "x" :steps [{:step :play :f inc}]})]
      (is (= ::ser/not-data (:error info)))
      (is (= [:steps 0 :f] (:path info)))
      (is (str/includes? (:message info) "function"))))
  (testing "an atom"
    (is (= [:state] (:path (not-data-info {:state (atom 1)})))))
  (testing "a record (would need a reader tag)"
    (is (= ::ser/not-data (:error (not-data-info {:r (->R 1)})))))
  (testing "a non-finite number"
    (is (= [:x] (:path (not-data-info {:x ##NaN}))))
    (is (= [:x] (:path (not-data-info {:x ##Inf})))))
  (testing "reader tags are refused on load"
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                 (ser/<-edn "{:at #inst \"2020-01-01\"}")))))
