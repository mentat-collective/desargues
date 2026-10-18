(ns desargues.lesson.schema-test
  "The lesson bundle: every block kind accepted, malformed blocks, impure
   values and broken :media references rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.lesson.schema :as ls]
            [desargues.objects.calc.core :as c]))

(defrecord R [a])

(def figure
  (c/figure {:title "area" :view (c/plane [-1 4] [-1 5])
             :objects [(c/area-under :parabola 0 2 {:upto (c/p :sweep)})]
             :params {:sweep 0.5}}))

(def construction
  {:id :tri :kind :construction
   :params [{:id 't :min 0 :max 1 :init 0 :label "rotate"}]
   :points [{:id :A :at [0 0]} {:id :B :at [2 0]} {:id :M :op :mid :of [:A :B]}]
   :draw [[:segment :A :B] [:point :M {:label "M"}]]
   :checks [{:distance [:A :B] :label "|AB|"}]})

(def scene
  {:scene "dot"
   :nodes {1 {:node :dot :id 1 :opts {:color :gold} :at [0 0]}}
   :steps [{:step :play :anims [{:anim :appear :target 1 :opts {}}]}
           {:step :hold :seconds 1}]
   :node-count 1})

(def blocks
  {:prose        {:block/id :b-md :block/kind :prose :md "# Area\nHow much?"}
   :tex          {:block/id :b-tex :block/kind :tex :expr '(integrate (expt x 2) x 0 2)}
   :figure       {:block/id :b-fig :block/kind :figure :figure figure}
   :construction {:block/id :b-con :block/kind :construction :construction construction}
   :viewer       {:block/id :b-view :block/kind :viewer
                  :fragment '[mafs.core/Mafs {:view-box {:x [-1 4]}}
                              [mafs.core/Cartesian {}]
                              [mafs.plot/OfX {:y (fn [x] (* x x))}]]}
   :scene        {:block/id :b-scn :block/kind :scene :scene scene}
   :media        {:block/id :b-png :block/kind :media :from :b-fig :format :png}})

(defn lesson [& bs]
  {:lesson/id :area :lesson/title "Area under a curve" :lesson/version "1.0.0"
   :lesson/slides [{:slide/id :s1 :slide/title "One" :slide/blocks (vec bs)}]})

(def full (apply lesson (vals blocks)))

(deftest a-full-lesson-is-valid
  (is (nil? (ls/explain full)))
  (is (ls/valid? full))
  (is (= full (ls/validate full)))
  (is (ls/pure-data? full)))

(deftest every-kind-alone
  (doseq [[kind b] (dissoc blocks :media)]
    (testing kind
      (is (nil? (ls/explain (lesson b))))))
  (testing ":media alongside its source"
    (doseq [fmt [:png :gif :mp4]]
      (is (ls/valid? (lesson (:figure blocks) (assoc (:media blocks) :format fmt)))))))

(deftest an-empty-lesson-is-valid
  (is (ls/valid? {:lesson/id :e :lesson/title "" :lesson/version "0" :lesson/slides []})))

(deftest lesson-and-slide-shape
  (is (:schema (ls/explain (dissoc full :lesson/title))))
  (is (:schema (ls/explain (assoc full :lesson/id "area"))))
  (is (:schema (ls/explain (assoc full :lesson/version 1))))
  (is (:schema (ls/explain (assoc-in full [:lesson/slides 0 :slide/title] :t))))
  (is (:schema (ls/explain (assoc-in full [:lesson/slides 0 :slide/blocks] '())))
      "blocks are a vector"))

(defn- bad [k f] (:schema (ls/explain (lesson (f (get blocks k))))))

(deftest block-failures
  (testing "block id and kind"
    (is (bad :prose #(dissoc % :block/id)))
    (is (bad :prose #(assoc % :block/id "b")))
    (is (bad :prose #(assoc % :block/kind :video))))
  (testing ":prose"
    (is (bad :prose #(assoc % :md 3)))
    (is (bad :prose #(dissoc % :md))))
  (testing ":tex takes frozen expressions only"
    (is (nil? (ls/explain (lesson (assoc (:tex blocks) :expr 'x)))))
    (is (nil? (ls/explain (lesson (assoc (:tex blocks) :expr 42)))))
    (is (bad :tex #(assoc % :expr :x)))
    (is (bad :tex #(assoc % :expr '(1 2))) "a list must be headed by an operator symbol")
    (is (bad :tex #(assoc % :expr '(+ x {:a 1})))))
  (testing ":figure must satisfy calc figure-problems"
    (is (bad :figure #(assoc-in % [:figure :view :view] :cube)))
    (is (bad :figure #(assoc % :figure {:objects []})))
    (is (bad :figure #(assoc-in % [:figure :params] {})) "unset param :sweep")
    (is (bad :figure #(assoc-in % [:figure :objects 0 :a] 3)) "a >= b"))
  (testing ":construction"
    (is (bad :construction #(assoc-in % [:construction :kind] :plot)))
    (is (bad :construction #(assoc-in % [:construction :points] [])))
    (is (bad :construction #(assoc-in % [:construction :points 0 :id] "A"))))
  (testing ":viewer"
    (is (bad :viewer #(assoc % :fragment "mafs")))
    (is (bad :viewer #(assoc % :fragment [])))
    (is (bad :viewer #(assoc % :fragment [{:a 1}]))))
  (testing ":scene"
    (is (bad :scene #(update % :scene dissoc :steps)))
    (is (bad :scene #(assoc-in % [:scene :steps 0] {:anims []}))))
  (testing ":media"
    (is (bad :media #(assoc % :format :jpeg)))
    (is (bad :media #(dissoc % :from)))))

(deftest media-references
  (let [refs (fn [l] (map :problem (:references (ls/explain l))))]
    (is (= [:media/unknown-from] (refs (lesson (:media blocks)))))
    (is (= [:media/self-reference]
           (refs (lesson (assoc (:media blocks) :from :b-png)))))
    (is (= [:media/from-media]
           (refs (lesson (:figure blocks) (:media blocks)
                         {:block/id :b-gif :block/kind :media :from :b-png :format :gif}))))
    (testing "a :media block may reference a block on another slide"
      (is (ls/valid? (update full :lesson/slides conj
                             {:slide/id :s2 :slide/title "Two"
                              :slide/blocks [{:block/id :b-mp4 :block/kind :media
                                              :from :b-scn :format :mp4}]}))))))

(deftest duplicate-ids
  (let [refs (fn [l] (set (map :problem (:references (ls/explain l)))))]
    (is (contains? (refs (lesson (:prose blocks) (:prose blocks))) :lesson/duplicate-block-id))
    (is (contains? (refs (update full :lesson/slides conj
                                 {:slide/id :s1 :slide/title "again" :slide/blocks []}))
                   :lesson/duplicate-slide-id))))

(deftest pure-data
  (testing "plain EDN"
    (is (ls/pure-data? {:a [1 2.5 "s" \c nil true 'sym #{:k} '(f x)]}))
    (is (ls/pure-data? figure))
    (is (ls/pure-data? scene)))
  (testing "rejected"
    (is (not (ls/pure-data? {:f inc})))
    (is (not (ls/pure-data? [(fn [x] x)])))
    (is (not (ls/pure-data? {:a (atom 1)})))
    (is (not (ls/pure-data? #?(:clj #'inc :cljs (var inc)))))
    (is (not (ls/pure-data? #"re")))
    (is (not (ls/pure-data? {:o #?(:clj (Object.) :cljs (js-obj))})))
    (is (not (ls/pure-data? ##NaN)))
    (is (not (ls/pure-data? (->R 1))) "records do not read back as EDN"))
  (testing "impure-paths names where"
    (is (= [[:a 1]] (ls/impure-paths {:a [0 inc]})))
    (is (= [] (ls/impure-paths {:a [0 1]})))))

(deftest impure-blocks-are-rejected
  (testing "a figure with a bare fn passes calc but is not data"
    (let [l (lesson (assoc-in (:figure blocks) [:figure :objects 0 :f] (fn [x] (* x x))))
          why (ls/explain l)]
      (is (= [[:lesson/slides 0 :slide/blocks 0 :figure :objects 0 :f]] (:impure why)))
      (is (not (ls/valid? l)))))
  (testing "an atom in a scene"
    (is (:impure (ls/explain (lesson (assoc-in (:scene blocks) [:scene :nodes 1 :opts] (atom {})))))))
  (testing "validate throws with the explanation"
    (let [e (try (ls/validate (lesson (:media blocks))) nil
                 (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e e))]
      (is (some? e))
      (is (= :media/unknown-from (-> e ex-data :explain :references first :problem))))))
