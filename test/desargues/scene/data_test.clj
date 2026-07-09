(ns desargues.scene.data-test
  "PURE tests for the RecordingBackend layout path — NO python. Asserts the
   backend-neutrality (LSP) contract: -render-layout! yields a realized scene
   graph of the same shape -render! does, not a raw-tree stub."
  (:require [clojure.test :refer [deftest testing is]]
            [desargues.layout.core :as l]
            [desargues.scene.data :as data]))

(def frame {:x 0 :y 0 :w 14.222222 :h 8.0})

(defn- sample-tree []
  (l/column {:width :fill :height :fill :padding 0.6 :spacing 0.4 :align :center-x :font-size 36}
    (l/box {:width :fill :height [:px 1.0] :bg :blue :align :center-x}
      (l/text "Title"))
    (l/row {:width :fill :height [:portion 3] :spacing 0.5}
      (l/box {:width [:portion 1] :height :fill :border :white}
        (l/text "Bank A"))
      (l/box {:width [:portion 1] :height :fill :border [:gold 3]}
        (l/math "\\Delta M = 1000")))
    (l/text "reserves" {:font-size 24})))

(deftest render-layout-shape-parity-with-render
  (testing "-render-layout! yields the same scene-graph keys -render! yields"
    (let [g (data/layout->scene-graph "s" (sample-tree) {})]
      (is (every? g [:scene :nodes :steps :node-count]))
      (is (= "s" (:scene g)))
      (is (map? (:nodes g)))
      (is (vector? (:steps g))))))

(deftest render-layout-is-not-a-stub
  (testing "the old stub echoed the raw tree and realized nothing; now it realizes"
    (let [g (data/layout->scene-graph "s" (sample-tree) {})]
      (is (nil? (:tree g)) "raw unresolved :tree passthrough is gone")
      (is (pos? (:node-count g)))
      (is (= (:node-count g) (count (:nodes g)))))))

(deftest render-layout-reveal-steps
  (testing ":steps is a whole-scene :appear reveal then a :hold"
    (let [g     (data/layout->scene-graph "s" (sample-tree) {:hold 2})
          kinds (mapv :step (:steps g))
          play  (first (:steps g))
          hold  (second (:steps g))]
      (is (= [:play :hold] kinds))
      (is (= 2 (:seconds hold)))
      (is (= (set (keys (:nodes g)))
             (set (:ids (first (:anims play)))))
          "the reveal lists every realized node id"))))

(deftest render-layout-default-hold-matches-manim
  (testing "default hold is 3s, matching ManimBackend/-render-layout!"
    (let [g (data/layout->scene-graph "s" (sample-tree) {})]
      (is (= 3 (:seconds (second (:steps g))))))))

(deftest render-layout-realizes-content-kinds
  (testing "leaves + decorated containers become typed visual nodes"
    (let [g     (data/layout->scene-graph "s" (sample-tree) {})
          kinds (frequencies (map :node (vals (:nodes g))))]
      (is (= 3 (:text kinds)))                       ; "Title", "Bank A", "reserves"
      (is (= 1 (:math kinds)))
      (is (= 3 (+ (:rectangle kinds 0) (:rounded-rectangle kinds 0)))))))

(deftest render-layout-boundedness
  (testing "every realized node box lies within the frame (browser-consumer invariant)"
    (let [g (data/layout->scene-graph "s" (sample-tree) {})]
      (is (l/within? (get (:layout g) l/box-key) frame))
      (is (every? #(l/within? (:box %) frame) (vals (:nodes g)))))))

(deftest render-layout-overflow-squeezed
  (testing "a title too long to fit is squeezed inside the frame, not overflowed"
    (let [tree (l/column {:width :fill :height :fill :padding 0.6 :font-size 40}
                 (l/text "A very very very long title that would overflow without measurement"))
          g    (data/layout->scene-graph "s" tree {})]
      (is (every? #(l/within? (:box %) frame) (vals (:nodes g)))))))

(deftest render-layout-inherits-style
  (testing "an unstyled leaf inherits its ancestor's font-size"
    (let [g     (data/layout->scene-graph "s" (sample-tree) {})
          title (first (filter #(= "Title" (:content %)) (vals (:nodes g))))
          resv  (first (filter #(= "reserves" (:content %)) (vals (:nodes g))))]
      (is (= 36 (:font-size (:style title))) "Title inherits column font-size")
      (is (= 24 (:font-size (:style resv))) "reserves keeps its own font-size"))))
