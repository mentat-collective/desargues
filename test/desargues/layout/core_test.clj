(ns desargues.layout.core-test
  "PURE tests for the layout algebra — NO python. A stub measure-fn returns
   controllable content sizes (via :tw/:th attr hints, ignored by the resolver),
   so we exercise fill/portion split, spacing, padding, clamp, alignment and the
   BOUNDEDNESS INVARIANT with zero dependency on manim."
  (:require [clojure.test :refer [deftest testing is]]
            [desargues.layout.core :as l]))

;; ---------------------------------------------------------------------------
;; Test harness
;; ---------------------------------------------------------------------------

(defn stub
  "Stub measure-fn: [node path] -> [w h]. Reads :tw/:th hints (default 1.0 x 0.5)."
  [node _path]
  [(double (get-in node [:attrs :tw] 1.0))
   (double (get-in node [:attrs :th] 0.5))])

(def frame {:x 0 :y 0 :w 12.0 :h 6.0})

(defn resolve* [tree] (l/resolve-layout tree frame stub))
(defn resolve-in [tree box] (l/resolve-layout tree box stub))

(defn child-boxes [resolved]
  (mapv #(get % :desargues.layout/box) (:children resolved)))

(defn all-bounded?
  "True iff every child ::box lies within its parent's content box, recursively."
  [node]
  (every? (fn [child]
            (and (l/within? (get child :desargues.layout/box) (l/content-box node))
                 (all-bounded? child)))
          (:children node)))

;; ---------------------------------------------------------------------------
;; Fill / portion split
;; ---------------------------------------------------------------------------

(deftest fill-splits-leftover-evenly
  (testing "two :fill children split the row width 50/50 (minus spacing)"
    (let [r (resolve-in
             (l/row {:width [:px 10] :height [:px 2] :spacing 1}
               (l/box {:width :fill :height :fill} (l/text "a"))
               (l/box {:width :fill :height :fill} (l/text "b")))
             {:x 0 :y 0 :w 10 :h 2})
          [b1 b2] (child-boxes r)]
      (is (= 4.5 (:w b1)))
      (is (= 4.5 (:w b2)))
      (is (= 0.0 (:x b1)))
      (is (= 5.5 (:x b2)))                     ; 4.5 + 1 gap
      (is (== 2.0 (:h b1)))                    ; :fill cross = full extent
      (is (== 2.0 (:h b2))))))

(deftest portion-weights-split-proportionally
  (testing "[:portion 1] and [:portion 3] split width 12 as 3 : 9"
    (let [r (resolve-in
             (l/row {:width [:px 12] :height 2}
               (l/box {:width [:portion 1] :height :fill} (l/text "a"))
               (l/box {:width [:portion 3] :height :fill} (l/text "b")))
             {:x 0 :y 0 :w 12 :h 2})
          [b1 b2] (child-boxes r)]
      (is (= 3.0 (:w b1)))
      (is (= 9.0 (:w b2))))))

;; ---------------------------------------------------------------------------
;; Spacing = n-1 gaps, nothing at the edges
;; ---------------------------------------------------------------------------

(deftest spacing-is-n-minus-1-gaps
  (testing "3 fixed children, spacing 1, packed at start: x = 0, 3, 6 (no edge gap)"
    (let [r (resolve-in
             (l/row {:width [:px 10] :height 1 :spacing 1}
               (l/box {:width [:px 2] :height :fill} (l/text "x"))
               (l/box {:width [:px 2] :height :fill} (l/text "y"))
               (l/box {:width [:px 2] :height :fill} (l/text "z")))
             {:x 0 :y 0 :w 10 :h 1})
          xs (mapv :x (child-boxes r))]
      (is (= [0.0 3.0 6.0] xs)))))

;; ---------------------------------------------------------------------------
;; Padding insets the content region before children are placed
;; ---------------------------------------------------------------------------

(deftest padding-insets-content
  (testing "column padding 1 shifts the single centered child in by the padding"
    (let [r (resolve-in
             (l/column {:width [:px 6] :height [:px 6] :padding 1 :align :center-x}
               (l/text "c" {:tw 2 :th 1}))
             {:x 0 :y 0 :w 6 :h 6})
          b (first (child-boxes r))]
      (is (= 2.0 (:x b)))                       ; left pad 1 + center: 1 + (4-2)/2
      (is (= 1.0 (:y b)))                       ; top pad 1, packed :start
      (is (= 2.0 (:w b)))
      (is (= 1.0 (:h b))))))

(deftest paddingXY-vector
  (testing "[x y] padding = left&right x, top&bottom y"
    (let [pad (l/normalize-padding [2 3])]
      (is (= {:left 2 :right 2 :top 3 :bottom 3} pad)))))

;; ---------------------------------------------------------------------------
;; min / max clamp
;; ---------------------------------------------------------------------------

(deftest max-clamp-caps-fill-and-reflows
  (testing "[:max 3] caps a fill child; the freed space reflows to the other fill"
    (let [r (resolve-in
             (l/row {:width [:px 10] :height 1}
               (l/box {:width [:max 3] :height :fill} (l/text "d"))
               (l/box {:width :fill :height :fill} (l/text "e")))
             {:x 0 :y 0 :w 10 :h 1})
          [b1 b2] (child-boxes r)]
      (is (= 3.0 (:w b1)))
      (is (= 7.0 (:w b2))))))                   ; reflow: 10 - 3

(deftest min-clamp-raises-shrink
  (testing "[:min 4] raises a shrink child up to 4"
    (let [r (resolve-in
             (l/row {:width [:px 10] :height 1}
               (l/box {:width [:min 4] :height :fill} (l/text "f" {:tw 1})))
             {:x 0 :y 0 :w 10 :h 1})
          b (first (child-boxes r))]
      (is (= 4.0 (:w b))))))

(deftest length-normalizer-forms
  (is (= {:mode :shrink} (l/normalize-length :shrink)))
  (is (= {:mode :portion :k 1} (l/normalize-length :fill)))
  (is (= {:mode :px :px 5} (l/normalize-length 5)))
  (is (= {:mode :px :px 5} (l/normalize-length [:px 5])))
  (is (= {:mode :portion :k 3} (l/normalize-length [:portion 3])))
  (is (= {:mode :shrink :min 2} (l/normalize-length [:min 2])))
  (is (= {:mode :portion :k 1 :max 9} (l/normalize-length [:max 9])))
  (is (= {:mode :px :px 4 :min 2 :max 9} (l/normalize-length [:clamp 2 9 [:px 4]]))))

;; ---------------------------------------------------------------------------
;; Alignment packing (start / center / end) + elm-ui push
;; ---------------------------------------------------------------------------

(deftest container-align-packs-main
  (testing "container :center-x centers the packed group; :right pushes to the end"
    (let [mk (fn [align]
               (child-boxes
                (resolve-in
                 (l/row {:width [:px 10] :height 1 :spacing 1 :align align}
                   (l/box {:width [:px 2] :height :fill} (l/text "x"))
                   (l/box {:width [:px 2] :height :fill} (l/text "y")))
                 {:x 0 :y 0 :w 10 :h 1})))]
      ;; total content = 2+1+2 = 5, free = 5
      (is (= [2.5 5.5] (mapv :x (mk :center-x))))   ; free/2 = 2.5 offset
      (is (= [5.0 8.0] (mapv :x (mk :right)))))))    ; free = 5 offset

(deftest child-self-align-pushes
  (testing "a child with :align :right in a row shoves it (and trailing sibs) to the far edge"
    (let [r (resolve-in
             (l/row {:width [:px 10] :height 1}
               (l/box {:width [:px 2] :height :fill} (l/text "x"))
               (l/box {:width [:px 2] :height :fill :align :right} (l/text "y")))
             {:x 0 :y 0 :w 10 :h 1})
          [b1 b2] (child-boxes r)]
      (is (= 0.0 (:x b1)))                       ; first stays at start
      (is (= 8.0 (:x b2))))))                    ; pushed: free (6) injected before it -> 0+2+6 = 8

(deftest cross-align-positions
  (testing "cross alignment positions a child within the cross extent"
    (let [r (resolve-in
             (l/row {:width [:px 10] :height [:px 4]}
               (l/box {:width [:px 2] :height [:px 1] :align :top} (l/text "t"))
               (l/box {:width [:px 2] :height [:px 1] :align :center-y} (l/text "c"))
               (l/box {:width [:px 2] :height [:px 1] :align :bottom} (l/text "b")))
             {:x 0 :y 0 :w 10 :h 4})
          [t c b] (child-boxes r)]
      (is (== 0.0 (:y t)))                       ; top
      (is (== 1.5 (:y c)))                       ; center: (4-1)/2
      (is (== 3.0 (:y b))))))                    ; bottom: 4-1

;; ---------------------------------------------------------------------------
;; BOUNDEDNESS INVARIANT — the core guarantee
;; ---------------------------------------------------------------------------

(deftest boundedness-nested-tree
  (testing "every child ::box lies within its parent content box; root within frame"
    (let [tree (l/column {:width :fill :height :fill :padding 0.6 :spacing 0.4 :align :center-x}
                 (l/box {:width :fill :height [:px 0.9] :align :center-x}
                   (l/text "Title" {:tw 4 :th 0.5}))
                 (l/row {:width :fill :height [:portion 3] :spacing 0.5 :align :center-y}
                   (l/column {:width [:portion 1] :height :fill :padding 0.3 :spacing 0.15}
                     (l/text "A" {:tw 2 :th 0.4}) (l/text "100" {:tw 2 :th 0.6}))
                   (l/column {:width [:portion 1] :height :fill :padding 0.3 :spacing 0.15}
                     (l/text "B" {:tw 2 :th 0.4}) (l/text "100" {:tw 2 :th 0.6})))
                 (l/row {:width :fill :height [:px 0.85] :spacing 0.18 :align :center-x}
                   (l/box {:width [:px 0.55] :height [:px 0.55]} (l/text "$" {:tw 0.2 :th 0.2}))
                   (l/box {:width [:px 0.55] :height [:px 0.55]} (l/text "$" {:tw 0.2 :th 0.2}))
                   (l/math "\\leftrightarrow" {:tw 1 :th 0.4})
                   (l/box {:width [:px 0.5] :height [:px 0.6]} (l/text "R" {:tw 0.2 :th 0.2})))
                 (l/row {:width :fill :height [:px 1.0] :spacing 1.0 :align :center-x}
                   (l/box {:padding [0.3 0.15]} (l/math "r=100" {:tw 2 :th 0.6}))
                   (l/box {:padding [0.3 0.15]} (l/math "dM=0" {:tw 2 :th 0.6}))))
          r (resolve* tree)
          rb (get r :desargues.layout/box)]
      ;; root == frame content
      (is (l/within? rb frame))
      (is (<= (:w rb) (:w frame)))
      (is (<= (:h rb) (:h frame)))
      ;; every nested child within its parent content region
      (is (all-bounded? r))
      ;; every ::box in the tree is within the frame
      (is (every? (fn [[_ n]] (l/within? (get n :desargues.layout/box) frame))
                  (l/walk-boxes r))))))

(deftest boundedness-overflow-is-squeezed
  (testing "over-wide shrink siblings are squeezed so the row can never overflow"
    (let [r (resolve-in
             (l/row {:width [:px 4] :height 1}
               (l/box {:width [:px 3] :height :fill} (l/text "x"))
               (l/box {:width [:px 3] :height :fill} (l/text "y"))
               (l/box {:width [:px 3] :height :fill} (l/text "z")))
             {:x 0 :y 0 :w 4 :h 1})]
      (is (all-bounded? r))
      (is (<= (reduce + (map :w (child-boxes r))) 4.0)))))

;; ---------------------------------------------------------------------------
;; Constructor / predicate sanity
;; ---------------------------------------------------------------------------

(deftest constructors-and-predicates
  (is (l/element? (l/text "hi")))
  (is (l/leaf? (l/text "hi")))
  (is (l/container? (l/row {})))
  (is (l/row? (l/row {})))
  (is (l/column? (l/col {})))
  (testing "row accepts a leading attrs map and returns a data-only element"
    (let [x (l/row {:spacing 0.4} (l/text "a") (l/text "b"))]
      (is (= :row (:type x)))
      (is (= {:spacing 0.4} (:attrs x)))
      (is (= 2 (count (:children x))))
      (is (nil? (:content x)))))
  (testing "(apply row attrs kids) flattens a single seq child"
    (let [x (apply l/row {} [(l/text "a") (l/text "b") (l/text "c")])]
      (is (= 3 (count (:children x))))))
  (testing "box with >1 children auto-wraps in a column"
    (let [x (l/box {} (l/text "a") (l/text "b"))]
      (is (= 1 (count (:children x))))
      (is (= :column (:type (first (:children x))))))))
