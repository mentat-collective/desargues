(ns desargues.player.view-test
  "The 2D viewport, all pure: zoom about the pointer keeps the point under
   it, pans follow the pointer, the scale is clamped, and frames are seen
   through a view with geometry scaled and markers kept."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.player.svg-path :as svg-path]
            [desargues.player.view :as view]))

(def ^:private coord (gen/double* {:min -20.0 :max 20.0 :NaN? false :infinite? false}))

(def ^:private scale (gen/double* {:min 0.1 :max 20.0 :NaN? false :infinite? false}))

(def ^:private factor (gen/double* {:min 0.2 :max 5.0 :NaN? false :infinite? false}))

(defn- close?
  ([a b] (close? a b 1e-7))
  ([a b tol] (every? #(< (Math/abs (double %)) tol) (map - a b))))

(def ^:private gen-view
  (gen/fmap (fn [[cx cy k]] {:center [cx cy] :scale k}) (gen/tuple coord coord scale)))

(defspec seen-and-source-are-inverse 200
  (prop/for-all [v gen-view x coord y coord]
    (and (close? [x y] (view/->source v (view/->seen v [x y])))
         (close? [x y] (view/->seen v (view/->source v [x y]))))))

(defspec zoom-keeps-the-point-under-the-pointer 300
  (prop/for-all [v gen-view m factor qx coord qy coord]
    (let [before (view/->source v [qx qy])
          v'     (view/zoom-at v m [qx qy])]
      (close? before (view/->source v' [qx qy]) 1e-6))))

(defspec zoom-is-clamped 200
  (prop/for-all [v gen-view m (gen/double* {:min 1e-6 :max 1e6 :NaN? false :infinite? false})]
    (let [[lo hi] view/scale-limits
          k (:scale (view/zoom-at v m [0 0]))]
      (<= lo k hi))))

(defspec pan-moves-the-picture-with-the-pointer 200
  (prop/for-all [v gen-view x coord y coord dx coord dy coord]
    (let [p  (view/->source v [x y])
          v' (view/pan v [dx dy])]
      (close? [(+ x dx) (+ y dy)] (view/->seen v' p) 1e-6))))

(deftest zoom-in-then-out-returns
  (let [v (-> nil (view/zoom-at 2.0 [1.0 1.0]) (view/zoom-at 0.5 [1.0 1.0]))]
    (is (close? [0.0 0.0] (:center v)))
    (is (close? [1.0] [(:scale v)]))
    (is (view/identity? nil))
    (is (not (view/identity? {:scale 2.0})))))

(defn- shown [s] (assoc s :opacity 1.0 :draw 1.0))

(def ^:private fr
  {:t 0 :shapes (mapv shown [{:id 1 :node :dot :at [1 0] :opts {:radius 0.1}}
                             {:id 2 :node :circle :at [0 1] :opts {:radius 2.0}}
                             {:id 3 :node :line :from [0 0] :to [1 1] :stroke {:width 4}}
                             {:id 4 :node :rectangle :at [0 0]}
                             {:id 5 :node :text :text "A" :at [1 1] :opts {:font-size 30}}
                             {:id 6 :node :path :points [[0 0] [1 0] [1 1]]}
                             {:id 7 :node :svg-path :d "M 0 0 L 1 0 Q 1 1 0 1 Z"}])})

(deftest frames-through-a-view
  (let [v {:center [1.0 0.0] :scale 2.0}
        g (view/seen-frame v fr)
        by-id (into {} (map (juxt :id identity)) (:shapes g))]
    (testing "positions go through the view"
      (is (close? [0.0 0.0] (:at (by-id 1))))
      (is (close? [-2.0 2.0] (:at (by-id 2))))
      (is (close? [-2.0 0.0] (:from (by-id 3))))
      (is (close? [0.0 2.0] (:to (by-id 3))))
      (is (= [[-2.0 0.0] [0.0 0.0] [0.0 2.0]] (:points (by-id 6)))))
    (testing "geometry scales, markers and strokes keep their size"
      (is (= 4.0 (get-in by-id [2 :opts :radius])))
      (is (= [8.0 4.0] [(get-in by-id [4 :opts :width]) (get-in by-id [4 :opts :height])])
          "a rectangle's default extent scales too")
      (is (= 0.1 (get-in by-id [1 :opts :radius])) "a dot stays a dot")
      (is (= 30 (get-in by-id [5 :opts :font-size])) "text keeps its size")
      (is (= 4 (get-in by-id [3 :stroke :width]))))
    (testing "an SVG path is rewritten point by point"
      (is (= [[:move -2.0 0.0] [:line 0.0 0.0] [:quad 0.0 2.0 -2.0 2.0] [:close]]
             (svg-path/segments (:d (by-id 7))))))
    (testing "the identity view leaves the frame alone"
      (is (identical? fr (view/seen-frame nil fr)))
      (is (identical? fr (view/seen-frame view/identity-view fr))))))

(deftest events-go-back-to-source-units
  (let [v {:center [1.0 1.0] :scale 4.0}]
    (is (= {:type :down :at [1.5 1.0] :hit 3}
           (view/source-event v {:type :down :at [2.0 0.0] :hit 3})))
    (is (= {:type :key :key "0"} (view/source-event v {:type :key :key "0"})))))
