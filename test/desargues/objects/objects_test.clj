(ns desargues.objects.objects-test
  "The object vocabulary: the coordinate transform round-trips, plot samples
   lie on the function, Riemann sums converge, and every object records
   through the RecordingBackend with matching step counts."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.objects :as o]
            [desargues.objects.axes :as ax]
            [desargues.objects.calculus :as calc]
            [desargues.objects.coords :as c]
            [desargues.objects.field :as fd]
            [desargues.objects.parts :as parts]
            [desargues.objects.plot :as pl]
            [desargues.objects.sample :as sample]
            [desargues.player :as player]
            [desargues.player.timeline :as tl]
            [desargues.scene :as s]))

(def ^:private gen-num (gen/double* {:min -50.0 :max 50.0 :NaN? false :infinite? false}))

(def ^:private gen-range
  (gen/fmap (fn [[a w]] [a (+ a w)])
            (gen/tuple gen-num (gen/double* {:min 0.1 :max 40.0 :NaN? false :infinite? false}))))

(def ^:private gen-cs
  (gen/fmap (fn [[xr yr xl yl cx cy]]
              (c/coord-system {:x-range xr :y-range yr :x-length xl :y-length yl :center [cx cy]}))
            (gen/tuple gen-range gen-range
                       (gen/double* {:min 1.0 :max 14.0 :NaN? false :infinite? false})
                       (gen/double* {:min 1.0 :max 8.0 :NaN? false :infinite? false})
                       (gen/double* {:min -3.0 :max 3.0 :NaN? false :infinite? false})
                       (gen/double* {:min -2.0 :max 2.0 :NaN? false :infinite? false}))))

(defn- close? [tol a b] (<= (Math/abs (- (double a) (double b))) tol))
(defn- close-pt? [tol p q] (every? true? (map #(close? tol %1 %2) p q)))

(defspec axes-transform-round-trips 200
  (prop/for-all [cs gen-cs x gen-num y gen-num]
    (and (close-pt? 1e-6 [x y] (c/->math cs (c/->scene cs [x y])))
         (close-pt? 1e-6 [x y] (c/->scene cs (c/->math cs [x y]))))))

(defspec axes-map-the-box-onto-its-lengths 200
  (prop/for-all [cs gen-cs]
    (let [[x0 x1] (:x-range cs) [y0 y1] (:y-range cs)
          [sx0 sy0] (c/->scene cs [x0 y0]) [sx1 sy1] (c/->scene cs [x1 y1])]
      (and (close? 1e-6 (:x-length cs) (- sx1 sx0))
           (close? 1e-6 (:y-length cs) (- sy1 sy0))
           (close-pt? 1e-6 (:center cs) [(/ (+ sx0 sx1) 2) (/ (+ sy0 sy1) 2)])))))

(def ^:private functions
  {"sin"   #(Math/sin %)
   "cubic" #(- (* 0.1 % % %) %)
   "exp"   #(Math/exp (* 0.5 %))
   "gauss" #(* 3 (Math/exp (- (* % %))))})

(defspec plot-samples-lie-on-the-function 60
  (prop/for-all [fname (gen/elements (keys functions))
                 a (gen/double* {:min -4.0 :max 0.0 :NaN? false :infinite? false})
                 w (gen/double* {:min 0.5 :max 4.0 :NaN? false :infinite? false})]
    (let [f  (functions fname)
          cs (c/coord-system {:x-range [-4 4 1] :y-range [-6 6 1] :x-length 10 :y-length 6})
          runs (pl/runs (pl/plot cs f {:domain [a (+ a w)] :tol 0.005}))]
      (and (seq runs)
           (every? (fn [run]
                     (every? (fn [p] (let [[x y] (c/->math cs p)] (close? 1e-9 (f x) y))) run))
                   runs)
           ;; between samples the polyline stays within tol (scene units) of the curve
           (every? (fn [run]
                     (every? (fn [[p q]]
                               (let [[xm _] (c/->math cs (parts/v* 0.5 (parts/v+ p q)))
                                     sm (c/->scene cs [xm (f xm)])
                                     chord (sample/lerp-run run (first sm))]
                                 (or (nil? chord) (close? 0.02 (second sm) chord))))
                             (partition 2 1 run)))
                   runs)))))

(deftest plot-spans-its-domain-and-splits-at-poles
  (let [cs (c/coord-system {:x-range [-4 4 1] :y-range [-3 3 1]})]
    (testing "endpoints are sampled exactly"
      (let [[run] (pl/runs (pl/plot cs #(* % %) {:domain [-1 1.5]}))]
        (is (close-pt? 1e-9 [-1 1] (c/->math cs (first run))))
        (is (close-pt? 1e-9 [1.5 2.25] (c/->math cs (peek run))))))
    (testing "tan is split into one run per branch, no vertical jump"
      (is (= 3 (count (pl/runs (pl/plot cs #(Math/tan %)))))))
    (testing "sqrt is undefined left of 0: the run starts near 0"
      (let [[run] (pl/runs (pl/plot cs #(Math/sqrt %) {:domain [-2 2]}))]
        (is (close? 0.05 0 (first (c/->math cs (first run)))))))
    (testing "a curvier function gets more samples"
      (is (< (count (first (pl/runs (pl/plot cs identity))))
             (count (first (pl/runs (pl/plot cs #(Math/sin (* 3 %)))))))))))

(defspec parametric-and-polar-samples-lie-on-the-curve 30
  (prop/for-all [r (gen/double* {:min 0.5 :max 2.0 :NaN? false :infinite? false})]
    (let [cs (c/coord-system {:x-range [-3 3 1] :y-range [-3 3 1] :x-length 6 :y-length 6})
          on-circle? (fn [p] (close? 1e-6 r (parts/norm (c/->math cs p))))]
      (and (every? on-circle? (apply concat (pl/runs (pl/parametric cs (fn [t] [(* r (Math/cos t)) (* r (Math/sin t))])
                                                                    [0 (* 2 Math/PI)]))))
           (every? on-circle? (apply concat (pl/runs (pl/polar cs (constantly r)))))))))

(deftest axes-parts-carry-ticks-labels-and-tex-names
  (let [cs (c/coord-system {:x-range [-3 3 1] :y-range [-2 2 1]})
        ps (o/parts-of (ax/axes cs {:x-label "x" :y-label "f(x)"}))
        by (group-by :prim ps)]
    (is (= 2 (count (filter #(= :axis (second (:key %))) ps))))
    (is (= (+ 6 4) (count (filter #(= :tick (second (:key %))) ps))) "ticks skip the origin")
    (is (= #{"x" "f(x)"} (set (map :tex (:tex by)))))
    (is (some #(= "-3" (:text %)) (:text by)))
    (is (< (count ps) (count (o/parts-of (ax/number-plane cs)))))))

(deftest vector-field-arrows-follow-the-field
  (let [cs (c/coord-system {:x-range [-3 3 1] :y-range [-3 3 1] :x-length 6 :y-length 6})
        f  (fn [[x y]] [(- y) x])
        vs (fd/field-vectors (fd/vector-field cs f))]
    (is (= 24 (count vs)) "5x5 interior grid minus the zero vector at the origin")
    (doseq [[p from to] vs]
      (let [d (parts/v- to from) v (f p)]
        (is (close? 1e-9 0 (- (* (first d) (second v)) (* (second d) (first v))))
            "arrow is parallel to the field")
        (is (<= (parts/norm d) 0.9000001) "the longest arrow fits one cell")))))

(deftest tangent-secant-and-riemann
  (let [cs (c/coord-system {:x-range [-3 3 1] :y-range [-1 9 1]})
        f  #(* % %)]
    (is (close? 1e-6 3.0 (calc/tangent-slope (calc/tangent cs f 1.5))))
    (is (close? 1e-9 2.5 (calc/secant-slope (calc/secant cs f 1 0.5))))
    (testing "the secant slope tends to the tangent slope"
      (is (< (Math/abs (- (calc/secant-slope (calc/secant cs f 1 1e-4)) 2.0)) 1e-3)))
    (testing "Riemann sums converge to the integral, left below and right above"
      (let [sum #(calc/riemann-sum (calc/riemann cs f 0 2 %1 {:rule %2}))]
        (is (< (sum 50 :left) (/ 8.0 3) (sum 50 :right)))
        (is (close? 1e-3 (/ 8.0 3) (sum 200 :midpoint)))))
    (is (= 8 (count (o/parts-of (calc/riemann cs f 0 2 8)))))))

(deftest every-object-records-through-the-facade
  (let [cs    (c/coord-system {:x-range [-3 3 1] :y-range [-2 2 1]})
        f     #(Math/sin %)
        specs [(ax/axes cs {:x-label "x"}) (ax/number-plane cs) (pl/plot cs f)
               (pl/parametric cs (fn [t] [(Math/cos t) (Math/sin t)]) [0 6])
               (pl/polar cs (fn [t] (Math/sin (* 2 t)))) (fd/vector-field cs (fn [[x y]] [y (- x)]))
               (fd/arrow [0 0] [1 1]) (calc/point-on-curve cs f 1 {:label "P"})
               (calc/tangent cs f 1) (calc/secant cs f 1 0.5) (calc/riemann cs f 0 2 6)
               (calc/area cs f 0 2)]]
    (doseq [spec specs]
      (let [g (player/record "Obj" (fn [st] (o/add! st spec)))]
        (is (= 1 (count (:steps g))) (str (:object spec)))
        (is (= (count (o/parts-of spec)) (count (:nodes g))) (str (:object spec)))))))

(deftest a-tex-label-is-its-own-node-kind
  (let [g (player/record "Tex" (fn [st] (s/play! st (s/appear (s/tex "\\int_0^1 x\\,dx")))))]
    (is (= :tex (:node (get (:nodes g) 1))))
    (is (= "\\int_0^1 x\\,dx" (:tex (get (:nodes g) 1))))
    (is (= 1.0 (:opacity (first (:shapes (tl/frame-at g (tl/schedule g) 2.0))))))))

(deftest sweeping-a-parameter-animates-in-place
  (let [cs (c/coord-system {:x-range [-3 3 1] :y-range [-1 9 1]})
        f  #(* % %)
        g  (player/record "Sweep"
                          (fn [st] (let [h (o/add! st (calc/tangent cs f -1))]
                                     (o/sweep! st h #(calc/tangent cs f %) [0 1 2]))))]
    (is (= 4 (count (:steps g))))
    (is (= 2 (count (:nodes g))) "the same line and dot move; nothing is re-created")
    (is (= #{:connect :glide} (set (map :anim (mapcat :children (mapcat :anims (rest (:steps g))))))))))
