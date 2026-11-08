(ns desargues.objects.calc-test
  "Calculus objects: values against exact results, Riemann convergence,
   exact readouts, validation, and that every demo frame is valid."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.objects.calc.core :as c :refer [p]]
            [desargues.objects.calc.demos :as demos]
            [desargues.objects.calc.figure :as figure]
            [desargues.objects.calc.numeric :as num]
            [desargues.objects.calc.readout :as readout]
            [desargues.objects.calc.symbolic :as sym]
            [desargues.player.frame :as frame]
            [desargues.player.protocols :as pp]
            [desargues.player.space :as space]
            [desargues.player.space-extras]))

(defn- close? ([a b] (close? a b 1e-6)) ([a b tol] (< (Math/abs (- (double a) (double b))) tol)))

(deftest areas-against-exact
  (is (close? (c/value (c/area-under :parabola 0 2) :jvm) 16/3))
  (testing "1.2 + sin 2x over [-3 3]: sin is odd"
    (is (close? (c/value (c/area-under :wave -3 3) :jvm) 7.2)))
  (testing "x sin x + 2 over [0 pi] = pi + 2 pi"
    (is (close? (c/value (c/area-under :xsinx 0 Math/PI) :jvm) (* 3 Math/PI))))
  (is (close? (c/value (c/area-under :parabola 0 2 {:upto 0.5}) :jvm) 11/3))
  (testing "area between x/2 + 1 and x^2 on [0 1]"
    (is (close? (c/value (c/area-between :root-like :square 0 1) :jvm) (- (+ 1/4 1) 1/3))))
  (testing "FTC: A(x) at x"
    (is (close? (c/value (c/accumulation :square 0 3) :jvm) 9.0))))

(deftest volumes-against-exact
  (testing "disk: y = x/2 + 1 on [0 2] about x"
    (is (close? (c/value (c/revolution :root-like 0 2 :disk) :jvm) (* Math/PI 14/3))))
  (testing "washer: x/2 + 1 outside x^2 on [0 1]"
    (is (close? (c/value (c/revolution :root-like 0 1 :washer {:g :square}) :jvm)
                (* Math/PI (- (+ 1/12 1/2 1) 1/5)))))
  (testing "shell: 4 - x^2 on [0 2] about y = 8 pi"
    (is (close? (c/value (c/revolution :parabola 0 2 :shell) :jvm) (* 8 Math/PI))))
  (testing "double integral of x y over [0 1] x [0 2] = 1"
    (is (close? (c/value (c/double-integral (fn [x y] (* x y)) [0 1] [0 2] 4) :jvm) 1.0))
    (is (close? (c/column-sum (c/double-integral (fn [x y] (* x y)) [0 1] [0 2] 4)) 1.0))))

(deftest line-integral-circulation
  (is (close? (c/value (c/line-integral (fn [[x y]] [(- y) x])
                                        (fn [t] [(Math/cos t) (Math/sin t)])
                                        [0 (* 2 Math/PI)]) :jvm)
              (* 2 Math/PI) 1e-6)))

(deftest riemann-converges
  (let [err (fn [rule n] (Math/abs (- (c/value (c/riemann :parabola 0 2 n {:rule rule}) :jvm) 16/3)))]
    (doseq [rule [:left :right :mid :trapezoid]]
      (testing (name rule)
        (is (> (err rule 4) (err rule 16) (err rule 64)))
        (is (< (err rule 256) 0.02))))
    (testing "midpoint and trapezoid are second order: 4x n, about 16x less error"
      (is (< 14 (/ (err :mid 8) (err :mid 32)) 18))
      (is (< 14 (/ (err :trapezoid 8) (err :trapezoid 32)) 18)))
    (testing "left and right are first order"
      (is (< 3.5 (/ (err :left 64) (err :left 256)) 4.5)))))

(deftest secant-tends-to-tangent
  (let [o (c/secant->tangent :cubic 1.5 1.0)
        slope (c/value (c/tangent-line :cubic 1.5) :jvm)
        gap (fn [u] (Math/abs (- (c/value (assoc o :approach u) :jvm) slope)))]
    (is (close? slope 1.25))
    (is (> (gap 0.0) (gap 0.5) (gap 0.9)))
    (is (< (gap 1.0) 1e-2))))

(deftest exact-readouts
  (is (= 16/3 (readout/exact-value (c/area-under :parabola 0 2))))
  (is (= {:pi-times 14/3} (readout/exact-value (c/revolution :root-like 0 2 :disk))))
  (is (= {:pi-times 8} (readout/exact-value (c/revolution :parabola 0 2 :shell))))
  (is (= 5/4 (readout/exact-value (c/tangent-line :cubic 3/2))))
  (testing "exact Riemann sum equals the numeric one"
    (doseq [rule [:left :right :mid :trapezoid]]
      (let [o (c/riemann :cubic 0 2 7 {:rule rule})]
        (is (close? (readout/exact-value o) (c/value o :jvm) 1e-9)))))
  (is (nil? (readout/exact-value (c/area-under :wave 0 1))) "not a polynomial")
  (is (= "14/3 π" (readout/exact-text (c/revolution :root-like 0 2 :disk)))))

(deftest symbolic-derivatives
  (let [g (sym/fn1 '(* x (sin x)))]
    (is (close? ((:df g) 1.0) (+ (Math/sin 1.0) (Math/cos 1.0))))
    (is (string? (get-in g [:infix :df]))))
  (is (= {0 4 2 -1} (sym/poly '(- 4 (square x)))))
  (is (nil? (sym/poly '(sin x)))))

(deftest numeric-port
  (is (= :jvm (num/-port-name (num/resolve-port :jvm))))
  (testing "an unavailable backend falls back to :jvm"
    (is (#{:jvm :raster} (num/-port-name (num/resolve-port :raster)))))
  (is (close? (num/integrate :jvm #(Math/exp %) 0 1) (- Math/E 1) 1e-12)))

(deftest validation
  (is (= [:riemann/n :riemann/rule] (c/problems (c/riemann :square 0 1 0 {:rule :nope}))))
  (is (= [:solid/g] (c/problems (c/revolution :square 0 1 :washer))))
  (is (= [:solid/shell-negative-x] (c/problems (c/revolution :square -1 1 :shell))))
  (is (some #{{:problem :figure/unset-param}}
            (c/figure-problems (c/figure {:view (c/plane [0 1] [0 1])
                                          :objects [(c/tangent-line :cubic (p :x0))]}))))
  (doseq [[k {:keys [figure]}] demos/demos]
    (testing (name k) (is (c/valid? figure (demos/defaults k))))))

(deftest frames-are-valid
  (doseq [[k {:keys [source-fn params]}] demos/entries
          t [0.0 1.0 2.5 100.0]]
    (testing (str k " at " t)
      (let [f (pp/-frame-at (source-fn params) t)]
        (is (seq (:shapes f)))
        (is (empty? (space/problems f)))
        (is (every? #(or (contains? frame/shape-kinds (:node %)) (space/three-d-shape? %))
                    (:shapes f)))
        (is (every? #(not-any? (fn [x] (and (number? x) (Double/isNaN x)))
                               (flatten (concat (:points %) (:at %) (:from %) (:to %))))
                    (:shapes (space/->2d f))))))))

(deftest widgets-and-clock-move-objects
  (let [{:keys [source-fn params]} (demos/entry :calc-riemann)
        n-of (fn [ps t] (count (filter #(re-find #"riemann/piece" (str (:id %)))
                                       (:shapes (pp/-frame-at (source-fn ps) t)))))]
    (is (< (n-of params 0.0) (n-of params 6.0)) "the clock refines n")
    (is (= 12 (n-of (assoc params :play false :n 12.0) 3.0)) "the slider sets n when paused"))
  (is (re-find #"exact: 16/3"
               (figure/readout-text demos/area-figure (demos/defaults :calc-area) 100))))
