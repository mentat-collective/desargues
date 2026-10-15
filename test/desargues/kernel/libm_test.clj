(ns desargues.kernel.libm-test
  "desargues.kernel.libm's sin/cos forms, evaluated on the JVM against
   java.lang.Math. Pure: no engine. The claims: absolute error at most 2^-53
   for |x| < 1e5, and at most 2 ulps (1 on [-20, 20]) away from the zeros."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.kernel.libm :as libm]))

(def ^:private jsin (eval (list 'fn '[^double x] (libm/trig-form :sin 'x 'Math/floor))))
(def ^:private jcos (eval (list 'fn '[^double x] (libm/trig-form :cos 'x 'Math/floor))))

(defn- ulps
  "|a - b| in units of b's ulp (b's magnitude floored at the smallest normal,
   so a zero reference does not divide by a subnormal ulp)."
  [^double a ^double b]
  (/ (Math/abs (- a b)) (Math/ulp (max Double/MIN_NORMAL (Math/abs b)))))

(def ^:private samples
  (let [rng (java.util.Random. 42)
        quarter (/ Math/PI 4)]
    (concat
     (map #(* % 1e-3) (range -20000 20001))                         ; [-20, 20] densely
     (for [k (range -64 65) d [-1e-12 0.0 1e-12]] (+ (* k quarter) d)) ; quadrant edges
     (repeatedly 20000 #(* 2e5 (- (.nextDouble rng) 0.5)))           ; up to |x| = 1e5
     [0.0 -0.0 1e-300 -1e-300 Double/MIN_VALUE])))

(deftest close-to-math
  ;; Absolute error is what an integration accumulates. Relative error blows
  ;; up only at results within ~1e-15 of a zero of sin/cos (k*pi/2), where
  ;; the 3-part reduction leaves ~1e-30 absolute: 25533 ulps of 6e-19.
  (doseq [[label f g] [["sin" jsin #(Math/sin %)] ["cos" jcos #(Math/cos %)]]]
    (testing label
      (testing "absolute error at most 2^-53 for |x| < 1e5"
        (is (<= (reduce max (map #(Math/abs (- (f %) (g %))) samples)) (Math/pow 2.0 -53))))
      (testing "at most 2 ulps away from the zeros"
        (is (<= (reduce max (map #(ulps (f %) (g %)) (filter #(> (Math/abs (g %)) 1e-14) samples))) 2.0)))
      (testing "at most 1 ulp on [-20, 20]"
        (is (<= (reduce max (map #(ulps (f %) (g %)) (filter #(and (<= (Math/abs %) 20.0) (> (Math/abs (g %)) 1e-14)) samples))) 1.0))))))

(deftest odd-and-even
  (doseq [x [0.3 1.7 2.9 10.0 123.456]]
    (is (== (jsin (- x)) (- (jsin x))))
    (is (== (jcos (- x)) (jcos x)))))

(deftest with-precise-trig-rewrites-every-call
  (let [form '(+ (Math/sin (* 2.0 (Math/cos y))) (Math/cos (- 0.0 y)) (Math/sqrt y))
        heads (fn [f] (set (keep #(when (seq? %) (first %)) (tree-seq coll? seq f))))
        rewritten (libm/with-precise-trig form)
        f (eval (list 'fn '[^double y] rewritten))
        ref (fn [y] (+ (Math/sin (* 2.0 (Math/cos y))) (Math/cos (- 0.0 y)) (Math/sqrt y)))]
    (testing "no Math/sin or Math/cos survives, other calls are untouched"
      (is (not-any? '#{Math/sin Math/cos} (heads rewritten)))
      (is (contains? (heads rewritten) 'Math/sqrt)))
    (testing "and the rewritten form computes the same function"
      (doseq [y [0.1 0.7 1.3 4.2 9.9]]
        (is (< (Math/abs (- (f y) (ref y))) 1e-15))))
    (testing "floor is spelled as the target asks"
      (is (contains? (heads (libm/with-precise-trig '(Math/sin x) 'raster.math/floor)) 'raster.math/floor)))))
