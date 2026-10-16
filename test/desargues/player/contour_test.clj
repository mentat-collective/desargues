(ns desargues.player.contour-test
  "Marching squares: a paraboloid's level curves are closed circles of the
   right radius, a saddle's are open hyperbola branches, and chaining is
   exact (shared edge crossings are identical points)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.player.contour :as contour]))

(defn- paraboloid [x y] (+ (* x x) (* y y)))

(deftest a-paraboloid-has-circles
  (let [g (contour/sample paraboloid [-2 2] [-2 2] 40)]
    (doseq [r [0.5 1.0 1.5 1.9]]
      (testing (str "radius " r)
        (let [ls (contour/lines g (* r r))]
          (is (= 1 (count ls)) "one curve per level")
          (is (contour/closed? (first ls)) "a closed loop: last point = first point")
          (is (every? (fn [[x y]] (< (Math/abs (- (Math/sqrt (paraboloid x y)) r)) 0.01)) (first ls))
              "every vertex on the circle")
          (is (> (count (first ls)) 12) "sampled all the way round"))))))

(deftest a-saddle-has-open-branches
  (let [g  (contour/sample (fn [x y] (- (* x x) (* y y))) [-2 2] [-2 2] 40)
        ls (contour/lines g 1.0)]
    (is (= 2 (count ls)) "x^2 - y^2 = 1 has two branches")
    (is (not-any? contour/closed? ls) "each ends on the domain's edge")
    (is (every? (fn [line] (every? (fn [[x y]] (< (Math/abs (- (* x x) (* y y) 1.0)) 0.05)) line)) ls))
    (is (= #{-1.0 1.0} (set (map (fn [line] (Math/signum (double (ffirst line)))) ls))))))

(deftest levels-lie-inside-the-range
  (let [g (contour/sample paraboloid [-2 2] [-2 2] 10)
        [lo hi] (contour/z-range g)]
    (is (= [0.0 8.0] [lo hi]))
    (is (= [2.0 4.0 6.0] (contour/levels g 3)))
    (is (empty? (contour/lines g 9.0)) "a level above the surface has no curve")))

(deftest chaining
  (is (= [[[1 0] [1 1] [0 0] [1 0]]] (contour/chain [[[1 0] [1 1]] [[0 0] [1 0]] [[1 1] [0 0]]]))
      "three segments of a triangle join into one closed loop")
  (is (= [[[0 0] [1 0] [2 0]] [[5 5] [6 6]]] (contour/chain [[[0 0] [1 0]] [[5 5] [6 6]] [[1 0] [2 0]]]))
      "open runs stay apart"))

(defspec circles-are-closed-for-any-radius-inside 40
  (prop/for-all [r (gen/double* {:min 0.3 :max 1.95 :NaN? false :infinite? false})
                 n (gen/choose 12 50)]
    (let [ls (contour/lines (contour/sample paraboloid [-2 2] [-2 2] n) (* r r))]
      (and (= 1 (count ls)) (contour/closed? (first ls))))))
