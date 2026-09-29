(ns desargues.geometry.projective-test
  "Property-based and proof tests for the projective layer: incidence
   round-trips, cross-ratio invariance under random invertible homographies,
   and the symbolic Desargues/Pappus proofs (determinants that must simplify
   to zero)."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [emmy.env :as e]
            [desargues.geometry.projective :as g]))

;; =============================================================================
;; Generators
;; =============================================================================

(def gen-coord (gen/double* {:min -5.0 :max 5.0 :NaN? false :infinite? false}))
(def gen-point (gen/vector gen-coord 2))

(def gen-line-config
  "Four points in general position (no join passing near-degenerate is
   excluded by construction of the checks below: joins only need two distinct
   points)."
  (gen/such-that (fn [[p q r s]] (and (not= p q) (not= r s)))
                 (gen/tuple gen-point gen-point gen-point gen-point)
                 100))

(def gen-collinear-four
  "A, B, and C, D on the line AB as A + t(B - A), with STRUCTURAL margins (no
   filtering): B is at least 0.2 from A in x, t1 is bounded away from 0 and 1,
   t2 is negative and bounded away from 0 — so all four points are distinct
   with room. A cross ratio with coincident points is 0/0, and near-coincident
   points are numeric poison; sized test.check doubles make a such-that
   distinctness filter fragile."
  (gen/let [A gen-point
            dx (gen/one-of [(gen/double* {:min -2.0 :max -0.2 :NaN? false :infinite? false})
                            (gen/double* {:min 0.2 :max 2.0 :NaN? false :infinite? false})])
            dy gen-coord
            t1 (gen/one-of [(gen/double* {:min 0.15 :max 0.85 :NaN? false :infinite? false})
                            (gen/double* {:min 1.15 :max 3.0 :NaN? false :infinite? false})])
            t2 (gen/double* {:min -3.0 :max -0.15 :NaN? false :infinite? false})]
    (let [B [(+ (A 0) dx) (+ (A 1) dy)]
          at (fn [t] [(+ (A 0) (* t dx)) (+ (A 1) (* t dy))])]
      [A B (at t1) (at t2)])))

(def gen-entry (gen/double* {:min -2.0 :max 2.0 :NaN? false :infinite? false}))
(def gen-perspective (gen/double* {:min -0.002 :max 0.002 :NaN? false :infinite? false}))

(def gen-homography-and-points
  "An invertible 3x3 H and four collinear points, built constructively rather
   than filtered: H = a similarity (scale s >= 0.5, so |det| = s² >= 0.25)
   followed by a tiny perspective (|g|,|h| <= 0.002, h33 = 1, so |w - 1| <=
   0.25 even for the largest generated points — nothing comes near the line at
   infinity). test.check's doubles are sized, so a such-that filter on
   |det| goes unsatisfiable as the size grows; this generator never filters."
  (gen/let [theta (gen/double* {:min -3.0 :max 3.0 :NaN? false :infinite? false})
            s (gen/double* {:min 0.5 :max 2.0 :NaN? false :infinite? false})
            tx gen-entry ty gen-entry
            g1 gen-perspective h1 gen-perspective
            pts gen-collinear-four]
    [(e/* (g/similarity-h [theta s tx ty])
          (e/matrix-by-rows [1.0 0.0 0.0] [0.0 1.0 0.0] [g1 h1 1.0]))
     pts]))

;; =============================================================================
;; Incidence properties
;; =============================================================================

(defspec join-contains-both-points 50
  (prop/for-all [[p q _ _] gen-line-config]
                (let [l (g/join p q)]
                  (and (g/on? p l) (g/on? q l)))))

(defspec meet-lies-on-both-lines 50
  (prop/for-all [[p q r s] gen-line-config]
                (let [x (g/meet (g/join p q) (g/join r s))]
                  (and (g/on? x (g/join p q))
                       (g/on? x (g/join r s))))))

(defspec meet-lines-is-the-affine-chart-of-meet 50
  (prop/for-all [[p q r s] gen-line-config]
                (let [x (g/meet (g/join p q) (g/join r s))
                      w (double (get x 2))]
                  (or (< (Math/abs w) 1e-9) ;; parallel: no affine intersection
                      (let [[ax ay] (g/meet-lines p q r s)
                            rx (/ (double (get x 0)) w)
                            ry (/ (double (get x 1)) w)]
                        (and (< (Math/abs (- ax rx)) (* 1e-6 (max 1.0 (Math/abs ax) (Math/abs rx))))
                             (< (Math/abs (- ay ry)) (* 1e-6 (max 1.0 (Math/abs ay) (Math/abs ry))))))))))

(defspec collinear-det-agrees-with-construction 50
  (prop/for-all [[A B C D] gen-collinear-four]
                (and (g/collinear? A B C)
                     (g/collinear? A B D)
                     (g/collinear? B C D))))

;; =============================================================================
;; Cross-ratio invariance
;; =============================================================================

(defn- approx= [a b tol]
  (< (Math/abs (- (double a) (double b)))
     (* tol (max 1.0 (Math/abs (double a)) (Math/abs (double b))))))

(defspec cross-ratio-invariant-under-homography 50
  (prop/for-all [[H [A B C D]] gen-homography-and-points]
                (approx= (g/cross-ratio A B C D)
                         (g/cross-ratio (g/apply-h H A) (g/apply-h H B)
                                        (g/apply-h H C) (g/apply-h H D))
                         1e-6)))

;; =============================================================================
;; Desargues configuration: the theorem holds numerically
;; =============================================================================

(defspec desargues-axis-points-stay-collinear 30
  (prop/for-all [ra (gen/double* {:min 0.5 :max 3.0 :NaN? false :infinite? false})
                 ra' (gen/double* {:min 3.5 :max 6.0 :NaN? false :infinite? false})
                 qx (gen/double* {:min -6.5 :max -2.5 :NaN? false :infinite? false})
                 a3 (gen/double* {:min -1.3 :max -0.8 :NaN? false :infinite? false})]
                (let [cfg (g/desargues-configuration
                           {:O [0.0 3.0] :P [1.0 -3.1] :Q [qx -3.1]
                            :angs [-2.25 -1.57 a3] :ra ra :ra' ra'})
                      {:keys [P Q R]} (:points cfg)]
                  (g/collinear? P Q R 1e-6))))

;; =============================================================================
;; Homography families and conics, unit checks
;; =============================================================================

(deftest families-compose-to-expected-maps
  (testing "similarity-h rotates, scales and translates as declared"
    (let [H (g/similarity-h [(/ Math/PI 2) 2.0 1.0 -1.0])
          [x y] (g/apply-h H [1.0 0.0])]
      (is (approx= x 1.0 1e-9))
      (is (approx= y 1.0 1e-9))))
  (testing "affine-h shears"
    (let [H (g/affine-h [1.0 1.0 0.0 1.0 0.0 0.0])]
      (is (approx= (first (g/apply-h H [1.0 2.0])) 3.0 1e-9))))
  (testing "h-lerp endpoints"
    (let [H (g/translate-h [3.0 4.0])]
      (is (approx= (first (g/apply-h (g/h-lerp g/I3 H 0.0) [0.0 0.0])) 0.0 1e-9))
      (is (approx= (first (g/apply-h (g/h-lerp g/I3 H 1.0) [0.0 0.0])) 3.0 1e-9)))))

(deftest conic-incidence-and-duality
  (let [C (g/conic [1 0 1 0 0 -1])] ;; the unit circle x² + y² - w² = 0
    (testing "points on/off the unit circle"
      (is (g/on-conic? C [0.6 0.8]))
      (is (not (g/on-conic? C [1.0 1.0]))))
    (testing "tangent at [1 0] is the line x = w"
      (let [t (g/tangent C [1.0 0.0])]
        (is (g/on? [1.0 0.0] t))
        (is (g/on? [1.0 5.0] t))
        (is (not (g/on? [0.0 0.0] t)))))
    (testing "pole of the polar returns the point"
      (let [p [0.6 0.8]
            pole (g/pole C (g/polar C p))
            [px py] (g/dehomog pole)]
        (is (approx= px 0.6 1e-6))
        (is (approx= py 0.8 1e-6))))))

;; =============================================================================
;; Symbolic proofs: determinants that must simplify to zero
;; =============================================================================

(deftest desargues-proof-simplifies-to-zero
  (is (e/zero? (:desargues g/proofs))))

(deftest pappus-proof-simplifies-to-zero
  (is (e/zero? (:pappus g/proofs))))
