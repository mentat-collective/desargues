(ns desargues.geometry.disposition-test
  "The disposition solver: logic tier finds feasible dispositions, the polish
  improves spacing, keyframes carry valid trajectories, and everything is
  deterministic given fixed domains."
  (:require [clojure.test :refer [deftest testing is]]
            [desargues.geometry.projective :as g]
            [desargues.geometry.disposition :as d]))

;; =============================================================================
;; Constraint vocabulary
;; =============================================================================

(deftest constraint-vocabulary
  (testing "within-frame?"
    (is (d/within-frame? {:a [0 0] :b [7.0 3.9]}))
    (is (not (d/within-frame? {:a [0 0] :b [7.2 0]})))
    (is (not (d/within-frame? {:a [0 -4.1]}))))
  (testing "spacing and min-spacing?"
    (is (= 1.0 (d/spacing [[0 0] [1 0] [3 0]])))
    (is (d/min-spacing? [[0 0] [1 0] [3 0]] 0.5))
    (is (not (d/min-spacing? [[0 0] [1 0] [3 0]] 1.5))))
  (testing "non-collinear?"
    (is (d/non-collinear? [0 0] [1 0] [0 1]))
    (is (not (d/non-collinear? [0 0] [1 0] [2 0]))))
  (testing "between?"
    (is (d/between? [0 0] [1 0] [2 0]))
    (is (not (d/between? [0 0] [3 0] [2 0])))
    (is (not (d/between? [0 0] [0 0] [2 0]))))
  (testing "no-label-overlap?"
    (is (d/no-label-overlap? [[0 0] [2 0]] [1.0 0.5]))
    (is (not (d/no-label-overlap? [[0 0] [0.5 0.1]] [1.0 0.5])))))

;; =============================================================================
;; Solving one disposition
;; =============================================================================

(deftest solve-disposition-satisfies-hard-constraints
  (let [{:keys [params points min-spacing] :as sol}
        (d/solve-disposition (d/desargues-disposition))]
    (testing "the solved points satisfy every hard constraint"
      (is (d/hard-ok? (:hard (d/desargues-disposition)) points)))
    (testing "the polish pushes min-spacing past the 0.45 floor"
      (is (>= min-spacing 0.45))
      (is (> min-spacing 0.5)))
    (testing "R still lands on the axis (the theorem survives solving)"
      (is (g/collinear? (:P points) (:Q points) (:R points) 1e-6)))
    (testing "params decode into the declared domains"
      (is (<= -3.2 (:py params) -2.2))
      (is (<= -6.5 (:qx params) -2.5))
      (is (<= -1.35 (:a3 params) -0.85))
      (is (<= 1.2 (:ra params) 2.5))
      (is (<= 3.4 (:ra' params) 4.6)))))

(deftest solve-is-deterministic
  (let [a (:params (d/solve-disposition (d/desargues-disposition)))
        b (:params (d/solve-disposition (d/desargues-disposition)))]
    (is (= a b))))

(deftest solve-fails-loudly-on-infeasible-domains
  (is (thrown? clojure.lang.ExceptionInfo
               (d/solve-disposition
                (d/desargues-disposition :min-spacing 5.0)))))

;; =============================================================================
;; Keyframes and trajectories
;; =============================================================================

(deftest solve-keyframes-carry-valid-trajectories
  (let [c (d/desargues-disposition)
        frames (d/solve-keyframes c 3 :closed? true :min-param-dist 1.0)]
    (testing "three distinct keyframes"
      (is (= 3 (count frames)))
      (is (= 3 (count (distinct frames)))))
    (testing "each solved keyframe satisfies the hard constraints"
      (doseq [f frames]
        (is (d/hard-ok? (:hard c) ((:derive c) f)))))
    (testing "every sampled interpolated frame satisfies them too"
      (is (d/valid-trajectory? c (conj frames (first frames)) 24)))
    (testing "keyframes differ by at least the param distance"
      (doseq [[a b] (partition 2 1 frames)]
        (is (not= a b))))))

(deftest lerp-params-interpolates-structure
  (is (= {:O [0.0 3.0] :ra 1.5}
         (d/lerp-params {:O [0.0 3.0] :ra 1.0} {:O [0.0 3.0] :ra 2.0} 0.5))))
