(ns desargues.geometry.fit-test
  "The perspective compute: off-frame figures get fitted inside the frame,
  degenerate homographies are never chosen, and the fit preserves cross
  ratios (it is a projective map, after all)."
  (:require [clojure.test :refer [deftest testing is]]
            [emmy.env :as e]
            [desargues.geometry.disposition :as d]
            [desargues.geometry.fit :as fit]
            [desargues.geometry.projective :as g]))

(defn- desargues-points []
  (:points (g/desargues-configuration
            {:O [0.0 3.0] :P [1.0 -3.1] :Q [-4.25 -3.1]
             :angs [-2.25 -1.57 -1.2] :ra 2.0 :ra' 4.3})))

(defn- transform [pts H]
  (into {} (map (fn [[k p]] [k (mapv double (g/apply-h H p))])) pts))

(defn- min-abs-w [H pts]
  (reduce min (map (fn [p] (Math/abs (double (get (g/apply-h* H p) 2)))) (vals pts))))

(deftest off-frame-config-gets-fitted-inside
  (let [base (desargues-points)
        ;; push the whole figure off-frame: translate +16 in x, blow up x4
        H-push (e/* (g/translate-h [16.0 2.0]) (g/scale-h 4.0))
        off (transform base H-push)
        before (fit/frame-cost off)
        {:keys [H]} (fit/fit-homography off :family :similarity)
        fitted (transform off H)
        after (fit/frame-cost fitted)]
    (testing "the off-frame figure really is off-frame"
      (is (not (d/within-frame? off))))
    (testing "the fit lands it back inside, with margin"
      (is (d/within-frame? fitted))
      (is (< after before))
      (is (< after 10.0)))))

(deftest degenerate-h-is-never-chosen
  (let [off (transform (desargues-points)
                       (e/* (g/translate-h [16.0 2.0]) (g/scale-h 4.0)))]
    (doseq [family [:similarity :affine :projective]]
      (let [{:keys [H cost]} (fit/fit-homography off :family family)]
        (testing (str family ": every figure point keeps w bounded away from 0")
          (is (>= (min-abs-w H off) 0.05)))
        (testing (str family ": the fitted cost beats any degenerate map")
          (is (< cost 1.0e6)))))))

(deftest fit-preserves-cross-ratio
  (let [base (desargues-points)
        off (transform base (e/* (g/translate-h [16.0 2.0]) (g/scale-h 4.0)))
        {:keys [H]} (fit/fit-homography off :family :projective)
        fitted (transform off H)
        ;; four collinear figure points: P, Q, R and B on the axis/sides
        cr-before (g/cross-ratio (:P base) (:Q base) (:R base)
                                 (g/meet-lines (:B base) (:C base)
                                               (:P base) (:Q base)))
        cr-after (g/cross-ratio (:P fitted) (:Q fitted) (:R fitted)
                                (g/meet-lines (:B fitted) (:C fitted)
                                              (:P fitted) (:Q fitted)))]
    (is (< (Math/abs (- (double cr-before) (double cr-after))) 1e-6))))

(deftest fit-keyframes-stay-fitted
  (let [c1 (desargues-points)
        c2 (:points (g/desargues-configuration
                     {:O [0.0 3.0] :P [1.0 -2.3] :Q [-3.5 -3.1]
                      :angs [-2.25 -1.57 -1.05] :ra 2.0 :ra' 4.3}))
        push (fn [pts dx] (transform pts (g/translate-h [dx 0.0])))
        fits (fit/fit-keyframes [(push c1 15.0) (push c2 15.0)]
                                :family :similarity :continuity 20.0)]
    (is (= 2 (count fits)))
    (doseq [[{:keys [H]} pts] (map vector fits [(push c1 15.0) (push c2 15.0)])]
      (is (d/within-frame? (transform pts H)))
      (is (>= (min-abs-w H pts) 0.05)))))
