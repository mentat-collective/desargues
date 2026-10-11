(ns desargues.geometry.kernel-test
  "The batched geometry kernels against the Emmy/map reference they replace,
   and every provider against the pure array kernel: the raster provider runs
   only under the :dynamics alias and skips loudly otherwise."
  (:require [clojure.test :refer [deftest is testing]]
            [emmy.env :as e]
            [desargues.geometry.disposition :as d]
            [desargues.geometry.fit :as homography]
            [desargues.geometry.kernel :as k]
            [desargues.geometry.projective :as g]))

(def ^:private raster (k/raster-kernel))

(defn- providers []
  (cond-> [["array" k/array-kernel]]
    raster (conj ["raster" raster])))

(def ^:private figure
  (let [base (:points (g/desargues-configuration
                       {:O [0.0 3.0] :P [1.0 -3.1] :Q [-4.25 -3.1]
                        :angs [-2.25 -1.57 -1.2] :ra 2.0 :ra' 4.3}))
        H (e/* (g/translate-h [16.0 2.0]) (g/scale-h 4.0))]
    (into {} (map (fn [[kk p]] [kk (mapv double (g/apply-h H p))])) base)))

(def ^:private homographies
  [(g/similarity-h [0.3 1.2 -1.0 2.0])
   (g/affine-h [1.1 0.2 -0.3 0.9 0.5 -0.25])
   (g/projective-h [1.0 0.1 0.2 -0.1 0.9 0.3 0.01 -0.02])])

(defn- transform [kernel h xs ys]
  (let [n (alength ^doubles xs)
        ox (double-array n) oy (double-array n) ow (double-array n)]
    (k/transform-cloud! kernel h xs ys ox oy ow)
    [(vec ox) (vec oy) (vec ow)]))

(defn- close? [a b eps]
  (every? (fn [[x y]] (< (Math/abs (- (double x) (double y))) eps)) (map vector a b)))

(defn- reference-cost
  "The fit cost as it was computed before the kernel: Emmy H . p per point."
  [H ps eps]
  (let [ws (mapv (fn [p] (double (get (g/apply-h* H p) 2))) ps)
        m (reduce min (map #(Math/abs ^double %) ws))
        same? (or (every? pos? ws) (every? neg? ws))]
    (+ (homography/frame-cost (mapv #(g/apply-h H %) ps))
       (if same? 0.0 1.0e6)
       (if (< m eps) (* 1.0e6 (- eps m)) 0.0))))

(deftest raster-provider-presence
  (if raster
    (is (satisfies? k/GeometryKernel raster))
    (do (println "[kernel-test] raster not on classpath (use -A:dynamics); raster provider skipped")
        (is (nil? (k/select-kernel :raster))))))

(deftest family-arrays-match-emmy-matrices
  (doseq [[a H] [[(k/similarity-a [0.3 1.2 -1.0 2.0]) (g/similarity-h [0.3 1.2 -1.0 2.0])]
                 [(k/affine-a [1.1 0.2 -0.3 0.9 0.5 -0.25]) (g/affine-h [1.1 0.2 -0.3 0.9 0.5 -0.25])]
                 [(k/projective-a [1 0.1 0.2 -0.1 0.9 0.3 0.01 -0.02])
                  (g/projective-h [1 0.1 0.2 -0.1 0.9 0.3 0.01 -0.02])]]]
    (is (= (vec a) (vec (k/h->array H))))))

(deftest transform-cloud-matches-emmy-apply-h
  (let [[xs ys] (k/cloud figure)
        ps (vec (vals figure))]
    (doseq [[label kernel] (providers)
            H homographies]
      (let [[ox oy ow] (transform kernel (k/h->array H) xs ys)]
        (testing (str label ": affine images")
          (is (close? ox (map #(first (g/apply-h H %)) ps) 1e-12))
          (is (close? oy (map #(second (g/apply-h H %)) ps) 1e-12)))
        (testing (str label ": homogeneous w")
          (is (close? ow (map #(get (g/apply-h* H %) 2) ps) 1e-12)))))))

(deftest batched-path-equals-array-path
  (let [[xs ys] (k/cloud figure)
        a (double-array [0.0 1.0 -2.0 3.5])
        b (double-array [1.0 -1.0 2.0 3.5])]
    (doseq [[label kernel] (providers)]
      (testing (str label ": transform-cloud! is bit-identical to the array kernel")
        (doseq [H homographies]
          (is (= (transform kernel (k/h->array H) xs ys)
                 (transform k/array-kernel (k/h->array H) xs ys)))))
      (testing (str label ": lerp-grid! is bit-identical to the array kernel")
        (is (= (vec (k/lerp-grid! kernel a b (double-array 24) 6))
               (vec (k/lerp-grid! k/array-kernel a b (double-array 24) 6))))))))

(deftest lerp-grid-is-lerp-params
  (let [a [0.0 1.0 -2.0] b [1.0 -1.0 2.5] samples 5
        grid (vec (k/lerp-grid! k/array-kernel (double-array a) (double-array b)
                                (double-array (* 3 samples)) samples))]
    (is (= grid
           (vec (mapcat (fn [i] (g/lerp-params a b (/ (double i) (inc samples))))
                        (range 1 (inc samples))))))))

(deftest h-cloud-cost-matches-reference
  (let [[xs ys] (k/cloud figure)
        ps (vec (vals figure))]
    (doseq [[label kernel] (providers)
            H (conj homographies (g/projective-h [1 0 0 0 1 0 0.2 0 1]))]
      (binding [k/*kernel* kernel]
        (is (< (Math/abs (- (k/h-cloud-cost (k/h->array H) xs ys d/default-frame 0.5
                                            {:oob 100.0 :margin 10.0 :spacing 1.0} 0.05)
                            (reference-cost H ps 0.05)))
               1e-9)
            (str label ": fused cost == Emmy-path cost"))))))

(defn- reference-valid-trajectory?
  "valid-trajectory? as it was: map-level lerp-params per sample."
  [{:keys [derive hard]} frames samples]
  (every? (fn [[a b]]
            (every? (fn [i]
                      (d/hard-ok? hard (derive (d/lerp-params a b (/ (double i) (inc samples))))))
                    (range 1 (inc samples))))
          (partition 2 1 frames)))

(deftest valid-trajectory-matches-reference
  (let [c (d/desargues-disposition)
        good (d/solve-keyframes c 2)
        far {:py -2.2 :qx -2.5 :a3 -0.85 :ra 2.5 :ra' 3.4}]
    (doseq [[label kernel] (providers)
            frames [good (conj good (first good)) [(first good) far] [far (first good)]]]
      (binding [k/*kernel* kernel]
        (is (= (reference-valid-trajectory? c frames 24)
               (d/valid-trajectory? c frames 24))
            (str label ": same verdict as the lerp-params reference"))))))

(deftest solvers-agree-across-providers
  (if raster
    (let [run (fn [kernel]
                (binding [k/*kernel* kernel]
                  {:dof (:dof (homography/fit-homography figure :family :projective))
                   :frames (d/solve-keyframes (d/desargues-disposition) 3)}))
          a (run k/array-kernel)
          r (run raster)]
      (is (close? (:dof a) (:dof r) 1e-9) "fit-homography: raster == array")
      (is (every? true? (map (fn [fa fr]
                               (close? (map fa (keys fa)) (map fr (keys fa)) 1e-9))
                             (:frames a) (:frames r)))
          "solve-keyframes: raster == array"))
    (is true)))
