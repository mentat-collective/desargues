(ns desargues.player.camera-path-test
  "Camera keyframes: the interpolated camera passes through every keyframe,
   clamps outside the path, moves continuously, and a fly-around turns once."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.player.camera-path :as cp]
            [desargues.player.space :as space]))

(defn- close? [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(def ^:private path
  [{:t 0.0 :camera {:yaw 0.0 :pitch 0.3 :distance 10.0 :target [0 0 0]}}
   {:t 2.0 :camera {:yaw 1.0 :pitch 0.6 :distance 8.0 :target [1 0 0]}}
   {:t 5.0 :camera {:yaw 3.0 :pitch 0.4 :distance 12.0 :target [0 1 0.5] :projection :orthographic}}])

(deftest passes-through-keyframes
  (doseq [{:keys [t camera]} path
          :let [c (cp/camera-at path t)]]
    (testing (str "t = " t)
      (doseq [k [:yaw :pitch :distance]]
        (is (close? (get camera k) (get c k) 1e-12)))
      (is (every? true? (map #(close? %1 %2 1e-12) (:target camera) (:target c)))))))

(deftest clamps-and-steps-projection
  (is (= (dissoc (cp/camera-at path -3.0) :projection) (dissoc (cp/camera-at path 0.0) :projection)))
  (is (= (cp/camera-at path 99.0) (cp/camera-at path 5.0)))
  (is (= :perspective (:projection (cp/camera-at path 4.9))) "fields are inherited from earlier keyframes")
  (is (= :orthographic (:projection (cp/camera-at path 5.0))))
  (is (nil? (cp/camera-at [] 1.0)))
  (is (= 10.0 (:distance (cp/camera-at [(first path)] 7.0))) "a one-keyframe path is a still"))

(deftest unsorted-keyframes-are-sorted
  (is (= (cp/camera-at path 1.3) (cp/camera-at (reverse path) 1.3))))

(defspec moves-continuously 200
  (prop/for-all [t (gen/double* {:min 0.0 :max 4.99 :NaN? false :infinite? false})]
    (let [a (cp/camera-at path t) b (cp/camera-at path (+ t 1e-6))]
      (every? #(close? (get a %) (get b %) 1e-4) [:yaw :pitch :distance]))))

(defspec every-interpolated-camera-is-valid 100
  (prop/for-all [t (gen/double* {:min -1.0 :max 30.0 :NaN? false :infinite? false})]
    (let [fly (cp/fly-around {:t0 0 :t1 20 :turns 1.0})]
      (empty? (space/camera-problems (cp/camera-at fly t))))))

(deftest fly-around
  (let [fly (cp/fly-around {:t0 0.0 :t1 12.0 :yaw -0.9 :pitch 0.5 :distance 11.0 :turns 1.0 :n 6})]
    (is (= 7 (count fly)))
    (is (close? -0.9 (:yaw (cp/camera-at fly 0.0)) 1e-12))
    (is (close? (+ -0.9 (* 2 Math/PI)) (:yaw (cp/camera-at fly 12.0)) 1e-12) "one full turn")
    (is (close? 0.5 (:pitch (cp/camera-at fly 12.0)) 1e-12) "back to the starting pitch")
    (is (< (:distance (cp/camera-at fly 6.0)) 11.0) "closer in mid-flight")
    (is (apply < (map #(:yaw (cp/camera-at fly %)) [0.5 3.0 6.0 9.0 11.5])) "the yaw keeps turning")))

(deftest zoom-is-carried-only-when-present
  (is (not (contains? (cp/camera-at path 1.0) :zoom)) "no keyframe zooms: no :zoom, so it stays 1")
  (let [zp (assoc-in path [2 :camera :zoom] 3.0)]
    (is (close? 1.0 (:zoom (cp/camera-at zp 0.0)) 1e-12) "missing zoom counts as 1")
    (is (close? 3.0 (:zoom (cp/camera-at zp 5.0)) 1e-12))
    (is (< 1.0 (:zoom (cp/camera-at zp 4.0)) 3.0) "and is interpolated between")))
