(ns desargues.tracks.lagrangian-test
  "The Lagrangian teach path: pendulum and spinning top integrated through
   desargues.physics/evolve, written as tracks, each with an energy-drift
   badge. The raster runs are skipped (loudly) without the :dynamics alias."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.backend :as backend]
            [desargues.player.protocols :as pp]
            [desargues.player.track-source :as ts]
            [desargues.tracks :as tracks]
            [desargues.tracks.lagrangian :as tl]
            [desargues.tracks.projection :as projection]))

(def ^:private raster? (backend/available? :raster))

(def drift-bound
  "Stated bound on the RK4 pendulum's energy drift, dt = 0.01 over 10 s,
   theta0 = 1 rad (l = 1 m, g = 9.8): RK4's local error is O(dt^5), so the
   relative drift sits near 1e-8 (measured 9.3e-9 on both :jvm and :raster).
   1e-6 leaves two orders of headroom and still fails any first-order scheme."
  1e-6)

(deftest rk4-pendulum-energy-drift-is-bounded
  (doseq [solver (cond-> [:jvm] raster? (conj :raster))]
    (let [{:keys [drift graph]} (tl/teach :pendulum {:dt 0.01 :duration 10.0 :solver solver})]
      (testing (str solver ": the drift is measured over the whole 10 s run")
        (is (= 1002 (:samples drift)))
        (is (= :relative (:basis drift))))
      (testing (str solver ": max |E(t)-E(0)|/|E(0)| stays under " drift-bound)
        (is (< (:max-relative drift) drift-bound)))
      (testing (str solver ": the badge is a value on every track")
        (is (every? #(= drift (tracks/badge % :energy-drift)) (vals (:tracks graph)))))))
  (when-not raster?
    (println "[tracks.lagrangian-test] raster not on classpath (use -M:test:dynamics); :raster run skipped")))

(deftest raster-agrees-with-the-stepper
  (if raster?
    (let [a (tl/teach :pendulum {:solver :jvm :duration 2.0})
          b (tl/teach :pendulum {:solver :raster :duration 2.0})]
      (is (every? (fn [[x y]] (< (Math/abs (- (get-in x [:state :theta]) (get-in y [:state :theta]))) 1e-8))
                  (map vector (:trajectory a) (:trajectory b)))))
    (is true)))

(deftest pendulum-lesson-writes-tracks-on-a-scene-graph
  (let [{:keys [graph]} (tl/teach :pendulum {:duration 2.0 :every 5})
        bob (get-in graph [:tracks :bob])]
    (is (tracks/valid-tracks? (:tracks graph)))
    (is (= #{:bob :rod} (set (keys (:tracks graph)))))
    (is (every? (:nodes graph) (keys (:tracks graph))) "every track names a node")
    (is (= [] (:steps graph)))
    (testing "the bob stays on the circle of radius l"
      (is (every? (fn [{{[x _ z] :translate} :transform}]
                    (< (Math/abs (- 1.0 (Math/hypot x z))) 1e-9))
                  (:keyframes bob))))
    (testing "thinned tracks still end where the run ends"
      (is (< (Math/abs (- 2.0 (:t (peek (:keyframes bob))))) 0.011)))))

(deftest top-lesson-conserves-energy-and-carries-rotation
  (let [{:keys [drift graph]} (tl/teach :top {:duration 5.0 :every 4})
        top (get-in graph [:tracks :top])]
    (is (< (:max-relative drift) 1e-6))
    (is (tracks/valid-tracks? (:tracks graph)))
    (testing "orientation is a unit quaternion at every key"
      (is (every? #(< (Math/abs (- 1.0 (Math/sqrt (reduce + (map * % %))))) 1e-9)
                  (map (comp :rotate :transform) (:keyframes top)))))
    (testing "the axis tip stays at distance h from the fixed tip"
      (is (every? #(< (Math/abs (- 1.0 (Math/sqrt (reduce + (map * % %))))) 1e-9)
                  (map (comp :translate :transform) (:keyframes top)))))
    (testing "the top precesses: its azimuth moves"
      (let [[x0 y0] (get-in (first (:keyframes top)) [:transform :translate])
            [x1 y1] (get-in (peek (:keyframes top)) [:transform :translate])]
        (is (> (Math/abs (- (Math/atan2 y1 x1) (Math/atan2 y0 x0))) 1e-3))))))

(deftest drift-of-a-constant-is-zero
  (is (= 0.0 (:max-relative (projection/energy-drift [{:time 0 :state 1} {:time 1 :state 1}] (constantly 2.0)))))
  (is (= :absolute (:basis (projection/energy-drift [{:time 0 :state 0}] (constantly 0.0))))))

(deftest lessons-are-a-registry
  (is (= #{:pendulum :top} (into #{} (filter #{:pendulum :top}) (keys (tl/lessons)))))
  (is (thrown? clojure.lang.ExceptionInfo (tl/teach :no-such-lesson))))

(deftest track-source-plays-a-lesson
  (let [{:keys [graph]} (tl/teach :pendulum {:duration 2.0})
        src (ts/track-source graph)
        f (pp/-frame-at src 1.0)
        by-id (into {} (map (juxt :id identity)) (:shapes f))]
    (is (< (Math/abs (- 2.0 (pp/-duration src))) 0.011))
    (is (= [:rod :bob :pivot] (mapv :id (:shapes f))) "draw order follows the graph's :order")
    (is (every? #(= 2 (count %)) [(:at (by-id :bob)) (:to (by-id :rod)) (:at (by-id :pivot))])
        "3D tracks are projected to the stage's 2D")
    (is (= (:at (by-id :bob)) (:to (by-id :rod))) "rod and bob are driven by the same run")
    (is (every? #(= 1.0 (:opacity %)) (:shapes f)))))
