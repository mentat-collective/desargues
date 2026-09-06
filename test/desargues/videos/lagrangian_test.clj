(ns desargues.videos.lagrangian-test
  "Emmy-derived systems behave like physics: energy is conserved, rods keep
   their length, and the state contract matches the integrators."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.videos.lagrangian :as lag]
            [desargues.videos.physics :as phys]))

(defn- dist [[x1 y1 z1] [x2 y2 z2]]
  (Math/sqrt (+ (Math/pow (- x2 x1) 2) (Math/pow (- y2 y1) 2) (Math/pow (- z2 z1) 2))))

(deftest planar-double-pendulum-conserves-energy
  (let [sys  (lag/double-pendulum {:l1 1.0 :l2 1.0 :theta1 2.0 :theta2 2.5})
        L    (lag/lagrangian-from-positions [1 1] (:positions sys) 9.8)
        traj (phys/evolve sys {:dt 0.002 :duration 1.0})
        E    (mapv #(double (lag/energy sys L (:state %))) traj)]
    (testing "state labels are the coordinates and their rates"
      (is (= [:theta1 :theta2 :theta1-dot :theta2-dot] (phys/state-labels sys))))
    (testing "501 samples, energy drift under 0.1 percent with RK4"
      (is (= 501 (count traj)))
      (is (< (/ (- (apply max E) (apply min E)) (Math/abs (first E))) 1e-3)))
    (testing "the compiled derivative agrees with the uncompiled one"
      (let [raw (lag/double-pendulum {:l1 1.0 :l2 1.0 :theta1 2.0 :theta2 2.5 :compile? false})
            s   (phys/get-state sys)
            a   (phys/derivatives sys s)
            b   (phys/derivatives raw s)]
        (is (every? #(< (Math/abs (- (a %) (b %))) 1e-8) (keys a)))))
    (testing "the rods keep their lengths"
      (doseq [{:keys [state]} (take-nth 50 traj)]
        (let [[p1 p2] (lag/positions-at sys state)]
          (is (< (Math/abs (- 1.0 (dist [0 0 0] p1))) 1e-9))
          (is (< (Math/abs (- 1.0 (dist p1 p2))) 1e-9)))))))

(deftest spherical-double-pendulum-moves-in-three-dimensions
  (let [sys  (lag/spherical-double-pendulum {})
        traj (phys/evolve sys {:dt 0.005 :duration 0.5})
        ps   (map #(lag/positions-at sys (:state %)) traj)]
    (is (= 8 (count (phys/state-labels sys))))
    (testing "the second bob leaves the plane it started in"
      (is (> (apply max (map (fn [[_ p2]] (Math/abs (nth p2 1))) ps)) 0.05)))
    (testing "both rods keep their lengths along the way"
      (doseq [[p1 p2] (take-nth 20 ps)]
        (is (< (Math/abs (- 1.0 (dist [0 0 0] p1))) 1e-9))
        (is (< (Math/abs (- 1.0 (dist p1 p2))) 1e-9))))))
