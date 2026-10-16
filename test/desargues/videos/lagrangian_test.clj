(ns desargues.videos.lagrangian-test
  "Emmy-derived systems behave like physics: energy is conserved, rods keep
   their length, and the state contract matches the integrators."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.physics :as lag]
            [desargues.physics :as phys]))

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

(deftest compiled-derivative-cache
  (testing "same physical params reuse the compiled state derivative"
    (let [a (lag/double-pendulum {:theta1 1.0})
          b (lag/double-pendulum {:theta1 0.5 :omega2 1.0})]
      (is (identical? (:state-derivative a) (:state-derivative b)))
      (is (= [0.5 2.5] (:q b)))))
  (testing "different physics never share one"
    (is (not (identical? (:state-derivative (lag/double-pendulum {:l1 1.0}))
                         (:state-derivative (lag/double-pendulum {:l1 2.0})))))
    (is (not (identical? (:state-derivative (lag/double-pendulum {:g 9.8}))
                         (:state-derivative (lag/double-pendulum {:g 9.81}))))))
  (testing "a cached system integrates exactly like a freshly compiled one"
    (let [fresh (lag/lagrangian-system
                 (let [s (lag/double-pendulum {})]
                   {:L (lag/lagrangian-from-positions [1.0 1.0] (:positions s) 9.8)
                    :labels [:theta1 :theta2] :positions (:positions s)
                    :q0 [2.0 2.5] :qdot0 [0.0 0.0]}))
          cached (lag/double-pendulum {})
          run #(mapv :state (phys/evolve % {:dt 0.01 :duration 0.5}))]
      (is (= (run fresh) (run cached))))))

(defn- step-loop
  "evolve without the ArraySystem fast path: the map-based StepperSolver loop."
  [sys {:keys [dt duration]}]
  (loop [s sys t 0.0 acc [{:time 0.0 :state (phys/get-state sys)}]]
    (if (>= t duration)
      acc
      (let [s' (phys/step phys/rk4 s dt) t' (+ t dt)]
        (recur s' t' (conj acc {:time t' :state (phys/get-state s')}))))))

(deftest primitive-kernel-path-is-bit-identical
  (doseq [[label sys opts] [["planar" (lag/double-pendulum {}) {:dt 0.01 :duration 8.0}]
                            ["spherical" (lag/spherical-double-pendulum
                                          {:l1 1.4 :l2 1.2 :theta1 1.3 :phi1 0.0 :theta2 1.6 :phi2 0.8
                                           :phi1-dot 1.4 :phi2-dot 0.9})
                             {:dt 0.01 :duration 8.0}]]]
    (testing (str label ": array RK4 loop == map RK4 loop, bit for bit")
      (is (some? (phys/array-kernel sys)))
      (is (= (step-loop sys opts) (phys/evolve sys opts))))
    (testing (str label ": compiled positions == Emmy positions, bit for bit")
      (let [plain (dissoc sys :position-kernels)]
        (doseq [{:keys [state]} (take-nth 37 (phys/evolve sys opts))]
          (is (= (lag/positions-at plain state) (lag/positions-at sys state))))))
    (testing (str label ": the structured state derivative matches the kernel")
      (let [s (phys/get-state sys)]
        (is (= (phys/derivatives sys s)
               (phys/derivatives (assoc sys :kernel nil) s)))))))
