(ns desargues.videos.physics-rk4-test
  "The all-Double RK4 path must land on the same doubles as the generic
   map-merging step it short-circuits (golden bits recorded from v0.2.0)."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.videos.physics :as phys]))

(defn- hex-state [state]
  (update-vals state #(Long/toHexString (Double/doubleToRawLongBits %))))

(deftest rk4-golden-bits
  (testing "damped pendulum, 500 steps"
    (is (= {:theta "bfd94474205ca27c" :omega "c003c44ff70ebb4e"}
           (hex-state (:state (last (phys/evolve (phys/make-pendulum {:damping 0.1 :initial-theta 1.2})
                                                 {:dt 0.01 :duration 5.0})))))))
  (testing "damped spring, 150 steps"
    (is (= {:x "3fe65f18b3abdf41" :v "3fdb364b06505725"}
           (hex-state (:state (last (phys/evolve (phys/make-spring-mass {:initial-x 1.0 :mu 0.2 :k 4.0})
                                                 {:dt 0.02 :duration 3.0}))))))))

(deftest rk4-generic-path-still-serves-non-doubles
  (testing "a rational dt or integer state takes the generic step unchanged"
    (let [traj (phys/evolve (phys/make-pendulum {:initial-theta 1}) {:dt 1/50 :duration 1})]
      (is (= 51 (count traj)))
      (is (every? number? (map (comp :theta :state) traj))))))
