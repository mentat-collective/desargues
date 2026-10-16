(ns desargues.physics.pendulums
  "CORE stratum of the physics bounded context: the catalog of Lagrangian
   systems shipped with desargues, the planar and the spherical (3D) double
   pendulum. A new system is one constructor here or in its own namespace,
   built on desargues.physics.lagrangian/lagrangian-system.

   Requires desargues.physics.lagrangian and Emmy. JVM .clj."
  (:require [desargues.physics.lagrangian :as lagrangian]
            [emmy.env :as e :refer [up]]))

(defn double-pendulum
  "Planar double pendulum in the x-z plane (z up, pivot at the origin):
   coordinates theta1 theta2 from the vertical. opts: :l1 :l2 :m1 :m2 :g
   :theta1 :theta2 :omega1 :omega2 :compile?"
  [{:keys [l1 l2 m1 m2 g theta1 theta2 omega1 omega2 compile?]
    :or {l1 1.0 l2 1.0 m1 1.0 m2 1.0 g 9.8 theta1 2.0 theta2 2.5 omega1 0.0 omega2 0.0 compile? true}}]
  (let [p1 (fn [[th1 _]] (up (e/* l1 (e/sin th1)) 0 (e/- (e/* l1 (e/cos th1)))))
        p2 (fn [[_ th2 :as q]] (e/+ (p1 q) (up (e/* l2 (e/sin th2)) 0 (e/- (e/* l2 (e/cos th2))))))]
    (lagrangian/lagrangian-system
     {:L (lagrangian/lagrangian-from-positions [m1 m2] [p1 p2] g)
      :labels [:theta1 :theta2]
      :positions [p1 p2]
      :q0 [theta1 theta2]
      :qdot0 [omega1 omega2]
      :compile? compile?
      :cache-key [::double-pendulum l1 l2 m1 m2 g]})))

(defn spherical-double-pendulum
  "Two chained spherical pendulums (z up, pivot at the origin): each bob has a
   polar angle theta from the downward vertical and an azimuth phi. opts: :l1
   :l2 :m1 :m2 :g :theta1 :phi1 :theta2 :phi2 and the four initial rates
   :theta1-dot :phi1-dot :theta2-dot :phi2-dot, :compile?

   theta = 0 and theta = pi are coordinate singularities (the mass matrix has
   sin theta on its diagonal): a trajectory that reaches a pole turns to NaN.
   Give each bob some azimuthal rate and it stays clear of them."
  [{:keys [l1 l2 m1 m2 g theta1 phi1 theta2 phi2 theta1-dot phi1-dot theta2-dot phi2-dot compile?]
    :or {l1 1.0 l2 1.0 m1 1.0 m2 1.0 g 9.8
         theta1 1.0 phi1 0.0 theta2 2.0 phi2 1.0
         theta1-dot 0.0 phi1-dot 0.8 theta2-dot 0.0 phi2-dot -0.4 compile? true}}]
  (let [bob (fn [l th ph] (up (e/* l (e/sin th) (e/cos ph))
                              (e/* l (e/sin th) (e/sin ph))
                              (e/- (e/* l (e/cos th)))))
        p1  (fn [[th1 ph1 _ _]] (bob l1 th1 ph1))
        p2  (fn [[_ _ th2 ph2 :as q]] (e/+ (p1 q) (bob l2 th2 ph2)))]
    (lagrangian/lagrangian-system
     {:L (lagrangian/lagrangian-from-positions [m1 m2] [p1 p2] g)
      :labels [:theta1 :phi1 :theta2 :phi2]
      :positions [p1 p2]
      :q0 [theta1 phi1 theta2 phi2]
      :qdot0 [theta1-dot phi1-dot theta2-dot phi2-dot]
      :compile? compile?
      :cache-key [::spherical-double-pendulum l1 l2 m1 m2 g]})))
