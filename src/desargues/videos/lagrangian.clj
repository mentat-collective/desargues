(ns desargues.videos.lagrangian
  "PhysicalSystems whose equations of motion Emmy derives from a Lagrangian.

   A system is L(t, q, q') over generalized coordinates q; the Cartesian
   positions of its masses are functions of q. Emmy turns L into a state
   derivative; desargues integrates it with any TrajectorySolver and renders
   the positions. Ships the planar and the spherical (3D) double pendulum."
  (:require [desargues.videos.physics :as phys]
            [emmy.env :as e :refer [up]]
            [emmy.expression.compile :as compile]
            [emmy.mechanics.lagrange :as lagrange]))

;; ── Lagrangians ─────────────────────────────────────────────────────────────

(defn lagrangian-from-positions
  "L = T - V for point masses `masses` whose Cartesian positions are
   `positions`, each a fn of the coordinate tuple q -> (up x y z), in uniform
   gravity `g` along -z. Velocities come from the Jacobian of each position."
  [masses positions g]
  (fn [[_t q qdot]]
    (let [T (reduce e/+ 0
                    (map (fn [m p]
                           (let [v (e/* ((e/D p) q) qdot)]
                             (e/* 1/2 m (e/dot-product v v))))
                         masses positions))
          V (reduce e/+ 0
                    (map (fn [m p] (e/* m g (nth (p q) 2)))
                         masses positions))]
      (e/- T V))))

;; ── the system record ───────────────────────────────────────────────────────

(def ^:private dot-label
  "label -> label-dot (memoized: it is pure, and it runs per state per step)."
  (memoize (fn [k] (keyword (str (name k) "-dot")))))

(defn- state-tuple [labels s]
  (let [q    (mapv #(double (get s %)) labels)
        qdot (mapv #(double (get s (dot-label %))) labels)]
    (up 0.0 (apply up q) (apply up qdot))))

(defn- labelled
  "{l0 a0 .. dl0 b0 ..} in labels-then-dot-labels order (what merging the two
   zipmaps built), values coerced with double."
  [labels as bs]
  (let [n (count labels)]
    (loop [i 0 m (transient {}) as (seq as)]
      (if (< i n)
        (recur (inc i) (assoc! m (nth labels i) (double (first as))) (next as))
        (loop [i 0 m m bs (seq bs)]
          (if (< i n)
            (recur (inc i) (assoc! m (dot-label (nth labels i)) (double (first bs))) (next bs))
            (persistent! m)))))))

(defrecord LagrangianSystem [labels positions state-derivative q qdot]
  phys/PhysicalSystem
  (get-state [_]
    (labelled labels q qdot))
  (set-state [this s]
    (assoc this
           :q    (mapv #(double (get s %)) labels)
           :qdot (mapv #(double (get s (dot-label %))) labels)))
  (derivatives [_ s]
    (let [[_ dq ddq] (seq (state-derivative (state-tuple labels s)))]
      (labelled labels dq ddq)))
  (state-labels [_]
    (vec (concat labels (map dot-label labels)))))

(defn positions-at
  "Cartesian [x y z] of every mass of `system` in `state` (a state map)."
  [{:keys [labels positions]} state]
  (let [q (apply up (map #(double (get state %)) labels))]
    (mapv (fn [p] (mapv double (seq (p q)))) positions)))

(def ^:private compiled-cache
  "cache-key -> compiled state derivative (see lagrangian-system :cache-key)."
  (atom {}))

(defn lagrangian-system
  "A PhysicalSystem from Lagrangian `L` (a fn of the (up t q q') local tuple).
   labels name the coordinates (their velocities are label-dot); positions are
   the mass position fns of q used by positions-at; q0 / qdot0 are the initial
   values. :compile? (default true) compiles the derived state derivative with
   Emmy's compiler, simplification off, before the first step.

   :cache-key (optional) names the system up to its initial state: compiled
   derivatives are memoized under [cache-key (count q0)], so a second system
   with the same key reuses the compiled fn. Two Lagrangians must share a key
   only when they are the same function of the local tuple."
  [{:keys [L labels positions q0 qdot0 compile? cache-key] :or {compile? true}}]
  (let [st0 (up 0.0 (apply up (map double q0)) (apply up (map double qdot0)))
        build (fn []
                (let [sd (lagrange/Lagrangian->state-derivative L)
                      compiled (compile/compile-state-fn (fn [] sd) [] st0 {:simplify? false})]
                  (fn [state] (compiled state []))))
        sd  (cond
              (not compile?) (lagrange/Lagrangian->state-derivative L)
              (nil? cache-key) (build)
              :else (let [k [cache-key (count q0)]]
                      (or (get @compiled-cache k)
                          (get (swap! compiled-cache
                                      (fn [c] (if (contains? c k) c (assoc c k (build)))))
                               k))))]
    (->LagrangianSystem (vec labels) (vec positions) sd (mapv double q0) (mapv double qdot0))))

;; ── double pendulums ────────────────────────────────────────────────────────

(defn double-pendulum
  "Planar double pendulum in the x-z plane (z up, pivot at the origin):
   coordinates theta1 theta2 from the vertical. opts: :l1 :l2 :m1 :m2 :g
   :theta1 :theta2 :omega1 :omega2 :compile?"
  [{:keys [l1 l2 m1 m2 g theta1 theta2 omega1 omega2 compile?]
    :or {l1 1.0 l2 1.0 m1 1.0 m2 1.0 g 9.8 theta1 2.0 theta2 2.5 omega1 0.0 omega2 0.0 compile? true}}]
  (let [p1 (fn [[th1 _]] (up (e/* l1 (e/sin th1)) 0 (e/- (e/* l1 (e/cos th1)))))
        p2 (fn [[_ th2 :as q]] (e/+ (p1 q) (up (e/* l2 (e/sin th2)) 0 (e/- (e/* l2 (e/cos th2))))))]
    (lagrangian-system {:L (lagrangian-from-positions [m1 m2] [p1 p2] g), :labels [:theta1 :theta2], :positions [p1 p2], :q0 [theta1 theta2], :qdot0 [omega1 omega2], :compile? compile?, :cache-key [:desargues.videos.lagrangian/double-pendulum l1 l2 m1 m2 g]})))

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
    (lagrangian-system {:L (lagrangian-from-positions [m1 m2] [p1 p2] g), :labels [:theta1 :phi1 :theta2 :phi2], :positions [p1 p2], :q0 [theta1 phi1 theta2 phi2], :qdot0 [theta1-dot phi1-dot theta2-dot phi2-dot], :compile? compile?, :cache-key [:desargues.videos.lagrangian/spherical-double-pendulum l1 l2 m1 m2 g]})))

(defn energy
  "Total energy T + V of a system with coordinate `labels` in `state`, from the
   Lagrangian `L` it was built from."
  [{:keys [labels]} L state]
  ((lagrange/Lagrangian->energy L) (state-tuple labels state)))
