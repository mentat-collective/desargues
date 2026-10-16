(ns desargues.tracks.lagrangian
  "APPLICATION stratum of the tracks context: the Emmy Lagrangian teach path.

   Each lesson is a Lagrangian L(t, q, q') handed to the physics domain's
   lagrangian-system, integrated by whichever solver the caller names through
   desargues.physics/evolve (:solver :raster integrates on raster's RK4 under
   the :dynamics alias; :jvm, the default, is the built-in RK4 stepper), and
   written as Tracks (desargues.tracks.projection) on a scene graph whose
   :nodes say what each tracked node is. Every track carries the run's
   energy-drift badge: max |E(t) - E(0)| / |E(0)|, from the same L.

   A lesson is DATA: {:system-fn :L-fn :keyer :nodes ...} registered in
   `lessons`; a new lesson is one `deflesson`, never an edit to `teach`
   (OCP). Coordinates are physics units, z up; the picture's projection is
   the consumer's (desargues.player.track-source).

   JVM .clj: lagrangian-system compiles primitive kernels with eval."
  (:require [desargues.physics :as phys]
            [desargues.tracks.core :as core]
            [desargues.tracks.projection :as projection]
            [emmy.env :as e :refer [up]]))

;; ── lesson registry (OCP) ───────────────────────────────────────────────────

(defonce ^:private registry (atom {}))

(defn register-lesson!
  "Register lesson `k`: {:title str :defaults {} :build (fn [params] {:system
   :L :keyer :nodes})}. :build returns the PhysicalSystem, its Lagrangian (for
   the energy), the keyer state -> {node-id keyframe-body} and the scene-graph
   :nodes the keyer drives. Returns k."
  [k lesson]
  (swap! registry assoc k lesson)
  k)

(defn lessons
  "{lesson-key lesson} of every registered lesson."
  []
  @registry)

;; ── the teach path ──────────────────────────────────────────────────────────

(defn teach
  "Run lesson `k` with `params` (merged over its :defaults): integrate
   through evolve, project onto tracks, badge every track with the energy
   drift. params also take :dt (0.01), :duration (10.0), :solver (:jvm) and
   :every (keep every n-th point on the tracks, default 1; the drift is
   always measured on the full run).

   Returns {:graph {:scene :nodes :steps [] :tracks} :drift {..} :trajectory
   :system :L}: the graph is a desargues scene graph with :tracks next to
   :nodes and :steps."
  ([k] (teach k {}))
  ([k params]
   (let [{:keys [title defaults build] :as lesson} (get @registry k)
         _ (when-not lesson
             (throw (ex-info (str "Unknown lesson " (pr-str k)) {:lesson k :known (keys @registry)})))
         {:keys [dt duration solver every] :or {dt 0.01 duration 10.0 solver :jvm every 1}
          :as params} (merge defaults params)
         {:keys [system L keyer nodes]} (build params)
         traj (vec (phys/evolve system {:dt dt :duration duration :solver solver}))
         drift (assoc (projection/energy-drift traj #(phys/energy system L %))
                      :solver solver :dt dt :duration duration)
         tracks (projection/with-drift-badge
                  (projection/trajectory->tracks traj keyer {:every every
                                                             :meta {:lesson k :solver solver}})
                  drift)]
     {:graph {:scene title :nodes nodes :steps [] :tracks tracks}
      :drift drift
      :trajectory traj
      :system system
      :L L})))

(defn drift-badge
  "The energy-drift badge a teach result's tracks carry (the same value on
   every track of the run)."
  [{:keys [graph]}]
  (core/badge (val (first (:tracks graph))) :energy-drift))

;; ── geometry helpers ────────────────────────────────────────────────────────

(defn- v+ [a b] (mapv + a b))
(defn- v* [s a] (mapv #(* s %) a))

(defn- qmul
  "Hamilton product of quaternions [w x y z]."
  [[aw ax ay az] [bw bx by bz]]
  [(- (* aw bw) (* ax bx) (* ay by) (* az bz))
   (+ (* aw bx) (* ax bw) (* ay bz) (- (* az by)))
   (+ (* aw by) (- (* ax bz)) (* ay bw) (* az bx))
   (+ (* aw bz) (* ax by) (- (* ay bx)) (* az bw))])

(defn- axis-quat [[x y z] angle]
  (let [h (* 0.5 angle) s (Math/sin h)]
    [(Math/cos h) (* s x) (* s y) (* s z)]))

(defn euler-zxz->quat
  "Unit quaternion of the z-x-z Euler rotation (precession phi about z, nutation
   theta about the line of nodes, spin psi about the body axis)."
  [phi theta psi]
  (qmul (qmul (axis-quat [0 0 1] phi) (axis-quat [1 0 0] theta)) (axis-quat [0 0 1] psi)))

(defn- rotate
  "Vector v rotated by unit quaternion q."
  [q v]
  (let [[w x y z] q
        conj-q [w (- x) (- y) (- z)]]
    (vec (rest (qmul (qmul q (into [0.0] v)) conj-q)))))

;; ── lesson: the simple pendulum ─────────────────────────────────────────────

(defn pendulum-system
  "A planar pendulum of length l, bob mass m, in gravity g (x-z plane, z up,
   pivot at the origin) from Emmy's L = T - V; coordinate :theta from the
   downward vertical. Returns {:system :L}."
  [{:keys [l m g theta0 omega0] :or {l 1.0 m 1.0 g 9.8 theta0 1.0 omega0 0.0}}]
  (let [bob (fn [[th]] (up (e/* l (e/sin th)) 0 (e/- (e/* l (e/cos th)))))
        L (phys/lagrangian-from-positions [m] [bob] g)]
    {:L L
     :system (phys/lagrangian-system {:L L :labels [:theta] :positions [bob]
                                      :q0 [theta0] :qdot0 [omega0]
                                      :cache-key [::pendulum l m g]})}))

(register-lesson!
 :pendulum
 {:title "Pendulum"
  :defaults {:l 1.0 :m 1.0 :g 9.8 :theta0 1.0 :omega0 0.0}
  :build (fn [{:keys [l] :as params}]
           (let [{:keys [system L]} (pendulum-system params)]
             {:system system
              :L L
              :nodes {:pivot {:node :dot :id :pivot :at [0.0 0.0 0.0] :opts {:color :grey}}
                      :rod   {:node :line :id :rod :from [0.0 0.0 0.0] :to [0.0 0.0 (- l)]
                              :opts {:color :grey :width 4}}
                      :bob   {:node :circle :id :bob :opts {:radius 0.12 :color :gold}
                              :fill {:color :gold :opacity 0.8}}}
              :keyer (fn [state]
                       (let [[p] (phys/positions-at system state)]
                         {:bob {:transform {:translate p}}
                          :rod {:shape-params {:from [0.0 0.0 0.0] :to p}}}))}))})

;; ── lesson: the heavy symmetric (spinning) top ──────────────────────────────

(defn top-lagrangian
  "L of a heavy symmetric top on a fixed tip, Euler angles q = (theta phi psi):
   A, C the transverse and axial moments about the tip, M g h the weight times
   the tip-to-centre distance.
     L = A/2 (theta'^2 + phi'^2 sin^2 theta) + C/2 (psi' + phi' cos theta)^2
         - M g h cos theta"
  [A C M g h]
  (fn [[_t [theta _phi _psi] [thetadot phidot psidot]]]
    (e/- (e/+ (e/* (/ A 2) (e/+ (e/square thetadot)
                                (e/* (e/square phidot) (e/square (e/sin theta)))))
              (e/* (/ C 2) (e/square (e/+ psidot (e/* phidot (e/cos theta))))))
         (e/* M g h (e/cos theta)))))

(defn top-system
  "A heavy symmetric top: Emmy derives its motion from top-lagrangian. The
   axis tip of the drawing is at distance h along the body axis. Returns
   {:system :L}. theta = 0 is a coordinate singularity of the Euler angles:
   start tilted (theta0 > 0)."
  [{:keys [A C M g h theta0 phi0 psi0 thetadot0 phidot0 psidot0]
    :or {A 1.0 C 0.5 M 1.0 g 9.8 h 1.0 theta0 0.5 phi0 0.0 psi0 0.0
         thetadot0 0.0 phidot0 0.0 psidot0 20.0}}]
  (let [L (top-lagrangian A C M g h)
        axis (fn [[theta phi _]] (up (e/* h (e/sin theta) (e/sin phi))
                                     (e/- (e/* h (e/sin theta) (e/cos phi)))
                                     (e/* h (e/cos theta))))]
    {:L L
     :system (phys/lagrangian-system {:L L :labels [:theta :phi :psi] :positions [axis]
                                      :q0 [theta0 phi0 psi0] :qdot0 [thetadot0 phidot0 psidot0]
                                      :cache-key [::top A C M g h]})}))

(register-lesson!
 :top
 {:title "Spinning top"
  :defaults {:A 1.0 :C 0.5 :M 1.0 :g 9.8 :h 1.0 :theta0 0.5 :phi0 0.0 :psi0 0.0
             :thetadot0 0.0 :phidot0 0.0 :psidot0 20.0 :r 0.35}
  :build (fn [{:keys [h r] :as params}]
           (let [{:keys [system L]} (top-system params)]
             {:system system
              :L L
              :nodes {:tip    {:node :dot :id :tip :at [0.0 0.0 0.0] :opts {:color :grey}}
                      :axis   {:node :line :id :axis :from [0.0 0.0 0.0] :to [0.0 0.0 h]
                               :opts {:color :blue :width 4}}
                      :top    {:node :circle :id :top :opts {:radius 0.1 :color :teal}
                               :fill {:color :teal :opacity 0.8}}
                      :marker {:node :line :id :marker :from [0.0 0.0 h] :to [r 0.0 h]
                               :opts {:color :red :width 3}}}
              :keyer (fn [{:keys [theta phi psi]}]
                       (let [q (euler-zxz->quat phi theta psi)
                             c (rotate q [0.0 0.0 h])
                             rim (v+ c (v* r (rotate q [1.0 0.0 0.0])))]
                         {:top    {:transform {:translate c :rotate q}}
                          :axis   {:shape-params {:from [0.0 0.0 0.0] :to c}}
                          :marker {:shape-params {:from c :to rim}}}))}))})
