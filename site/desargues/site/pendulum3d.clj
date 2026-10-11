(ns desargues.site.pendulum3d
  "The landing page's 3D double pendulum: Emmy-derived spherical double
   pendulum, integrated by RK4, filmed by an orthographic camera that orbits
   slowly about the vertical axis. Bobs are filled discs whose radius scales
   with depth; the second bob's trail and a floor ring are re-projected through
   the moving camera every sample."
  (:require [desargues.scene :as s]
            [desargues.scene.data :as rec]
            [desargues.videos.lagrangian :as lag]
            [desargues.videos.physics :as phys]))

(def spherical
  "Two chained spherical pendulums; Emmy derives the equations of motion from
   L = T - V, desargues integrates them with RK4. Initial conditions keep both
   polar angles away from the coordinate poles for the whole 8 s."
  (lag/spherical-double-pendulum {:l1 1.4 :l2 1.2 :theta1 1.3 :phi1 0.0 :theta2 1.6 :phi2 0.8
                                  :phi1-dot 1.4 :phi2-dot 0.9}))

(def camera
  "Elevation, scale and pivot are fixed; the azimuth starts at :az0 and turns
   at :omega rad/s. :depth-gain sets how strongly depth changes a bob's size."
  {:az0 0.7 :omega 0.12 :el 0.45 :scale 1.15 :origin [0 2.3] :depth-gain 0.16})

(defn project
  "Orthographic view of [x y z] (z up) from azimuth az and elevation el, scaled
   and hung from `origin`: [X Y depth], depth positive towards the viewer."
  [[x y z] az el scale [ox oy]]
  (let [ca (Math/cos az) sa (Math/sin az) ce (Math/cos el) se (Math/sin el)
        xr (- (* x ca) (* y sa))
        yr (+ (* x sa) (* y ca))]
    [(+ ox (* scale xr))
     (+ oy (* scale (+ (* z ce) (* yr se))))
     (- (* z se) (* yr ce))]))

(defn view-at
  "The camera at time t: a fn from a world point to [X Y depth]."
  [{:keys [az0 omega el scale origin]} t]
  (fn [p] (project p (+ az0 (* omega t)) el scale origin)))

(defn depth-radius
  "Bob radius r0 scaled by depth: nearer is bigger, clamped to [0.6, 1.5] x r0."
  [r0 depth gain]
  (* r0 (max 0.6 (min 1.5 (+ 1.0 (* gain depth))))))

(defn disc
  "A closed n-gon approximating the circle of radius r about [cx cy]."
  ([center r] (disc center r 28))
  ([[cx cy] r n]
   (mapv (fn [k] (let [a (/ (* 2 Math/PI k) n)]
                   [(+ cx (* r (Math/cos a))) (+ cy (* r (Math/sin a)))]))
         (range n))))

(defn floor-ring
  "The circle of radius R on the horizontal plane z = h, as 3D points."
  [R h n]
  (mapv (fn [k] (let [a (/ (* 2 Math/PI k) n)] [(* R (Math/cos a)) (* R (Math/sin a)) h]))
        (range n)))

(defn frames
  "One map per 0.1 s sample: the camera time and both bobs in world coordinates."
  [system duration]
  (->> (phys/evolve system {:dt 0.01 :duration duration})
       (take-nth 10)
       (map-indexed (fn [i {:keys [state]}]
                      {:t (* 0.1 i) :bobs (lag/positions-at system state)}))
       vec))

(defn shot
  "Everything the scene draws at one frame, in screen coordinates."
  [cam ring history {:keys [t bobs]}]
  (let [view (view-at cam t)
        [[x1 y1 d1] [x2 y2 d2]] (mapv view bobs)
        xy (fn [[x y _]] [x y])]
    {:p1 [x1 y1] :p2 [x2 y2]
     :disc1 (disc [x1 y1] (depth-radius 0.22 d1 (:depth-gain cam)))
     :disc2 (disc [x2 y2] (depth-radius 0.26 d2 (:depth-gain cam)))
     :trail (mapv (comp xy view) history)
     :ring (mapv (comp xy view) ring)}))

(defn construct
  "Two rods, two depth-sized bobs, the second bob's trail and a floor ring,
   one sample per 0.1 s while the camera orbits."
  [stage]
  (let [cam    camera
        origin (:origin cam)
        fs     (frames spherical 8.0)
        ring   (floor-ring 2.0 -2.6 48)
        shots  (mapv (fn [i] (shot cam ring (map (comp second :bobs) (subvec fs 0 (inc i))) (nth fs i)))
                     (range (count fs)))
        s0     (first shots)
        title  (-> (s/text "3D double pendulum: Emmy derives the equations, RK4 integrates" :font-size 28 :color :white)
                   (s/move-to [0 3.4]))
        floor  (s/path (:ring s0) :closed? true :color :grey :width 1)
        pivot  (-> (s/dot :color :grey) (s/move-to origin))
        trail  (s/path (conj (:trail s0) (:p2 s0)) :color :gold :width 2)
        rod1   (s/line origin (:p1 s0) :color :grey :width 4)
        rod2   (s/line (:p1 s0) (:p2 s0) :color :grey :width 4)
        bob1   (-> (s/path (:disc1 s0) :closed? true :color :teal :width 2) (s/fill! :teal :opacity 0.8))
        bob2   (-> (s/path (:disc2 s0) :closed? true :color :gold :width 2) (s/fill! :gold :opacity 0.8))
        clock  (-> (s/decimal 0 :font-size 30 :color :grey :num-decimal-places 1) (s/move-to [5.2 -3.2]))
        unit   (-> (s/text "s" :font-size 26 :color :grey) (s/move-to [5.9 -3.2]))]
    (s/play! stage (s/together (map #(s/appear % :run-time 0.5)
                                    [title floor pivot trail rod1 rod2 bob1 bob2 clock unit])))
    (doseq [[i sh] (map-indexed vector shots)
            :when (pos? i)]
      (s/play! stage (s/together [(s/reshape floor (:ring sh) :run-time 0.1)
                                  (s/reshape trail (:trail sh) :run-time 0.1)
                                  (s/connect rod1 origin (:p1 sh) :run-time 0.1)
                                  (s/connect rod2 (:p1 sh) (:p2 sh) :run-time 0.1)
                                  (s/reshape bob1 (:disc1 sh) :run-time 0.1)
                                  (s/reshape bob2 (:disc2 sh) :run-time 0.1)
                                  (s/count-to clock (* 0.1 i) :run-time 0.1)])))
    (s/hold! stage 1)))

(def graph
  (s/with-backend (rec/recording-backend)
    (s/render! "desargues-double-pendulum" construct)))
