(ns desargues.studio.demos
  "Parametric constructs for the live player: each is (fn [params] (fn [stage]
   ...)) built only on the desargues.scene facade, so the same value renders
   an mp4 through Manim or plays live through desargues.player.

   Physics comes from the physics domain's `evolve`; pass {:solver :raster}
   (with the :dynamics alias) to integrate on raster instead of the JVM RK4."
  (:require [desargues.scene :as s]
            [desargues.videos.physics :as phys]))

(def pendulum-controls
  "Widget specs for `pendulum`, one per parameter."
  [{:id :length  :label "length (m)"     :min 0.5 :max 3.0 :value 2.0}
   {:id :damping :label "damping"        :min 0.0 :max 1.0 :value 0.15}
   {:id :theta0  :label "theta0 (rad)"   :min 0.1 :max 3.0 :value 1.2}])

(defn- pendulum-samples
  "Bob positions every `every` s over `duration` s, integrated at dt 0.01."
  [{:keys [length damping theta0 solver duration every]
    :or {length 2.0 damping 0.15 theta0 1.2 solver :jvm duration 10.0 every 0.05}}]
  (let [top  [0 2.2 0]
        sys  (phys/make-pendulum {:length length :damping damping
                                  :initial-theta theta0 :top-point top})
        traj (phys/evolve sys {:dt 0.01 :duration duration :solver solver})
        step (max 1 (long (Math/round (/ every 0.01))))]
    {:top top
     :every every
     :points (mapv (fn [{:keys [state]}] (vec (take 2 (phys/pendulum-position sys state))))
                   (take-nth step traj))}))

(defn pendulum
  "A damped pendulum: rod, bob, a fading trail and a clock."
  [params]
  (let [{:keys [top every points]} (pendulum-samples params)
        top2 (vec (take 2 top))]
    (fn [stage]
      (let [title (-> (s/text "Damped pendulum, RK4 at dt = 0.01" :font-size 30 :color :white)
                      (s/move-to [0 3.4]))
            pivot (-> (s/dot :color :grey) (s/move-to top2))
            rod   (s/line top2 (first points) :color :grey :width 4)
            bob   (-> (s/circle :radius 0.25 :color :gold)
                      (s/move-to (first points))
                      (s/fill! :gold :opacity 0.8))
            trail (s/path [(first points) (first points)] :color :teal :width 2)
            clock (-> (s/decimal 0 :font-size 30 :color :grey :num-decimal-places 2)
                      (s/move-to [5.6 -3.3]))]
        (s/play! stage (s/together [(s/appear title :run-time 0.4) (s/appear pivot :run-time 0.4)
                                    (s/draw rod :run-time 0.4) (s/draw bob :run-time 0.4)
                                    (s/appear trail :run-time 0.4) (s/appear clock :run-time 0.4)]))
        (doseq [[i p] (map-indexed vector (rest points))]
          (s/play! stage (s/together
                          [(s/connect rod top2 p :run-time every)
                           (s/glide bob p :run-time every)
                           (s/reshape trail (subvec points (max 0 (- (+ i 2) 40)) (+ i 2))
                                      :run-time every)
                           (s/count-to clock (* every (inc i)) :run-time every)])))
        (s/hold! stage 1)))))
