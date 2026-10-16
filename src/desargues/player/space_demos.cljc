(ns desargues.player.space-demos
  "A 3D source for the stage: a parametric surface (a saddle) with a space
   curve drawn on it, orbitable in a 3D host and projected to 2D everywhere
   else. Plain data through desargues.player.space; the source is an ISource
   like any other, so the player cannot tell it from a 2D one (LSP).

     (require '[desargues.player :as player] '[desargues.player.javafx :as fx]
              '[desargues.player.space-demos :as sd])
     (def p (player/open-source! (fx/host) sd/source-fn
                                 {:title \"surface + curve\" :params sd/defaults
                                  :controls sd/controls}))
     ;; drag on empty space to orbit, or:
     (player/set-camera! p {:yaw 0.4 :pitch 0.8})

   Host-free .cljc."
  (:require [desargues.player.frame :as frame]
            [desargues.player.protocols :as p]
            [desargues.player.space :as space]))

(def defaults
  "Saddle height and how many turns the curve makes."
  {:height 0.35 :turns 2.0})

(def controls
  "Slider specs for the demo's parameters."
  [{:id :height :label "saddle height" :min -0.6 :max 0.6 :value 0.35}
   {:id :turns :label "curve turns" :min 0.5 :max 4.0 :value 2.0}])

(def duration
  "Seconds: the surface fades in, then the curve draws on."
  4.0)

(defn saddle
  "z = h (x^2 - y^2), as (fn [u v] [x y z])."
  [h]
  (fn [u v] [u v (* h (- (* u u) (* v v)))]))

(defn spiral-on
  "A curve on the saddle: a plane spiral lifted onto it."
  [h turns]
  (fn [t]
    (let [r (* 1.8 t) a (* 2.0 Math/PI turns t)
          x (* r (Math/cos a)) y (* r (Math/sin a))]
      [x y (* h (- (* x x) (* y y)))])))

(defn- ramp [t a b] (max 0.0 (min 1.0 (/ (- (double t) a) (- b a)))))

(defn frame-at
  "The demo frame at time `t` for `params`."
  [{:keys [height turns] :or {height 0.35 turns 2.0}} t]
  (assoc (frame/frame
          t
          [{:id "axes" :node :axes3 :opacity (ramp t 0.0 0.5) :draw 1.0
            :opts {:range [[-2.5 2.5] [-2.5 2.5] [-1.8 1.8]]}}
           (assoc (space/surface (saddle height) [-2 2] [-2 2]
                                 {:id "surface" :nu 20 :nv 20 :color :blue
                                  :opacity 0.9 :wire "#2A5F70"})
                  :opacity (ramp t 0.0 1.0))
           (assoc (space/curve (spiral-on height turns) [0 1]
                               {:id "curve" :n 200 :color :gold :width 6})
                  :draw (ramp t 1.0 3.5))
           {:id "title" :node :text :text "a saddle and a curve on it: drag to orbit"
            :at [0 3.5] :opacity 1.0 :draw 1.0 :opts {:font-size 30 :color :grey}}])
         :camera {:target [0 0 0] :distance 11.0 :yaw -0.9 :pitch 0.5}))

(defrecord SpaceDemo [params]
  p/ISource
  (-duration [_] duration)
  (-frame-at [_ t] (frame-at params t)))

(defn source-fn
  "(fn [params] ISource) for desargues.player/open-source!."
  [params]
  (->SpaceDemo (merge defaults params)))
