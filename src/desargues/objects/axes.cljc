(ns desargues.objects.axes
  "PURE stratum: axes and the number plane, as parts over a CoordSystem.

   (axes cs opts) and (number-plane cs opts) are object SPECS (plain data);
   desargues.objects/add! realizes them on a stage. Ticks come from the third
   entry of each range ([min max step]); labels are plain-text numbers, axis
   names are TeX."
  (:require [desargues.objects.coords :as c]
            [desargues.objects.parts :as parts]))

(def axes-style
  {:color :grey :width 3 :tick-size 0.1 :font-size 22 :labels? true})

(defn axes
  "Axes spec over CoordSystem `cs`. opts: :color :width :tick-size (scene
   units) :font-size :labels? (numbers at ticks) :x-label :y-label (TeX)."
  ([cs] (axes cs {}))
  ([cs opts] (merge {:object :axes :coords cs} axes-style opts)))

(defn- axis-parts
  "Axis line, ticks and numbers along one axis (:x or :y)."
  [cs dir {:keys [color width tick-size font-size labels?]}]
  (let [[ox oy] (c/->math cs (c/origin cs))
        rng     (if (= dir :x) (:x-range cs) (:y-range cs))
        at      (fn [v] (c/->scene cs (if (= dir :x) [v oy] [ox v])))
        style   {:color color :width width}
        normal  (if (= dir :x) [0.0 1.0] [1.0 0.0])
        ts      (remove #(< (Math/abs (double (- % (if (= dir :x) ox oy)))) 1e-9)
                        (c/ticks rng))]
    (concat
     [(parts/line [dir :axis] (at (first rng)) (at (second rng)) style)]
     (for [v ts]
       (let [p (at v)]
         (parts/line [dir :tick v] (parts/v- p (parts/v* tick-size normal))
                     (parts/v+ p (parts/v* tick-size normal)) (assoc style :width 2))))
     (when labels?
       (for [v ts]
         (parts/text [dir :number v] (parts/format-number v)
                     (parts/v- (at v) (parts/v* (* 3.2 tick-size) normal))
                     {:color color :font-size font-size}))))))

(defmethod parts/parts :axes [{:keys [coords x-label y-label color font-size] :as spec}]
  (let [cs coords
        [x1 _] (c/->scene cs [(second (:x-range cs)) 0])
        [_ y1] (c/->scene cs [0 (second (:y-range cs))])
        [ox oy] (c/origin cs)]
    (vec (concat (axis-parts cs :x spec)
                 (axis-parts cs :y spec)
                 (when x-label [(parts/tex [:x :label] x-label [(+ x1 0.35) oy]
                                           {:color color :font-size (* 1.4 font-size)})])
                 (when y-label [(parts/tex [:y :label] y-label [ox (+ y1 0.35)]
                                           {:color color :font-size (* 1.4 font-size)})])))))

(defn number-plane
  "Axes plus a background grid at every tick. opts: axes opts plus
   :grid-color :grid-width."
  ([cs] (number-plane cs {}))
  ([cs opts] (merge {:object :number-plane :coords cs :grid-color :blue :grid-width 1}
                    axes-style opts)))

(defmethod parts/parts :number-plane [{:keys [coords grid-color grid-width] :as spec}]
  (let [cs coords
        [x0 x1] (:x-range cs) [y0 y1] (:y-range cs)
        style {:color grid-color :width grid-width}
        grid (concat
              (for [x (c/ticks (:x-range cs))]
                (parts/line [:grid :x x] (c/->scene cs [x y0]) (c/->scene cs [x y1]) style))
              (for [y (c/ticks (:y-range cs))]
                (parts/line [:grid :y y] (c/->scene cs [x0 y]) (c/->scene cs [x1 y]) style)))]
    (into (vec grid) (parts/parts (assoc spec :object :axes)))))
