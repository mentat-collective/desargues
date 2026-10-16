(ns desargues.objects.field
  "PURE stratum: arrows and vector fields.

   An arrow is a COMPOSITE: a shaft (:line) plus a filled triangular head
   (closed :path). A vector field samples (f [x y]) -> [vx vy] on the tick
   grid of a CoordSystem and draws one arrow per grid point, scaled so the
   longest arrow fits one grid cell."
  (:require [desargues.objects.coords :as c]
            [desargues.objects.parts :as parts]))

(defn arrow
  "Arrow spec from scene point `from` to `to`. opts: :color :width :tip (head
   length, scene units)."
  ([from to] (arrow from to {}))
  ([from to opts]
   (merge {:object :arrow :from (vec from) :to (vec to) :color :yellow :width 3 :tip 0.2}
          opts)))

(defn arrow-parts
  "Shaft + head parts of an arrow from `from` to `to`, keys under `k`."
  [k from to {:keys [color width tip]}]
  (let [d    (parts/v- to from)
        len  (parts/norm d)
        tip  (min (double tip) (* 0.5 len))
        u    (parts/unit d)
        base (parts/v- to (parts/v* tip u))
        side (parts/v* (* 0.5 tip) (parts/rot90 u))]
    [(parts/line [k :shaft] from base {:color color :width width})
     (parts/path [k :head] [to (parts/v+ base side) (parts/v- base side)] true
                 {:color color :width 1 :fill color :fill-opacity 1.0})]))

(defmethod parts/parts :arrow [{:keys [from to] :as spec}]
  (arrow-parts :arrow from to spec))

(defn vector-field
  "Field spec: `f` maps math [x y] to a math vector [vx vy]. opts: :color
   :width :tip :step [dx dy] (grid spacing, default the tick steps) :scale
   (scene units per unit vector length; default fits the longest arrow in
   one cell) :min-length (skip shorter arrows, scene units)."
  ([cs f] (vector-field cs f {}))
  ([cs f opts]
   (merge {:object :vector-field :coords cs :f f :color :teal :width 2 :tip 0.12
           :min-length 1e-6}
          opts)))

(defn grid-points
  "Math points of the field grid: every step multiple strictly inside the box."
  [cs [dx dy]]
  (let [[x0 x1] (:x-range cs) [y0 y1] (:y-range cs)
        inner (fn [lo hi step] (remove #(or (== % lo) (== % hi)) (c/ticks [lo hi step])))]
    (for [x (inner x0 x1 dx) y (inner y0 y1 dy)] [x y])))

(defn field-vectors
  "[[math-point scene-from scene-to] ...] for a field spec."
  [{:keys [coords f step scale min-length]}]
  (let [cs    coords
        step  (or step [(nth (:x-range cs) 2 1.0) (nth (:y-range cs) 2 1.0)])
        cell  (* 0.9 (min (* (c/x-unit cs) (first step)) (* (c/y-unit cs) (second step))))
        pts   (grid-points cs step)
        vs    (map (fn [p] (let [[vx vy] (f p)]
                             [(* (double vx) (c/x-unit cs)) (* (double vy) (c/y-unit cs))]))
                   pts)
        vmax  (reduce max 1e-12 (map parts/norm vs))
        k     (or scale (/ cell vmax))]
    (keep (fn [[p v]]
            (let [from (c/->scene cs p)
                  d    (parts/v* k v)]
              (when (> (parts/norm d) min-length)
                [p from (parts/v+ from d)])))
          (map vector pts vs))))

(defmethod parts/parts :vector-field [spec]
  (vec (mapcat (fn [[p from to]] (arrow-parts [:arrow p] from to spec))
               (field-vectors spec))))
