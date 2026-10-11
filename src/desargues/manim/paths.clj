(ns desargues.manim.paths
  "Manim bindings for polylines: a VMobject whose points are set as corners,
   and the animation that moves those corners."
  (:require [desargues.manim.core :as core]
            [libpython-clj2.python :as py]))

(defn polyline
  "A VMobject with straight segments through `points` ([x y z] each), in order.

   Usage:
   (polyline [[0 0 0] [1 1 0] [2 0 0]])
   (polyline [[0 0 0] [1 1 0] [2 0 0]] :color RED :stroke_width 4)

   Options: any VMobject kwarg (:color :stroke_width :stroke_opacity ...)"
  [points & {:as kwargs}]
  (let [cls (core/get-class "VMobject")
        vm  (if kwargs (py/call-attr-kw cls "__call__" [] kwargs) (cls))]
    (py/call-attr vm "set_points_as_corners" (core/->py-list (mapv vec points)))
    vm))

(defn move-corners
  "Animate a VMobject's corners to `points` (vm.animate.set_points_as_corners).

   Options: :run_time :rate_func"
  [vm points & {:as kwargs}]
  (let [builder (py/call-attr-kw (py/get-attr vm "animate") "__call__" [] (or kwargs {}))]
    (py/call-attr builder "set_points_as_corners" (core/->py-list (mapv vec points)))))
