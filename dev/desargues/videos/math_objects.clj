(ns desargues.videos.math-objects
  "Mathematical objects DSL for visualization.

   This namespace provides high-level abstractions for mathematical objects
   commonly used in 3Blue1Brown videos:
   - Coordinate systems (Axes, NumberLine, NumberPlane, ComplexPlane)
   - Graphs and curves (function graphs, parametric curves)
   - Vectors and vector fields
   - Phase space visualizations
   - Geometric shapes with mathematical properties

   ## Design Philosophy

   Mathematical objects are DECLARATIVE DATA. Properties are:
   - Immutable (transform creates new objects)
   - Composable (VGroup-like grouping)
   - Observable (reactive to parameter changes)

   ## Example Usage

   ```clojure
   ;; Create axes with labels
   (def my-axes (axes {:x-range [-5 5] :y-range [-3 3]
                       :labels true :grid true}))

   ;; Create a graph on those axes
   (def sine-graph (graph my-axes #(Math/sin %) {:color :blue}))

   ;; Create phase space visualization
   (def phase (phase-space pendulum {:x :theta :y :omega}))
   ```"
  (:require [clojure.spec.alpha :as s]
            [desargues.videos.physics :as phys]))

;; =============================================================================
;; Protocols
;; =============================================================================

(defprotocol Mobject
  "Protocol for all mathematical objects (like Manim's Mobject)."
  (get-center [this] "Returns center point [x y z]")
  (get-bounds [this] "Returns [[min-x min-y] [max-x max-y]]")
  (shift [this delta] "Returns shifted object")
  (scale-obj [this factor] "Returns scaled object")
  (set-color [this color] "Returns object with new color")
  (get-color [this] "Returns current color"))

(defprotocol CoordinateSystem
  "Protocol for objects that define coordinate systems."
  (c2p [this coords] "Coords to point: Convert mathematical coords to scene coords")
  (p2c [this point] "Point to coords: Convert scene coords to mathematical coords")
  (get-x-axis [this] "Returns x-axis object")
  (get-y-axis [this] "Returns y-axis object")
  (get-origin [this] "Returns origin point in scene coords"))

(defprotocol Graphable
  "Protocol for objects that can have graphs plotted on them."
  (get-graph [this f opts] "Create a graph of function f")
  (get-parametric-curve [this f opts] "Create parametric curve")
  (get-riemann-rectangles [this graph opts] "Create Riemann sum rectangles"))

;; =============================================================================
;; VGroup - Grouping Objects
;; =============================================================================

(defrecord VGroup [children center stroke-color fill-color opacity]
  Mobject
  (get-center [_] center)
  (get-bounds [this]
    (if (empty? children)
      [[0 0] [0 0]]
      (let [bounds (map get-bounds children)
            mins (map first bounds)
            maxs (map second bounds)]
        [[(apply min (map first mins)) (apply min (map second mins))]
         [(apply max (map first maxs)) (apply max (map second maxs))]])))
  (shift [this delta]
    (-> this
        (update :children #(mapv (fn [c] (shift c delta)) %))
        (update :center #(mapv + % delta))))
  (scale-obj [this factor]
    (update this :children #(mapv (fn [c] (scale-obj c factor)) %)))
  (set-color [this color]
    (-> this
        (assoc :stroke-color color)
        (update :children #(mapv (fn [c] (set-color c color)) %))))
  (get-color [this] stroke-color))

(defn vgroup
  "Create a group of objects."
  [& children]
  (let [children-vec (vec children)
        center (if (empty? children-vec)
                 [0 0 0]
                 (let [centers (map get-center children-vec)]
                   (mapv #(/ % (count centers))
                         (reduce (fn [acc c] (mapv + acc c)) [0 0 0] centers))))]
    (->VGroup children-vec center nil nil 1.0)))

(defn add-to-group
  "Add objects to a VGroup."
  [group & objects]
  (update group :children into objects))

;; =============================================================================
;; Basic Shapes
;; =============================================================================

(defrecord Circle [center radius stroke-color stroke-width fill-color fill-opacity]
  Mobject
  (get-center [_] center)
  (get-bounds [_]
    [[(- (first center) radius) (- (second center) radius)]
     [(+ (first center) radius) (+ (second center) radius)]])
  (shift [this delta]
    (update this :center #(mapv + % delta)))
  (scale-obj [this factor]
    (update this :radius * factor))
  (set-color [this color]
    (assoc this :stroke-color color))
  (get-color [_] stroke-color))

(defn circle
  "Create a circle."
  [& {:keys [center radius color stroke-width fill-color fill-opacity]
      :or {center [0 0 0] radius 1 color :white stroke-width 2
           fill-color nil fill-opacity 0}}]
  (->Circle center radius color stroke-width fill-color fill-opacity))

(defrecord Rectangle [center width height stroke-color stroke-width fill-color fill-opacity]
  Mobject
  (get-center [_] center)
  (get-bounds [_]
    [[(- (first center) (/ width 2)) (- (second center) (/ height 2))]
     [(+ (first center) (/ width 2)) (+ (second center) (/ height 2))]])
  (shift [this delta]
    (update this :center #(mapv + % delta)))
  (scale-obj [this factor]
    (-> this
        (update :width * factor)
        (update :height * factor)))
  (set-color [this color]
    (assoc this :stroke-color color))
  (get-color [_] stroke-color))

(defn rectangle
  "Create a rectangle."
  [& {:keys [center width height color stroke-width fill-color fill-opacity]
      :or {center [0 0 0] width 2 height 1 color :white stroke-width 2
           fill-color nil fill-opacity 0}}]
  (->Rectangle center width height color stroke-width fill-color fill-opacity))

(defn square
  "Create a square."
  [& {:keys [center side color stroke-width fill-color fill-opacity]
      :or {center [0 0 0] side 2 color :white stroke-width 2}}]
  (rectangle :center center :width side :height side
             :color color :stroke-width stroke-width
             :fill-color fill-color :fill-opacity fill-opacity))

(defrecord Line [start end stroke-color stroke-width]
  Mobject
  (get-center [_]
    (mapv #(/ % 2) (mapv + start end)))
  (get-bounds [_]
    [[(min (first start) (first end)) (min (second start) (second end))]
     [(max (first start) (first end)) (max (second start) (second end))]])
  (shift [this delta]
    (-> this
        (update :start #(mapv + % delta))
        (update :end #(mapv + % delta))))
  (scale-obj [this factor]
    (let [center (get-center this)
          scale-point (fn [p] (mapv + center (mapv #(* factor %) (mapv - p center))))]
      (-> this
          (update :start scale-point)
          (update :end scale-point))))
  (set-color [this color]
    (assoc this :stroke-color color))
  (get-color [_] stroke-color))

(defn line
  "Create a line."
  [start end & {:keys [color stroke-width]
                :or {color :white stroke-width 2}}]
  (->Line start end color stroke-width))

(defn dashed-line
  "Create a dashed line."
  [start end & {:keys [color stroke-width num-dashes]
                :or {color :white stroke-width 2 num-dashes 10}}]
  {:type :dashed-line
   :start start
   :end end
   :stroke-color color
   :stroke-width stroke-width
   :num-dashes num-dashes})

;; =============================================================================
;; Vector (Arrow)
;; =============================================================================

(defrecord Arrow [start end color thickness tip-length tip-width]
  Mobject
  (get-center [_]
    (mapv #(/ % 2) (mapv + start end)))
  (get-bounds [_]
    [[(min (first start) (first end)) (min (second start) (second end))]
     [(max (first start) (first end)) (max (second start) (second end))]])
  (shift [this delta]
    (-> this
        (update :start #(mapv + % delta))
        (update :end #(mapv + % delta))))
  (scale-obj [this factor]
    (let [center (get-center this)]
      (-> this
          (update :start #(mapv + center (mapv (fn [x] (* factor x)) (mapv - % center))))
          (update :end #(mapv + center (mapv (fn [x] (* factor x)) (mapv - % center)))))))
  (set-color [this new-color]
    (assoc this :color new-color))
  (get-color [_] color))

(defn vector-arrow
  "Create a vector (arrow).
   Like Manim's Vector."
  [direction & {:keys [start color thickness tip-length tip-width]
                :or {start [0 0 0] color :yellow thickness 3
                     tip-length 0.25 tip-width 0.25}}]
  (->Arrow start (mapv + start direction) color thickness tip-length tip-width))

(def vector
  "Alias for vector-arrow."
  vector-arrow)

(defn arrow
  "Create an arrow from start to end."
  [start end & {:keys [color thickness tip-length tip-width buff]
                :or {color :white thickness 3 tip-length 0.25 tip-width 0.25 buff 0}}]
  (->Arrow start end color thickness tip-length tip-width))

;; =============================================================================
;; NumberLine
;; =============================================================================

(defrecord NumberLine [x-range unit-size center include-numbers include-tip
                       tick-frequency stroke-color stroke-width]
  Mobject
  (get-center [_] center)
  (get-bounds [this]
    (let [[x-min x-max _] x-range
          half-width (* (- x-max x-min) unit-size 0.5)]
      [[(- (first center) half-width) (- (second center) 0.5)]
       [(+ (first center) half-width) (+ (second center) 0.5)]]))
  (shift [this delta]
    (update this :center #(mapv + % delta)))
  (scale-obj [this factor]
    (update this :unit-size * factor))
  (set-color [this color]
    (assoc this :stroke-color color))
  (get-color [_] stroke-color)

  CoordinateSystem
  (c2p [this [x]]
    (let [[cx cy cz] center]
      [(+ cx (* (- x (first x-range)) unit-size)) cy cz]))
  (p2c [this [px _ _]]
    [(+ (first x-range) (/ (- px (first center)) unit-size))])
  (get-x-axis [this] this)
  (get-y-axis [_] nil)
  (get-origin [this]
    (c2p this [0])))

(defn number-line
  "Create a number line."
  [& {:keys [x-range unit-size center include-numbers include-tip tick-frequency
             stroke-color stroke-width]
      :or {x-range [-5 5 1] unit-size 1 center [0 0 0]
           include-numbers true include-tip true tick-frequency 1
           stroke-color :white stroke-width 2}}]
  (->NumberLine x-range unit-size center include-numbers include-tip
                tick-frequency stroke-color stroke-width))

(defn n2p
  "Number to point - convert number to scene coordinates."
  [number-line n]
  (c2p number-line [n]))

(defn p2n
  "Point to number - convert scene coordinates to number."
  [number-line point]
  (first (p2c number-line point)))

(defn get-range
  "Get the x-range from a NumberLine or Axes."
  [coord-system]
  (:x-range coord-system))

(defn move-to
  "Move a mobject to a target position, optionally in a specific direction."
  [mobject target & [direction]]
  (assoc mobject :position target :aligned-edge direction))

(defn next-to
  "Position a mobject next to another mobject in a given direction."
  [mobject target direction & {:keys [buff] :or {buff 0.25}}]
  (assoc mobject :next-to {:target target :direction direction :buff buff}))

(defn add-numbers
  "Add number labels to a coordinate system."
  [coord-system & {:keys [font-size] :or {font-size 24}}]
  (assoc coord-system :numbers-visible true :number-font-size font-size))

(defn add-coordinate-labels
  "Add coordinate labels to axes."
  [axes & {:keys [font-size] :or {font-size 24}}]
  (assoc axes :coordinate-labels true :label-font-size font-size))

(defn always
  "Create an always updater that runs every frame."
  [mobject update-fn]
  (assoc mobject :updater update-fn))

(defn always-redraw
  "Create a mobject that redraws every frame using the given function."
  [draw-fn]
  {:type :always-redraw :draw-fn draw-fn})

(defn f-always
  "Create a function-based always updater."
  [mobject f]
  (assoc mobject :f-updater f))

(defn arrange
  "Arrange mobjects in a row or column."
  [mobjects & {:keys [direction buff center] :or {direction :right buff 0.25 center true}}]
  {:type :arrangement :children mobjects :direction direction :buff buff :center center})

(defn copy
  "Create a copy of a mobject."
  [mobject]
  (assoc mobject :id (gensym "copy")))

(defn replicate
  "Create n copies of a mobject."
  [mobject n]
  (repeatedly n #(copy mobject)))

(defn get-center
  "Get the center point of a mobject."
  [mobject]
  (or (:center mobject) (:position mobject) [0 0 0]))

(defn get-right
  "Get the right edge point of a mobject."
  [mobject]
  (let [[x y z] (get-center mobject)
        w (or (:width mobject) 1)]
    [(+ x (/ w 2)) y (or z 0)]))

(defn get-top
  "Get the top edge point of a mobject."
  [mobject]
  (let [[x y z] (get-center mobject)
        h (or (:height mobject) 1)]
    [x (+ y (/ h 2)) (or z 0)]))

(defn get-width
  "Get the width of a mobject."
  [mobject]
  (or (:width mobject) 1))

(defn children
  "Get the children of a group mobject."
  [group]
  (or (:children group) []))

(defn replace-mobject
  "Replace a mobject with another, preserving position."
  [target replacement]
  (assoc replacement :position (get-center target)))

(def replace
  "Alias for replace-mobject."
  replace-mobject)

(defn rotate
  "Rotate a mobject by an angle."
  [mobject angle & {:keys [axis about-point] :or {axis [0 0 1]}}]
  (assoc mobject :rotation {:angle angle :axis axis :about-point about-point}))

(defn scale
  "Scale a mobject by a factor."
  [mobject factor & {:keys [about-point]}]
  (assoc mobject :scale {:factor factor :about-point about-point}))

(defn set-color
  "Set the color of a mobject."
  [mobject color]
  (assoc mobject :color color))

(defn set-fill
  "Set the fill color and opacity of a mobject."
  [mobject color & {:keys [opacity] :or {opacity 1}}]
  (assoc mobject :fill-color color :fill-opacity opacity))

(defn set-stroke
  "Set the stroke color and width of a mobject."
  [mobject color & {:keys [width opacity] :or {width 2 opacity 1}}]
  (assoc mobject :stroke-color color :stroke-width width :stroke-opacity opacity))

(defn set-width
  "Set the width of a mobject, scaling proportionally."
  [mobject width]
  (assoc mobject :width width))

(defn shift
  "Shift a mobject by a vector."
  [mobject & deltas]
  (let [delta (if (= 1 (count deltas)) (first deltas) (vec deltas))]
    (assoc mobject :shift delta)))

(defn stretch
  "Stretch a mobject along an axis."
  [mobject factor dim]
  (assoc mobject :stretch {:factor factor :dim dim}))

(defn surrounding-rectangle
  "Create a rectangle surrounding a mobject."
  [mobject & {:keys [buff color stroke-width] :or {buff 0.1 color :yellow stroke-width 2}}]
  {:type :surrounding-rect :target mobject :buff buff :color color :stroke-width stroke-width})

(defn to-corner
  "Move a mobject to a corner of the screen."
  [mobject corner & {:keys [buff] :or {buff 0.5}}]
  (assoc mobject :corner corner :corner-buff buff))

(defn to-edge
  "Move a mobject to an edge of the screen."
  [mobject edge & {:keys [buff] :or {buff 0.5}}]
  (assoc mobject :edge edge :edge-buff buff))

(defn tex
  "Create a TeX mobject."
  [content & {:keys [color font-size] :or {color :white font-size 36}}]
  {:type :tex :content content :color color :font-size font-size})

(defn text
  "Create a text mobject."
  [content & {:keys [color font-size font] :or {color :white font-size 36}}]
  {:type :text :content content :color color :font-size font-size :font font})

(defn unit-interval
  "Create a unit interval [0, 1] number line."
  [& opts]
  (apply number-line :x-range [0 1 0.25] opts))

(defn x-axis
  "Get the x-axis from axes."
  [axes]
  (:x-axis axes))

(defn y-axis
  "Get the y-axis from axes."
  [axes]
  (:y-axis axes))

(defn z-axis
  "Get the z-axis from 3D axes."
  [axes]
  (:z-axis axes))

(declare brace)

(defn add-label
  "Add a label to a mobject."
  [mobject label & {:keys [direction buff font-size] :or {direction :up buff 0.25 font-size 24}}]
  (assoc mobject :label {:text label :direction direction :buff buff :font-size font-size}))

(defn brace-horizontal
  "Create a horizontal brace under a mobject."
  [mobject & {:keys [buff color] :or {buff 0.1 color :white}}]
  (brace mobject :down :buff buff :color color))

(defn c2p
  "Coordinates to point - convert graph coordinates to scene coordinates."
  [coord-system coords]
  (let [[x y] coords
        [cx cy _] (or (:center coord-system) [0 0 0])
        width (or (:width coord-system) 10)
        height (or (:height coord-system) 6)
        [x-min x-max _] (:x-range coord-system)
        [y-min y-max _] (:y-range coord-system)
        scale-x (/ width (- x-max x-min))
        scale-y (/ height (- y-max y-min))]
    [(+ cx (* (- x (/ (+ x-min x-max) 2)) scale-x))
     (+ cy (* (- y (/ (+ y-min y-max) 2)) scale-y))
     0]))

(defn p2c
  "Point to coordinates - convert scene coordinates to graph coordinates."
  [coord-system point]
  (let [[px py _] point
        [cx cy _] (or (:center coord-system) [0 0 0])
        width (or (:width coord-system) 10)
        height (or (:height coord-system) 6)
        [x-min x-max _] (:x-range coord-system)
        [y-min y-max _] (:y-range coord-system)
        scale-x (/ width (- x-max x-min))
        scale-y (/ height (- y-max y-min))]
    [(+ (/ (+ x-min x-max) 2) (/ (- px cx) scale-x))
     (+ (/ (+ y-min y-max) 2) (/ (- py cy) scale-y))]))

(def c
  "Alias for c2p - coordinates to point."
  c2p)

(def n
  "Alias for n2p - number to point."
  n2p)

(defn curved-arrow
  "Create a curved arrow between two points."
  [start end & {:keys [color thickness angle] :or {color :white thickness 3 angle 0.5}}]
  {:type :curved-arrow :start start :end end :color color :thickness thickness :angle angle})

(defn dashed-graph
  "Create a dashed graph/curve."
  [axes func & {:keys [x-range color stroke-width num-dashes]
                :or {color :white stroke-width 2 num-dashes 20}}]
  {:type :dashed-graph :axes axes :func func :x-range (or x-range (:x-range axes))
   :color color :stroke-width stroke-width :num-dashes num-dashes})

(defn get-start
  "Get the start point of a line or arrow."
  [mobject]
  (or (:start mobject) (first (:points mobject)) [0 0 0]))

(defn move-along-path
  "Create an animation that moves a mobject along a path."
  [mobject path & {:keys [run-time] :or {run-time 1.0}}]
  {:type :move-along-path :target mobject :path path :run-time run-time})

(defn parametric-curve
  "Create a parametric curve."
  [func & {:keys [t-range color stroke-width]
           :or {t-range [0 1] color :white stroke-width 2}}]
  {:type :parametric-curve :func func :t-range t-range :color color :stroke-width stroke-width})

(defn three-d-axes
  "Create 3D axes."
  [& {:keys [x-range y-range z-range] :or {x-range [-5 5 1] y-range [-5 5 1] z-range [-5 5 1]}}]
  {:type :three-d-axes :x-range x-range :y-range y-range :z-range z-range})

;; =============================================================================
;; Axes (2D Coordinate System)
;; =============================================================================

(defrecord Axes [x-range y-range width height center
                 include-numbers include-tip grid-lines
                 stroke-color stroke-width axis-config]
  Mobject
  (get-center [_] center)
  (get-bounds [this]
    (let [[cx cy cz] center]
      [[(- cx (/ width 2)) (- cy (/ height 2))]
       [(+ cx (/ width 2)) (+ cy (/ height 2))]]))
  (shift [this delta]
    (update this :center #(mapv + % delta)))
  (scale-obj [this factor]
    (-> this
        (update :width * factor)
        (update :height * factor)))
  (set-color [this color]
    (assoc this :stroke-color color))
  (get-color [_] stroke-color)

  CoordinateSystem
  (c2p [this [x y]]
    (let [[cx cy cz] center
          [x-min x-max _] x-range
          [y-min y-max _] y-range
          x-unit (/ width (- x-max x-min))
          y-unit (/ height (- y-max y-min))
          px (+ cx (* (- x (/ (+ x-min x-max) 2)) x-unit))
          py (+ cy (* (- y (/ (+ y-min y-max) 2)) y-unit))]
      [px py cz]))
  (p2c [this [px py _]]
    (let [[cx cy _] center
          [x-min x-max _] x-range
          [y-min y-max _] y-range
          x-unit (/ width (- x-max x-min))
          y-unit (/ height (- y-max y-min))
          x (+ (/ (+ x-min x-max) 2) (/ (- px cx) x-unit))
          y (+ (/ (+ y-min y-max) 2) (/ (- py cy) y-unit))]
      [x y]))
  (get-x-axis [this]
    (number-line :x-range x-range
                 :unit-size (/ width (- (second x-range) (first x-range)))
                 :center (c2p this [0 0])))
  (get-y-axis [this]
    ;; Return vertical number line (rotated)
    {:type :vertical-number-line
     :y-range y-range
     :unit-size (/ height (- (second y-range) (first y-range)))
     :center (c2p this [0 0])})
  (get-origin [this]
    (c2p this [0 0]))

  Graphable
  (get-graph [this f {:keys [x-min x-max color stroke-width num-points]
                      :or {x-min (first x-range)
                           x-max (second x-range)
                           color :blue
                           stroke-width 3
                           num-points 200}}]
    {:type :graph
     :axes this
     :function f
     :x-min x-min
     :x-max x-max
     :color color
     :stroke-width stroke-width
     :points (let [step (/ (- x-max x-min) num-points)]
               (for [x (range x-min x-max step)]
                 (c2p this [x (f x)])))})

  (get-parametric-curve [this f {:keys [t-min t-max color stroke-width num-points]
                                 :or {t-min 0 t-max 1 color :green stroke-width 3 num-points 200}}]
    {:type :parametric-curve
     :axes this
     :function f
     :t-min t-min
     :t-max t-max
     :color color
     :stroke-width stroke-width
     :points (let [step (/ (- t-max t-min) num-points)]
               (for [t (range t-min t-max step)]
                 (let [[x y] (f t)]
                   (c2p this [x y]))))})

  (get-riemann-rectangles [this graph {:keys [dx x-min x-max start-color end-color fill-opacity]
                                       :or {dx 0.1 start-color :blue end-color :green fill-opacity 0.6}}]
    (let [f (:function graph)
          x-min (or x-min (:x-min graph))
          x-max (or x-max (:x-max graph))
          n-rects (int (/ (- x-max x-min) dx))
          colors (for [i (range n-rects)]
                   (let [t (/ i n-rects)]
                     {:r (+ (* (- 1 t) 0) (* t 0.5))
                      :g (+ (* (- 1 t) 0.5) (* t 1))
                      :b (+ (* (- 1 t) 1) (* t 0))}))]
      {:type :riemann-rectangles
       :axes this
       :rectangles (for [i (range n-rects)]
                     (let [x (+ x-min (* i dx))
                           y (f x)
                           [px py pz] (c2p this [x 0])
                           [_ top-y _] (c2p this [x y])]
                       {:x px :y py :width (* dx (/ (:width this) (- (second (:x-range this)) (first (:x-range this)))))
                        :height (- top-y py)
                        :color (nth colors i)
                        :fill-opacity fill-opacity}))})))

(defn axes
  "Create a 2D coordinate system."
  [& {:keys [x-range y-range width height center
             include-numbers include-tip grid-lines
             stroke-color stroke-width axis-config]
      :or {x-range [-5 5 1] y-range [-3 3 1] width 10 height 6 center [0 0 0]
           include-numbers true include-tip true grid-lines false
           stroke-color :white stroke-width 2 axis-config {}}}]
  (->Axes x-range y-range width height center
          include-numbers include-tip grid-lines
          stroke-color stroke-width axis-config))

;; =============================================================================
;; NumberPlane - Full coordinate grid
;; =============================================================================

(defn number-plane
  "Create a number plane with background grid lines.
   Like Manim's NumberPlane."
  [& {:keys [x-range y-range width height center
             background-line-style faded-line-style
             stroke-color stroke-width]
      :or {x-range [-7 7 1] y-range [-4 4 1] width 14 height 8 center [0 0 0]
           background-line-style {:color :blue :width 1}
           faded-line-style {:color :blue :width 0.5 :opacity 0.25}
           stroke-color :white stroke-width 2}}]
  {:type :number-plane
   :axes (axes :x-range x-range :y-range y-range :width width :height height :center center)
   :background-line-style background-line-style
   :faded-line-style faded-line-style})

;; =============================================================================
;; ComplexPlane
;; =============================================================================

(defn complex [re im]
  "Create a complex number as [re im]."
  [re im])

(defn complex-plane
  "Create a complex plane coordinate system.
   Like Manim's ComplexPlane."
  [& {:keys [x-range y-range width height center
             background-line-style faded-line-style]
      :or {x-range [-3 3 1] y-range [-3 3 1] width 6 height 6 center [0 0 0]
           background-line-style {:color :blue :width 1}
           faded-line-style {:color :blue :width 0.5 :opacity 0.25}}}]
  (let [base-axes (axes :x-range x-range :y-range y-range
                        :width width :height height :center center)]
    {:type :complex-plane
     :axes base-axes
     :background-line-style background-line-style
     :faded-line-style faded-line-style
     ;; Complex-specific methods
     :n2p (fn [z] (c2p base-axes [(if (number? z) z (first z))
                                  (if (number? z) 0 (second z))]))
     :p2n (fn [p] (let [[x y] (p2c base-axes p)] (complex x y)))}))

;; =============================================================================
;; Phase Space Visualization
;; =============================================================================

(defn phase-space
  "Create a phase space visualization from a physical system trajectory.

   Arguments:
   - system: PhysicalSystem from physics.clj
   - trajectory: Output from phys/evolve
   - opts: {:x :theta :y :omega :axes-opts {...}}

   Returns a map with axes, curve, and velocity field data."
  [system trajectory & {:keys [x y axes-opts color stroke-width]
                        :or {color :green stroke-width 2 axes-opts {}}}]
  (let [labels (phys/state-labels system)
        x-var (or x (first labels))
        y-var (or y (second labels))
        points (phys/phase-portrait system trajectory :vars [x-var y-var])
        xs (map first points)
        ys (map second points)
        x-range [(apply min xs) (apply max xs)]
        y-range [(apply min ys) (apply max ys)]
        padding 0.1
        x-range-padded [(- (first x-range) padding) (+ (second x-range) padding) 0.5]
        y-range-padded [(- (first y-range) padding) (+ (second y-range) padding) 0.5]
        phase-axes (axes :x-range x-range-padded :y-range y-range-padded)]
    {:type :phase-space
     :system system
     :trajectory trajectory
     :axes phase-axes
     :curve {:type :phase-curve
             :points (mapv #(c2p phase-axes %) points)
             :color color
             :stroke-width stroke-width}
     :x-label (name x-var)
     :y-label (name y-var)}))

;; =============================================================================
;; Live-drawn graph (tracking a value over time)
;; =============================================================================

(defn live-drawn-graph
  "Create a graph that draws itself over time.
   Like 3b1b's get_live_drawn_graph.

   Arguments:
   - axes: Axes to draw on
   - value-fn: Function of time returning the y value
   - opts: {:t-max 10 :t-step 0.01 :color :green}"
  [axes value-fn & {:keys [t-max t-step color stroke-width]
                    :or {t-max 10 t-step 0.01 color :green stroke-width 3}}]
  {:type :live-drawn-graph
   :axes axes
   :value-fn value-fn
   :t-max t-max
   :t-step t-step
   :color color
   :stroke-width stroke-width
   ;; Current state
   :current-time (atom 0)
   :points (atom [])})

(defn update-live-graph!
  "Update a live-drawn graph with new time."
  [graph new-time]
  (let [t-step (:t-step graph)
        value-fn (:value-fn graph)
        axes (:axes graph)]
    (while (< @(:current-time graph) new-time)
      (let [t @(:current-time graph)
            y (value-fn t)
            point (c2p axes [t y])]
        (swap! (:points graph) conj point)
        (swap! (:current-time graph) + t-step)))))

;; =============================================================================
;; Brace and Labels
;; =============================================================================

(defn brace
  "Create a brace under/over/beside an object.
   Like Manim's Brace."
  [mobject direction & {:keys [buff color]
                        :or {buff 0.1 color :white}}]
  {:type :brace
   :target mobject
   :direction direction
   :buff buff
   :color color})

(defn brace-text
  "Get text positioned relative to a brace."
  [brace text & {:keys [buff font-size]
                 :or {buff 0.1 font-size 36}}]
  {:type :brace-text
   :brace brace
   :text text
   :buff buff
   :font-size font-size})

;; =============================================================================
;; Dot and GlowDot
;; =============================================================================

(defn dot
  "Create a dot."
  [& {:keys [point radius color]
      :or {point [0 0 0] radius 0.08 color :white}}]
  {:type :dot
   :point point
   :radius radius
   :color color})

(defn glow-dot
  "Create a glowing dot."
  [& {:keys [point radius color glow-factor]
      :or {point [0 0 0] radius 0.15 color :yellow glow-factor 2}}]
  {:type :glow-dot
   :point point
   :radius radius
   :color color
   :glow-factor glow-factor})

;; =============================================================================
;; Arc
;; =============================================================================

(defn arc
  "Create an arc."
  [& {:keys [start-angle angle radius arc-center color stroke-width]
      :or {start-angle 0 angle Math/PI radius 1 arc-center [0 0 0]
           color :white stroke-width 2}}]
  {:type :arc
   :start-angle start-angle
   :angle angle
   :radius radius
   :arc-center arc-center
   :color color
   :stroke-width stroke-width})

;; =============================================================================
;; Specs
;; =============================================================================

(s/def ::point (s/coll-of number? :min-count 2 :max-count 3))
(s/def ::color keyword?)
(s/def ::stroke-width (s/and number? pos?))
(s/def ::radius (s/and number? pos?))

(s/def ::x-range (s/coll-of number? :min-count 2 :max-count 3))
(s/def ::y-range (s/coll-of number? :min-count 2 :max-count 3))
