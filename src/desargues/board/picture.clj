(ns desargues.board.picture
  "Pure layer of boards: a sampled Board as a still picture, as data.

   A Board (desargues.board/sample-board or compile-board!) names what it
   draws by output keys; its :board/frame holds the numbers. `primitives`
   reads the layers against the frame into plain geometry in world
   coordinates:

     {:prim :point   :at [x y] :label s :color c}
     {:prim :segment :a [x y] :b [x y] :color c}
     {:prim :line    :a [x y] :b [x y] :color c}      ; infinite, through a and b
     {:prim :polygon :pts [[x y] ..] :color c :fill-opacity o}
     {:prim :path    :pts [[x y] ..] :color c}        ; also a :trace
     {:prim :value   :label s :value v}               ; a check's readout

   and two renderings of them, both plain data:

     ->mafs    the quoted emmy-viewers Mafs fragment (mafs.core/Mafs ...)
     ->scene   a desargues.scene.data recording the headless player paints

   A free or dragged point (a :handle) is drawn as a point: a still picture
   has nothing to drag. A 3D board (:board/view) is projected orthographically
   by its camera's yaw and pitch at their initial values. Coordinates are
   rounded to `digits` decimals, so the data is the same on every host
   (java.lang.Math may differ in the last ulp across CPUs)."
  (:require [desargues.player.frame :as frame]))

(def digits 6)

(defn- rnd [x]
  (let [m (Math/pow 10 digits)
        r (/ (Math/rint (* (double x) m)) m)]
    (if (== r 0.0) 0.0 r)))

(defn- param-init [board v]
  (if (keyword? v)
    (double (or (some #(when (= v (:id %)) (:init %)) (:board/params board)) 0))
    (double v)))

(defn- projector
  "World coordinates -> plane [x y]: identity in 2D; in 3D, turn about z by
   yaw, then tilt toward the viewer by pitch, times the view's scale."
  [board]
  (if-let [{:keys [yaw pitch scale]} (:board/view board)]
    (let [yaw (param-init board yaw) pitch (param-init board pitch) s (double (or scale 1))]
      (fn [[x y z]]
        (let [x1 (- (* x (Math/cos yaw)) (* y (Math/sin yaw)))
              y1 (+ (* x (Math/sin yaw)) (* y (Math/cos yaw)))]
          [(* s x1) (* s (- (* (or z 0.0) (Math/cos pitch)) (* y1 (Math/sin pitch))))])))
    (fn [[x y]] [x y])))

(defn- color-of [layer default]
  (let [c (or (:color layer) default)]
    (if (keyword? c) (get frame/palette c (name c)) (str c))))

(defn primitives
  "The picture of `board`: its drawn layers read against its frame, in draw
   order (see ns doc)."
  [board]
  (let [fr (:board/frame board)
        proj (projector board)
        at1 (fn [ks] (mapv rnd (proj (mapv #(double (first (get fr %))) ks))))
        samples (fn [ks] (let [cols (mapv #(get fr %) ks)]
                           (vec (distinct (apply map (fn [& c] (mapv rnd (proj (mapv double c)))) cols)))))]
    (vec
     (keep
      (fn [{:keys [layer] :as l}]
        (case layer
          :point   {:prim :point :at (at1 (:at l)) :label (:label l) :color (color-of l :gold)}
          :handle  {:prim :point :at (at1 (:at l)) :label (:label l) :color (color-of l :gold)}
          :segment {:prim :segment :a (at1 (:a l)) :b (at1 (:b l)) :color (color-of l :blue)}
          :line    {:prim :line :a (at1 (:a l)) :b (at1 (:b l)) :color (color-of l :grey)}
          :polygon {:prim :polygon :pts (mapv at1 (:pts l)) :color (color-of l :blue)
                    :fill-opacity (double (or (:fill-opacity l) 0.12))}
          :path    {:prim :path :pts (mapv at1 (:pts l)) :color (color-of l :blue)}
          :trace   {:prim :path :pts (samples (:of l)) :color (color-of l :teal)}
          :value   {:prim :value :label (:label l) :value (rnd (first (get fr (:of l))))}
          nil))
      (:board/layers board)))))

(defn- window [board]
  (let [{:keys [x y]} (:board/window board)] [(mapv double x) (mapv double y)]))

(defn- readout [{:keys [label value]}] (str label " = " value))

;; ---------------------------------------------------------------------------
;; emmy-viewers (Mafs)

(defn ->mafs
  "The quoted Mafs fragment drawing `board`: a Cartesian plane over the
   board's window (none for a 3D board), then every primitive. Wrap it as
   (emmy.viewer/fragment '<form>) to show it in Clerk."
  [board]
  (let [[[x0 x1] [y0 y1]] (window board)
        prims (primitives board)
        values (filter #(= :value (:prim %)) prims)]
    (into ['mafs.core/Mafs {:view-box {:x [x0 x1] :y [y0 y1]} :preserve-aspect-ratio "contain"}]
          (concat
           (when-not (:board/view board) [['mafs.coordinates/Cartesian {}]])
           (mapcat
            (fn [{:keys [prim color] :as p}]
              (case prim
                :polygon [['mafs.core/Polygon {:points (:pts p) :color color :fill-opacity (:fill-opacity p)}]]
                :path    [['mafs.core/Polyline {:points (:pts p) :color color}]]
                :segment [['mafs.line/Segment {:point1 (:a p) :point2 (:b p) :color color}]]
                :line    [['mafs.line/ThroughPoints {:point1 (:a p) :point2 (:b p) :color color}]]
                :point   (cond-> [['mafs.core/Point {:x (first (:at p)) :y (second (:at p)) :color color}]]
                           (:label p) (conj ['mafs.core/Text {:x (first (:at p)) :y (second (:at p))
                                                               :attach "ne" :color color} (:label p)]))
                nil))
            prims)
           (map-indexed (fn [i v]
                          ['mafs.core/Text {:x x0 :y (- y1 (* (inc i) 0.06 (- y1 y0))) :attach "e"}
                           (readout v)])
                        values)))))

;; ---------------------------------------------------------------------------
;; Scene graph (headless player)

(def ^:private scene-frame [(/ 128.0 9.0) 8.0])

(defn ->scene
  "`board` as a desargues.scene.data recording: its window fitted into the
   player's frame, every primitive appearing at once, then a hold. Painted
   by desargues.player.headless like any recorded scene."
  ([board] (->scene board {}))
  ([board {:keys [scene hold] :or {hold 1.0}}]
   (let [[[x0 x1] [y0 y1]] (window board)
         [fw fh] scene-frame
         k (* 0.92 (min (/ fw (- x1 x0)) (/ fh (- y1 y0))))
         cx (/ (+ x0 x1) 2) cy (/ (+ y0 y1) 2)
         ->s (fn [[x y]] [(rnd (* k (- x cx))) (rnd (* k (- y cy)))])
         far (* 2 (max (- x1 x0) (- y1 y0)))
         prims (primitives board)
         values (filter #(= :value (:prim %)) prims)
         nodes (concat
                (when-not (:board/view board)
                  [{:node :line :from (->s [x0 0.0]) :to (->s [x1 0.0]) :opts {:color :grey :width 1}}
                   {:node :line :from (->s [0.0 y0]) :to (->s [0.0 y1]) :opts {:color :grey :width 1}}])
                (mapcat
                 (fn [{:keys [prim color] :as p}]
                   (case prim
                     :polygon [{:node :path :points (mapv ->s (:pts p)) :opts {:color color :width 3 :closed? true}
                                :fill {:color color :opacity (:fill-opacity p)}}]
                     :path    [{:node :path :points (mapv ->s (:pts p)) :opts {:color color :width 3}}]
                     :segment [{:node :line :from (->s (:a p)) :to (->s (:b p)) :opts {:color color :width 4}}]
                     :line    (let [[ax ay] (:a p) [bx by] (:b p)
                                    d (Math/hypot (- bx ax) (- by ay))
                                    [ux uy] (if (zero? d) [0.0 0.0] [(/ (- bx ax) d) (/ (- by ay) d)])]
                                [{:node :line :from (->s [(- ax (* far ux)) (- ay (* far uy))])
                                  :to (->s [(+ ax (* far ux)) (+ ay (* far uy))]) :opts {:color color :width 2}}])
                     :point   (cond-> [{:node :dot :at (->s (:at p)) :opts {:color color :radius 0.09}}]
                                (:label p) (conj {:node :text :text (:label p)
                                                  :at (mapv + (->s (:at p)) [0.3 0.3])
                                                  :opts {:color color :font-size 28}}))
                     nil))
                 prims)
                (map-indexed (fn [i v]
                               {:node :text :text (readout v) :at [(- 0.3 (/ fw 2)) (- (/ fh 2) 0.5 (* 0.5 i))]
                                :opts {:color :white :font-size 26 :anchor "start"}})
                             values))
         nodes (map-indexed (fn [i n] (assoc n :id (inc i))) nodes)]
     {:scene (or scene (name (:board/id board)))
      :nodes (into {} (map (juxt :id identity)) nodes)
      :steps [{:step :play :anims (mapv (fn [n] {:anim :appear :target (:id n) :opts nil}) nodes)}
              {:step :hold :seconds hold}]
      :node-count (count nodes)})))
