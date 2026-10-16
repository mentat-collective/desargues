(ns desargues.player.drag
  "PURE stratum of the player: draggable parameters.

   A drag spec is a parameter spec carrying :drag,

     {:id :a :drag {:node \"A\"}}

   and says: dragging the shape `:node` writes its new position into
   parameter `:id`. The player then re-records the construct exactly as a
   slider change does, so the picture follows the pointer.

   :node names the shape. A string or keyword matches the node's
   [:opts :name] (`(s/dot :name \"A\")`; Manim's Mobject takes `name` too, so
   the same construct still renders an mp4) or one of a :points3 shape's
   [:opts :names]; an integer is a raw scene-graph node id.

   How the position becomes parameters (pure, (fn [params p]) params'):
     default            params[:id] = p, [x y] (or [x y z] for a 3D point)
     :axis :x | :y | :z params[:id] = that coordinate, a one-dimensional handle
     :->params f        any function of the position vector, e.g. a point
                        constrained to a circle or to a surface

   A point of a :points3 shape is dragged in world space. The pointer's scene
   point is unprojected to a ray through the frame's camera
   (desargues.player.space/unproject) and the ray meets a plane:

     {:id :p :drag {:node \"A\" :plane {:point [0 0 0] :normal [0 0 1]}}}

   moves A in the plane z = 0. :plane may also be (fn [params] plane). Without
   :plane, the point moves in the plane through it facing the camera. The
   frame handed to `pointer` must carry the camera the viewer sees (the
   player passes the orbited one).

   A drag is a small value, {:spec :offset}, threaded through `pointer`:
   :down on a draggable shape starts one (remembering the grab offset, in
   scene units or in world space for a 3D point, so the shape does not jump
   under the cursor), :move yields new params, :up ends it. Host-free .cljc."
  (:require [desargues.player.space :as space]))

(defn drag-spec?
  "A parameter spec that is moved by dragging rather than by a widget."
  [spec]
  (contains? spec :drag))

(defn- named? [x] (or (string? x) (keyword? x) (symbol? x)))

(defn point-index
  "Index of the point named `n` among :points3 shape `s`'s [:opts :names],
   or nil."
  [s n]
  (when (named? n)
    (first (keep-indexed (fn [i x] (when (and (named? x) (= (name n) (name x))) i))
                         (get-in s [:opts :names])))))

(defn targets?
  "Does drag `spec` name shape `s`?"
  [spec s]
  (let [n (get-in spec [:drag :node])]
    (if (named? n)
      (or (= (name n) (some-> (get-in s [:opts :name]) name))
          (some? (point-index s n)))
      (= n (:id s)))))

(defn- shape [f id] (some #(when (= id (:id %)) %) (:shapes f)))

(defn world-point
  "The world position of the 3D point drag `spec` names in shape `s`, or nil
   when `s` is not a :points3 shape naming it."
  [spec s]
  (when (= :points3 (:node s))
    (when-let [i (point-index s (get-in spec [:drag :node]))]
      (mapv double (nth (:points s) i)))))

(defn- anchor
  "Where shape `s` sits on screen: its :at, else the midpoint of a line."
  [s]
  (or (some-> (:at s) (subvec 0 2))
      (when-let [[a b] (and (:from s) [(:from s) (:to s)])]
        [(/ (+ (double (first a)) (double (first b))) 2.0)
         (/ (+ (double (second a)) (double (second b))) 2.0)])
      [0.0 0.0]))

(defn- screen-anchor
  "Where the handle drag `spec` grabs in shape `s` shows in frame `f`."
  [spec f s]
  (if-let [w (world-point spec s)]
    (:at (space/project (space/camera f) w))
    (anchor s)))

(defn- dist2 [[ax ay] [bx by]]
  (let [dx (- (double ax) (double bx)) dy (- (double ay) (double by))]
    (+ (* dx dx) (* dy dy))))

(defn spec-for
  "The drag spec among `specs` whose node is shape id `hit` in frame `f`.
   When several name handles of that shape (the points of one :points3),
   the one shown nearest scene point `at` wins."
  ([specs f hit] (spec-for specs f hit nil))
  ([specs f hit at]
   (when (some? hit)
     (when-let [s (shape f hit)]
       (let [cands (filter #(and (drag-spec? %) (targets? % s)) specs)]
         (if (and at (next cands))
           (apply min-key #(dist2 at (screen-anchor % f s)) cands)
           (first cands)))))))

(defn ->params
  "Parameters `params` after drag `spec` moved its handle to point `p`
   ([x y] in the scene, or [x y z] in the world for a 3D point)."
  [spec params p]
  (let [{:keys [axis] f :->params} (:drag spec)
        p (mapv double p)]
    (cond
      f            (f params p)
      (= :x axis)  (assoc params (:id spec) (nth p 0))
      (= :y axis)  (assoc params (:id spec) (nth p 1))
      (= :z axis)  (assoc params (:id spec) (nth p 2))
      :else        (assoc params (:id spec) p))))

(defn drag-plane
  "The world plane {:point :normal} a 3D drag of `spec` moves in: its :plane
   (a value or (fn [params] plane)), else the plane through world point `w`
   facing camera `cam`."
  [spec params cam w]
  (let [pl (get-in spec [:drag :plane])]
    (cond
      (fn? pl) (pl params)
      pl       pl
      :else    {:point w :normal (:forward (space/basis cam))})))

(defn- on-plane
  "Where the pointer at scene point `at` meets `plane` under camera `cam`."
  [cam at plane]
  (space/ray-plane (space/unproject cam at) plane))

(defn- start
  "The drag value for a press at scene point `at` on shape `s` of frame `f`."
  [spec f s params at]
  (if-let [w (world-point spec s)]
    (let [cam   (space/camera f)
          plane (drag-plane spec params cam w)
          hit   (on-plane cam at plane)]
      {:spec spec :plane plane :camera cam
       :offset (if hit (space/v- w hit) [0.0 0.0 0.0])})
    (let [[sx sy] (anchor s) [px py] at]
      {:spec spec :offset [(- sx px) (- sy py)]})))

(defn- moved
  "The handle position for drag value `drag` with the pointer at `at`, or
   nil when the pointer's ray misses the drag plane."
  [{:keys [offset plane camera]} at]
  (if plane
    (some-> (on-plane camera at plane) (space/v+ offset))
    (let [[ox oy] offset [px py] at]
      [(+ (double px) ox) (+ (double py) oy)])))

(defn pointer
  "One pointer `event` ({:type :at :hit}) against drag value `drag` (nil when
   idle), the drag `specs`, the current frame `f` (with the camera the viewer
   sees, for a 3D frame) and `params`.
   Returns {:drag drag' :params params'}; params' is nil when unchanged."
  [drag specs f params {:keys [type at hit]}]
  (case type
    :down (if-let [spec (spec-for specs f hit at)]
            {:drag (start spec f (shape f hit) params at) :params nil}
            {:drag nil :params nil})
    :move (if drag
            {:drag drag :params (some->> (moved drag at) (->params (:spec drag) params))}
            {:drag nil :params nil})
    :up   {:drag nil :params nil}
    {:drag drag :params nil}))
