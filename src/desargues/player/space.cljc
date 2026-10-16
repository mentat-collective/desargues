(ns desargues.player.space
  "CORE + PURE stratum of the player: the 3D frame vocabulary and its
   projection to 2D.

   A 3D frame is an ordinary Frame (desargues.player.frame) that also carries
   a :camera and shapes of the 3D kinds below. Coordinates are scene units in
   a right-handed space with z up (the mathematician's z = f(x, y)).

     :surface3  {:grid [[[x y z] ...] ...]}  rows of a parametric surface, or
                {:vertices [[x y z] ...] :faces [[i j k] | [i j k l] ...]}
                :fill {:color :opacity} shades it; :opts {:wire color} adds a
                wireframe.
     :curve3    {:points [[x y z] ...]}      a space curve (:draw draws it on).
     :points3   {:points [[x y z] ...] :opts {:radius r :names [\"A\" ...]}}
     :axes3     {:opts {:range [[x0 x1] [y0 y1] [z0 z1]] :labels? true}}

   The camera orbits a target:
     {:target [x y z] :distance d :yaw radians :pitch radians
      :projection (:perspective | :orthographic)}
   yaw turns about z, pitch lifts the eye above the xy plane. Perspective
   keeps the target plane at scene scale: a point at depth z is scaled by
   distance / z, so a 3D frame and its flat projection agree at the target.

   A host that renders 3D natively opts in through
   desargues.player.protocols/ISpace (ISP: 2D hosts are untouched). Every
   other host is handed `->2d` of the frame: the 3D shapes become ordinary 2D
   shapes (closed :path faces, :line segments, :dot points, :text labels),
   depth sorted back to front (painter's algorithm, with faces that pass
   through each other cut apart first), so golden images, the headless PNG
   host and the browser canvas render 3D frames unchanged. 2D shapes in a 3D
   frame are an overlay drawn after the 3D content.

   `unproject` inverts `project`: a scene point becomes the world ray it
   shows, and `ray-plane` meets that ray with a plane, which is how a point
   is dragged in space (desargues.player.drag).

   Host-free .cljc: no interop, no reader conditionals."
  (:require [desargues.player.cut :as cut]
            [desargues.player.frame :as frame]
            [desargues.player.vec3 :as vec3]))

;; ── Vocabulary ─────────────────────────────────────────────────────────────

(def shape-kinds
  "The 3D node kinds a Frame can carry beside desargues.player.frame/shape-kinds."
  #{:surface3 :curve3 :points3 :axes3})

(def default-camera
  "Looking at the origin from the first octant, a little above the xy plane."
  {:target [0.0 0.0 0.0] :distance 12.0 :yaw -1.0 :pitch 0.45 :projection :perspective})

(def near
  "Primitives with a vertex closer than this (scene units) to the eye are culled." 0.05)

(declare primitives)
(defn three-d-shape?
  "A shape of a 3D kind: one of shape-kinds, or any kind with a `primitives` method (OCP)."
  [s]
  (let [k (:node s)] (or (contains? shape-kinds k) (contains? (methods primitives) k))))

(defn three-d?
  "A frame carrying a camera or any 3D shape."
  [f]
  (boolean (or (:camera f) (some three-d-shape? (:shapes f)))))

(defn camera
  "The camera of frame `f`, defaults filled in."
  [f]
  (merge default-camera (:camera f)))

(defn with-camera
  "Frame `f` seen through camera `cam` (merged over its own)."
  [f cam]
  (if cam (assoc f :camera (merge (camera f) cam)) f))

;; ── Vector math (desargues.player.vec3, re-exported) ────────────────────────

(def v+ vec3/v+)
(def v- vec3/v-)
(def v* vec3/v*)
(def dot3 vec3/dot3)
(def cross vec3/cross)
(def norm vec3/norm)
(def unit vec3/unit)

;; ── Camera and projection ──────────────────────────────────────────────────

(defn eye
  "Where camera `cam` sits."
  [{:keys [target distance yaw pitch]}]
  (let [cp (Math/cos pitch)]
    (v+ target (v* distance [(* cp (Math/cos yaw)) (* cp (Math/sin yaw)) (Math/sin pitch)]))))

(defn basis
  "Camera `cam`'s orthonormal frame {:eye :right :up :forward}: forward points
   from the eye to the target, up is as close to +z as forward allows."
  [cam]
  (let [cam (merge default-camera cam)
        e   (eye cam)
        fwd (unit (v- (:target cam) e))
        r0  (cross fwd [0.0 0.0 1.0])
        rgt (unit (if (< (norm r0) 1e-9) [1.0 0.0 0.0] r0))]
    {:eye e :right rgt :up (cross rgt fwd) :forward fwd}))

(defn ->camera
  "World point `p` in camera coordinates [x y depth] under basis `b`: x right,
   y up, depth along forward (positive in front of the eye)."
  [{:keys [eye right up forward]} p]
  (let [d (v- (mapv double p) eye)]
    [(dot3 d right) (dot3 d up) (dot3 d forward)]))

(defn project
  "Scene point [x y] and depth of world point `p` under camera `cam` (or a
   precomputed `basis`). Perspective scales by distance / depth."
  ([cam p] (project cam (basis cam) p))
  ([cam b p]
   (let [cam     (merge default-camera cam)
         [x y z] (->camera b p)]
     (if (= :orthographic (:projection cam))
       {:at [x y] :depth z :scale 1.0}
       (let [k (/ (double (:distance cam)) (max near z))]
         {:at [(* k x) (* k y)] :depth z :scale k})))))

;; ── Unprojection (scene point -> world ray) ─────────────────────────────────

(defn unproject
  "The world ray {:origin :dir} of everything camera `cam` (or a precomputed
   basis `b`) shows at scene point [sx sy]; the inverse of `project`. Under
   perspective the ray leaves the eye through the scene point on the target
   plane (origin eye, dir along sx right + sy up + distance forward); under
   orthographic it leaves the eye's plane at sx right + sy up and runs along
   forward. :dir has length 1."
  ([cam at] (unproject cam (basis cam) at))
  ([cam {:keys [eye right up forward]} [sx sy]]
   (let [cam  (merge default-camera cam)
         side (v+ (v* sx right) (v* sy up))]
     (if (= :orthographic (:projection cam))
       {:origin (v+ eye side) :dir forward}
       {:origin eye :dir (unit (v+ side (v* (:distance cam) forward)))}))))

(defn ray-plane
  "Where ray {:origin :dir} meets plane {:point :normal}, or nil when the ray
   runs parallel to the plane or meets it behind its origin."
  [{:keys [origin dir]} {:keys [point normal]}]
  (let [den (double (dot3 normal dir))]
    (when (> (Math/abs den) 1e-12)
      (let [t (/ (double (dot3 normal (v- point origin))) den)]
        (when (>= t 0.0)
          (v+ origin (v* t dir)))))))

;; ── Builders (pure data) ──────────────────────────────────────────────────

(defn- samples [a b n]
  (let [n (max 1 (long n))]
    (map #(+ (double a) (* (- (double b) (double a)) (/ (double %) n))) (range (inc n)))))

(defn parametric-grid
  "The grid of surface `f` (fn [u v] [x y z]) over [u0 u1] x [v0 v1] with
   `nu` x `nv` cells: (inc nu) rows of (inc nv) points."
  [f [u0 u1] [v0 v1] nu nv]
  (mapv (fn [u] (mapv (fn [v] (mapv double (f u v))) (samples v0 v1 nv)))
        (samples u0 u1 nu)))

(defn parametric-points
  "Points of curve `f` (fn [t] [x y z]) over [t0 t1] in `n` steps."
  [f [t0 t1] n]
  (mapv #(mapv double (f %)) (samples t0 t1 n)))

(defn surface
  "A :surface3 shape of parametric `f` over the given ranges. opts: :id :nu
   :nv :color :opacity :wire."
  [f u-range v-range {:keys [id nu nv color opacity wire] :or {nu 24 nv 24 color :blue opacity 1.0}}]
  (cond-> {:id id :node :surface3 :grid (parametric-grid f u-range v-range nu nv)
           :fill {:color color :opacity opacity} :opacity 1.0 :draw 1.0}
    wire (assoc-in [:opts :wire] wire)))

(defn curve
  "A :curve3 shape of parametric `f` over `t-range`. opts: :id :n :color :width."
  [f t-range {:keys [id n color width] :or {n 120 color :yellow width 4}}]
  {:id id :node :curve3 :points (parametric-points f t-range n)
   :stroke {:color color :width width} :opacity 1.0 :draw 1.0})

(defn grid->mesh
  "{:vertices :faces} of a rectangular `grid` (quads, counter-clockwise)."
  [grid]
  (let [rows (count grid) cols (count (first grid))
        idx  (fn [i j] (+ (* i cols) j))]
    {:vertices (vec (apply concat grid))
     :faces    (vec (for [i (range (dec rows)) j (range (dec cols))]
                      [(idx i j) (idx (inc i) j) (idx (inc i) (inc j)) (idx i (inc j))]))}))

(defn mesh
  "The {:vertices :faces} of :surface3 shape `s`, from :grid or as given."
  [s]
  (if (:grid s) (grid->mesh (:grid s)) (select-keys s [:vertices :faces])))

;; ── Validation (pure) ─────────────────────────────────────────────────────

(defn- finite? [x]
  (and (number? x) #?(:clj (not (or (Double/isNaN (double x)) (Double/isInfinite (double x))))
                      :cljs (js/isFinite x))))

(defn point3?
  "A 3-vector of finite numbers."
  [p]
  (and (sequential? p) (= 3 (count p)) (every? finite? p)))

(defn camera-problems
  "Problems with camera `cam` (nil is fine: the default camera)."
  [cam]
  (when cam
    (let [c (merge default-camera cam)]
      (cond-> []
        (not (point3? (:target c)))                       (conj :camera/target)
        (not (and (finite? (:distance c)) (pos? (:distance c)))) (conj :camera/distance)
        (not (finite? (:yaw c)))                          (conj :camera/yaw)
        (not (finite? (:pitch c)))                        (conj :camera/pitch)
        (not (#{:perspective :orthographic} (:projection c))) (conj :camera/projection)))))

(defmulti shape-problems
  "Problems (keywords) with 3D shape `s`; empty when it is well formed. One
   method per 3D kind (OCP)."
  :node)

(defmethod shape-problems :default [_] [])

(defn- points-problems [pts k]
  (cond (not (sequential? pts)) [k]
        (not (every? point3? pts)) [k]
        :else []))

(defmethod shape-problems :surface3 [s]
  (if-let [g (:grid s)]
    (cond (not (and (sequential? g) (>= (count g) 2))) [:surface3/grid]
          (not (apply = (map count g)))                [:surface3/ragged-grid]
          (< (count (first g)) 2)                      [:surface3/grid]
          (not (every? point3? (apply concat g)))      [:surface3/point]
          :else [])
    (let [{:keys [vertices faces]} s
          n (count vertices)]
      (cond (not (seq vertices))                 [:surface3/vertices]
            (not (every? point3? vertices))      [:surface3/point]
            (not (seq faces))                    [:surface3/faces]
            (not (every? #(and (<= 3 (count %) 4)
                               (every? (fn [i] (and (integer? i) (< -1 i n))) %))
                         faces))                 [:surface3/face-index]
            :else []))))

(defmethod shape-problems :curve3 [s]
  (let [p (points-problems (:points s) :curve3/points)]
    (if (and (empty? p) (< (count (:points s)) 2)) [:curve3/too-short] p)))

(defmethod shape-problems :points3 [s]
  (points-problems (:points s) :points3/points))

(defmethod shape-problems :axes3 [s]
  (let [r (get-in s [:opts :range])]
    (if (or (nil? r)
            (and (= 3 (count r)) (every? #(and (= 2 (count %)) (every? finite? %)) r)))
      []
      [:axes3/range])))

(defn problems
  "Every problem in frame `f`: [{:shape id :problem kw} ...] plus camera
   problems as {:problem kw}. Empty for a well-formed frame."
  [f]
  (into (mapv (fn [k] {:problem k}) (camera-problems (:camera f)))
        (for [s (:shapes f) :when (three-d-shape? s) k (shape-problems s)]
          {:shape (:id s) :problem k})))

(defn valid?
  "A frame with no problems."
  [f]
  (empty? (problems f)))

;; ── Shading (pure colour math) ────────────────────────────────────────────

(defn- parse-hex [c]
  (let [s (if (keyword? c) (get frame/palette c) c)]
    (when (and (string? s) (re-matches #"#[0-9a-fA-F]{6}" s))
      (mapv #(#?(:clj Integer/parseInt :cljs js/parseInt) (subs s % (+ % 2)) 16) [1 3 5]))))

(defn shade
  "Colour `c` (palette keyword or #RRGGBB) dimmed to `k` (0..1) as a CSS
   rgb() string; `c` unchanged when it cannot be parsed."
  [c k]
  (if-let [rgb (parse-hex c)]
    (let [[r g b] (map #(Math/round (* (double %) (double k))) rgb)]
      (str "rgb(" r "," g "," b ")"))
    c))

;; ── Flattening a 3D frame to 2D (painter's algorithm) ─────────────────────

(defmulti primitives
  "The 2D primitives of 3D shape `s` under camera `cam` and basis `b`: 2D
   shapes (desargues.player.frame kinds) each with a :depth for sorting. A
   face also carries its :world vertices, so faces that pass through each
   other can be cut apart before sorting (`split-intersecting`).
   One method per 3D kind (OCP)."
  (fn [_cam _b s] (:node s)))

(defmethod primitives :default [_ _ _] [])

(defn- carry
  "The fields a primitive inherits from its 3D shape."
  [s]
  (select-keys s [:id :opacity :emphasis]))

(defn- in-front? [ps] (every? #(> (double (:depth %)) near) ps))

(defn- mean [xs] (/ (reduce + 0.0 xs) (max 1 (count xs))))

(defmethod primitives :surface3 [cam b s]
  (let [{:keys [vertices faces]} (mesh s)
        proj  (mapv #(project cam b %) vertices)
        n     (long (Math/ceil (* (double (:draw s 1.0)) (count faces))))
        color (get-in s [:fill :color] :blue)
        alpha (get-in s [:fill :opacity] 1.0)
        wire  (get-in s [:opts :wire])]
    (for [face (take n faces)
          :let [ps (mapv proj face)]
          :when (in-front? ps)
          :let [[a c d] (map vertices [(face 0) (face 1) (face 2)])
                nrm  (unit (cross (v- c a) (v- d a)))
                lit  (+ 0.3 (* 0.7 (Math/abs (double (dot3 nrm (:forward b))))))]]
      (merge (carry s)
             {:node :path :points (mapv :at ps) :opts {:closed? true}
              :fill {:color (shade color lit) :opacity alpha}
              :stroke (if wire {:color wire :width 1} {:color nil})
              :draw 1.0 :depth (mean (map :depth ps))
              :world (mapv #(mapv double (vertices %)) face)}))))

(def ^:private line-bias
  "Lines sort this much nearer (scene units) than faces at the same depth, so
   a curve lying on a surface is not hidden by the faces it crosses. Measured
   on the space demo (20 x 20 saddle): 0.05 keeps 64% of the curve, 0.3 keeps
   95%, and points still farther behind a face stay hidden."
  0.3)

(defn- segments [cam b s pts]
  (for [[p q] (partition 2 1 pts)
        :let [a (project cam b p) c (project cam b q)]
        :when (in-front? [a c])]
    (merge (carry s)
           {:node :line :from (:at a) :to (:at c) :draw 1.0
            :stroke (:stroke s {:color :white :width 4})
            :depth (- (mean [(:depth a) (:depth c)]) line-bias)})))

(defmethod primitives :curve3 [cam b s]
  (let [pts (:points s)
        n   (long (Math/ceil (* (double (:draw s 1.0)) (count pts))))]
    (segments cam b s (take (max 0 n) pts))))

(defmethod primitives :points3 [cam b s]
  (let [r     (get-in s [:opts :radius] 0.08)
        names (get-in s [:opts :names])
        color (or (get-in s [:opts :color]) (get-in s [:stroke :color]) :white)]
    (for [[i p] (map-indexed vector (:points s))
          :let [{:keys [at depth scale]} (project cam b p)]
          :when (> depth near)]
      (merge (carry s)
             {:node :dot :at at :draw 1.0 :depth (- depth (* 2 line-bias))
              :opts (cond-> {:radius (* r scale) :color color}
                      (get names i) (assoc :name (get names i)))}))))

(defmethod primitives :axes3 [cam b s]
  (let [[[x0 x1] [y0 y1] [z0 z1]] (get-in s [:opts :range] [[-3 3] [-3 3] [-3 3]])
        color  (or (get-in s [:opts :color]) :grey)
        axes   [[[x0 0 0] [x1 0 0] "x"] [[0 y0 0] [0 y1 0] "y"] [[0 0 z0] [0 0 z1] "z"]]
        stroke {:color color :width 2}]
    (concat
     (mapcat (fn [[p q _]] (segments cam b (assoc s :stroke stroke) [p q])) axes)
     (when (get-in s [:opts :labels?] true)
       (for [[_ q label] axes
             :let [{:keys [at depth]} (project cam b (v* 1.08 q))]
             :when (> depth near)]
         (merge (carry s) {:node :text :text label :at at :draw 1.0 :depth (- depth line-bias)
                           :opts {:font-size 30 :color color}}))))))

(defn split-intersecting
  "Primitives `prims` (as `primitives` makes them under camera `cam` and
   basis `b`) with faces that pass through each other cut apart along their
   common line (desargues.player.cut), so a depth sort orders the pieces."
  [cam b prims]
  (cut/split-intersecting #(let [q (project cam b %)] (when (> (double (:depth q)) near) q))
                          prims))

(defn ->2d
  "Frame `f` with its 3D shapes projected to 2D primitives, back to front,
   followed by its 2D shapes in their own order (an overlay). Faces that pass
   through each other are cut apart first (`split-intersecting`). A frame
   with no 3D content is returned unchanged; the :camera is dropped from the
   result."
  [f]
  (if-not (three-d? f)
    f
    (let [cam   (camera f)
          b     (basis cam)
          shown (filter #(pos? (double (:opacity % 0.0))) (:shapes f))
          prims (->> (filter three-d-shape? shown)
                     (mapcat #(primitives cam b %))
                     (split-intersecting cam b)
                     (sort-by :depth >)
                     (map #(dissoc % :depth :world)))]
      (-> f
          (dissoc :camera)
          (assoc :shapes (vec (concat prims (remove three-d-shape? (:shapes f)))))))))
