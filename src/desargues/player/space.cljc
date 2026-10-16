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
   depth sorted back to front (painter's algorithm), so golden images, the
   headless PNG host and the browser canvas render 3D frames unchanged.
   2D shapes in a 3D frame are an overlay drawn after the 3D content.

   Host-free .cljc: no interop, no reader conditionals."
  (:require [desargues.player.frame :as frame]))

;; ── Vocabulary ─────────────────────────────────────────────────────────────

(def shape-kinds
  "The 3D node kinds a Frame can carry beside desargues.player.frame/shape-kinds."
  #{:surface3 :curve3 :points3 :axes3})

(def default-camera
  "Looking at the origin from the first octant, a little above the xy plane."
  {:target [0.0 0.0 0.0] :distance 12.0 :yaw -1.0 :pitch 0.45 :projection :perspective})

(def near
  "Primitives with a vertex closer than this (scene units) to the eye are culled."
  0.05)

(defn three-d-shape?
  "A shape of one of the 3D kinds."
  [s]
  (contains? shape-kinds (:node s)))

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

;; ── Vector math ────────────────────────────────────────────────────────────

(defn v+ [a b] (mapv + a b))
(defn v- [a b] (mapv - a b))
(defn v* [k a] (mapv #(* (double k) (double %)) a))
(defn dot3 [[a b c] [x y z]] (+ (* a x) (* b y) (* c z)))
(defn cross [[a b c] [x y z]] [(- (* b z) (* c y)) (- (* c x) (* a z)) (- (* a y) (* b x))])
(defn norm [a] (Math/sqrt (double (dot3 a a))))

(defn unit
  "`a` scaled to length 1 (or `a` itself when it has none)."
  [a]
  (let [n (norm a)] (if (zero? n) a (v* (/ 1.0 n) a))))

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
   shapes (desargues.player.frame kinds) each with a :depth for sorting.
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
              :draw 1.0 :depth (mean (map :depth ps))}))))

(def ^:private line-bias
  "Lines sort this much nearer than faces at the same depth, so a curve on a
   surface is not hidden by the faces it lies on."
  0.05)

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

(defn ->2d
  "Frame `f` with its 3D shapes projected to 2D primitives, back to front,
   followed by its 2D shapes in their own order (an overlay). A frame with no
   3D content is returned unchanged; the :camera is dropped from the result."
  [f]
  (if-not (three-d? f)
    f
    (let [cam   (camera f)
          b     (basis cam)
          shown (filter #(pos? (double (:opacity % 0.0))) (:shapes f))
          prims (->> (filter three-d-shape? shown)
                     (mapcat #(primitives cam b %))
                     (sort-by :depth >)
                     (map #(dissoc % :depth)))]
      (-> f
          (dissoc :camera)
          (assoc :shapes (vec (concat prims (remove three-d-shape? (:shapes f)))))))))
