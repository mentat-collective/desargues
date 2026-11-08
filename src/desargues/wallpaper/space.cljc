(ns desargues.wallpaper.space
  "Pure layer of 3D wallpaper loops: a fixed pinhole camera, projection into
   the render's scene units, depth cues, and 3D shapes turned into the 2D
   shape maps `desargues.wallpaper.render` already paints (:dot :line :path).

   Coordinates are right-handed, z up by default. A camera is data:

     (camera {:eye [0 -6 2] :target [0 0 0] :up [0 0 1]
              :fov 40                 ; vertical field of view, degrees
              :view [14.22 8.0]       ; the spec's :view window, scene units
              :near 0.05})

   `project` maps a world point to [sx sy depth]: sx, sy in the scene units
   of that view window (x right, y up, origin at the window centre, so the
   vertical edge of the window is the edge of the field of view) and depth
   along the viewing axis. A point nearer than :near has no image (nil).

   Depth cues are a map {:range [d-near d-far] :alpha [a0 a1] :size [s0 s1]
   :fog [f0 f1]}: `depth-cue` turns a depth into the opacity, size factor and
   fog amount of a point at that depth, linear between the two depths.

   Shapes (`dot-shapes` `segment-shapes` `path-shapes` `face-shape`) carry a
   :depth; `frame` sorts them far to near and strips it, giving the value of
   a spec's :frame fn. `splat-rows` makes the rows of the additive splat
   layer (desargues.wallpaper.splat) for very many glowing points.

   Host-free .cljc: no interop beyond Math.")

;; ── Vectors ──────────────────────────────────────────────────────────────

(defn v+ [[a b c] [x y z]] [(+ a x) (+ b y) (+ c z)])
(defn v- [[a b c] [x y z]] [(- a x) (- b y) (- c z)])
(defn v* [k [a b c]] [(* k a) (* k b) (* k c)])
(defn dot [[a b c] [x y z]] (+ (* a x) (* b y) (* c z)))
(defn cross [[a b c] [x y z]] [(- (* b z) (* c y)) (- (* c x) (* a z)) (- (* a y) (* b x))])
(defn norm [a] (Math/sqrt (double (dot a a))))

(defn unit
  "`a` scaled to length 1 (or `a` itself when it has none)."
  [a]
  (let [n (norm a)] (if (zero? n) a (v* (/ 1.0 n) a))))

(defn rotate
  "Point `p` turned by `angle` radians about the axis through the origin along
   `axis` (Rodrigues)."
  [p axis angle]
  (let [k (unit axis) c (Math/cos angle) s (Math/sin angle)]
    (v+ (v+ (v* c p) (v* s (cross k p)))
        (v* (* (dot k p) (- 1.0 c)) k))))

(defn lerp
  "a + t (b - a)."
  [a b t]
  (+ a (* t (- b a))))

(defn mix-rgb
  "Colour [r g b] from `a` to `b` by `t`."
  [a b t]
  (mapv #(lerp %1 %2 t) a b))

(defn clamp01 [x] (max 0.0 (min 1.0 (double x))))

;; ── Camera and projection ────────────────────────────────────────────────

(def default-view
  "Manim's frame, as in desargues.wallpaper.render: 128/9 x 8 scene units."
  [(/ 128.0 9.0) 8.0])

(defn camera
  "A pinhole camera from `spec` {:eye :target :up :fov :view :near}. Adds the
   orthonormal :right :up :forward and the :focal length (scene units per
   unit of x/depth) that makes :fov the vertical field of view of :view."
  [{:keys [eye target up fov view near]
    :or {eye [0.0 -6.0 2.0] target [0.0 0.0 0.0] up [0.0 0.0 1.0] fov 40.0
         view default-view near 0.05}}]
  (let [fwd (unit (v- target eye))
        r0  (cross fwd up)
        rgt (unit (if (< (norm r0) 1e-9) (cross fwd [0.0 1.0 0.0]) r0))]
    {:eye (mapv double eye) :target (mapv double target)
     :right rgt :up (cross rgt fwd) :forward fwd
     :fov fov :view view :near near
     :focal (/ (/ (double (second view)) 2.0)
               (Math/tan (/ (* (double fov) Math/PI) 360.0)))}))

(defn to-camera
  "World point `p` in camera coordinates [x y depth]: x right, y up, depth
   along the viewing direction."
  [{:keys [eye right up forward]} p]
  (let [d (v- p eye)]
    [(dot d right) (dot d up) (dot d forward)]))

(defn- from-camera
  [{:keys [focal]} [x y d]]
  [(* focal (/ x d)) (* focal (/ y d)) d])

(defn project
  "[sx sy depth] of world point `p` under `cam`, sx sy in scene units; nil
   when the point is nearer than the camera's :near."
  [cam p]
  (let [[_ _ d :as c] (to-camera cam p)]
    (when (>= d (:near cam))
      (from-camera cam c))))

(defn magnification
  "Scene units per world unit, across the view, at `depth`."
  [cam depth]
  (/ (:focal cam) depth))

(defn project-all
  "The projections of `pts` that lie in front of the camera, in order."
  [cam pts]
  (into [] (keep #(project cam %)) pts))

(defn facing?
  "True when a surface at `centre` with `normal` turns its front to the eye."
  [cam centre normal]
  (pos? (double (dot normal (v- (:eye cam) centre)))))

(defn clip-polyline
  "Polyline `pts` (world) as the vector of runs that survive the near plane,
   each a vector of projected [sx sy depth]. A segment crossing the plane is
   cut where it crosses, not dropped."
  [cam pts]
  (let [near (:near cam)
        cs   (mapv #(to-camera cam %) pts)
        cut  (fn [a b] (let [t (/ (- near (a 2)) (- (b 2) (a 2)))]
                         (v+ a (v* t (v- b a)))))
        flush (fn [runs run] (if (> (count run) 1) (conj runs run) runs))
        [runs run]
        (reduce (fn [[runs run] [a b]]
                  (let [ia (>= (a 2) near) ib (>= (b 2) near)]
                    (cond (and ia ib) [runs (conj (if (empty? run) [a] run) b)]
                          ia          [(flush runs (conj (if (empty? run) [a] run) (cut a b))) []]
                          ib          [runs [(cut a b) b]]
                          :else       [runs run])))
                [[] []]
                (partition 2 1 cs))]
    (mapv #(mapv (partial from-camera cam) %) (flush runs run))))

;; ── Depth cue ────────────────────────────────────────────────────────────

(defn depth-cue
  "{:t :alpha :size :fog} at `depth` under `cue` {:range [d0 d1] :alpha [a0 a1]
   :size [s0 s1] :fog [f0 f1]}: t is 0 at d0 and 1 at d1 (clamped) and each
   of the others runs linearly from its first value to its second. A nil cue
   cues nothing."
  [cue depth]
  (let [[d0 d1] (:range cue [0.0 1.0])
        t (if (nil? cue) 0.0 (clamp01 (/ (- depth d0) (- d1 d0))))
        at (fn [k dflt] (let [[a b] (get cue k dflt)] (lerp a b t)))]
    {:t t :alpha (at :alpha [1.0 1.0]) :size (at :size [1.0 1.0]) :fog (at :fog [0.0 0.0])}))

;; ── Depth order ──────────────────────────────────────────────────────────

(defn sort-far-to-near
  "`items` with a :depth (or whatever `depth-fn` reads), the farthest first:
   the painter's order."
  ([items] (sort-far-to-near :depth items))
  ([depth-fn items] (sort-by #(double (or (depth-fn %) 0.0)) > items)))

(defn frame
  "A Frame (the value of a spec's :frame fn) of `shapes` carrying :depth,
   painted far to near; the :depth keys are dropped. opts {:background}."
  [shapes & [{:keys [background]}]]
  (cond-> {:shapes (mapv #(dissoc % :depth) (sort-far-to-near shapes))}
    background (assoc :background background)))

;; ── 3D to 2D shape maps ──────────────────────────────────────────────────

(def ^:private visible 0.004)

(defn- pick
  "`v` when it is a colour value, (v i p) when it is a function of the
   index and the point."
  [v i p]
  (if (fn? v) (v i p) v))

(defn dot-shapes
  "A :dot shape per point of `pts` in front of the camera. opts: :radius
   (world units, default 0.05), :color (palette key, CSS string, or a
   function of index and point), :cue (depth cue), :opacity."
  [cam pts & [{:keys [radius color cue opacity] :or {radius 0.05 color :white opacity 1.0}}]]
  (into []
        (keep-indexed
         (fn [i p]
           (when-let [[sx sy d] (project cam p)]
             (let [{:keys [alpha size]} (depth-cue cue d)
                   o (* opacity alpha)]
               (when (> o visible)
                 {:node :dot :at [sx sy] :depth d :opacity o
                  :opts {:radius (* radius size (magnification cam d))}
                  :fill {:color (pick color i p) :opacity 1.0} :stroke {:color nil}})))))
        pts))

(defn segment-shapes
  "A :line shape per segment of polyline `pts` that lies in front of the
   camera, each cued at its own depth, so a fibre fades along its length.
   opts: :color (as dot-shapes), :width (Manim px at 1080p, default 3), :cue,
   :opacity, :closed? to join the last point to the first."
  [cam pts & [{:keys [color width cue opacity closed?] :or {color :white width 3.0 opacity 1.0}}]]
  (let [pts (if (and closed? (seq pts)) (conj (vec pts) (first pts)) pts)]
    (into []
          (comp (mapcat #(clip-polyline cam %))
                (mapcat (fn [run] (partition 2 1 run)))
                (keep (fn [[[x0 y0 d0] [x1 y1 d1]]]
                        (let [d (* 0.5 (+ d0 d1))
                              {:keys [alpha size]} (depth-cue cue d)
                              o (* opacity alpha)]
                          (when (> o visible)
                            {:node :line :from [x0 y0] :to [x1 y1] :depth d :opacity o
                             :stroke {:color (pick color 0 nil) :width (* width size)}})))))
          [pts])))

(defn path-shapes
  "A :path shape per surviving run of polyline `pts`: one stroke of one
   opacity (cued at the run's mean depth). Cheaper than segment-shapes, and
   without per-segment joins. opts as segment-shapes."
  [cam pts & [{:keys [color width cue opacity closed?] :or {color :white width 3.0 opacity 1.0}}]]
  (let [pts (if (and closed? (seq pts)) (conj (vec pts) (first pts)) pts)]
    (into []
          (keep (fn [run]
                  (let [d (/ (reduce + 0.0 (map #(% 2) run)) (count run))
                        {:keys [alpha size]} (depth-cue cue d)
                        o (* opacity alpha)]
                    (when (> o visible)
                      {:node :path :points (mapv (fn [[x y _]] [x y]) run) :depth d :opacity o
                       :opts {:closed? false}
                       :stroke {:color (pick color 0 nil) :width (* width size)}}))))
          (clip-polyline cam pts))))

(defn face-shape
  "A closed filled :path of the convex world polygon `verts`, or nil when any
   vertex is behind the near plane or, with :cull?, the face turns its back
   to the eye (counter-clockwise seen from the front). opts: :fill (colour),
   :fill-opacity, :stroke (colour or nil), :width, :cue, :cull?."
  [cam verts & [{:keys [fill fill-opacity stroke width cue cull?]
                 :or {fill :white fill-opacity 1.0 width 1.0}}]]
  (let [ps (mapv #(project cam %) verts)]
    (when (and (>= (count verts) 3) (every? some? ps)
               (or (not cull?)
                   (let [[a b c] verts]
                     (facing? cam a (cross (v- b a) (v- c a))))))
      (let [d (/ (reduce + 0.0 (map #(% 2) ps)) (count ps))
            {:keys [alpha size]} (depth-cue cue d)]
        {:node :path :points (mapv (fn [[x y _]] [x y]) ps) :depth d :opacity alpha
         :opts {:closed? true}
         :fill {:color fill :opacity fill-opacity}
         :stroke (if stroke {:color stroke :width (* width size)} {:color nil})}))))

;; ── Rows for the splat layer ─────────────────────────────────────────────

(defn splat-rows
  "Rows [x y radius r g b] (scene units; r g b in [0, 1], already scaled by
   the point's opacity) for desargues.wallpaper.splat, one per point of `pts`
   in front of the camera. opts: :radius (world units, default 0.04),
   :color [r g b] or a function of index and point, :intensity (default 1),
   :cue (depth cue: its :alpha dims, its :size scales the radius, its :fog
   mixes toward :fog-color, default black, i.e. dims too)."
  [cam pts & [{:keys [radius color intensity cue fog-color]
               :or {radius 0.04 color [1.0 1.0 1.0] intensity 1.0 fog-color [0.0 0.0 0.0]}}]]
  (into []
        (keep-indexed
         (fn [i p]
           (when-let [[sx sy d] (project cam p)]
             (let [{:keys [alpha size fog]} (depth-cue cue d)
                   k (* intensity alpha)]
               (when (> k visible)
                 (let [[r g b] (v* k (mix-rgb (pick color i p) fog-color fog))]
                   [sx sy (* radius size (magnification cam d)) r g b])))))
         )
        pts))
