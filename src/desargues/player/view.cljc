(ns desargues.player.view
  "PURE stratum of the player: the 2D viewport a viewer zooms and pans.

   A view is {:center [x y] :scale k}: the source scene point at `center`
   shows at the middle of the surface, magnified `k` times. nil (or
   `identity-view`) is the source's own framing. It is applied to a 2D Frame
   before painting (`seen-frame`), as a transform in scene units, so every
   host (headless PNG, JavaFX, browser canvas, native) shows the zoomed
   picture without knowing views exist, and hit-testing the painted frame
   answers for what the viewer sees.

   What scales: positions and the extent of geometric shapes (a circle's
   radius, a rectangle's width and height, an SVG path). What does not:
   dots, text, stroke widths and dashes, which keep their size on screen, as
   in a plotting tool. `shape` is a multimethod on the node kind (OCP); the
   default maps the position fields (:at :from :to :points).

   Pointer events arrive in seen units (desargues.player.frame/px->scene of
   the painted frame); `->source` takes them back to the source's scene
   units, which is what a drag writes into parameters.

   `zoom-at` keeps the scene point under the pointer fixed while it scales;
   `pan` moves the picture by a pointer travel. The scale is clamped to
   `scale-limits`. Host-free .cljc."
  (:require [clojure.string :as str]
            [desargues.player.svg-path :as svg-path]))

(def identity-view
  "The source's own framing: nothing moved, nothing scaled."
  {:center [0.0 0.0] :scale 1.0})

(def scale-limits
  "The smallest and largest magnification a view may reach."
  [0.05 50.0])

(defn view
  "View `v` with defaults filled in (nil is `identity-view`)."
  [v]
  (merge identity-view v))

(defn clamp-scale
  "Scale `k` held within `scale-limits`."
  [k]
  (let [[lo hi] scale-limits]
    (max lo (min hi (double k)))))

(defn identity?
  "Does view `v` leave the picture as the source frames it?"
  [v]
  (let [{[cx cy] :center k :scale} (view v)]
    (and (zero? (double cx)) (zero? (double cy)) (== 1.0 (double k)))))

(defn ->seen
  "Where source scene point `p` shows under view `v` (extra coordinates kept)."
  [v [x y & more]]
  (let [{[cx cy] :center k :scale} (view v)
        k (double k)]
    (into [(* k (- (double x) (double cx))) (* k (- (double y) (double cy)))] more)))

(defn ->source
  "The source scene point shown at seen point `q` under view `v`: the
   inverse of `->seen`."
  [v [x y & more]]
  (let [{[cx cy] :center k :scale} (view v)
        k (double k)]
    (into [(+ (double cx) (/ (double x) k)) (+ (double cy) (/ (double y) k))] more)))

(defn zoom-at
  "View `v` magnified `m` times (m > 1 zooms in) about seen point `at`: the
   source point under `at` stays under it. The scale is clamped, so `m` may
   be only partly applied."
  [v m at]
  (let [v       (view v)
        [px py] (->source v at)
        k'      (clamp-scale (* (double (:scale v)) (double m)))
        [qx qy] at]
    {:center [(- px (/ (double qx) k')) (- py (/ (double qy) k'))] :scale k'}))

(defn pan
  "View `v` moved so the picture follows a pointer travel of [dx dy] seen
   units (dragging right moves the picture right)."
  [v [dx dy]]
  (let [{[cx cy] :center k :scale} (view v)
        k (double k)]
    {:center [(- (double cx) (/ (double dx) k)) (- (double cy) (/ (double dy) k))] :scale k}))

;; ── Frames through a view (one method per kind, OCP) ───────────────────

(defn- seen-d
  "SVG path string `d` with every point taken through view `v`."
  [v d]
  (let [pts (fn [xs] (str/join " " (mapcat #(->seen v %) (partition 2 xs))))]
    (str/join " "
              (for [[op & xs] (svg-path/segments d)]
                (case op
                  :move  (str "M " (pts xs))
                  :line  (str "L " (pts xs))
                  :quad  (str "Q " (pts xs))
                  :cubic (str "C " (pts xs))
                  "Z")))))

(defmulti shape
  "Shape `s` as it shows under view `v` (positions and geometric extents in
   seen units). A new node kind that carries extents gets a method."
  (fn [_v s] (:node s)))

(defn- positions
  "Shape `s` with its position fields taken through view `v`."
  [v s]
  (cond-> s
    (:at s)     (update :at #(->seen v %))
    (:from s)   (update :from #(->seen v %))
    (:to s)     (update :to #(->seen v %))
    (:points s) (update :points #(mapv (fn [p] (->seen v p)) %))))

(defmethod shape :default [v s] (positions v s))

(defn- with-extents
  "Shape `s` (positions already seen) with the :opts lengths in `defaults`
   ({key default}) filled in where absent and multiplied by the view's scale."
  [v s defaults]
  (let [k (double (:scale (view v)))]
    (reduce (fn [s [key dflt]]
              (assoc-in s [:opts key] (* k (double (get-in s [:opts key] dflt)))))
            s defaults)))

(defmethod shape :circle [v s]
  (with-extents v (positions v s) {:radius 1.0}))

(defmethod shape :rectangle [v s]
  (with-extents v (positions v s) {:width 4.0 :height 2.0}))

(defmethod shape :rounded-rectangle [v s]
  (with-extents v (positions v s) {:width 4.0 :height 2.0 :corner-radius 0.25}))

(defmethod shape :svg-path [v s]
  (cond-> (positions v s)
    (string? (:d s)) (update :d #(seen-d v %))))

(defn seen-frame
  "2D Frame `f` as it shows under view `v`; `f` itself when `v` is nil or
   the identity."
  [v f]
  (if (or (nil? v) (identity? v))
    f
    (update f :shapes #(mapv (fn [s] (shape v s)) %))))

(defn source-event
  "Pointer `event` with its :at taken back from seen to source scene units."
  [v event]
  (if (and v (:at event)) (update event :at #(->source v %)) event))
