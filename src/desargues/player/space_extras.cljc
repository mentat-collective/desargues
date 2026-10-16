(ns desargues.player.space-extras
  "CORE + PURE stratum of the player: more 3D kinds beside those of
   desargues.player.space, registered on its open multimethods (OCP), so
   validation (`space/problems`) and flattening (`space/->2d`) know them with
   no edit to space itself.

     :arrow3    {:from [x y z] :to [x y z] :stroke {:color :width}
                 :opts {:head 0.25 :head-radius 0.09}}
                a shaft and a cone head (gradients, normals). :draw grows it
                from :from; a zero-length arrow draws nothing.
     :plane3    {:point [x y z] :normal [a b c] :size 1.0}  or
                {:point p :u [..] :v [..] :size [a b]}
                a finite patch: point + s u + t v for |s| <= a, |t| <= b
                (tangent planes, slicing planes). With :normal, u and v are an
                orthonormal pair perpendicular to it. :fill {:color :opacity},
                :opts {:wire color :cells n}.
     :contour3  {:lines [[[x y z] ...] ...] :stroke {:color :width}}
                polylines, typically level curves from desargues.player.contour
                lifted to a height (`level-curves`).
     :label3    {:at [x y z] :text \"P\" :opts {:font-size 28 :color :white
                 :anchor \"start\"}}  text anchored at a point in space.

   The first three are composites: `expand` turns them into the base kinds
   of desargues.player.space (:curve3, :surface3), and their flattening is
   the flattening of that expansion, so a host that renders space natively
   draws them by expanding too (studio/desargues/player/javafx3d.clj).
   :label3 flattens to a :text label. Host-free .cljc."
  (:require [desargues.player.contour :as contour]
            [desargues.player.space :as space]))

;; ── Composites ────────────────────────────────────────────────────────────

(defmulti expand
  "The base 3D shapes (desargues.player.space kinds) composite shape `s`
   stands for, each inheriting its :id, :opacity and :emphasis; empty for a
   kind that is not a composite. One method per kind (OCP)."
  :node)

(defmethod expand :default [_] [])

(defn- inherit [s base]
  (merge base (select-keys s [:id :opacity :emphasis])))

(defn- draw-fraction [s] (max 0.0 (min 1.0 (double (:draw s 1.0)))))

(defn perpendicular
  "A unit vector perpendicular to non-zero vector `n`."
  [n]
  (let [n (space/unit n)
        a (if (< (Math/abs (double (nth n 2))) 0.9) [0.0 0.0 1.0] [1.0 0.0 0.0])]
    (space/unit (space/cross n a))))

(defn- cone
  "A :surface3 mesh of a cone with apex `tip`, base centre `base`, radius `r`."
  [base tip r sides]
  (let [ax   (space/unit (space/v- tip base))
        u    (perpendicular ax)
        v    (space/cross ax u)
        ring (for [k (range sides)
                   :let [a (/ (* 2.0 Math/PI k) sides)]]
               (space/v+ base (space/v+ (space/v* (* r (Math/cos a)) u)
                                        (space/v* (* r (Math/sin a)) v))))]
    {:node :surface3 :vertices (vec (cons tip ring))
     :faces (vec (for [k (range sides)] [0 (inc k) (inc (mod (inc k) sides))]))}))

(defmethod expand :arrow3 [{:keys [from to] :as s}]
  (let [from   (mapv double from)
        tip    (space/v+ from (space/v* (draw-fraction s) (space/v- (mapv double to) from)))
        len    (space/norm (space/v- tip from))
        head   (min (double (get-in s [:opts :head] 0.25)) (* 0.6 len))
        r      (get-in s [:opts :head-radius] (* 0.38 head))
        stroke (:stroke s {:color :white :width 4})]
    (if (< len 1e-9)
      []
      (let [base (space/v+ from (space/v* (- len head) (space/unit (space/v- tip from))))]
        [(inherit s {:node :curve3 :points [from base] :stroke stroke :draw 1.0})
         (inherit s (assoc (cone base tip r 10)
                           :fill {:color (:color stroke :white) :opacity 1.0} :draw 1.0))]))))

(defn plane-basis
  "The spanning pair [u v] of :plane3 shape `s`: its :u and :v, or an
   orthonormal pair perpendicular to its :normal."
  [{:keys [u v normal]}]
  (if (and u v)
    [(mapv double u) (mapv double v)]
    (let [n (space/unit (mapv double normal)) a (perpendicular n)]
      [a (space/cross n a)])))

(defn- half-sizes [s]
  (let [z (:size s 1.0)] (if (sequential? z) (mapv double z) [(double z) (double z)])))

(defmethod expand :plane3 [{:keys [point] :as s}]
  (let [[u v] (plane-basis s)
        [a b] (half-sizes s)
        p     (mapv double point)
        n     (get-in s [:opts :cells] 4)
        wire  (get-in s [:opts :wire])]
    [(inherit s (cond-> {:node :surface3
                         :grid (space/parametric-grid
                                (fn [i j] (space/v+ p (space/v+ (space/v* i u) (space/v* j v))))
                                [(- a) a] [(- b) b] n n)
                         :fill (merge {:color :teal :opacity 0.5} (:fill s))
                         :draw (draw-fraction s)}
                  wire (assoc-in [:opts :wire] wire)))]))

(defmethod expand :contour3 [s]
  (vec (for [line (:lines s) :when (> (count line) 1)]
         (inherit s {:node :curve3 :points (mapv #(mapv double %) line)
                     :stroke (:stroke s {:color :white :width 2}) :draw (draw-fraction s)}))))

(defn- expanded [cam b s] (mapcat #(space/primitives cam b %) (expand s)))

(defmethod space/primitives :arrow3 [cam b s] (expanded cam b s))
(defmethod space/primitives :plane3 [cam b s] (expanded cam b s))
(defmethod space/primitives :contour3 [cam b s] (expanded cam b s))

;; ── Labels ────────────────────────────────────────────────────────────────

(def ^:private label-bias
  "Labels sort this much nearer (scene units) than what they annotate."
  0.6)

(defmethod space/primitives :label3 [cam b s]
  (let [{:keys [at depth]} (space/project cam b (:at s))]
    (when (> depth space/near)
      [(merge (select-keys s [:id :opacity :emphasis])
              {:node :text :text (str (:text s)) :at at :draw 1.0 :depth (- depth label-bias)
               :opts (merge {:font-size 28 :color :white} (:opts s))})])))

;; ── Validation ────────────────────────────────────────────────────────────

(defn- nonzero? [v] (and (space/point3? v) (> (space/norm v) 1e-12)))

(defmethod space/shape-problems :arrow3 [s]
  (cond-> []
    (not (space/point3? (:from s))) (conj :arrow3/from)
    (not (space/point3? (:to s)))   (conj :arrow3/to)))

(defn- span-ok? [{:keys [normal u v]}]
  (if normal
    (nonzero? normal)
    (and (nonzero? u) (nonzero? v) (> (space/norm (space/cross u v)) 1e-12))))

(defmethod space/shape-problems :plane3 [s]
  (let [size  (:size s 1.0)
        sizes (if (sequential? size) size [size])]
    (cond-> []
      (not (space/point3? (:point s)))                  (conj :plane3/point)
      (not (span-ok? s))                                (conj :plane3/span)
      (not (every? #(and (number? %) (pos? %)) sizes))  (conj :plane3/size))))

(defmethod space/shape-problems :contour3 [s]
  (let [ls (:lines s)]
    (if (and (sequential? ls)
             (every? #(and (sequential? %) (>= (count %) 2) (every? space/point3? %)) ls))
      []
      [:contour3/lines])))

(defmethod space/shape-problems :label3 [s]
  (cond-> []
    (not (space/point3? (:at s))) (conj :label3/at)
    (not (string? (:text s)))     (conj :label3/text)))

;; ── Builders (pure data) ──────────────────────────────────────────────────

(defn arrow
  "An :arrow3 from `from` to `to`. opts: :id :color :width :head."
  [from to {:keys [id color width head] :or {color :white width 4}}]
  (cond-> {:id id :node :arrow3 :from from :to to :stroke {:color color :width width}
           :opacity 1.0 :draw 1.0}
    head (assoc-in [:opts :head] head)))

(defn plane
  "A :plane3 through `point`, given {:normal n} or {:u u :v v}. opts: :id
   :size :color :opacity :wire."
  [point {:keys [id normal u v size color opacity wire] :or {size 1.0 color :teal opacity 0.5}}]
  (cond-> {:id id :node :plane3 :point point :size size
           :fill {:color color :opacity opacity} :opacity 1.0 :draw 1.0}
    normal                  (assoc :normal normal)
    (and u v (nil? normal)) (assoc :u u :v v)
    wire                    (assoc-in [:opts :wire] wire)))

(defn label
  "A :label3 `text` at world point `at`. opts: :id :font-size :color :anchor."
  [at text {:keys [id] :as opts}]
  {:id id :node :label3 :at at :text text :opacity 1.0 :draw 1.0
   :opts (merge {:font-size 28 :color :white} (dissoc opts :id))})

(defn level-curves
  "Polylines [[x y z] ...] of the level curves of `f` (fn [x y] z) over
   `x-range` x `y-range`: `n` levels (or the given :values), sampled on a
   `cells` grid. Each curve sits at height (z-of level): a constant for the
   floor, identity (the default) for the surface itself."
  [f x-range y-range {:keys [n values cells z-of] :or {n 8 cells 40 z-of identity}}]
  (let [g (contour/sample f x-range y-range cells)]
    (vec (for [c    (or values (contour/levels g n))
               line (contour/lines g c)
               :let [z (double (z-of c))]]
           (mapv (fn [[x y]] [x y z]) line)))))

(defn contours
  "A :contour3 of `lines`. opts: :id :color :width."
  [lines {:keys [id color width] :or {color :white width 2}}]
  {:id id :node :contour3 :lines lines :stroke {:color color :width width}
   :opacity 1.0 :draw 1.0})
