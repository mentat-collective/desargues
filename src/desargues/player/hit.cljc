(ns desargues.player.hit
  "PURE stratum of the player: hit-testing a Frame.

   Given a Frame (desargues.player.frame) and a point in scene units, which
   shape is under it? Every host asks the same question for its pointer
   (desargues.player.protocols/IPointer), so the answer lives here once,
   host-free.

   Rules:
     - only visible shapes are hit (frame/visible-shapes);
     - a shape is a candidate when its `distance` to the point is within the
       tolerance (scene units; hit-px converts a pixel tolerance);
     - among candidates the highest `priority` wins, then the topmost (last
       in draw order). Dots outrank strokes so a point sitting on a line or a
       circle through it stays grabbable, as in every compass construction.

   Both `distance` and `priority` are multimethods on the node kind (OCP): a
   new kind becomes hittable by a defmethod. Kinds without a method are never
   hit. Host-free .cljc."
  (:require [desargues.player.frame :as frame]))

;; ── Geometry (pure helpers) ──────────────────────────────────────────────

(defn- sq [x] (* x x))

(defn dist
  "Euclidean distance between 2D points `p` and `q`."
  [[px py] [qx qy]]
  (Math/sqrt (+ (sq (- (double qx) (double px))) (sq (- (double qy) (double py))))))

(defn segment-dist
  "Distance from point `p` to segment `a`-`b`."
  [p a b]
  (let [[ax ay] (map double a) [bx by] (map double b) [px py] (map double p)
        dx (- bx ax) dy (- by ay)
        l2 (+ (sq dx) (sq dy))]
    (if (zero? l2)
      (dist p a)
      (let [u (-> (/ (+ (* (- px ax) dx) (* (- py ay) dy)) l2) (max 0.0) (min 1.0))]
        (dist p [(+ ax (* u dx)) (+ ay (* u dy))])))))

(defn- polyline-dist [p pts]
  (when (seq pts)
    (if (= 1 (count pts))
      (dist p (first pts))
      (reduce min (map #(segment-dist p %1 %2) pts (rest pts))))))

(defn inside-polygon?
  "Even-odd test: is `p` inside the closed polygon `pts`?"
  [[px py] pts]
  (let [pts (mapv #(mapv double %) pts)
        n   (count pts)
        px  (double px) py (double py)]
    (and (>= n 3)
         (odd? (count (filter (fn [i]
                                (let [[xi yi] (pts i) [xj yj] (pts (mod (dec i) n))]
                                  (and (not= (> yi py) (> yj py))
                                       (< px (+ xi (/ (* (- xj xi) (- py yi)) (- yj yi)))))))
                              (range n)))))))

(defn- xy [p] (vec (take 2 (or p [0 0]))))

(defn- filled? [s] (some? (:fill s)))

;; ── Per-kind rules (OCP) ─────────────────────────────────────────────────

(defmulti distance
  "Distance in scene units from point `p` to shape `s`, 0 when inside a
   filled area; nil when the kind is not hittable."
  (fn [s _p] (:node s)))

(defmethod distance :default [_ _] nil)

(defmethod distance :dot [s p]
  (max 0.0 (- (dist p (xy (:at s))) (double (get-in s [:opts :radius] 0.08)))))

(defmethod distance :circle [s p]
  (let [r (double (get-in s [:opts :radius] 1.0))
        d (dist p (xy (:at s)))]
    (if (and (filled? s) (<= d r)) 0.0 (Math/abs (- d r)))))

(defmethod distance :line [s p]
  (segment-dist p (xy (:from s)) (xy (:to s))))

(defmethod distance :path [s p]
  (let [pts     (mapv xy (:points s))
        closed? (get-in s [:opts :closed?])]
    (if (and closed? (filled? s) (inside-polygon? p pts))
      0.0
      (polyline-dist p (cond-> pts (and closed? (seq pts)) (conj (first pts)))))))

(defn- box-distance [s p]
  (let [[cx cy] (xy (:at s))
        hw (/ (double (get-in s [:opts :width] 4.0)) 2.0)
        hh (/ (double (get-in s [:opts :height] 2.0)) 2.0)
        [px py] (map double p)
        ox (- (Math/abs (- px cx)) hw)
        oy (- (Math/abs (- py cy)) hh)]
    (if (and (<= ox 0.0) (<= oy 0.0))
      (if (filled? s) 0.0 (min (- ox) (- oy)))
      (Math/sqrt (+ (sq (max ox 0.0)) (sq (max oy 0.0)))))))

(defmethod distance :rectangle [s p] (box-distance s p))
(defmethod distance :rounded-rectangle [s p] (box-distance s p))

(defn- label-distance
  "Text-like shapes: a small disc around the anchor (no font metrics here)."
  [s p]
  (max 0.0 (- (dist p (xy (:at s))) 0.25)))

(defmethod distance :text [s p] (label-distance s p))
(defmethod distance :tex [s p] (label-distance s p))
(defmethod distance :decimal [s p] (label-distance s p))

(defmulti priority
  "Rank of shape `s` among candidates under the pointer; higher wins."
  :node)

(defmethod priority :default [_] 0)
(defmethod priority :dot [_] 1)

;; ── Queries ──────────────────────────────────────────────────────────────

(defn candidates
  "Visible shapes of frame `f` within `tol` scene units of point `p`, in
   draw order, each as {:id :distance :priority :order}."
  [f p tol]
  (into []
        (keep-indexed
         (fn [i s]
           (when-let [d (distance s p)]
             (when (<= d (double tol))
               {:id (:id s) :distance d :priority (priority s) :order i}))))
        (frame/visible-shapes f)))

(defn hit
  "Id of the shape of frame `f` under scene point `p` within `tol` scene
   units, or nil. Highest priority wins, then topmost."
  [f p tol]
  (let [rank (juxt :priority :order)]
    (some->> (seq (candidates f p tol))
             (reduce (fn [a b] (if (pos? (compare (rank b) (rank a))) b a)))
             :id)))

(defn hit-px
  "Like `hit`, from a pixel point and a pixel tolerance under viewport `vp`."
  [f vp px tol-px]
  (hit f (frame/px->scene vp px) (/ (double tol-px) (double (:scale vp)))))
