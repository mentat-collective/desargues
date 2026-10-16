(ns desargues.player.cut
  "PURE stratum of the player: cutting apart faces that pass through each
   other, so the flattened view of a 3D frame (desargues.player.space/->2d)
   can order them by depth.

   Sorting whole faces by mean depth cannot order two faces that
   interpenetrate: each is in front of the other on one side of their common
   line. Such a pair is cut along that line first, each face by the other's
   plane, so every piece lies on one side of every face it crossed and sorts
   on its own depth. Faces that touch only along a shared edge or vertex
   (neighbours in a mesh) are not cut, so a frame where nothing
   interpenetrates flattens exactly as before.

   Polygons are vectors of world points [x y z]. Host-free .cljc."
  (:require [desargues.player.vec3 :refer [v+ v- v* dot3 cross norm unit]]))

(def ^:private plane-eps
  "Distances (scene units) within this of a plane count as on it."
  1e-9)

(defn plane-of
  "The plane {:point :normal} of polygon `ps`: its centroid and its Newell
   normal, which stays well defined for a slightly non-planar quad."
  [ps]
  (let [nrm (reduce v+ [0.0 0.0 0.0]
                    (map (fn [[x1 y1 z1] [x2 y2 z2]]
                           [(* (- y1 y2) (+ z1 z2)) (* (- z1 z2) (+ x1 x2)) (* (- x1 x2) (+ y1 y2))])
                         ps (concat (rest ps) [(first ps)])))]
    {:point (v* (/ 1.0 (count ps)) (reduce v+ ps)) :normal (unit nrm)}))

(defn- signed [{:keys [point normal]} p] (double (dot3 normal (v- p point))))

(defn- edge-cut
  "Where edge `p`-`q` crosses the plane, given their signed distances `dp`, `dq`."
  [p q dp dq]
  (v+ p (v* (/ dp (- dp dq)) (v- q p))))

(defn- crosses? [dp dq]
  (or (and (< dp (- plane-eps)) (> dq plane-eps))
      (and (> dp plane-eps) (< dq (- plane-eps)))))

(defn straddles?
  "Does polygon `ps` have vertices strictly on both sides of `plane`?"
  [ps plane]
  (let [ds (map #(signed plane %) ps)]
    (boolean (and (some #(< % (- plane-eps)) ds) (some #(> % plane-eps) ds)))))

(defn split-polygon
  "Polygon `ps` cut by `plane` into [behind in-front], each a vector of points
   (empty when nothing lies on that side). A vertex on the plane goes to both."
  [ps plane]
  (let [ps (vec ps)
        n  (count ps)
        ds (mapv #(signed plane %) ps)]
    (reduce (fn [[neg pos] i]
              (let [j (mod (inc i) n)
                    p (ps i) dp (ds i) dq (ds j)
                    neg (cond-> neg (<= dp plane-eps) (conj p))
                    pos (cond-> pos (>= dp (- plane-eps)) (conj p))]
                (if (crosses? dp dq)
                  (let [x (edge-cut p (ps j) dp dq)] [(conj neg x) (conj pos x)])
                  [neg pos])))
            [[] []]
            (range n))))

(defn- crossing-extent
  "The [lo hi] extent along `dir` of where polygon `ps` meets `plane`."
  [ps plane dir]
  (let [ps (vec ps)
        n  (count ps)
        ds (mapv #(signed plane %) ps)
        xs (mapcat (fn [i]
                     (let [j (mod (inc i) n) dp (ds i) dq (ds j)]
                       (cond-> []
                         (<= (Math/abs dp) plane-eps) (conj (ps i))
                         (crosses? dp dq)             (conj (edge-cut (ps i) (ps j) dp dq)))))
                   (range n))
        ts (map #(double (dot3 dir %)) xs)]
    (when (seq ts) [(reduce min ts) (reduce max ts)])))

(defn faces-intersect?
  "Do polygons `a` and `b` pass through each other? Each must straddle the
   other's plane and the two crossings must overlap along the planes' common
   line. Polygons sharing a vertex are neighbours, not a cut."
  [a b]
  (boolean
   (and (not (some (set a) b))
        (let [pa  (plane-of a)
              pb  (plane-of b)
              dir (cross (:normal pa) (:normal pb))]
          (and (> (norm dir) 1e-12)
               (straddles? a pb)
               (straddles? b pa)
               (let [[a0 a1] (crossing-extent a pb dir)
                     [b0 b1] (crossing-extent b pa dir)]
                 (and a0 b0 (< (max a0 b0) (- (min a1 b1) plane-eps)))))))))

(defn- bounds [ps]
  (let [cols (apply map vector ps)]
    [(mapv #(reduce min %) cols) (mapv #(reduce max %) cols)]))

(defn- boxes-overlap? [[lo1 hi1] [lo2 hi2]]
  (every? true? (map #(and (<= %1 %4) (<= %3 %2)) lo1 hi1 lo2 hi2)))

(defn cutting-planes
  "{index [plane ...]}: for every polygon of `faces` that passes through
   others, the planes of those others. Faces are swept in x so only faces
   whose bounding boxes overlap are compared."
  [faces]
  (let [faces (vec faces)
        bs    (mapv bounds faces)
        order (vec (sort-by #(get-in bs [% 0 0]) (range (count faces))))]
    (reduce
     (fn [acc k]
       (let [i (order k)]
         (reduce (fn [acc j]
                   (if (and (boxes-overlap? (bs i) (bs j))
                            (faces-intersect? (faces i) (faces j)))
                     (-> acc
                         (update i (fnil conj []) (plane-of (faces j)))
                         (update j (fnil conj []) (plane-of (faces i))))
                     acc))
                 acc
                 (take-while #(<= (get-in bs [% 0 0]) (get-in bs [i 1 0]))
                             (map order (range (inc k) (count order)))))))
     {}
     (range (count order)))))

(defn pieces
  "Polygon `ps` cut by every plane of `planes` it straddles."
  [ps planes]
  (reduce (fn [frags plane]
            (mapcat #(if (straddles? % plane)
                       (filter (fn [q] (>= (count q) 3)) (split-polygon % plane))
                       [%])
                    frags))
          [ps]
          planes))

(defn- mean [xs] (/ (reduce + 0.0 xs) (max 1 (count xs))))

(defn- redraw
  "Face primitive `prim` redrawn as its piece `ps`, projected by `proj`, or
   nil when a corner of the piece is not shown."
  [proj prim ps]
  (let [qs (mapv proj ps)]
    (when (every? some? qs)
      (assoc prim :points (mapv :at qs) :depth (mean (map :depth qs)) :world ps))))

(defn split-intersecting
  "Primitives `prims` with every face that passes through another face
   replaced by its pieces, cut along the other's plane, so a depth sort
   orders them. A face primitive is a closed :path carrying its :world
   polygon; anything without :world passes through. `proj` maps a world
   point to {:at [x y] :depth d}, or nil when it is behind the eye.
   Returns `prims` (as a vector) when nothing passes through anything."
  [proj prims]
  (let [prims (vec prims)
        idx   (filterv #(:world (prims %)) (range (count prims)))
        cuts  (cutting-planes (mapv #(:world (prims %)) idx))]
    (if (empty? cuts)
      prims
      (let [cut-at (into {} (map (fn [[k planes]] [(idx k) planes]) cuts))]
        (into []
              (mapcat (fn [i]
                        (let [p (prims i)]
                          (if-let [planes (cut-at i)]
                            (keep #(redraw proj p %) (pieces (:world p) planes))
                            [p]))))
              (range (count prims)))))))
