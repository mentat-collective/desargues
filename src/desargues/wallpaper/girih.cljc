(ns desargues.wallpaper.girih
  "Pure layer of wallpaper loops: Islamic star patterns by Hankin's
   polygons-in-contact method.

   E. H. Hankin (1925) read the star patterns of Islamic ceilings and screens
   as a periodic tiling of polygons with rays drawn from each edge midpoint.
   Each midpoint sends two rays into its polygon, one leaning towards each
   end of its edge, at the contact angle θ to the edge. Near each corner, the
   ray from one edge meets the ray from the next edge. The segments
   midpoint -> meeting point -> next midpoint are the strands of the
   pattern. Inside each polygon they close into a star (the rosette).
   Sweep θ and one tiling gives the whole family: small θ traces the tiling
   itself, θ near 3π/8 on the 4.8.8 tiling gives the eight-pointed khatam
   stars, wider θ sharpens every star. With `delta`, each ray starts `delta`
   along the edge on either side of the midpoint (Hankin's two-point
   variant), so two strands cross at every edge.

   A tiling is periodic: {:lattice [a b] :cell [{:kind k :vertices [[x y] ..]} ..]},
   vertices counter-clockwise, and `cover` repeats the cell over a
   rectangle. Everything is plain vectors of doubles, so the same data feeds
   the headless host, a browser canvas or an SVG writer."
  (:require [desargues.wallpaper.loop :as lp]))

;; ── Plane vectors ─────────────────────────────────────────────────────────

(defn v+ [[ax ay] [bx by]] [(+ ax bx) (+ ay by)])
(defn v- [[ax ay] [bx by]] [(- ax bx) (- ay by)])
(defn v* [k [x y]] [(* k x) (* k y)])
(defn vlen [[x y]] (Math/sqrt (+ (* x x) (* y y))))
(defn unit [v] (let [l (vlen v)] (if (zero? l) v (v* (/ 1.0 l) v))))
(defn rot
  "Vector `v` turned counter-clockwise by `a` radians."
  [a [x y]]
  (let [c (Math/cos a) s (Math/sin a)]
    [(- (* c x) (* s y)) (+ (* s x) (* c y))]))
(defn cross [[ax ay] [bx by]] (- (* ax by) (* ay bx)))
(defn mid [a b] (v* 0.5 (v+ a b)))

(defn centroid [pts] (v* (/ 1.0 (count pts)) (reduce v+ pts)))

(defn regular-polygon
  "The `n` vertices, counter-clockwise, of the regular polygon about `c`
   with circumradius `r`, the first vertex at angle `a0`."
  [n c r a0]
  (vec (for [k (range n)
             :let [a (+ a0 (/ (* lp/tau k) n))]]
         (v+ c [(* r (Math/cos a)) (* r (Math/sin a))]))))

;; ── Periodic tilings ──────────────────────────────────────────────────────

(def ^:private sqrt2 (Math/sqrt 2.0))
(def ^:private sqrt3 (Math/sqrt 3.0))

(def tilings
  "Periodic tilings, unit edge: lattice vectors and one cell of polygons.

     :square   4.4.4.4    squares
     :hex      6.6.6      hexagons
     :4.8.8    4.8.8      octagons and squares (khatam and eight-fold rosettes)
     :3.6.3.6  3.6.3.6    hexagons and triangles (six-fold stars)"
  (let [r8 (/ 1.0 (* 2.0 (Math/sin (/ Math/PI 8.0))))
        L  (+ 1.0 sqrt2)
        h  (/ L 2.0)
        q  (/ 1.0 sqrt2)]
    {:square  {:lattice [[1.0 0.0] [0.0 1.0]]
               :cell [{:kind :square :vertices (regular-polygon 4 [0.0 0.0] q (/ Math/PI 4.0))}]}
     :hex     {:lattice [[sqrt3 0.0] [(/ sqrt3 2.0) 1.5]]
               :cell [{:kind :hexagon :vertices (regular-polygon 6 [0.0 0.0] 1.0 (/ Math/PI 6.0))}]}
     :4.8.8   {:lattice [[L 0.0] [0.0 L]]
               :cell [{:kind :octagon :vertices (regular-polygon 8 [0.0 0.0] r8 (/ Math/PI 8.0))}
                      {:kind :square :vertices [[h (- h q)] [(+ h q) h] [h (+ h q)] [(- h q) h]]}]}
     :3.6.3.6 {:lattice [[2.0 0.0] [1.0 sqrt3]]
               :cell [{:kind :hexagon :vertices (regular-polygon 6 [0.0 0.0] 1.0 0.0)}
                      {:kind :triangle :vertices [[1.0 0.0] [1.5 (/ sqrt3 2.0)] [0.5 (/ sqrt3 2.0)]]}
                      {:kind :triangle :vertices [[1.5 (/ sqrt3 2.0)] [2.5 (/ sqrt3 2.0)] [2.0 sqrt3]]}]}}))

(defn tiling
  "The tiling named `k` (a key of `tilings`), or `k` itself when it is
   already a tiling map."
  [k]
  (if (map? k)
    k
    (or (get tilings k)
        (throw (ex-info (str "Unknown tiling " (pr-str k)) {:tiling k :known (keys tilings)})))))

(defn- lattice-range
  "Integer [i0 i1 j0 j1] such that i·a + j·b over the ranges covers the
   rectangle [x0 x1] x [y0 y1] with `margin` cells to spare."
  [[[ax ay] [bx by]] [x0 x1 y0 y1] margin]
  (let [det (- (* ax by) (* ay bx))
        ij  (fn [[x y]] [(/ (- (* x by) (* y bx)) det) (/ (- (* ax y) (* ay x)) det)])
        cs  (map ij [[x0 y0] [x1 y0] [x0 y1] [x1 y1]])
        is  (map first cs) js (map second cs)]
    [(- (long (Math/floor (apply min is))) margin) (+ (long (Math/ceil (apply max is))) margin)
     (- (long (Math/floor (apply min js))) margin) (+ (long (Math/ceil (apply max js))) margin)]))

(defn cover
  "The polygons of tiling `t` (a key or map) covering the rectangle
   `rect` = [x0 x1 y0 y1] in tiling units, each tagged with its :cell [i j]
   and :centre."
  ([t rect] (cover t rect 1))
  ([t rect margin]
   (let [{:keys [lattice cell]} (tiling t)
         [a b] lattice
         [i0 i1 j0 j1] (lattice-range lattice rect margin)]
     (vec (for [i (range i0 (inc i1))
                j (range j0 (inc j1))
                :let [off (v+ (v* (double i) a) (v* (double j) b))]
                {:keys [vertices] :as poly} cell
                :let [vs (mapv #(v+ off %) vertices)]]
            (assoc poly :vertices vs :cell [i j] :centre (centroid vs)))))))

;; ── Hankin's rays ─────────────────────────────────────────────────────────

(defn- ray-meet
  "The point where ray p + t·r meets ray q + s·w, t, s > 0; when they do not
   meet ahead (parallel, or crossing behind), the point halfway between the
   two starts, so the strand degrades to a short dent instead of a spike."
  [p r q w]
  (let [d (cross r w)]
    (if (< (Math/abs (double d)) 1e-12)
      (mid p q)
      (let [qp (v- q p)
            t  (/ (cross qp w) d)
            s  (/ (cross qp r) d)]
        (if (and (pos? t) (pos? s))
          (v+ p (v* t r))
          (mid p q))))))

(defn hankin
  "Hankin's construction in one polygon (vertices counter-clockwise) at
   contact angle `theta` (radians, from the edge) and offset `delta` (edge
   lengths from the midpoint). Returns

     {:strands [[[x y] [x y] [x y]] ..]   one polyline per corner:
                                          start on edge i, meeting point, start on edge i+1
      :rosette [[x y] ..]}                the closed star the strands bound

   Corner k's strand belongs to vertex k+1."
  ([vertices theta] (hankin vertices theta 0.0))
  ([vertices theta delta]
   (let [n  (count vertices)
         vs (vec vertices)
         edge (fn [i] (let [a (vs (mod i n)) b (vs (mod (inc i) n))]
                        {:m (mid a b) :d (unit (v- b a)) :len (vlen (v- b a))}))
         strands (vec (for [i (range n)
                            :let [{m0 :m d0 :d l0 :len} (edge i)
                                  {m1 :m d1 :d l1 :len} (edge (inc i))
                                  p (v+ m0 (v* (* delta l0) d0))
                                  q (v- m1 (v* (* delta l1) d1))
                                  x (ray-meet p (rot theta d0) q (rot (- theta) (v* -1.0 d1)))]]
                        [p x q]))]
     {:strands strands
      :rosette (vec (mapcat (fn [[p x _]] [p x]) strands))})))

(defn pattern
  "The star pattern of tiling `t` over rectangle `rect` (tiling units):
   every covering polygon with its Hankin strands and rosette.
   `theta-of` is a number, or (fn [poly] theta), so the contact angle may
   vary across the plane; likewise `delta-of`."
  [t rect theta-of delta-of]
  (let [f (fn [x poly] (if (fn? x) (x poly) x))]
    (mapv (fn [poly]
            (merge poly (hankin (:vertices poly) (f theta-of poly) (f delta-of poly))))
          (cover t rect))))

(defn beads
  "Points every `spacing` along polyline `pts` (closed when `closed?`),
   the first one `phase` spacings from the start, so advancing `phase` by 1
   lands every bead where its neighbour was."
  [pts spacing phase closed?]
  (let [pts (vec pts)
        segs (vec (partition 2 1 (cond-> pts closed? (conj (first pts)))))
        lens (mapv (fn [[a b]] (vlen (v- b a))) segs)
        total (reduce + lens)
        spacing (double spacing)
        n (long (Math/floor (/ total spacing)))
        spacing (if (pos? n) (/ total n) spacing)
        start (* spacing (lp/frac (double phase)))]
    (loop [k 0 out [] si 0 acc 0.0]
      (let [s (+ start (* k spacing))]
        (cond
          (or (>= k n) (>= si (count segs))) out
          (> s (+ acc (lens si))) (recur k out (inc si) (+ acc (lens si)))
          :else (let [[a b] (segs si)
                      t (if (zero? (lens si)) 0.0 (/ (- s acc) (lens si)))]
                  (recur (inc k) (conj out (v+ a (v* t (v- b a)))) si acc)))))))

(defn scale-about
  "Polygon `pts` scaled by `k` and turned by `turn` radians about point `c`."
  [pts c k turn]
  (mapv #(v+ c (v* k (rot turn (v- % c)))) pts))

(defn khatam-angle
  "The contact angle that makes the classic eight-pointed khatam star on
   the 4.8.8 tiling: 3π/8."
  []
  (* 3.0 (/ Math/PI 8.0)))
