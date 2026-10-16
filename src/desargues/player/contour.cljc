(ns desargues.player.contour
  "PURE stratum of the player: level curves of z = f(x, y) by marching
   squares.

   `sample` evaluates f on a rectangular grid, `segments` cuts every grid cell
   by the level c (one segment per cell, two for a saddle cell, resolved by
   the cell's centre value), and `chain` joins the segments into polylines.
   A crossing on a grid edge is computed from the edge's endpoints in one
   fixed order, so the two cells sharing an edge produce the identical point
   and chaining is exact: a closed level curve comes back as a polyline whose
   last point equals its first.

     (def g (sample (fn [x y] (+ (* x x) (* y y))) [-2 2] [-2 2] 40))
     (lines g 1.0)          ; => [[[1.0 0.0] ... [1.0 0.0]]], the unit circle
     (levels g 6)           ; six values strictly inside the grid's z range

   desargues.player.space-extras draws the result as a :contour3 shape.
   Host-free .cljc.")

(defn- steps [a b n]
  (let [a (double a) b (double b) n (max 1 (long n))]
    (mapv #(+ a (* (- b a) (/ (double %) n))) (range (inc n)))))

(defn sample
  "The grid of `f` (fn [x y] z) over [x0 x1] x [y0 y1] with `n` cells a side
   (or [nx ny]): {:xs [...] :ys [...] :z [[z ...] ...]} with (get-in z [i j])
   = f(xs_i, ys_j)."
  [f [x0 x1] [y0 y1] n]
  (let [[nx ny] (if (sequential? n) n [n n])
        xs (steps x0 x1 nx)
        ys (steps y0 y1 ny)]
    {:xs xs :ys ys
     :z  (mapv (fn [x] (mapv (fn [y] (double (f x y))) ys)) xs)}))

(defn z-range
  "[zmin zmax] over grid `g`."
  [g]
  (let [zs (apply concat (:z g))]
    [(reduce min zs) (reduce max zs)]))

(defn levels
  "`n` evenly spaced levels strictly inside the z range of grid `g` (or
   inside [lo hi])."
  ([g n] (let [[lo hi] (z-range g)] (levels lo hi n)))
  ([lo hi n]
   (let [lo (double lo) hi (double hi) n (max 1 (long n))]
     (mapv #(+ lo (* (- hi lo) (/ (+ 1.0 %) (inc n)))) (range n)))))

(defn- node [{:keys [xs ys z]} [i j]] [(xs i) (ys j) (get-in z [i j])])

(defn- crossing
  "Where level `c` crosses the grid edge between nodes `a` and `b`, as [x y],
   or nil when it does not. Computed from the endpoints in index order, so
   both cells sharing the edge agree bit for bit."
  [g c a b]
  (let [[a b] (if (neg? (compare a b)) [a b] [b a])
        [xa ya za] (node g a)
        [xb yb zb] (node g b)]
    (when (not= (> za c) (> zb c))
      (let [t (/ (- c za) (- zb za))]
        [(+ xa (* t (- xb xa))) (+ ya (* t (- yb ya)))]))))

(defn- cell-segments
  "The segments of level `c` in the cell whose low corner is node [i j]."
  [g c i j]
  (let [n0 [i j] n1 [(inc i) j] n2 [(inc i) (inc j)] n3 [i (inc j)]
        e  [(crossing g c n0 n1) (crossing g c n1 n2) (crossing g c n3 n2) (crossing g c n0 n3)]
        hits (keep-indexed (fn [k p] (when p k)) e)]
    (case (count hits)
      2 [[(e (first hits)) (e (second hits))]]
      4 (let [zs     (map #(nth (node g %) 2) [n0 n1 n2 n3])
              center (/ (reduce + zs) 4.0)]
          ;; a saddle cell: when the centre sides with corner 0, corners 0 and
          ;; 2 join through it, so the cuts isolate corners 1 and 3
          (if (= (> center c) (> (first zs) c))
            [[(e 0) (e 1)] [(e 2) (e 3)]]
            [[(e 3) (e 0)] [(e 1) (e 2)]]))
      [])))

(defn segments
  "Every segment [[x y] [x y]] of level `c` over grid `g`, zero-length ones
   dropped. Cells whose four corners all lie on one side of `c` are skipped
   before any crossing is computed."
  [g c]
  (let [c (double c)
        z (:z g)]
    (for [i (range (dec (count (:xs g))))
          :let [col (z i) nxt (z (inc i))]
          j (range (dec (count (:ys g))))
          :let [a (> (double (col j)) c)]
          :when (not (= a (> (double (col (inc j))) c)
                        (> (double (nxt j)) c) (> (double (nxt (inc j))) c)))
          [p q :as s] (cell-segments g c i j)
          :when (not= p q)]
      s)))

(defn chain
  "Segments `segs` joined end to end into polylines. A closed loop comes back
   with its first point repeated at the end."
  [segs]
  (let [segs (vec segs)
        adj  (reduce-kv (fn [m k [p q]]
                          (-> m (update p (fnil conj []) k) (update q (fnil conj []) k)))
                        {} segs)
        grow (fn [used line end]
               (loop [used used line line end end]
                 (if-let [k (first (remove used (adj end)))]
                   (let [[a b] (segs k) nxt (if (= a end) b a)]
                     (recur (conj used k) (conj line nxt) nxt))
                   [used line])))]
    (loop [used #{} k 0 out []]
      (cond
        (= k (count segs)) out
        (used k)           (recur used (inc k) out)
        :else
        (let [[p q]       (segs k)
              [used fwd]  (grow (conj used k) [p q] q)
              [used back] (if (= (peek fwd) p) [used []] (grow used [] p))]
          (recur used (inc k) (conj out (into (vec (reverse back)) fwd))))))))

(defn lines
  "The level curves of value `c` over grid `g` as polylines of [x y]."
  [g c]
  (chain (segments g c)))

(defn closed?
  "A polyline whose last point is its first."
  [line]
  (and (> (count line) 2) (= (first line) (peek line))))
