(ns desargues.objects.sample
  "PURE stratum: turn a function into polyline samples.

   Adaptive bisection: start from a uniform seed grid, then split any segment
   whose midpoint strays from the chord by more than `tol` (measured in SCENE
   units, so the tolerance is what the eye sees, independent of the math
   scale). Points where the function is not finite (poles, sqrt of negatives)
   break the curve into separate runs, so a plot of tan never draws the
   vertical jump.

   Every sampler returns a vector of RUNS, each a vector of points; a run of
   fewer than two points is dropped. Host-free .cljc.")

(def default-opts
  "seed: initial uniform segments; tol: max chord deviation in scene units;
   max-depth: bisection depth cap per seed segment."
  {:seed 24 :tol 0.01 :max-depth 10})

(defn finite-point?
  [p]
  (and (some? p) (every? #(and (number? %) (not (NaN? (double %))) (not (infinite? (double %)))) p)))

(defn- safe-call [f t]
  (try (f t) (catch #?(:clj Throwable :cljs :default) _ nil)))

(defn- dist2 [[x0 y0] [x1 y1]]
  (let [dx (- (double x1) (double x0)) dy (- (double y1) (double y0))]
    (+ (* dx dx) (* dy dy))))

(defn- chord-deviation
  "Distance of point m from the segment p-q (scene units)."
  [p q m]
  (let [[px py] p [qx qy] q [mx my] m
        dx (- qx px) dy (- qy py)
        l2 (+ (* dx dx) (* dy dy))]
    (if (zero? l2)
      (Math/sqrt (dist2 p m))
      (let [u (max 0.0 (min 1.0 (/ (+ (* (- mx px) dx) (* (- my py) dy)) l2)))]
        (Math/sqrt (dist2 m [(+ px (* u dx)) (+ py (* u dy))]))))))

(defn- refine
  "Points strictly after t0 up to and including t1 for curve `g` (t -> scene
   point or nil), bisecting while the chord misses the midpoint by > tol.
   A non-finite midpoint is kept as nil so the caller can split runs."
  [g t0 p0 t1 p1 tol depth]
  (let [tm (/ (+ t0 t1) 2.0)
        pm (g tm)]
    (cond
      (zero? depth) [pm p1]
      (not (and (finite-point? p0) (finite-point? p1) (finite-point? pm)))
      (if (and (not (finite-point? p0)) (not (finite-point? p1)) (not (finite-point? pm)))
        [nil p1]
        (into (conj (refine g t0 p0 tm pm tol (dec depth)) pm)
              (refine g tm pm t1 p1 tol (dec depth))))
      (<= (chord-deviation p0 p1 pm) tol) [p1]
      :else (-> (refine g t0 p0 tm pm tol (dec depth))
                (into (refine g tm pm t1 p1 tol (dec depth)))))))

(defn- split-runs
  "Split a point seq at nils (and at huge jumps) into runs of >= 2 points."
  [pts max-jump]
  (let [jump2 (when max-jump (* max-jump max-jump))]
    (->> (reduce (fn [runs p]
                   (cond
                     (not (finite-point? p)) (conj runs [])
                     (and jump2 (seq (peek runs)) (> (dist2 (peek (peek runs)) p) jump2))
                     (conj runs [p])
                     :else (conj (pop runs) (conj (peek runs) p))))
                 [[]] pts)
         (filterv #(>= (count %) 2)))))

(defn sample-curve
  "Runs of scene points of curve `g` (fn [t] scene-point-or-nil) over [t0 t1].
   opts: default-opts plus :max-jump (scene units; a longer step splits the
   run, which hides asymptote jumps)."
  ([g t0 t1] (sample-curve g t0 t1 {}))
  ([g t0 t1 opts]
   (let [{:keys [seed tol max-depth max-jump]} (merge default-opts opts)
         g  (fn [t] (let [p (safe-call g t)] (when (finite-point? p) (mapv double p))))
         t0 (double t0) t1 (double t1)
         ts (mapv #(+ t0 (* (- t1 t0) (/ (double %) seed))) (range (inc seed)))
         pts (reduce (fn [acc [ta tb]]
                       (into acc (refine g ta (g ta) tb (g tb) tol max-depth)))
                     [(g t0)]
                     (partition 2 1 ts))]
     (split-runs pts max-jump))))

(defn graph-fn
  "The scene curve of y = f(x) under coordinate mapping `to-scene`
   ([x y] -> scene point), parametrised by x."
  [to-scene f]
  (fn [x] (let [y (f x)] (when (number? y) (to-scene [x y])))))

(defn parametric-fn
  "The scene curve of t -> [x y] under `to-scene`."
  [to-scene f]
  (fn [t] (let [p (f t)] (when (finite-point? p) (to-scene p)))))

(defn polar-fn
  "The scene curve of theta -> r (r(theta) in math units, plotted as
   [r cos theta, r sin theta]) under `to-scene`."
  [to-scene r]
  (fn [th] (let [rv (r th)]
             (when (number? rv)
               (to-scene [(* rv (Math/cos th)) (* rv (Math/sin th))])))))

(defn lerp-run
  "Linear interpolation of the polyline `run` (points sorted by x) at x, or
   nil outside it. Used by the property tests and by point-on-curve."
  [run x]
  (some (fn [[[x0 y0] [x1 y1]]]
          (when (<= (min x0 x1) x (max x0 x1))
            (if (== x0 x1) y0 (+ y0 (* (- y1 y0) (/ (- x x0) (- x1 x0)))))))
        (partition 2 1 run)))
