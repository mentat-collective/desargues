(ns desargues.geometry.kernel
  "Batched numeric kernels for the solve-time cost evaluations of
   desargues.geometry.fit and desargues.geometry.disposition, over flat
   double arrays.

   A GeometryKernel provides the two batched operations:

     transform-cloud!  apply a homography (row-major double[9]) to a point
                       cloud given as xs, ys; writes the affine images into
                       ox, oy and the homogeneous w into ow, returns ow.
     lerp-grid!        sample the straight line a -> b in leaf space at the
                       interior t_s = (s+1)/(samples+1), s = 0..samples-1;
                       writes row s at out[s*L .. s*L+L-1], returns out.

   Providers: `array-kernel` (pure Clojure, always present) and the raster
   provider desargues.infrastructure.raster-adapter/geometry-kernel (only
   under the :dynamics alias, resolved with requiring-resolve). Both compute
   the same arithmetic in the same order, so their results are identical.

   *kernel* is the provider the solvers use; it defaults to `array-kernel`.
   (select-kernel :raster) answers the raster provider or nil when raster is
   not on the classpath; :auto answers the default.

   Also here: the double-array forms of the frame cost and the degeneracy
   cost, and of the homography families, so a minimizer never builds an Emmy
   matrix per evaluation.")

(defprotocol GeometryKernel
  (transform-cloud! [k h xs ys ox oy ow]
    "H . (x, y, 1) for every point: affine images into ox, oy, w into ow.
     All arguments are double arrays; h is row-major 3x3. Returns ow.")
  (lerp-grid! [k a b out samples]
    "out[s*L + j] = a[j] + t_s * (b[j] - a[j]), t_s = (s+1)/(samples+1), for
     s in [0, samples), L = (alength a). Returns out."))

(defrecord ArrayKernel []
  GeometryKernel
  (transform-cloud! [_ h xs ys ox oy ow]
    (let [^doubles h h ^doubles xs xs ^doubles ys ys
          ^doubles ox ox ^doubles oy oy ^doubles ow ow
          h0 (aget h 0) h1 (aget h 1) h2 (aget h 2)
          h3 (aget h 3) h4 (aget h 4) h5 (aget h 5)
          h6 (aget h 6) h7 (aget h 7) h8 (aget h 8)]
      (dotimes [i (alength xs)]
        (let [x (aget xs i) y (aget ys i)
              w (+ (+ (* h6 x) (* h7 y)) h8)]
          (aset ow i w)
          (aset ox i (/ (+ (+ (* h0 x) (* h1 y)) h2) w))
          (aset oy i (/ (+ (+ (* h3 x) (* h4 y)) h5) w))))
      ow))
  (lerp-grid! [_ a b out samples]
    (let [^doubles a a ^doubles b b ^doubles out out
          l (alength a)
          denom (double (inc (long samples)))]
      (dotimes [s (long samples)]
        (let [t (/ (double (inc s)) denom)
              base (* s l)]
          (dotimes [j l]
            (aset out (+ base j) (+ (aget a j) (* t (- (aget b j) (aget a j))))))))
      out)))

(def array-kernel
  "The pure-Clojure GeometryKernel."
  (->ArrayKernel))

(defn raster-kernel
  "The raster GeometryKernel, or nil when raster is not on the classpath. A
   load failure that is not raster's absence is rethrown."
  []
  (when-let [f (try (requiring-resolve 'desargues.infrastructure.raster-adapter/geometry-kernel)
                    (catch Throwable t
                      (let [root (loop [e t] (if-let [c (.getCause e)] (recur c) e))]
                        (when-not (.contains (str (.getMessage root)) "raster")
                          (throw t)))
                      nil))]
    (f)))

(defn select-kernel
  "Kernel by name: :array, :raster (nil when raster is absent) or :auto (the
   default, array-kernel)."
  [k]
  (case k
    :array array-kernel
    :raster (raster-kernel)
    :auto array-kernel))

(def ^:dynamic *kernel*
  "The GeometryKernel the solvers evaluate through."
  array-kernel)

;; =============================================================================
;; Homographies and point clouds as double arrays
;; =============================================================================

(defn h->array
  "A 3x3 homography (Emmy matrix or nested rows) -> row-major double[9]."
  ^doubles [H]
  (double-array (for [r (range 3) c (range 3)] (double (get-in H [r c])))))

(defn similarity-a
  "double[9] of desargues.geometry.projective/similarity-h."
  ^doubles [[theta s tx ty]]
  (let [theta (double theta) s (double s)
        c (* s (Math/cos theta)) sn (* s (Math/sin theta))]
    (double-array [c (- sn) tx sn c ty 0.0 0.0 1.0])))

(defn affine-a
  "double[9] of desargues.geometry.projective/affine-h."
  ^doubles [[a b c d tx ty]]
  (double-array [a b tx c d ty 0.0 0.0 1.0]))

(defn projective-a
  "double[9] of desargues.geometry.projective/projective-h."
  ^doubles [[a b c d e f g h]]
  (double-array [a b c d e f g h 1.0]))

(defn cloud
  "Points (a map's vals or a seq of [x y]) -> [xs ys] double arrays."
  [points]
  (let [ps (vec (if (map? points) (vals points) points))]
    [(double-array (map #(double (nth % 0)) ps))
     (double-array (map #(double (nth % 1)) ps))]))

;; =============================================================================
;; Costs over arrays
;; =============================================================================

(defn spacing-arrays
  "Minimum pairwise Euclidean distance (Double/MAX_VALUE below two points)."
  ^double [^doubles xs ^doubles ys]
  (let [n (alength xs)]
    (loop [i 0 m Double/MAX_VALUE]
      (if (< i n)
        (recur (inc i)
               (loop [j (inc i) m m]
                 (if (< j n)
                   (recur (inc j)
                          (min m (Math/hypot (- (aget xs j) (aget xs i))
                                             (- (aget ys j) (aget ys i)))))
                   m)))
        m))))

(defn frame-cost-arrays
  "desargues.geometry.fit/frame-cost over xs, ys: out-of-bounds distance
   (weight :oob) + margin shortfall (weight :margin) + inverse min-spacing
   (weight :spacing)."
  [^doubles xs ^doubles ys [[xlo xhi] [ylo yhi]] margin weights]
  (let [xlo (double xlo) xhi (double xhi) ylo (double ylo) yhi (double yhi)
        margin (double margin)
        n (alength xs)
        [o s] (loop [i 0 o 0.0 s 0.0]
                (if (< i n)
                  (let [x (aget xs i) y (aget ys i)]
                    (recur (inc i)
                           (+ o (+ (max 0.0 (- xlo x) (- x xhi))
                                   (max 0.0 (- ylo y) (- y yhi))))
                           (+ s (+ (max 0.0 (- margin (- x xlo)))
                                   (max 0.0 (- margin (- xhi x)))
                                   (max 0.0 (- margin (- y ylo)))
                                   (max 0.0 (- margin (- yhi y)))))))
                  [o s]))
        inv-spacing (/ 1.0 (+ 1e-6 (spacing-arrays xs ys)))]
    (+ (+ (* (double (:oob weights)) (double o))
          (* (double (:margin weights)) (double s)))
       (* (double (:spacing weights)) inv-spacing))))

(defn degeneracy-cost-arrays
  "Big unless every |w| >= eps and all w share a sign (no point sent to, or
   through, infinity)."
  ^double [^doubles ws eps]
  (let [eps (double eps)
        n (alength ws)
        m (loop [i 0 m Double/MAX_VALUE]
            (if (< i n) (recur (inc i) (min m (Math/abs (aget ws i)))) m))
        same-sign? (or (every? pos? ws) (every? neg? ws))]
    (+ (if same-sign? 0.0 1.0e6)
       (if (< m eps) (* 1.0e6 (- eps m)) 0.0))))

(defn h-cloud-cost
  "frame-cost-arrays + degeneracy-cost-arrays of H . cloud, transformed
   through *kernel*. h is row-major double[9]; xs, ys the source cloud."
  [^doubles h ^doubles xs ^doubles ys frame margin weights eps]
  (let [n (alength xs)
        ox (double-array n) oy (double-array n) ow (double-array n)]
    (transform-cloud! *kernel* h xs ys ox oy ow)
    (+ (double (frame-cost-arrays ox oy frame margin weights))
       (degeneracy-cost-arrays ow eps))))
