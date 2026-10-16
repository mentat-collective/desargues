(ns desargues.objects.coords
  "CORE stratum of the object vocabulary: the coordinate-system value object.

   A CoordSystem maps MATH coordinates (the function's x and y) to SCENE
   coordinates (Manim's frame units, y up, origin at the frame centre) and
   back. It is the one place that knows where a plot lives on screen, so every
   object built on it (axes, plots, fields, Riemann rectangles) agrees on the
   mapping and a slider that moves the axes moves everything with them.

   Axis-aligned and affine per axis, like Manim's Axes: the x-range
   [x-min x-max] spans `x-length` scene units, the y-range spans `y-length`,
   and the box is centred on `center`. Host-free .cljc: plain data, no interop.")

(defprotocol ICoords
  "A mapping between math and scene coordinates. Small on purpose (ISP): a
   polar or log system is another record implementing the same two methods."
  (->scene [cs p] "Scene point [sx sy] of math point [x y].")
  (->math  [cs p] "Math point [x y] of scene point [sx sy]."))

(defn- span [[lo hi]] (- (double hi) (double lo)))
(defn- mid  [[lo hi]] (/ (+ (double lo) (double hi)) 2.0))

(defrecord CoordSystem [x-range y-range x-length y-length center]
  ICoords
  (->scene [_ [x y]]
    (let [[cx cy] center]
      [(+ (double cx) (* (/ (- (double x) (mid x-range)) (span x-range)) (double x-length)))
       (+ (double cy) (* (/ (- (double y) (mid y-range)) (span y-range)) (double y-length)))]))
  (->math [_ [sx sy]]
    (let [[cx cy] center]
      [(+ (mid x-range) (* (/ (- (double sx) (double cx)) (double x-length)) (span x-range)))
       (+ (mid y-range) (* (/ (- (double sy) (double cy)) (double y-length)) (span y-range)))])))

(def defaults
  "Manim's Axes defaults: x in [-7 7], y in [-4 4] filling most of the frame."
  {:x-range [-7.0 7.0 1.0] :y-range [-4.0 4.0 1.0]
   :x-length 12.0 :y-length 6.0 :center [0.0 0.0]})

(defn coord-system
  "A CoordSystem from opts {:x-range [min max step?] :y-range [min max step?]
   :x-length :y-length :center [cx cy]}; omitted keys take `defaults`. The
   optional third entry of a range is its tick step (see ticks)."
  ([] (coord-system {}))
  ([opts]
   (let [{:keys [x-range y-range x-length y-length center]} (merge defaults opts)]
     (when-not (and (pos? (span x-range)) (pos? (span y-range)))
       (throw (ex-info "coord-system: each range needs max > min"
                       {:x-range x-range :y-range y-range})))
     (->CoordSystem (vec x-range) (vec y-range) (double x-length) (double y-length)
                    (mapv double center)))))

(defn coord-system?
  [x]
  (satisfies? ICoords x))

(defn x-unit
  "Scene units per math unit along x."
  [cs]
  (/ (double (:x-length cs)) (span (:x-range cs))))

(defn y-unit
  "Scene units per math unit along y."
  [cs]
  (/ (double (:y-length cs)) (span (:y-range cs))))

(defn origin
  "Scene point of the math origin clamped into the box, where the axes cross."
  [cs]
  (let [clamp (fn [[lo hi] v] (max (double lo) (min (double hi) v)))]
    (->scene cs [(clamp (:x-range cs) 0.0) (clamp (:y-range cs) 0.0)])))

(defn ticks
  "Tick values of `rng` [min max step]: every multiple of step inside the
   range (step defaults to 1). Multiples, not offsets from min, so 0 is a
   tick whenever it is inside."
  [[lo hi step]]
  (let [lo (double lo) hi (double hi) step (double (or step 1.0))
        eps (* 1e-9 step)]
    (when (pos? step)
      (->> (iterate inc (long (Math/ceil (/ (- lo eps) step))))
           (map #(* (double %) step))
           (take-while #(<= % (+ hi eps)))
           vec))))

(defn inside?
  "True when math point [x y] lies in the system's box (inclusive)."
  [cs [x y]]
  (let [[x0 x1] (:x-range cs) [y0 y1] (:y-range cs)]
    (and (<= x0 x x1) (<= y0 y y1))))
