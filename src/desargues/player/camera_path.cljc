(ns desargues.player.camera-path
  "PURE stratum of the player: a camera animated over time.

   A camera path is a seq of keyframes {:t seconds :camera {...}}, the camera
   in desargues.player.space's vocabulary ({:target :distance :yaw :pitch
   :projection}). `camera-at` interpolates it: every numeric field (and each
   coordinate of :target) runs along a cubic Hermite spline through the
   keyframes, with Catmull-Rom tangents inside and zero tangents at the ends,
   so the camera passes through every keyframe exactly, eases in and out of
   the path, and has no velocity jump at a keyframe. :projection is not
   interpolated: it is the one of the keyframe in force.

     (def path (fly-around {:t0 0 :t1 12 :yaw -0.9 :pitch 0.5 :distance 11}))
     (camera-at path 3.0)   ; => {:yaw ... :pitch ... :distance ... :target [...]}

   A source animates its :camera by putting (camera-at path t) in each frame
   (desargues.player.calculus-demos). Host-free .cljc.")

(def numeric-fields
  "The camera fields interpolated as numbers."
  [:yaw :pitch :distance])

(defn keyframes
  "Keyframes `ks` sorted by :t, each camera filled from desargues.player.space
   defaults where the keyframe leaves a field out of the first one."
  [ks]
  (let [ks (vec (sort-by :t ks))]
    (if (empty? ks)
      ks
      (let [base (merge {:target [0.0 0.0 0.0] :distance 12.0 :yaw -1.0 :pitch 0.45
                         :projection :perspective}
                        (:camera (first ks)))]
        (:out (reduce (fn [{:keys [prev out]} k]
                        (let [cam (merge prev (:camera k))]
                          {:prev cam :out (conj out (assoc k :camera cam))}))
                      {:prev base :out []} ks))))))

(defn- segment-index
  "Index i of the keyframe interval [t_i, t_i+1] holding time `t`."
  [ts t]
  (let [n (count ts)]
    (loop [i 0]
      (if (and (< i (- n 2)) (>= t (nth ts (inc i)))) (recur (inc i)) i))))

(defn- tangent
  "Catmull-Rom tangent of values `vs` at times `ts`, index `i` (0 at the ends)."
  [ts vs i]
  (let [n (count ts)]
    (if (or (zero? i) (= i (dec n)))
      0.0
      (/ (- (double (nth vs (inc i))) (double (nth vs (dec i))))
         (- (double (nth ts (inc i))) (double (nth ts (dec i))))))))

(defn hermite
  "The cubic Hermite spline through values `vs` at increasing times `ts`, at
   time `t` (clamped to the first and last time)."
  [ts vs t]
  (let [n (count ts)]
    (cond
      (= n 1) (double (first vs))
      (<= t (first ts)) (double (first vs))
      (>= t (peek ts)) (double (peek vs))
      :else
      (let [i  (segment-index ts t)
            t0 (double (nth ts i)) t1 (double (nth ts (inc i)))
            h  (- t1 t0)
            s  (/ (- t t0) h)
            s2 (* s s) s3 (* s2 s)
            p0 (double (nth vs i)) p1 (double (nth vs (inc i)))
            m0 (tangent ts vs i) m1 (tangent ts vs (inc i))]
        (+ (* (+ (* 2 s3) (* -3 s2) 1) p0)
           (* (+ s3 (* -2 s2) s) h m0)
           (* (+ (* -2 s3) (* 3 s2)) p1)
           (* (- s3 s2) h m1))))))

(defn camera-at
  "The camera of path `ks` (keyframes) at time `t`, or nil for an empty path.
   A :zoom (magnification of the target plane) is interpolated like the
   other numbers when any keyframe carries one (1 where one is missing), and
   left out otherwise."
  [ks t]
  (let [ks (keyframes ks)]
    (when (seq ks)
      (let [t      (double t)
            ts     (mapv #(double (:t %)) ks)
            zoom?  (some #(contains? (:camera %) :zoom) ks)
            cams   (cond->> (mapv :camera ks) zoom? (mapv #(update % :zoom (fnil double 1.0))))
            fields (cond-> numeric-fields zoom? (conj :zoom))
            cur    (or (last (keep-indexed (fn [i ti] (when (<= ti t) (nth cams i))) ts))
                       (first cams))
            num    (fn [k] (hermite ts (mapv k cams) t))]
        (merge cur
               (into {} (map (fn [k] [k (num k)])) fields)
               {:target (mapv (fn [c] (hermite ts (mapv #(nth (:target %) c) cams) t)) (range 3))})))))

(defn fly-around
  "Keyframes of a fly-around between `t0` and `t1`: the yaw turns `turns`
   full circles from `yaw`, the pitch swings between `pitch` and
   `pitch` + `lift` and back, and the distance breathes in by `zoom`.
   `n` keyframes per turn (default 6)."
  [{:keys [t0 t1 yaw pitch distance target turns lift zoom n]
    :or {t0 0.0 t1 12.0 yaw -0.9 pitch 0.5 distance 11.0 target [0.0 0.0 0.0]
         turns 1.0 lift 0.35 zoom 2.0 n 6}}]
  (let [steps (max 2 (long (Math/ceil (* (double n) (double turns)))))]
    (vec (for [k (range (inc steps))
               :let [s (/ (double k) steps)
                     w (Math/sin (* Math/PI s))]]
           {:t (+ (double t0) (* s (- (double t1) (double t0))))
            :camera {:target target
                     :yaw (+ (double yaw) (* 2.0 Math/PI (double turns) s))
                     :pitch (+ (double pitch) (* (double lift) w))
                     :distance (- (double distance) (* (double zoom) w))}}))))
