(ns desargues.tracks.sample
  "PURE stratum of the tracks context: a Track resolved at time t.

   sample is the one interpolation rule every consumer shares, so a live
   player and a later exporter agree on what a track means between keys:
     - before the first key / after the last: clamped to that key, exactly;
     - at a key time: that key's values, exactly (alpha = 0, no arithmetic);
     - between keys: translate, scale and numeric shape params linearly,
       rotation by normalized quaternion lerp along the shorter arc;
     - a component only one of the two bracketing keys carries holds the
       earlier key's value (step), so sparse keys never invent data.
   Host-free .cljc."
  (:require [desargues.tracks.core :as core]))

(defn- lerp [a b alpha] (+ a (* (- b a) alpha)))

(defn- lerp-vec [p q alpha] (mapv #(lerp (double %1) (double %2) alpha) p q))

(defn- dot4 [p q] (reduce + (map * p q)))

(defn nlerp
  "Normalized lerp of unit quaternions [w x y z], taking the shorter arc."
  [p q alpha]
  (let [q (if (neg? (dot4 p q)) (mapv - q) q)
        r (lerp-vec p q alpha)
        n (Math/sqrt (dot4 r r))]
    (if (zero? n) (vec p) (mapv #(/ % n) r))))

(defn- lerp-value [a b alpha]
  (cond
    (and (number? a) (number? b)) (lerp (double a) (double b) alpha)
    (and (vector? a) (vector? b) (= (count a) (count b))) (lerp-vec a b alpha)
    :else a))

(defn- lerp-map
  "Keys of map `a` interpolated toward `b` with per-key fn `f`; keys only `a`
   has hold, keys only `b` has are absent (step semantics)."
  [a b alpha f]
  (when a
    (reduce-kv (fn [m k va]
                 (assoc m k (if (contains? b k) (f k va (get b k) alpha) va)))
               {} a)))

(defn- lerp-transform [a b alpha]
  (lerp-map a b alpha (fn [k va vb alpha]
                        (if (= k :rotate) (nlerp va vb alpha) (lerp-vec va vb alpha)))))

(defn- lerp-params [a b alpha]
  (lerp-map a b alpha (fn [_ va vb alpha] (lerp-value va vb alpha))))

(defn bracket
  "Index i of the last keyframe of `kfs` (ascending :t) with :t <= t, clamped
   to [0, n-1]. Binary search: tracks from a 10 s run at dt 0.01 hold 1000 keys."
  [kfs t]
  (let [n (count kfs)]
    (loop [lo 0 hi (dec n)]
      (if (>= lo hi)
        lo
        (let [mid (quot (+ lo hi 1) 2)]
          (if (<= (:t (nth kfs mid)) t)
            (recur mid hi)
            (recur lo (dec mid))))))))

(defn- at-time [kf t] (assoc kf :t t))

(defn sample
  "Keyframe-shaped value of track `trk` at time `t`: {:t t :transform ..
   :shape-params ..} (only the components the track carries)."
  [trk t]
  (let [kfs (core/keyframes trk)
        t (double t)
        first-kf (first kfs)
        last-kf (peek kfs)]
    (cond
      (<= t (double (:t first-kf))) (at-time first-kf t)
      (>= t (double (:t last-kf)))  (at-time last-kf t)
      :else
      (let [i (bracket kfs t)
            a (nth kfs i)
            b (nth kfs (inc i))
            ta (double (:t a))]
        (if (== t ta)
          (at-time a t)
          (let [alpha (/ (- t ta) (- (double (:t b)) ta))]
            (cond-> {:t t}
              (:transform a)    (assoc :transform (lerp-transform (:transform a) (:transform b) alpha))
              (:shape-params a) (assoc :shape-params (lerp-params (:shape-params a) (:shape-params b) alpha)))))))))

(defn sample-tracks
  "{node-id sample} of every track in `tracks` at time `t`."
  [tracks t]
  (persistent!
   (reduce-kv (fn [m id trk] (assoc! m id (sample trk t))) (transient {}) tracks)))

(defn duration
  "Latest end time over `tracks` (0.0 when empty)."
  [tracks]
  (reduce max 0.0 (map (comp double core/end-time) (vals tracks))))
