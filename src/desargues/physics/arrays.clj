(ns desargues.physics.arrays
  "PURE stratum (JVM-only) of the physics bounded context: the primitive
   double[] fast paths of RK4.

   Both functions perform the same IEEE operations, in the same order, as
   the generic map-merging RK4 step in desargues.physics.integrators; the
   raster, wasm and gpu conformance tests compare their output bit for bit.
   No state escapes: every array is local to one call.

   Requires only desargues.physics.protocols. JVM .clj (primitive arrays,
   java.lang.Double)."
  (:require [desargues.physics.protocols :as p]))

(defn all-doubles?
  "Every value of m a java.lang.Double, keys exactly ks (a vector)."
  [m ks]
  (and (= (count m) (count ks))
       (every? #(instance? Double (get m %)) ks)))

(defn rk4-step-doubles
  "The generic RK4 step, specialised to all-Double states and derivatives
   with one key set: the same IEEE operations in the same order, on
   double[]s. Answers nil (caller falls back to the generic path) when a
   derivative map breaks that shape."
  [system state ^double dt]
  (let [ks (vec (keys state))
        n (count ks)
        s (double-array (map #(get state %) ks))
        h2 (/ dt 2.0) h6 (/ dt 6.0)
        ->arr (fn [d] (when (all-doubles? d ks) (double-array (map #(get d %) ks))))
        ->state (fn [^doubles a]
                  (persistent! (reduce (fn [m i] (assoc! m (nth ks i) (aget a i)))
                                       (transient state) (range n))))
        axpy (fn [^doubles k ^double h]
               (let [o (double-array n)]
                 (dotimes [i n] (aset o i (+ (aget s i) (* (aget k i) h))))
                 o))]
    (when-let [^doubles k1 (->arr (p/derivatives system state))]
      (when-let [^doubles k2 (->arr (p/derivatives system (->state (axpy k1 h2))))]
        (when-let [^doubles k3 (->arr (p/derivatives system (->state (axpy k2 h2))))]
          (when-let [^doubles k4 (->arr (p/derivatives system (->state (axpy k3 dt))))]
            (let [o (double-array n)]
              (dotimes [i n]
                (let [c (+ (+ (+ (aget k1 i) (* 2.0 (aget k2 i))) (* 2.0 (aget k3 i))) (aget k4 i))]
                  (aset o i (+ (aget s i) (* c h6)))))
              (->state o))))))))

(defn rk4-fast-step
  "The RK4 step on the double[] path when state and dt are all Doubles,
   else nil (the caller takes the generic step)."
  [system state dt]
  (when (and (instance? Double dt) (seq state) (all-doubles? state (vec (keys state))))
    (rk4-step-doubles system state (double dt))))

(defn rk4-trajectory-arrays
  "The stepper's rk4 loop on an ArraySystem kernel (see p/array-kernel): the
   same IEEE operations, in the same order, as rk4-step-doubles, with every
   intermediate in a reused double[]."
  [{:keys [state deriv! ->state]} ^double dt ^double duration]
  (let [^doubles s state
        n (alength s)
        k1 (double-array n) k2 (double-array n) k3 (double-array n) k4 (double-array n)
        tmp (double-array n)
        h2 (/ dt 2.0) h6 (/ dt 6.0)
        axpy! (fn [^doubles k ^double h]
                (dotimes [i n] (aset tmp i (+ (aget s i) (* (aget k i) h)))))]
    (loop [t 0.0
           trajectory (transient [{:time 0.0 :state (->state s)}])]
      (if (>= t duration)
        (persistent! trajectory)
        (do (deriv! s k1)
            (axpy! k1 h2) (deriv! tmp k2)
            (axpy! k2 h2) (deriv! tmp k3)
            (axpy! k3 dt) (deriv! tmp k4)
            (dotimes [i n]
              (let [c (+ (+ (+ (aget k1 i) (* 2.0 (aget k2 i))) (* 2.0 (aget k3 i))) (aget k4 i))]
                (aset s i (+ (aget s i) (* c h6)))))
            (let [new-t (+ t dt)]
              (recur new-t (conj! trajectory {:time new-t :state (->state s)}))))))))

(defn array-kernel-for
  "The ArraySystem kernel of `system` when the stepper may take the array
   loop (dt a Double, the system an ArraySystem with a kernel), else nil."
  [system dt]
  (and (instance? Double dt)
       (satisfies? p/ArraySystem system)
       (p/array-kernel system)))
