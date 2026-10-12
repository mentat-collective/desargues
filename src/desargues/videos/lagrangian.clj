(ns desargues.videos.lagrangian
  "PhysicalSystems whose equations of motion Emmy derives from a Lagrangian.

   A system is L(t, q, q') over generalized coordinates q; the Cartesian
   positions of its masses are functions of q. Emmy turns L into a state
   derivative; desargues integrates it with any TrajectorySolver and renders
   the positions. Ships the planar and the spherical (3D) double pendulum."
  (:require [desargues.videos.physics :as phys]
            [emmy.env :as e :refer [up]]
            [emmy.expression.compile :as compile]
            [emmy.mechanics.lagrange :as lagrange]))

;; ── Lagrangians ─────────────────────────────────────────────────────────────

(defn lagrangian-from-positions
  "L = T - V for point masses `masses` whose Cartesian positions are
   `positions`, each a fn of the coordinate tuple q -> (up x y z), in uniform
   gravity `g` along -z. Velocities come from the Jacobian of each position."
  [masses positions g]
  (fn [[_t q qdot]]
    (let [T (reduce e/+ 0
                    (map (fn [m p]
                           (let [v (e/* ((e/D p) q) qdot)]
                             (e/* 1/2 m (e/dot-product v v))))
                         masses positions))
          V (reduce e/+ 0
                    (map (fn [m p] (e/* m g (nth (p q) 2)))
                         masses positions))]
      (e/- T V))))

;; ── the system record ───────────────────────────────────────────────────────

(def ^:private dot-label
  "label -> label-dot (memoized: it is pure, and it runs per state per step)."
  (memoize (fn [k] (keyword (str (name k) "-dot")))))

(defn- state-tuple [labels s]
  (let [q    (mapv #(double (get s %)) labels)
        qdot (mapv #(double (get s (dot-label %))) labels)]
    (up 0.0 (apply up q) (apply up qdot))))

(defn- labelled
  "{l0 a0 .. dl0 b0 ..} in labels-then-dot-labels order (what merging the two
   zipmaps built), values coerced with double."
  [labels as bs]
  (let [n (count labels)]
    (loop [i 0 m (transient {}) as (seq as)]
      (if (< i n)
        (recur (inc i) (assoc! m (nth labels i) (double (first as))) (next as))
        (loop [i 0 m m bs (seq bs)]
          (if (< i n)
            (recur (inc i) (assoc! m (dot-label (nth labels i)) (double (first bs))) (next bs))
            (persistent! m)))))))

(def ^:private no-params (double-array 0))

(defn- kernel-derivatives
  "derivatives through the primitive kernel: state map in, labelled map out."
  [labels kernel s]
  (let [n (count labels)
        in (double-array (inc (* 2 n)))
        out (double-array (inc (* 2 n)))]
    (dotimes [i n]
      (aset in (inc i) (double (get s (nth labels i))))
      (aset in (+ 1 n i) (double (get s (dot-label (nth labels i))))))
    (kernel in out no-params)
    (labelled labels
              (map #(aget out %) (range 1 (inc n)))
              (map #(aget out %) (range (inc n) (inc (* 2 n)))))))

(defrecord LagrangianSystem [labels positions state-derivative q qdot kernel]
  phys/PhysicalSystem
  (get-state [_]
    (labelled labels q qdot))
  (set-state [this s]
    (assoc this
           :q    (mapv #(double (get s %)) labels)
           :qdot (mapv #(double (get s (dot-label %))) labels)))
  (derivatives [_ s]
    (if kernel
      (kernel-derivatives labels kernel s)
      (let [[_ dq ddq] (seq (state-derivative (state-tuple labels s)))]
        (labelled labels dq ddq))))
  (state-labels [_]
    (vec (concat labels (map dot-label labels))))
  phys/ArraySystem
  (array-kernel [_]
    (when kernel
      (let [n (count labels)
            m (* 2 n)
            in (double-array (inc m))
            out (double-array (inc m))
            s0 (double-array m)]
        (dotimes [i n]
          (aset s0 i (double (nth q i)))
          (aset s0 (+ n i) (double (nth qdot i))))
        {:state s0
         :deriv! (fn [^doubles s ^doubles ds]
                   (System/arraycopy s 0 in 1 m)
                   (kernel in out no-params)
                   (System/arraycopy out 1 ds 0 m))
         :->state (fn [^doubles s]
                    (labelled labels
                              (map #(aget s %) (range n))
                              (map #(aget s %) (range n m))))}))))

(defn positions-at
  "Cartesian [x y z] of every mass of `system` in `state` (a state map)."
  [{:keys [labels positions position-kernels]} state]
  (if position-kernels
    (let [in (double-array (count labels))]
      (dotimes [i (count labels)] (aset in i (double (get state (nth labels i)))))
      (mapv (fn [k] (let [out (double-array 3)]
                      (k in out no-params)
                      [(aget out 0) (aget out 1) (aget out 2)]))
            position-kernels))
    (let [q (apply up (map #(double (get state %)) labels))]
      (mapv (fn [p] (mapv double (seq (p q)))) positions))))

(def ^:private compiled-cache
  "cache-key -> compiled state derivative (see lagrangian-system :cache-key)."
  (atom {}))

(def ^:private chunk-size
  "Let bindings per generated method; keeps each well under HotSpot's
   8000-byte huge-method limit (beyond it a method is never JIT-compiled)."
  120)

(defn- split-kernel-source
  "Rewrite Emmy's :primitive kernel source (fn [in out ps] (let [..] body))
   into chunk fns of at most chunk-size bindings that hand their locals over
   through a double[] scratch array. Every binding is evaluated by the same
   expression on the same values, so the result is bit-identical."
  [[_ [in-sym out-sym ps-sym] [_ bindings body]]]
  (let [pairs (vec (partition 2 bindings))]
    (if (<= (count pairs) chunk-size)
      (list 'fn [in-sym out-sym ps-sym] (list 'let bindings body))
      (let [index (into {} (map-indexed (fn [i [sym _]] [sym i]) pairs))
            scratch (with-meta (gensym "scratch") {:tag 'doubles})
            refs (fn [form] (into #{} (filter index) (tree-seq coll? seq form)))
            load (fn [syms] (mapcat (fn [s] [s (list 'clojure.core/aget scratch (index s))]) (sort-by index syms)))
            chunks (partition-all chunk-size pairs)
            chunk-fns (map (fn [chunk]
                             (let [own (set (map first chunk))
                                   needed (remove own (refs (map second chunk)))]
                               (list 'fn [in-sym ps-sym scratch]
                                     (list 'let (vec (concat (load needed) (mapcat identity chunk)))
                                           (cons 'do (concat (map (fn [[s _]] (list 'clojure.core/aset scratch (index s) s)) chunk)
                                                             [nil]))))))
                           chunks)
            names (repeatedly (count chunks) #(gensym "chunk"))]
        (list 'let (vec (interleave names chunk-fns))
              (list 'fn [in-sym out-sym ps-sym]
                    (list 'let [scratch (list 'clojure.core/double-array (count pairs))]
                          (cons 'do (concat (map (fn [nm] (list nm in-sym ps-sym scratch)) names)
                                            [(list 'let (vec (load (refs body))) body)])))))))))

(defn- kernel->state-fn
  "The (up t q q') -> (up 1 q' q'') state derivative, run on a primitive kernel."
  [kernel n]
  (fn [state]
    (let [m (* 2 n)
          in (double-array (inc m))
          out (double-array (inc m))
          [t q qdot] (seq state)]
      (aset in 0 (double t))
      (dotimes [i n]
        (aset in (inc i) (double (nth (seq q) i)))
        (aset in (+ 1 n i) (double (nth (seq qdot) i))))
      (kernel in out no-params)
      (up (aget out 0)
          (apply up (map #(aget out %) (range 1 (inc n))))
          (apply up (map #(aget out %) (range (inc n) (inc m))))))))

(defn- compile-position
  "Primitive kernel (fn [^doubles q ^doubles xyz ps]) for position fn p, or nil
   when p does not compile to a 3-vector."
  [p n]
  (try
    (let [src (compile/compile-state-fn p false (apply up (repeat n 1.0))
                                        {:simplify? false :mode :source :cache? false
                                         :calling-convention :primitive})]
      (binding [*ns* (the-ns 'desargues.videos.lagrangian)]
        (eval src)))
    (catch Exception _ nil)))

(defn lagrangian-system
  "A PhysicalSystem from Lagrangian `L` (a fn of the (up t q q') local tuple).
   labels name the coordinates (their velocities are label-dot); positions are
   the mass position fns of q used by positions-at; q0 / qdot0 are the initial
   values. :compile? (default true) compiles the derived state derivative with
   Emmy's compiler, simplification off, into a primitive double[] kernel
   before the first step (the system then implements phys/ArraySystem), and
   the position fns into primitive kernels for positions-at. The kernel's
   Emmy source stays on the system as :kernel-source, for engines that
   compile it themselves (desargues.infrastructure.raster-kernels).

   :cache-key (optional) names the system up to its initial state: compiled
   derivatives are memoized under [cache-key (count q0)], so a second system
   with the same key reuses the compiled fn. Two Lagrangians must share a key
   only when they are the same function of the local tuple (and so the same
   positions)."
  [{:keys [L labels positions q0 qdot0 compile? cache-key] :or {compile? true}}]
  (let [n (count q0)
        st0 (up 0.0 (apply up (map double q0)) (apply up (map double qdot0)))
        build (fn []
                (let [sd (lagrange/Lagrangian->state-derivative L)
                      src (compile/compile-state-fn (fn [] sd) [] st0
                                                    {:simplify? false :calling-convention :primitive
                                                     :mode :source})
                      kernel (binding [*ns* (the-ns 'desargues.videos.lagrangian)]
                               (eval (split-kernel-source src)))
                      pks (mapv #(compile-position % n) positions)]
                  {:kernel kernel
                   :kernel-source src
                   :sd (kernel->state-fn kernel n)
                   :position-kernels (when (every? some? pks) pks)}))
        compiled (cond
                   (not compile?) nil
                   (nil? cache-key) (build)
                   :else (let [k [cache-key n]]
                           (or (get @compiled-cache k)
                               (get (swap! compiled-cache
                                           (fn [c] (if (contains? c k) c (assoc c k (build)))))
                                    k))))
        sd (if compiled (:sd compiled) (lagrange/Lagrangian->state-derivative L))]
    (cond-> (->LagrangianSystem (vec labels) (vec positions) sd (mapv double q0) (mapv double qdot0)
                                (:kernel compiled))
      (:kernel-source compiled) (assoc :kernel-source (:kernel-source compiled))
      (:position-kernels compiled) (assoc :position-kernels (:position-kernels compiled)))))

;; ── double pendulums ────────────────────────────────────────────────────────

(defn double-pendulum
  "Planar double pendulum in the x-z plane (z up, pivot at the origin):
   coordinates theta1 theta2 from the vertical. opts: :l1 :l2 :m1 :m2 :g
   :theta1 :theta2 :omega1 :omega2 :compile?"
  [{:keys [l1 l2 m1 m2 g theta1 theta2 omega1 omega2 compile?]
    :or {l1 1.0 l2 1.0 m1 1.0 m2 1.0 g 9.8 theta1 2.0 theta2 2.5 omega1 0.0 omega2 0.0 compile? true}}]
  (let [p1 (fn [[th1 _]] (up (e/* l1 (e/sin th1)) 0 (e/- (e/* l1 (e/cos th1)))))
        p2 (fn [[_ th2 :as q]] (e/+ (p1 q) (up (e/* l2 (e/sin th2)) 0 (e/- (e/* l2 (e/cos th2))))))]
    (lagrangian-system {:L (lagrangian-from-positions [m1 m2] [p1 p2] g), :labels [:theta1 :theta2], :positions [p1 p2], :q0 [theta1 theta2], :qdot0 [omega1 omega2], :compile? compile?, :cache-key [:desargues.videos.lagrangian/double-pendulum l1 l2 m1 m2 g]})))

(defn spherical-double-pendulum
  "Two chained spherical pendulums (z up, pivot at the origin): each bob has a
   polar angle theta from the downward vertical and an azimuth phi. opts: :l1
   :l2 :m1 :m2 :g :theta1 :phi1 :theta2 :phi2 and the four initial rates
   :theta1-dot :phi1-dot :theta2-dot :phi2-dot, :compile?

   theta = 0 and theta = pi are coordinate singularities (the mass matrix has
   sin theta on its diagonal): a trajectory that reaches a pole turns to NaN.
   Give each bob some azimuthal rate and it stays clear of them."
  [{:keys [l1 l2 m1 m2 g theta1 phi1 theta2 phi2 theta1-dot phi1-dot theta2-dot phi2-dot compile?]
    :or {l1 1.0 l2 1.0 m1 1.0 m2 1.0 g 9.8
         theta1 1.0 phi1 0.0 theta2 2.0 phi2 1.0
         theta1-dot 0.0 phi1-dot 0.8 theta2-dot 0.0 phi2-dot -0.4 compile? true}}]
  (let [bob (fn [l th ph] (up (e/* l (e/sin th) (e/cos ph))
                              (e/* l (e/sin th) (e/sin ph))
                              (e/- (e/* l (e/cos th)))))
        p1  (fn [[th1 ph1 _ _]] (bob l1 th1 ph1))
        p2  (fn [[_ _ th2 ph2 :as q]] (e/+ (p1 q) (bob l2 th2 ph2)))]
    (lagrangian-system {:L (lagrangian-from-positions [m1 m2] [p1 p2] g), :labels [:theta1 :phi1 :theta2 :phi2], :positions [p1 p2], :q0 [theta1 phi1 theta2 phi2], :qdot0 [theta1-dot phi1-dot theta2-dot phi2-dot], :compile? compile?, :cache-key [:desargues.videos.lagrangian/spherical-double-pendulum l1 l2 m1 m2 g]})))

(defn energy
  "Total energy T + V of a system with coordinate `labels` in `state`, from the
   Lagrangian `L` it was built from."
  [{:keys [labels]} L state]
  ((lagrange/Lagrangian->energy L) (state-tuple labels state)))
