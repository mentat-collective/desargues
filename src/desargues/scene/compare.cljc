(ns desargues.scene.compare
  "PURE stratum (host-free .cljc): compare two EDN scene graphs structurally,
   numbers within a tolerance. Hosts differ in the last ulp of libm (JVM
   StrictMath intrinsics vs V8) and in how they print integral doubles (1.0
   vs 1), so cross-host parity is equality up to that, not `=`.")

(def default-tol
  "Absolute tolerance for numbers, in scene units."
  1e-9)

(defn- num-close? [tol a b]
  (<= (abs (- (double a) (double b))) tol))

(defn diffs
  "The paths where `a` and `b` differ (numbers compared within `tol`), at
   most `limit` of them. [] when the graphs match."
  ([a b] (diffs a b {}))
  ([a b {:keys [tol limit] :or {tol default-tol limit 20}}]
   (letfn [(walk [path x y acc]
             (cond
               (>= (count acc) limit) acc
               (and (number? x) (number? y))
               (if (num-close? tol x y) acc (conj acc {:path path :a x :b y}))
               (and (map? x) (map? y))
               (if (not= (set (keys x)) (set (keys y)))
                 (conj acc {:path path :keys-a (sort-by str (keys x)) :keys-b (sort-by str (keys y))})
                 (reduce (fn [acc k] (walk (conj path k) (get x k) (get y k) acc)) acc (keys x)))
               (and (sequential? x) (sequential? y))
               (if (not= (count x) (count y))
                 (conj acc {:path path :count-a (count x) :count-b (count y)})
                 (reduce (fn [acc i] (walk (conj path i) (nth x i) (nth y i) acc))
                         acc (range (count x))))
               (= x y) acc
               :else (conj acc {:path path :a x :b y})))]
     (walk [] a b []))))

(defn same-graph?
  "Whether scene graphs `a` and `b` match up to numeric tolerance."
  ([a b] (empty? (diffs a b)))
  ([a b opts] (empty? (diffs a b opts))))
