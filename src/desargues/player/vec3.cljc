(ns desargues.player.vec3
  "CORE stratum of the player: 3-vector arithmetic on plain vectors [x y z].
   desargues.player.space re-exports it; desargues.player.cut builds on it.
   Host-free .cljc.")

(defn v+ [a b] (mapv + a b))
(defn v- [a b] (mapv - a b))
(defn v* [k a] (mapv #(* (double k) (double %)) a))
(defn dot3 [[a b c] [x y z]] (+ (* a x) (* b y) (* c z)))
(defn cross [[a b c] [x y z]] [(- (* b z) (* c y)) (- (* c x) (* a z)) (- (* a y) (* b x))])
(defn norm [a] (Math/sqrt (double (dot3 a a))))

(defn unit
  "`a` scaled to length 1 (or `a` itself when it has none)."
  [a]
  (let [n (norm a)] (if (zero? n) a (v* (/ 1.0 n) a))))
