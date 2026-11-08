(ns desargues.wallpaper.curves
  "Pure layer of wallpaper loops: space-filling and fractal curves by
   L-system and turtle.

     (expand system n)    the symbol string after n rewrites
     (turtle system s)    the polyline the turtle draws for string s
     (curve :hilbert 5)   order-5 polyline, centred, fitted to [-1, 1]
     (arc-params pts)     cumulative arc length of each vertex, 0 .. 1

   A system is {:axiom \"A\" :rules {\\A \"...\"} :angle degrees :draw #{\\F}
   :closed? bool}: `+` turns left, `-` right, a symbol in :draw moves
   forward drawing, anything else is ignored by the turtle. Presets in
   `systems`.")

(def systems
  "L-systems of classic curves. :closed? marks a curve that ends where it
   began (the Moore curve), so light can run around it without a seam."
  {:hilbert   {:axiom "A" :angle 90 :draw #{\F}
               :rules {\A "+BF-AFA-FB+" \B "-AF+BFB+FA-"}}
   :moore     {:axiom "LFL+F+LFL" :angle 90 :draw #{\F} :closed? true
               :rules {\L "-RF+LFL+FR-" \R "+LF-RFR-FL+"}}
   :peano     {:axiom "X" :angle 90 :draw #{\F}
               :rules {\X "XFYFX+F+YFXFY-F-XFYFX" \Y "YFXFY-F-XFYFX+F+YFXFY"}}
   :gosper    {:axiom "A" :angle 60 :draw #{\A \B}
               :rules {\A "A-B--B+A++AA+B-" \B "+A-BB--B-A++A+B"}}
   :dragon    {:axiom "FX" :angle 90 :draw #{\F}
               :rules {\X "X+YF+" \Y "-FX-Y"}}
   :arrowhead {:axiom "A" :angle 60 :draw #{\A \B}
               :rules {\A "B-A-B" \B "A+B+A"}}})

(defn system
  "The system named `k`, or `k` itself when it is already a system map."
  [k]
  (if (map? k)
    k
    (or (get systems k)
        (throw (ex-info (str "Unknown curve " (pr-str k)) {:curve k :known (keys systems)})))))

(defn expand
  "The symbol string of system `sys` after `n` rewrites."
  [sys n]
  (let [{:keys [axiom rules]} (system sys)]
    (loop [s axiom i 0]
      (if (>= i n)
        s
        (recur (apply str (map #(get rules % (str %)) s)) (inc i))))))

(defn turtle
  "The vertices the turtle visits drawing string `s` under system `sys`,
   from [0 0] heading along +x, unit steps."
  [sys s]
  (let [{:keys [angle draw]} (system sys)
        a (* (double angle) (/ Math/PI 180.0))]
    (loop [[c & more] (seq s) x 0.0 y 0.0 h 0.0 out (transient [[0.0 0.0]])]
      (cond
        (nil? c) (persistent! out)
        (= c \+) (recur more x y (+ h a) out)
        (= c \-) (recur more x y (- h a) out)
        (contains? draw c) (let [nx (+ x (Math/cos h)) ny (+ y (Math/sin h))]
                             (recur more nx ny h (conj! out [nx ny])))
        :else (recur more x y h out)))))

(defn fit
  "Polyline `pts` translated and scaled uniformly so its bounding box is
   centred on the origin with its longer side spanning [-1, 1]."
  [pts]
  (let [xs (map first pts) ys (map second pts)
        x0 (apply min xs) x1 (apply max xs) y0 (apply min ys) y1 (apply max ys)
        s (/ 2.0 (max 1e-12 (- x1 x0) (- y1 y0)))
        cx (/ (+ x0 x1) 2.0) cy (/ (+ y0 y1) 2.0)]
    (mapv (fn [[x y]] [(* s (- x cx)) (* s (- y cy))]) pts)))

(defn curve
  "The order-`n` polyline of system `sys` (a key of `systems` or a map),
   fitted to [-1, 1]. A closed curve repeats its first vertex last."
  [sys n]
  (let [{:keys [closed?] :as s} (system sys)
        pts (fit (turtle s (expand s n)))]
    (if (and closed? (not= (first pts) (peek pts)))
      (conj pts (first pts))
      pts)))

(defn arc-params
  "Cumulative arc length at each vertex of `pts`, divided by the total: a
   vector of numbers from 0 to 1."
  [pts]
  (let [ls (map (fn [[ax ay] [bx by]] (Math/hypot (- bx ax) (- by ay))) pts (rest pts))
        cum (reductions + 0.0 ls)
        total (max 1e-12 (last cum))]
    (mapv #(/ % total) cum)))
