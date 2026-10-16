(ns desargues.objects.calc.shapes2d
  "TARGET stratum: the 2D calculus kinds -> player frame shapes (methods of
   desargues.objects.calc.shapes/->shapes), built from the small builders
   there: a region is a closed filled :path, a Riemann piece a closed :path
   (rectangle or trapezoid), a line a :line, a moving point a :dot.
   Host-free .cljc."
  (:require [desargues.objects.calc.core :as calc]
            [desargues.objects.calc.shapes :as sh]))

(defn- fill-of [o] (:color o))

(defmethod sh/->shapes :graph [{:keys [a b] :as o} ctx]
  [(sh/path (sh/sid o "curve") (sh/graph-points ctx o :f a b) {:color (:color o) :width 4})])

(defn- region
  "Closed scene outline of the region between slot `top` (or y = 0) and slot
   `bottom` (or y = 0) of `o` over [a x]."
  [ctx o top bottom a x]
  (let [n   (max 2 (long (* 80 (/ (- x a) (max 1e-9 (- (double (:b o)) (double (:a o))))))))
        xs  (sh/sample-xs a x n)
        y-of (fn [k t] (if k (calc/f-at o k t) 0.0))]
    (concat (map #(sh/->scene ctx [% (y-of top %)]) xs)
            (map #(sh/->scene ctx [% (y-of bottom %)]) (reverse xs)))))

(defn- upto-x [{:keys [a b upto]}]
  (+ (double a) (* (double (or upto 1.0)) (- (double b) (double a)))))

(defmethod sh/->shapes :area-under [{:keys [a] :as o} ctx]
  (let [x (upto-x o)]
    (if (<= x (+ (double a) 1e-9))
      []
      [(sh/path (sh/sid o "region") (region ctx o :f nil a x)
                {:closed? true :color (fill-of o) :width 1 :fill (fill-of o) :fill-opacity 0.45})
       (sh/line (sh/sid o "edge") (sh/->scene ctx [x 0]) (sh/->scene ctx [x (calc/f-at o :f x)])
                {:color (fill-of o) :width 3})])))

(defmethod sh/->shapes :area-between [{:keys [a] :as o} ctx]
  (let [x (upto-x o)]
    (if (<= x (+ (double a) 1e-9))
      []
      [(sh/path (sh/sid o "region") (region ctx o :f :g a x)
                {:closed? true :color (fill-of o) :width 1 :fill (fill-of o) :fill-opacity 0.45})])))

(defmethod sh/->shapes :riemann [{:keys [rule] :as o} ctx]
  (vec (map-indexed
        (fn [i {:keys [l r hl hr h]}]
          (let [[tl tr] (if (= :trapezoid rule) [hl hr] [h h])]
            (sh/path (sh/sid o "piece" i)
                     (map #(sh/->scene ctx %) [[l 0] [r 0] [r tr] [l tl]])
                     {:closed? true :color :white :width 1 :fill (:color o) :fill-opacity 0.5})))
        (calc/riemann-pieces o))))

(defn- line-through
  "Scene segment of the line through math [x0 y0] with `slope`, x in
   [x0 - lo, x0 + hi]."
  [ctx [x0 y0] slope lo hi]
  (let [at (fn [x] (sh/->scene ctx [x (+ y0 (* slope (- x x0)))]))]
    [(at (- x0 lo)) (at (+ x0 hi))]))

(defmethod sh/->shapes :tangent-line [{:keys [x0 half] :as o} ctx]
  (let [y (calc/f-at o :f x0)
        [p q] (line-through ctx [x0 y] (calc/df-at o :f x0) half half)]
    [(sh/line (sh/sid o "line") p q {:color (:color o) :width 4})
     (sh/dot (sh/sid o "point") (sh/->scene ctx [x0 y]) {:color (:color o) :radius 0.08})]))

(defn- secant-shapes [o ctx x0 h]
  (let [x1 (+ x0 h) y0 (calc/f-at o :f x0) y1 (calc/f-at o :f x1)
        m  (/ (- y1 y0) h)
        [p q] (line-through ctx [x0 y0] m (+ 0.8 (max 0 (- h))) (+ 0.8 (max 0 h)))]
    [(sh/line (sh/sid o "line") p q {:color (:color o) :width 3})
     (sh/line (sh/sid o "run") (sh/->scene ctx [x0 y0]) (sh/->scene ctx [x1 y0])
              {:color :grey :width 2 :dash [0.08 0.06]})
     (sh/line (sh/sid o "rise") (sh/->scene ctx [x1 y0]) (sh/->scene ctx [x1 y1])
              {:color :grey :width 2 :dash [0.08 0.06]})
     (sh/dot (sh/sid o "p0") (sh/->scene ctx [x0 y0]) {:color (:color o)})
     (sh/dot (sh/sid o "p1") (sh/->scene ctx [x1 y1]) {:color (:color o)})]))

(defmethod sh/->shapes :secant [{:keys [x0 h] :as o} ctx]
  (secant-shapes o ctx (double x0) (double h)))

(defmethod sh/->shapes :secant->tangent [{:keys [x0] :as o} ctx]
  (let [x0 (double x0) y (calc/f-at o :f x0)
        [p q] (line-through ctx [x0 y] (calc/df-at o :f x0) 1.4 1.4)]
    (into [(sh/line (sh/sid o "tangent") p q {:color :red :width 2 :dash [0.12 0.08]})]
          (secant-shapes o ctx x0 (calc/approach-h o)))))

(defn- cumulative
  "[[t A(t)] ...] of the accumulation of slot :f from a to x, trapezoid on
   `n` steps (the trace; the exact value comes through the numeric port)."
  [o a x n]
  (let [xs (vec (sh/sample-xs a x n))]
    (reductions (fn [[t s] t2] [t2 (+ s (* 0.5 (- t2 t) (+ (calc/f-at o :f t) (calc/f-at o :f t2))))])
                [(first xs) 0.0] (rest xs))))

(defmethod sh/->shapes :accumulation [{:keys [a x] :as o} ctx]
  (let [a (double a) x (double x)]
    (if (<= x (+ a 1e-9))
      []
      (let [tr (cumulative o a x 120)
            [_ ax] (last tr)]
        [(sh/path (sh/sid o "region") (region ctx (assoc o :b x) :f nil a x)
                  {:closed? true :color :teal :width 1 :fill :teal :fill-opacity 0.35})
         (sh/path (sh/sid o "A") (map #(sh/->scene ctx %) tr) {:color (:color o) :width 5})
         (sh/line (sh/sid o "drop") (sh/->scene ctx [x 0]) (sh/->scene ctx [x ax])
                  {:color (:color o) :width 2 :dash [0.08 0.06]})
         (sh/dot (sh/sid o "Ax") (sh/->scene ctx [x ax]) {:color (:color o) :radius 0.09})]))))

(defn- grid [[a b] step]
  (let [s (double step)]
    (for [i (range (inc (long (Math/floor (/ (- b a) s)))))] (+ (double a) (* i s)))))

(defn- field-arrows [o ctx F step]
  (let [{:keys [x-range y-range]} (:view ctx)
        pts (for [x (rest (grid x-range step)) y (rest (grid y-range step))
                  :when (and (< x (second x-range)) (< y (second y-range)))] [x y])
        vs  (map F pts)
        vmax (reduce max 1e-9 (map (fn [[u v]] (Math/sqrt (+ (* u u) (* v v)))) vs))
        k   (/ (* 0.85 step) vmax)]
    (vec (mapcat (fn [[x y] [u v]]
                   (sh/arrow2 (sh/sid o "arrow" x y) (sh/->scene ctx [x y])
                              (sh/->scene ctx [(+ x (* k u)) (+ y (* k v))])
                              {:color (:color o) :width 2 :tip 0.1}))
                 pts vs))))

(defmethod sh/->shapes :vector-field [{:keys [F step] :as o} ctx]
  (field-arrows o ctx F (double step)))

(defmethod sh/->shapes :line-integral [{:keys [F r t-range upto] :as o} ctx]
  (let [[t0 t1] (map double t-range)
        t   (+ t0 (* (double upto) (- t1 t0)))
        all (map r (sh/sample-xs t0 t1 160))
        done (map r (sh/sample-xs t0 t 160))
        [px py] (r t) [u v] (F [px py]) [dx dy] (calc/r-prime r t)
        vl  (max 1e-9 (Math/sqrt (+ (* dx dx) (* dy dy))))
        fl  (max 1e-9 (Math/sqrt (+ (* u u) (* v v))))]
    (into [(sh/path (sh/sid o "path") (map #(sh/->scene ctx %) all) {:color :grey :width 2})
           (sh/path (sh/sid o "done") (map #(sh/->scene ctx %) done) {:color (:color o) :width 6})
           (sh/dot (sh/sid o "point") (sh/->scene ctx [px py]) {:color :white :radius 0.09})]
          (concat (sh/arrow2 (sh/sid o "F") (sh/->scene ctx [px py])
                             (sh/->scene ctx [(+ px (* 0.9 (/ u fl))) (+ py (* 0.9 (/ v fl)))])
                             {:color :orange :width 4})
                  (sh/arrow2 (sh/sid o "T") (sh/->scene ctx [px py])
                             (sh/->scene ctx [(+ px (* 0.7 (/ dx vl))) (+ py (* 0.7 (/ dy vl)))])
                             {:color :white :width 3})))))

(defmethod sh/->shapes :label [{:keys [text at color size] :as o} ctx]
  [(sh/text (sh/sid o) text (sh/->scene ctx at) {:color color :size (or size 26)})])
