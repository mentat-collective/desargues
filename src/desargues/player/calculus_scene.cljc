(ns desargues.player.calculus-scene
  "PURE stratum of the partial-derivatives demo: the 3D picture of a surface
   z = f(x, y) at a point P = (x0, y0, f(x0, y0)), as shapes of
   desargues.player.space and desargues.player.space-extras.

   Everything here is geometry from a surface {:f :fx :fy} (numeric fns of
   x and y, as desargues.player.calculus-surfaces gives them) and a point:

     tangent plane   P + s (1, 0, f_x) + t (0, 1, f_y): it contains P and its
                     normal is (-f_x, -f_y, 1)
     x-slice         the curve y = y0 on the surface, t -> (t, y0, f(t, y0)),
                     and its tangent line at P, direction (1, 0, f_x)
     y-slice         x = x0, direction (0, 1, f_y)
     gradient        (f_x, f_y) drawn on the floor from below P
     level curves    marching squares (desargues.player.contour) on the floor
                     and on the surface

   `shapes` assembles one frame's shapes from a scene map; how far each part
   is drawn comes from its :reveal map, which desargues.player.calculus-demos
   animates over time. Host-free .cljc."
  (:require [desargues.player.contour :as contour]
            [desargues.player.space :as space]
            [desargues.player.space-extras :as sx]))

(def domain
  "The interval [-2 2]; every surface is drawn over domain x domain."
  [-2.0 2.0])

(defn clamp-domain
  "`v` clamped into the domain."
  [v]
  (-> (double v) (max (first domain)) (min (second domain))))

;; ── Pure geometry ─────────────────────────────────────────────────────────

(defn point-on
  "P = [x y f(x, y)] on surface `surf`."
  [{:keys [f]} [x y]]
  [(double x) (double y) (double (f x y))])

(defn gradient
  "[f_x f_y] of `surf` at [x y]."
  [{:keys [fx fy]} [x y]]
  [(double (fx x y)) (double (fy x y))])

(defn tangent-span
  "The spanning vectors [u v] of the tangent plane of `surf` at [x y]:
   u = (1, 0, f_x), v = (0, 1, f_y)."
  [surf xy]
  (let [[gx gy] (gradient surf xy)]
    [[1.0 0.0 gx] [0.0 1.0 gy]]))

(defn normal
  "The upward unit normal of `surf` at [x y]: (-f_x, -f_y, 1) normalised."
  [surf xy]
  (let [[gx gy] (gradient surf xy)]
    (space/unit [(- gx) (- gy) 1.0])))

(defn x-slice
  "Points of the curve y = `y0` on `surf` over the domain, `n` steps."
  [{:keys [f]} y0 n]
  (space/parametric-points (fn [t] [t y0 (f t y0)]) domain n))

(defn y-slice
  "Points of the curve x = `x0` on `surf` over the domain, `n` steps."
  [{:keys [f]} x0 n]
  (space/parametric-points (fn [t] [x0 t (f x0 t)]) domain n))

(defn tangent-line
  "The segment [P - h d, P + h d] of the tangent line through P with unit
   direction d along (1, 0, f_x) for axis :x or (0, 1, f_y) for :y."
  [surf xy axis h]
  (let [p     (point-on surf xy)
        [u v] (tangent-span surf xy)
        d     (space/unit (if (= axis :x) u v))]
    [(space/v- p (space/v* h d)) (space/v+ p (space/v* h d))]))

(defn ascent-path
  "Points [x y] of gradient ascent on `surf` from [x y]: up to `n` steps of
   size `eta`, stopping at the domain's edge or where the gradient vanishes."
  [surf xy {:keys [n eta] :or {n 40 eta 0.12}}]
  (loop [path [(mapv double xy)] k 0]
    (let [[x y]   (peek path)
          [gx gy] (gradient surf [x y])
          nx (+ x (* eta gx)) ny (+ y (* eta gy))]
      (if (or (>= k n) (< (+ (* gx gx) (* gy gy)) 1e-8)
              (not= nx (clamp-domain nx)) (not= ny (clamp-domain ny)))
        path
        (recur (conj path [nx ny]) (inc k))))))

(defn along
  "The point at fraction `u` (0..1) of polyline `pts`, by arc index."
  [pts u]
  (let [n (dec (count pts))]
    (if (<= n 0)
      (first pts)
      (let [s (* (max 0.0 (min 1.0 (double u))) n)
            i (min (dec n) (long (Math/floor s)))
            w (- s i)
            [ax ay] (nth pts i) [bx by] (nth pts (inc i))]
        [(+ ax (* w (- bx ax))) (+ ay (* w (- by ay)))]))))

(defn z-range
  "[zmin zmax] of `surf` over the domain, sampled on a `cells` grid."
  [{:keys [f]} cells]
  (contour/z-range (contour/sample f domain domain cells)))

(defn level-lines
  "The level curves of `surf` (9 levels, one marching-squares pass) as
   {:floor lines :surface lines}: each curve at height `floor`, and at its
   own level (on the surface, lifted a hair so it is not hidden by it)."
  [{:keys [f]} floor]
  (let [g (contour/sample f domain domain 40)
        curves (for [c (contour/levels g 9) line (contour/lines g c)] [c line])
        lift   (fn [z-of] (vec (for [[c line] curves] (mapv (fn [[x y]] [x y (z-of c)]) line))))]
    {:floor (lift (constantly (double floor))) :surface (lift #(+ (double %) 0.01))}))

;; ── Shapes ────────────────────────────────────────────────────────────────

(defn- shown [s a] (assoc s :opacity (max 0.0 (min 1.0 (double a)))))

(defn- polyline [id pts color width]
  {:id id :node :curve3 :points (vec pts) :stroke {:color color :width width}
   :opacity 1.0 :draw 1.0})

(defn- floor-frame [z]
  (let [[a b] domain]
    (sx/contours [[[a a z] [b a z] [b b z] [a b z] [a a z]]]
                 {:id "floor" :color "#4A4A55" :width 2})))

(defn- slice-shapes
  "The slicing plane while it sweeps, the slice curve and its tangent line."
  [surf {:keys [axis at sweep tangent color zmid zhalf]} xy]
  (let [k      (name axis)
        pts    (if (= axis :x) (x-slice surf at 80) (y-slice surf at 80))
        [a b]  (tangent-line surf xy axis 1.1)]
    (cond-> [(shown (polyline (str k "-slice") pts color 5) (* 3.0 sweep))]
      (< 0.0 sweep 1.0)
      (conj (shown (sx/plane (if (= axis :x) [0.0 at zmid] [at 0.0 zmid])
                             {:id (str k "-slice-plane") :u (if (= axis :x) [1 0 0] [0 1 0])
                              :v [0 0 1] :size [2.0 zhalf] :color color :opacity 0.2})
                   (min 1.0 (* 6.0 (- 1.0 sweep)))))
      (and (>= sweep 1.0) (pos? tangent))
      (conj (shown (polyline (str k "-tangent") [a b] color 6) tangent)))))

(defn shapes
  "The 3D shapes of one frame. `scene`:
     {:surf {:f :fx :fy}   the surface (numeric)
      :point [x y]         where P is now
      :floor z  :zmax z    the floor height and the surface's top
      :levels {:floor :surface}   level curves (`level-lines`), computed
                           here when absent
      :show {:tangent :xslice :yslice :gradient :levels :wire}   toggles
      :slices {:x y-at :y x-at}   where each slice is drawn (a sweep moves it)
      :reveal {:axes :surface :levels :xsweep :xtan :ysweep :ytan :plane :grad}
              each 0..1: how far that part has appeared (missing = 1)}"
  [{:keys [surf point floor zmax levels show slices reveal]}]
  (let [{:keys [f]} surf
        r       (fn [k] (double (get reveal k 1.0)))
        p       (point-on surf point)
        [x y]   point
        q       [x y floor]
        [gx gy] (gradient surf point)
        zmid    (/ (+ floor zmax) 2.0)
        zhalf   (max 0.5 (/ (- zmax floor) 2.0))
        [u v]   (tangent-span surf point)
        glen    (Math/sqrt (+ (* gx gx) (* gy gy)))
        gk      (if (> glen 1.6) (/ 1.6 glen) 1.0)
        tip     [(+ x (* gk gx)) (+ y (* gk gy)) floor]]
    (cond-> [(shown {:id "axes" :node :axes3 :draw 1.0
                     :opts {:range [[-2.6 2.6] [-2.6 2.6] [floor (+ zmax 0.6)]] :color "#77777F"}}
                    (r :axes))
             (shown (floor-frame floor) (r :axes))
             (assoc (space/surface (fn [a b] [a b (f a b)]) domain domain
                                   (cond-> {:id "surface" :nu 24 :nv 24 :color :blue :opacity 0.82}
                                     (:wire show) (assoc :wire "#2A5F70")))
                    :opacity (min 1.0 (* 2.0 (r :surface))) :draw (r :surface))]

      (:levels show)
      (into (let [ls (or levels (level-lines surf floor))]
              [(shown (assoc (sx/contours (:floor ls) {:id "levels-floor" :color :gold :width 2})
                             :draw (r :levels))
                      (r :levels))
               (shown (sx/contours (:surface ls) {:id "levels-surface" :color "#E8E8F0" :width 1.5})
                      (* 0.55 (r :levels)))]))

      (:xslice show)
      (into (slice-shapes surf {:axis :x :at (get slices :x y) :sweep (r :xsweep) :tangent (r :xtan)
                                :color :red :zmid zmid :zhalf zhalf} point))

      (:yslice show)
      (into (slice-shapes surf {:axis :y :at (get slices :y x) :sweep (r :ysweep) :tangent (r :ytan)
                                :color :green :zmid zmid :zhalf zhalf} point))

      (:tangent show)
      (into [(shown (sx/plane p {:id "tangent-plane" :u u :v v :size 0.9 :color :gold
                                 :opacity 0.5 :wire "#9C7A3C"})
                    (r :plane))
             (shown (sx/arrow p (space/v+ p (space/v* 0.9 (normal surf point)))
                              {:id "normal" :color :white :width 3 :head 0.18})
                    (r :plane))])

      (:gradient show)
      (into [(shown (assoc (sx/arrow q tip {:id "gradient" :color :orange :width 6 :head 0.26})
                           :draw (r :grad))
                    (* 3.0 (r :grad)))
             (shown (sx/label (space/v+ tip [0.1 0.1 0.0]) "grad f"
                              {:id "gradient-label" :font-size 24 :color :orange})
                    (r :grad))])

      true
      (into [(polyline "drop" [p q] "#9A9AA6" 2)
             {:id "point" :node :points3 :points [p q] :opacity 1.0 :draw 1.0
              :opts {:names ["P" "Q"] :radius 0.1 :color :yellow}}
             (sx/label (space/v+ p [0.12 0.12 0.2]) "P"
                       {:id "label-P" :font-size 30 :color :yellow})]))))
