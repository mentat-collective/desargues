(ns desargues.objects.calc.shapes
  "TARGET stratum: calculus objects -> player frame shapes.

   `->shapes` is the one multimethod of this target, on the object's :kind:
   (->shapes obj ctx) returns player shapes (desargues.player.frame kinds for
   a :plane view, desargues.player.space kinds for a :space view). ctx is
   {:view view :cs CoordSystem (plane views) :port numeric-port}. The 2D
   kinds live in desargues.objects.calc.shapes2d, the 3D ones in
   .shapes3d; this namespace holds the seam and the small shape builders
   they share. Host-free .cljc."
  (:require [desargues.objects.calc.core :as calc]
            [desargues.objects.coords :as c]))

(defmulti ->shapes
  "Player shapes of resolved calculus object `o` under ctx {:view :cs :port}."
  (fn [o _ctx] (:kind o)))

(defmethod ->shapes :default [o _]
  (throw (ex-info (str "no ->shapes for " (pr-str (:kind o))) {:kind (:kind o)})))

;; ── ctx ──────────────────────────────────────────────────────────────────────

(defn coords
  "The CoordSystem of plane view `view`."
  [{:keys [x-range y-range x-length y-length center]}]
  (c/coord-system {:x-range x-range :y-range y-range :x-length x-length
                   :y-length y-length :center center}))

(defn ctx
  "The rendering context of `view` with numeric `port`."
  [view port]
  (cond-> {:view view :port port}
    (= :plane (:view view)) (assoc :cs (coords view))))

(defn ->scene [{:keys [cs]} p] (c/->scene cs p))

;; ── 2D shape builders (scene units) ──────────────────────────────────────────

(defn sid [o & parts] (apply str (:id o) (map #(str "/" %) parts)))

(defn path
  "A :path shape through scene `pts`."
  [id pts {:keys [closed? color width fill fill-opacity dash] :or {width 3}}]
  (cond-> {:id id :node :path :points (mapv vec pts) :opacity 1.0 :draw 1.0
           :opts {:closed? (boolean closed?)}
           :stroke {:color color :width width}}
    fill (assoc :fill {:color fill :opacity (or fill-opacity 0.45)})
    dash (assoc :dash dash)))

(defn line [id a b {:keys [color width dash] :or {width 3}}]
  (cond-> {:id id :node :line :from (vec a) :to (vec b) :opacity 1.0 :draw 1.0
           :stroke {:color color :width width}}
    dash (assoc :dash dash)))

(defn dot [id at {:keys [color radius] :or {radius 0.07}}]
  {:id id :node :dot :at (vec at) :opacity 1.0 :draw 1.0 :opts {:radius radius :color color}})

(defn text [id s at {:keys [color size anchor] :or {size 24 color :white}}]
  {:id id :node :text :text (str s) :at (vec at) :opacity 1.0 :draw 1.0
   :opts (cond-> {:font-size size :color color} anchor (assoc :anchor anchor))})

(defn arrow2
  "Shaft and filled head from scene `a` to `b`."
  [id a b {:keys [color width tip] :or {width 3 tip 0.16}}]
  (let [[ax ay] a [bx by] b dx (- bx ax) dy (- by ay)
        len (Math/sqrt (+ (* dx dx) (* dy dy)))]
    (if (< len 1e-6)
      []
      (let [ux (/ dx len) uy (/ dy len) t (min tip (* 0.5 len))
            base [(- bx (* t ux)) (- by (* t uy))]
            side [(* -0.5 t uy) (* 0.5 t ux)]]
        [(line (str id "/shaft") a base {:color color :width width})
         (path (str id "/head") [b (mapv + base side) (mapv - base side)]
               {:closed? true :color color :width 1 :fill color :fill-opacity 1.0})]))))

(defn sample-xs
  "n + 1 evenly spaced xs over [a b]."
  [a b n]
  (let [a (double a) b (double b)]
    (map #(+ a (* (- b a) (/ (double %) n))) (range (inc n)))))

(defn graph-points
  "Scene points of y = f (slot `k` of `o`) over [a b], `n` steps, clipped to
   the view's y-range (a run breaks where it leaves the box)."
  ([ctx o k a b] (graph-points ctx o k a b 160))
  ([ctx o k a b n]
   (let [[y0 y1] (get-in ctx [:view :y-range])]
     (for [x (sample-xs a b n)
           :let [y (calc/f-at o k x)]]
       (->scene ctx [x (max (double y0) (min (double y1) y))])))))

;; ── view furniture ───────────────────────────────────────────────────────────

(defn- ticks [[lo hi] step]
  (let [s (double step)]
    (filter #(and (<= lo %) (<= % hi) (> (Math/abs %) 1e-9))
            (map #(* s %) (range (long (Math/ceil (/ lo s))) (inc (long (Math/floor (/ hi s)))))))))

(defn- tick-label [v] (let [r (Math/round (double v))] (if (== r v) (str r) (str v))))

(defn plane-axes
  "Axis lines, ticks and numbers of plane view ctx."
  [{:keys [view] :as ctx}]
  (let [{:keys [x-range y-range]} view
        [xa xb] x-range [ya yb] y-range
        ox (max xa (min xb 0.0)) oy (max ya (min yb 0.0))
        st {:color :grey :width 2}]
    (concat
     [(line "axis/x" (->scene ctx [xa oy]) (->scene ctx [xb oy]) st)
      (line "axis/y" (->scene ctx [ox ya]) (->scene ctx [ox yb]) st)]
     (for [x (ticks x-range (:x-step view 1))]
       (text (str "axis/x/" x) (tick-label x) (mapv + (->scene ctx [x oy]) [0 -0.28])
             {:color :grey :size 18}))
     (for [y (ticks y-range (:y-step view 1))]
       (text (str "axis/y/" y) (tick-label y) (mapv + (->scene ctx [ox y]) [-0.28 0])
             {:color :grey :size 18})))))

(defn space-axes
  "The :axes3 of space view `view`."
  [view]
  {:id "axes" :node :axes3 :opacity 1.0 :draw 1.0 :opts {:range (:range view) :color "#77777F"}})
