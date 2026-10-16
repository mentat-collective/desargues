(ns desargues.calc-emmy
  "TARGET stratum (JVM, :notebooks alias): calculus figures -> emmy-viewers
   fragments (Mafs for :plane views, MathBox for :space views, Leva for the
   params), for Clerk notebooks.

   `->emmy-viewers` is the one multimethod of this target, on :kind:
   (->emmy-viewers obj ctx) returns Reagent fragments, ctx {:params p :atom
   sym}. Function slots naming a catalog entry become EMMY functions of its
   :body (desargues.objects.calc.symbolic/emmy-fn), so the viewers compile
   them for the page and Emmy's D differentiates them there: a tangent whose
   x0 is a param ref is drawn LIVE from the Leva atom with ev/with-params,
   f(x0) + f'(x0) (x - x0). Regions and pieces are polygons computed on the
   JVM through the same core the player uses.

     (figure->mafs demos/riemann-figure {:n 12.0})
     (figure->viewer demos/tangent-figure controls)   ; Mafs + live Leva panel"
  (:require [desargues.objects.calc.core :as c]
            [desargues.objects.calc.functions :as fns]
            [desargues.objects.calc.symbolic :as sym]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.mathbox.plot :as plot]
            [emmy.viewer :as ev]))

(defn emmy-f
  "An Emmy function for function slot `k` of `obj` at `params`: the catalog
   entry's :body through Emmy, else the numeric fn."
  [obj k params]
  (let [{:keys [body f]} (fns/as-fn (c/resolve-params (get obj k) params))]
    (if body (sym/emmy-fn body) f)))

(defn- color [o] (name (:color o :blue)))
(defn- res [o {:keys [params]}] (c/resolve-params o params))

(defmulti ->emmy-viewers
  "Emmy-viewers fragments of calculus object `o` (param refs kept) under ctx
   {:params :atom}."
  (fn [o _ctx] (:kind o)))

(defmethod ->emmy-viewers :default [_ _] nil)

(defmethod ->emmy-viewers :graph [o ctx]
  (mafs/of-x (emmy-f o :f (:params ctx)) {:color (color o)}))

(defn- region-points [o top bottom a x]
  (let [a (double a) x (double x)
        xs (map #(+ a (* (- x a) (/ % 60.0))) (range 61))
        y  (fn [k t] (if k (c/f-at o k t) 0.0))]
    (vec (concat (map (fn [t] [t (y top t)]) xs) (map (fn [t] [t (y bottom t)]) (reverse xs))))))

(defn- upto-x [{:keys [a b upto]}] (+ (double a) (* (double (or upto 1.0)) (- (double b) (double a)))))

(defmethod ->emmy-viewers :area-under [o ctx]
  (let [o (res o ctx)]
    (mafs/polygon (region-points o :f nil (:a o) (upto-x o))
                  {:color (color o) :fill-opacity 0.4 :weight 1})))

(defmethod ->emmy-viewers :area-between [o ctx]
  (let [o (res o ctx)]
    (mafs/polygon (region-points o :f :g (:a o) (upto-x o))
                  {:color (color o) :fill-opacity 0.4 :weight 1})))

(defmethod ->emmy-viewers :riemann [o ctx]
  (let [o (res o ctx)]
    (into [:<>]
          (for [{:keys [l r hl hr h]} (c/riemann-pieces o)
                :let [[tl tr] (if (= :trapezoid (:rule o)) [hl hr] [h h])]]
            (mafs/polygon [[l 0] [r 0] [r tr] [l tl]]
                          {:color (color o) :fill-opacity 0.45 :weight 1})))))

(defmethod ->emmy-viewers :tangent-line [o {:keys [params atom] :as ctx}]
  (let [f (emmy-f o :f params) x0-ref (:x0 o)]
    (if (and atom (c/param-ref? x0-ref))
      [:<>
       (mafs/of-x (ev/with-params {:atom atom :params [(:calc/param x0-ref)]}
                    (fn [x0] (fn [x] (e/+ (f x0) (e/* ((e/D f) x0) (e/- x x0))))))
                  {:color (color o)})]
      (let [o (res o ctx) x0 (double (:x0 o))]
        (mafs/point-slope {:point [x0 (c/f-at o :f x0)] :slope (c/df-at o :f x0)
                           :color (color o)})))))

(defmethod ->emmy-viewers :secant [o ctx]
  (let [{:keys [x0 h] :as o} (res o ctx)]
    (mafs/through-points {:point1 [x0 (c/f-at o :f x0)]
                          :point2 [(+ x0 h) (c/f-at o :f (+ x0 h))] :color (color o)})))

(defmethod ->emmy-viewers :secant->tangent [o ctx]
  (let [o (res o ctx)]
    (->emmy-viewers (assoc o :kind :secant :h (c/approach-h o)) {})))

(defmethod ->emmy-viewers :accumulation [o {:keys [params] :as ctx}]
  (let [o' (res o ctx) f (emmy-f o :f params) a (double (:a o')) x (double (:x o'))]
    [:<>
     (mafs/polygon (region-points (assoc o' :b x) :f nil a x)
                   {:color "teal" :fill-opacity 0.3 :weight 1})
     ;; A(t), integral of f from a to t: Emmy's quadrature on the JVM
     (mafs/polyline (vec (for [i (range 41) :let [t (+ a (* (- x a) (/ i 40.0)))]]
                           [t (if (== t a) 0.0 (e/definite-integral f a t))]))
                    {:color (color o) :weight 4})]))

(defmethod ->emmy-viewers :vector-field [o {:keys [view]}]
  ;; F is a numeric fn (any Clojure code), not an Emmy one, so the arrows are
  ;; sampled on the JVM and drawn as Mafs vectors.
  (let [F (:F o) s (double (:step o))
        [x0 x1] (:x-range view [-3 3]) [y0 y1] (:y-range view [-3 3])
        pts (for [x (range (+ x0 s) x1 s) y (range (+ y0 s) y1 s)] [x y])
        vs  (map F pts)
        vmax (reduce max 1e-9 (map (fn [[u v]] (Math/sqrt (+ (* u u) (* v v)))) vs))
        k   (/ (* 0.85 s) vmax)]
    (into [:<>] (map (fn [[x y] [u v]] (mafs/vector {:tail [x y] :tip [(+ x (* k u)) (+ y (* k v))]
                                                     :color (color o)}))
                     pts vs))))

(defmethod ->emmy-viewers :line-integral [o ctx]
  (let [{:keys [r t-range upto] :as o} (res o ctx)
        [t0 t1] t-range t (+ t0 (* upto (- t1 t0)))
        pts (fn [ta tb] (vec (for [i (range 121)] (r (+ ta (* (- tb ta) (/ i 120.0)))))))]
    [:<>
     (mafs/polyline (pts t0 t1) {:color "gray"})
     (mafs/polyline (pts t0 t) {:color (color o) :weight 5})
     (mafs/point (r t) {:color "white"})]))

;; ── 3D: MathBox ──────────────────────────────────────────────────────────────

(defmethod ->emmy-viewers :revolution [o {:keys [params] :as ctx}]
  (let [{:keys [a b method sweep] :as o'} (res o ctx)
        f    (emmy-f o :f params)
        turn (* 2 Math/PI (double sweep))]
    (plot/parametric-surface
     {:f (if (= :shell method)
           (fn [[u th]] [(e/* u (e/cos th)) (e/* u (e/sin th)) (f u)])
           (fn [[u th]] [u (e/* (f u) (e/cos th)) (e/* (f u) (e/sin th))]))
      :u [a b] :v [0 turn] :color (color o') :opacity 0.6 :grid-u 8 :grid-v 16})))

(defmethod ->emmy-viewers :double-integral [o ctx]
  (let [{:keys [f] :as o'} (res o ctx)]
    (into [:<>]
          (for [{:keys [x0 x1 y0 y1 h]} (c/column-cells o')
                [p q] [[[x0 y0 h] [x1 y0 h]] [[x1 y0 h] [x1 y1 h]] [[x1 y1 h] [x0 y1 h]]
                       [[x0 y1 h] [x0 y0 h]] [[x0 y0 0] [x0 y0 h]] [[x1 y1 0] [x1 y1 h]]]]
            (plot/line {:coords [p q] :color (color o') :width 2})))))

;; ── figures ──────────────────────────────────────────────────────────────────

(defn leva-schema
  "A Leva schema for the numeric slider controls: {id {:min :max :step}}."
  [controls]
  (into {} (for [{:keys [id min max kind]} controls :when (and (nil? kind) min)]
             [id {:min min :max max :step (/ (- max min) 100.0)}])))

(defn figure->mafs
  "One Mafs scene of plane figure `fig` at `params`."
  ([fig] (figure->mafs fig {}))
  ([fig params]
   (let [ps (merge (:params fig) params) {:keys [x-range y-range]} (:view fig)]
     (apply mafs/mafs {:view-box {:x x-range :y y-range} :height 360}
            (mafs/cartesian)
            (keep #(->emmy-viewers % {:params ps :view (:view fig)}) (:objects fig))))))

(defn figure->mathbox
  "One MathBox scene of space figure `fig` at `params`."
  ([fig] (figure->mathbox fig {}))
  ([fig params]
   (let [ps (merge (:params fig) params)]
     (apply plot/scene {:range (get-in fig [:view :range])}
            (keep #(->emmy-viewers % {:params ps}) (:objects fig))))))

(defn figure->viewer
  "Plane figure `fig` as Mafs with a live Leva panel over its numeric params
   (`controls`, demo control specs): objects that can draw live follow the
   client-side atom; the rest is drawn at `params`."
  ([fig controls] (figure->viewer fig {} controls))
  ([fig params controls]
   (let [schema (leva-schema controls)
         ps     (merge (:params fig) params)
         {:keys [x-range y-range]} (:view fig)]
     (ev/with-state (select-keys ps (keys schema))
       (fn [!p]
         [:<>
          (leva/controls {:atom !p :schema schema})
          (apply mafs/mafs {:view-box {:x x-range :y y-range} :height 360}
                 (mafs/cartesian)
                 (keep #(->emmy-viewers % {:params ps :atom !p :view (:view fig)})
                       (:objects fig)))])))))
