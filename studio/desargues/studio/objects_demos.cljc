(ns desargues.studio.objects-demos
  "One parametric demo construct per math object of desargues.objects, each
   (fn [params] (fn [stage] ...)) over the scene facade only, so the same value
   renders an mp4 through Manim or plays live in the stage. Each `*-controls`
   vector is the slider spec for desargues.player/open!.

   The registry `demos` maps a keyword to {:construct :controls :title}, so
   the REPL (or a menu) opens any of them the same way; adding a demo is one
   map entry."
  (:require [desargues.objects :as o]
            [desargues.objects.axes :as ax]
            [desargues.objects.calculus :as calc]
            [desargues.objects.coords :as c]
            [desargues.objects.field :as fd]
            [desargues.objects.plot :as pl]
            [desargues.scene :as s]))

(defn- title! [stage txt]
  (s/play! stage (s/appear (-> (s/tex txt :font-size 40 :color :white) (s/move-to [0 3.5]))
                           :run-time 0.5)))

(def ^:private plane
  (c/coord-system {:x-range [-4 4 1] :y-range [-3 3 1] :x-length 11 :y-length 6.4
                   :center [0 -0.3]}))

;; ── axes / number plane ───────────────────────────────────────────────────

(def axes-controls
  [{:id :x-max :label "x max" :min 1.0 :max 8.0 :value 4.0}
   {:id :y-max :label "y max" :min 1.0 :max 5.0 :value 3.0}])

(defn axes-demo
  "A number plane whose ranges are sliders: the transform re-maps everything."
  [{:keys [x-max y-max] :or {x-max 4.0 y-max 3.0}}]
  (let [cs (c/coord-system {:x-range [(- x-max) x-max 1] :y-range [(- y-max) y-max 1]
                            :x-length 11 :y-length 6.4 :center [0 -0.3]})]
    (fn [stage]
      (title! stage "\\text{Number plane } (x, y) \\mapsto (s_x, s_y)")
      (o/add! stage (ax/number-plane cs {:x-label "x" :y-label "y"}) :run-time 1.5)
      (o/add! stage (calc/point-on-curve cs (constantly 1.0) 2 {:label "(2, 1)" :drops? true}))
      (s/hold! stage 1))))

;; ── plot ──────────────────────────────────────────────────────────────────

(def plot-controls
  [{:id :freq  :label "frequency" :min 0.2 :max 4.0 :value 1.0}
   {:id :amp   :label "amplitude" :min 0.2 :max 2.8 :value 2.0}
   {:id :phase :label "phase"     :min 0.0 :max 6.3 :value 0.0}])

(defn plot-demo
  "y = A sin(k x + phi), adaptively sampled; the sliders set A, k, phi."
  [{:keys [freq amp phase] :or {freq 1.0 amp 2.0 phase 0.0}}]
  (fn [stage]
    (title! stage "y = A \\sin(k x + \\varphi)")
    (o/add! stage (ax/axes plane {:x-label "x" :y-label "y"}))
    (o/add! stage (pl/plot plane #(* amp (Math/sin (+ (* freq %) phase))) {:color :blue})
            :run-time 2.0)
    (o/add! stage (pl/plot plane #(Math/tan %) {:color :maroon :width 2}) :run-time 1.0)
    (s/hold! stage 1)))

;; ── parametric ────────────────────────────────────────────────────────────

(def parametric-controls
  [{:id :a     :label "a (x freq)" :min 1.0 :max 6.0 :value 3.0}
   {:id :b     :label "b (y freq)" :min 1.0 :max 6.0 :value 2.0}
   {:id :delta :label "delta"      :min 0.0 :max 3.2 :value 1.57}])

(defn parametric-demo
  "A Lissajous curve t -> (2.5 sin(a t + delta), 2.5 sin(b t))."
  [{:keys [a b delta] :or {a 3.0 b 2.0 delta 1.57}}]
  (fn [stage]
    (title! stage "(x, y) = (\\sin(a t + \\delta),\\; \\sin(b t))")
    (o/add! stage (ax/axes plane {:labels? false}))
    (o/add! stage (pl/parametric plane (fn [t] [(* 2.5 (Math/sin (+ (* a t) delta)))
                                                (* 2.5 (Math/sin (* b t)))])
                                 [0 (* 2 Math/PI)] {:color :gold})
            :run-time 3.0)
    (s/hold! stage 1)))

;; ── polar ─────────────────────────────────────────────────────────────────

(def polar-controls
  [{:id :petals :label "k (petals)" :min 1.0 :max 8.0 :value 3.0}
   {:id :radius :label "radius"     :min 0.5 :max 2.9 :value 2.5}])

(defn polar-demo
  "A rose r = R cos(k theta)."
  [{:keys [petals radius] :or {petals 3.0 radius 2.5}}]
  (fn [stage]
    (title! stage "r = R \\cos(k \\theta)")
    (o/add! stage (ax/number-plane plane {:labels? false}))
    (o/add! stage (pl/polar plane #(* radius (Math/cos (* petals %))) {:color :pink})
            :run-time 3.0)
    (s/hold! stage 1)))

;; ── vector field ──────────────────────────────────────────────────────────

(def field-controls
  [{:id :curl :label "rotation" :min -1.0 :max 1.0 :value 1.0}
   {:id :div  :label "source"   :min -1.0 :max 1.0 :value 0.2}])

(defn field-demo
  "F(x, y) = c (-y, x) + d (x, y): rotation plus a source; sliders mix them."
  [{:keys [curl div] :or {curl 1.0 div 0.2}}]
  (let [cs (c/coord-system {:x-range [-4 4 0.5] :y-range [-3 3 0.5] :x-length 11
                            :y-length 6.4 :center [0 -0.3]})]
    (fn [stage]
      (title! stage "\\vec F = c\\,(-y, x) + d\\,(x, y)")
      (o/add! stage (ax/axes plane {:labels? false}) :run-time 0.5)
      (o/add! stage (fd/vector-field cs (fn [[x y]] [(+ (* curl (- y)) (* div x))
                                                     (+ (* curl x) (* div y))]))
              :run-time 1.5 :lag-ratio 0.01)
      (s/hold! stage 1))))

;; ── point on curve / tangent / secant ─────────────────────────────────────

(def tangent-controls
  [{:id :x0 :label "x0" :min -2.5 :max 2.5 :value 1.0}
   {:id :h  :label "h (secant)" :min 0.05 :max 2.0 :value 1.0}])

(defn tangent-demo
  "f(x) = x^3/3 - x: a point at x0 with its tangent and the secant to x0 + h.
   The construct also sweeps x0 itself, so the recording shows motion; the
   sliders set where the sweep ends and the secant's h."
  [{:keys [x0 h] :or {x0 1.0 h 1.0}}]
  (let [f  #(- (/ (* % % %) 3.0) %)
        cs plane]
    (fn [stage]
      (title! stage "f(x) = \\tfrac{x^3}{3} - x,\\quad f'(x_0)")
      (o/add! stage (ax/axes cs {:x-label "x" :y-label "y"}))
      (o/add! stage (pl/plot cs f {:domain [-3 3]}) :run-time 1.5)
      (let [p (o/add! stage (calc/point-on-curve cs f -2.0 {:drops? true}))
            t (o/add! stage (calc/tangent cs f -2.0 {:point? false}))
            xs (map #(+ -2.0 (* (- x0 -2.0) (/ % 12.0))) (range 1 13))]
        (reduce (fn [[p t] x]
                  [(o/update! stage p (calc/point-on-curve cs f x {:drops? true}) :run-time 0.15)
                   (o/update! stage t (calc/tangent cs f x {:point? false}) :run-time 0.15)])
                [p t] xs))
      (o/add! stage (calc/secant cs f x0 h))
      (s/hold! stage 1))))

;; ── Riemann rectangles and area ───────────────────────────────────────────

(def riemann-controls
  [{:id :n    :label "n rectangles" :min 1.0 :max 40.0 :value 8.0}
   {:id :rule :label "rule 0 left 1 mid 2 right" :min 0.0 :max 2.0 :value 0.0}])

(defn riemann-demo
  "Riemann rectangles of f(x) = 1.2 + sin(2x) over [-3, 3] with n slider
   rectangles, refined to 2n and 4n in place, then the exact area."
  [{:keys [n rule] :or {n 8 rule 0}}]
  (let [f    #(+ 1.2 (Math/sin (* 2 %)))
        n    (max 1 (long (Math/round (double n))))
        rule ([:left :midpoint :right] (min 2 (max 0 (long (Math/round (double rule))))))
        cs   plane
        spec #(calc/riemann cs f -3 3 % {:rule rule})]
    (fn [stage]
      (title! stage "\\sum_i f(x_i^*)\\,\\Delta x \\to \\int_{-3}^{3} f\\,dx")
      (o/add! stage (ax/axes cs {:x-label "x"}))
      (o/add! stage (pl/plot cs f {:domain [-3.5 3.5]}))
      (let [r (o/add! stage (spec n) :lag-ratio 0.05)]
        (o/sweep! stage r spec [(* 2 n) (* 4 n)] :run-time 0.8))
      (o/add! stage (calc/area cs f -3 3 {:color :teal}))
      (s/hold! stage 1))))

;; ── TeX labels ────────────────────────────────────────────────────────────

(def tex-controls
  [{:id :size :label "font size" :min 20.0 :max 90.0 :value 48.0}])

(defn tex-demo
  "TeX labels: typeset by Manim in the mp4, by JLaTeXMath in the JavaFX stage."
  [{:keys [size] :or {size 48.0}}]
  (fn [stage]
    (title! stage "\\text{TeX labels}")
    (let [ls (map-indexed (fn [i src] (-> (s/tex src :font-size size :color ([:gold :teal :blue :pink] i))
                                          (s/move-to [0 (- 1.8 (* 1.4 i))])))
                          ["e^{i\\pi} + 1 = 0"
                           "\\int_0^1 x^2\\,dx = \\frac{1}{3}"
                           "\\nabla \\times \\vec B = \\mu_0 \\vec J"
                           "\\sum_{n=1}^{\\infty} \\frac{1}{n^2} = \\frac{\\pi^2}{6}"])]
      (s/play! stage (s/stagger (mapv #(s/appear % :run-time 0.8) ls) :lag-ratio 0.5))
      (s/hold! stage 1))))

;; ── registry ──────────────────────────────────────────────────────────────

(def demos
  {:axes       {:title "number plane"        :construct axes-demo       :controls axes-controls}
   :plot       {:title "plot"                :construct plot-demo       :controls plot-controls}
   :parametric {:title "parametric"          :construct parametric-demo :controls parametric-controls}
   :polar      {:title "polar"               :construct polar-demo      :controls polar-controls}
   :field      {:title "vector field"        :construct field-demo      :controls field-controls}
   :tangent    {:title "tangent and secant"  :construct tangent-demo    :controls tangent-controls}
   :riemann    {:title "Riemann sums"        :construct riemann-demo    :controls riemann-controls}
   :tex        {:title "TeX labels"          :construct tex-demo        :controls tex-controls}})

(defn defaults
  "The default params of demo `k`, read off its slider specs."
  [k]
  (into {} (map (juxt :id :value)) (get-in demos [k :controls])))
