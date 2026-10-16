(ns desargues.browser.demos
  "The eight object demos of studio/desargues/studio/objects_demos.clj, as
   host-free .cljc over desargues.browser.stage, so a browser re-records one
   on every slider move with no JVM. Same constructs, same slider specs, same
   registry shape {:title :construct :controls}; adding a demo is one map
   entry. A construct is (fn [params] (fn [stage] ...))."
  (:require [desargues.browser.stage :as st]
            [desargues.objects.axes :as ax]
            [desargues.objects.calculus :as calc]
            [desargues.objects.coords :as c]
            [desargues.objects.field :as fd]
            [desargues.objects.plot :as pl]))

(defn- title! [stage txt]
  (st/play! stage (st/appear (st/tex stage txt [0 3.5] {:font-size 40 :color :white})
                             :run-time 0.5)))

(def ^:private plane
  (c/coord-system {:x-range [-4 4 1] :y-range [-3 3 1] :x-length 11 :y-length 6.4
                   :center [0 -0.3]}))

(defn- sin [x] (Math/sin x))
(defn- cos [x] (Math/cos x))
(def ^:private tau (* 2 Math/PI))

(defn- round [x] (Math/round (double x)))

;; ── axes / number plane ───────────────────────────────────────────────────

(def axes-controls
  [{:id :x-max :label "x max" :min 1.0 :max 8.0 :value 4.0}
   {:id :y-max :label "y max" :min 1.0 :max 5.0 :value 3.0}])

(defn axes-demo
  [{:keys [x-max y-max] :or {x-max 4.0 y-max 3.0}}]
  (let [cs (c/coord-system {:x-range [(- x-max) x-max 1] :y-range [(- y-max) y-max 1]
                            :x-length 11 :y-length 6.4 :center [0 -0.3]})]
    (fn [stage]
      (title! stage "\\text{Number plane } (x, y) \\mapsto (s_x, s_y)")
      (st/add! stage (ax/number-plane cs {:x-label "x" :y-label "y"}) :run-time 1.5)
      (st/add! stage (calc/point-on-curve cs (constantly 1.0) 2 {:label "(2, 1)" :drops? true}))
      (st/hold! stage 1))))

;; ── plot ──────────────────────────────────────────────────────────────────

(def plot-controls
  [{:id :freq  :label "frequency" :min 0.2 :max 4.0 :value 1.0}
   {:id :amp   :label "amplitude" :min 0.2 :max 2.8 :value 2.0}
   {:id :phase :label "phase"     :min 0.0 :max 6.3 :value 0.0}])

(defn plot-demo
  [{:keys [freq amp phase] :or {freq 1.0 amp 2.0 phase 0.0}}]
  (fn [stage]
    (title! stage "y = A \\sin(k x + \\varphi)")
    (st/add! stage (ax/axes plane {:x-label "x" :y-label "y"}))
    (st/add! stage (pl/plot plane #(* amp (sin (+ (* freq %) phase))) {:color :blue})
             :run-time 2.0)
    (st/add! stage (pl/plot plane #(Math/tan %) {:color :maroon :width 2}) :run-time 1.0)
    (st/hold! stage 1)))

;; ── parametric ────────────────────────────────────────────────────────────

(def parametric-controls
  [{:id :a     :label "a (x freq)" :min 1.0 :max 6.0 :value 3.0}
   {:id :b     :label "b (y freq)" :min 1.0 :max 6.0 :value 2.0}
   {:id :delta :label "delta"      :min 0.0 :max 3.2 :value 1.57}])

(defn parametric-demo
  [{:keys [a b delta] :or {a 3.0 b 2.0 delta 1.57}}]
  (fn [stage]
    (title! stage "(x, y) = (\\sin(a t + \\delta),\\; \\sin(b t))")
    (st/add! stage (ax/axes plane {:labels? false}))
    (st/add! stage (pl/parametric plane (fn [t] [(* 2.5 (sin (+ (* a t) delta)))
                                                 (* 2.5 (sin (* b t)))])
                                  [0 tau] {:color :gold})
             :run-time 3.0)
    (st/hold! stage 1)))

;; ── polar ─────────────────────────────────────────────────────────────────

(def polar-controls
  [{:id :petals :label "k (petals)" :min 1.0 :max 8.0 :value 3.0}
   {:id :radius :label "radius"     :min 0.5 :max 2.9 :value 2.5}])

(defn polar-demo
  [{:keys [petals radius] :or {petals 3.0 radius 2.5}}]
  (fn [stage]
    (title! stage "r = R \\cos(k \\theta)")
    (st/add! stage (ax/number-plane plane {:labels? false}))
    (st/add! stage (pl/polar plane #(* radius (cos (* petals %))) {:color :pink})
             :run-time 3.0)
    (st/hold! stage 1)))

;; ── vector field ──────────────────────────────────────────────────────────

(def field-controls
  [{:id :curl :label "rotation" :min -1.0 :max 1.0 :value 1.0}
   {:id :div  :label "source"   :min -1.0 :max 1.0 :value 0.2}])

(defn field-demo
  [{:keys [curl div] :or {curl 1.0 div 0.2}}]
  (let [cs (c/coord-system {:x-range [-4 4 0.5] :y-range [-3 3 0.5] :x-length 11
                            :y-length 6.4 :center [0 -0.3]})]
    (fn [stage]
      (title! stage "\\vec F = c\\,(-y, x) + d\\,(x, y)")
      (st/add! stage (ax/axes plane {:labels? false}) :run-time 0.5)
      (st/add! stage (fd/vector-field cs (fn [[x y]] [(+ (* curl (- y)) (* div x))
                                                      (+ (* curl x) (* div y))]))
               :run-time 1.5 :lag-ratio 0.01)
      (st/hold! stage 1))))

;; ── point on curve / tangent / secant ─────────────────────────────────────

(def tangent-controls
  [{:id :x0 :label "x0" :min -2.5 :max 2.5 :value 1.0}
   {:id :h  :label "h (secant)" :min 0.05 :max 2.0 :value 1.0}])

(defn tangent-demo
  [{:keys [x0 h] :or {x0 1.0 h 1.0}}]
  (let [f  #(- (/ (* % % %) 3.0) %)
        cs plane]
    (fn [stage]
      (title! stage "f(x) = \\tfrac{x^3}{3} - x,\\quad f'(x_0)")
      (st/add! stage (ax/axes cs {:x-label "x" :y-label "y"}))
      (st/add! stage (pl/plot cs f {:domain [-3 3]}) :run-time 1.5)
      (let [p  (st/add! stage (calc/point-on-curve cs f -2.0 {:drops? true}))
            t  (st/add! stage (calc/tangent cs f -2.0 {:point? false}))
            xs (map #(+ -2.0 (* (- x0 -2.0) (/ % 12.0))) (range 1 13))]
        (reduce (fn [[p t] x]
                  [(st/update! stage p (calc/point-on-curve cs f x {:drops? true}) :run-time 0.15)
                   (st/update! stage t (calc/tangent cs f x {:point? false}) :run-time 0.15)])
                [p t] xs))
      (st/add! stage (calc/secant cs f x0 h))
      (st/hold! stage 1))))

;; ── Riemann rectangles and area ───────────────────────────────────────────

(def riemann-controls
  [{:id :n    :label "n rectangles" :min 1.0 :max 40.0 :value 8.0}
   {:id :rule :label "rule 0 left 1 mid 2 right" :min 0.0 :max 2.0 :value 0.0}])

(defn riemann-demo
  [{:keys [n rule] :or {n 8 rule 0}}]
  (let [f    #(+ 1.2 (sin (* 2 %)))
        n    (max 1 (round n))
        rule ([:left :midpoint :right] (min 2 (max 0 (round rule))))
        cs   plane
        spec #(calc/riemann cs f -3 3 % {:rule rule})]
    (fn [stage]
      (title! stage "\\sum_i f(x_i^*)\\,\\Delta x \\to \\int_{-3}^{3} f\\,dx")
      (st/add! stage (ax/axes cs {:x-label "x"}))
      (st/add! stage (pl/plot cs f {:domain [-3.5 3.5]}))
      (let [r (st/add! stage (spec n) :lag-ratio 0.05)]
        (st/sweep! stage r spec [(* 2 n) (* 4 n)] :run-time 0.8))
      (st/add! stage (calc/area cs f -3 3 {:color :teal}))
      (st/hold! stage 1))))

;; ── TeX labels ────────────────────────────────────────────────────────────

(def tex-controls
  [{:id :size :label "font size" :min 20.0 :max 90.0 :value 48.0}])

(defn tex-demo
  [{:keys [size] :or {size 48.0}}]
  (fn [stage]
    (title! stage "\\text{TeX labels}")
    (let [ls (map-indexed (fn [i src] (st/tex stage src [0 (- 1.8 (* 1.4 i))]
                                              {:font-size size
                                               :color ([:gold :teal :blue :pink] i)}))
                          ["e^{i\\pi} + 1 = 0"
                           "\\int_0^1 x^2\\,dx = \\frac{1}{3}"
                           "\\nabla \\times \\vec B = \\mu_0 \\vec J"
                           "\\sum_{n=1}^{\\infty} \\frac{1}{n^2} = \\frac{\\pi^2}{6}"])]
      (st/play! stage (st/stagger (mapv #(st/appear % :run-time 0.8) ls) :lag-ratio 0.5))
      (st/hold! stage 1))))

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
