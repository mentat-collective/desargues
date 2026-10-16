(ns desargues.objects.calc.demos
  "The calculus figures the stage, the browser, the showcase and the
   notebook open: each a figure built by layering calculus objects
   (desargues.objects.calc.core) over a view, with widget controls whose
   values are the figure's params, so a slider moves the objects.

   `demos` maps a key to {:title :figure :controls}; `entry` adds the
   :source-fn and :params a 3D-or-2D source registry wants
   (desargues.studio.spaces). Host-free .cljc; on the JVM the exact
   readouts (desargues.objects.calc.readout) load with it."
  (:require [desargues.objects.calc.core :as c :refer [p]]
            [desargues.objects.calc.figure :as figure]
            [desargues.objects.calc.functions :as fns]
            #?(:clj [desargues.objects.calc.readout])))

(def ^:private fn-select
  {:id :f :kind :select :label "f(x)" :value :parabola :options (fns/options)})

(def ^:private common
  [{:id :play :kind :toggle :label "animate" :value true}
   {:id :exact :kind :toggle :label "exact readout (Emmy)" :value true}
   {:id :port :kind :select :label "numeric port" :value :jvm
    :options [{:value :jvm :label "jvm"} {:value :raster :label "raster (if loaded)"}]}])

(def area-figure
  (c/figure
   {:title "area under a curve: the region sweeps in"
    :view (c/plane [-0.5 3.5] [-1 5])
    :params {:f :parabola :a 0.0 :b 2.0 :sweep 1.0}
    :animate {:param :sweep :from 0.0 :to 1.0 :duration 4.0}
    :objects [(c/area-under (p :f) (p :a) (p :b) {:upto (p :sweep)})
              (c/graph (p :f) -0.5 3.5)]}))

(def riemann-figure
  (c/figure
   {:title "Riemann sums converge to the integral"
    :view (c/plane [-0.5 3.5] [-1 5])
    :params {:f :parabola :a 0.0 :b 2.0 :n 4.0 :rule :left}
    :animate {:param :n :from 1.0 :to 40.0 :duration 6.0}
    :objects [(c/riemann (p :f) (p :a) (p :b) (p :n) {:rule (p :rule)})
              (c/graph (p :f) -0.5 3.5)
              (c/area-under (p :f) (p :a) (p :b) {:id "exact" :color :teal})]}))

(def between-figure
  (c/figure
   {:title "area between two curves"
    :view (c/plane [-0.5 3.5] [-1 5])
    :params {:f :root-like :g :square :a 0.0 :b 1.28 :sweep 1.0}
    :animate {:param :sweep :from 0.0 :to 1.0 :duration 4.0}
    :objects [(c/area-between (p :f) (p :g) (p :a) (p :b) {:upto (p :sweep)})
              (c/graph (p :f) -0.5 3.5 {:id "graph-f"})
              (c/graph (p :g) -0.5 2.2 {:id "graph-g" :color :red})]}))

(def tangent-figure
  (c/figure
   {:title "secant to tangent: h -> 0"
    :view (c/plane [-2.5 2.5] [-1 5] {:center [0.0 -0.4]})
    :params {:f :cubic :x0 1.5 :h0 1.0 :approach 0.0}
    :animate {:param :approach :from 0.0 :to 1.0 :duration 5.0}
    :objects [(c/graph (p :f) -2.5 2.5)
              (c/secant->tangent (p :f) (p :x0) (p :h0) {:approach (p :approach)})
              (c/tangent-line (p :f) (p :x0) {:half 0.6})]}))

(def ftc-figure
  (c/figure
   {:title "FTC: A(x) = integral of f from 0 to x, A' = f"
    :view (c/plane [-0.5 3.5] [-1 5])
    :params {:f :root-like :a 0.0 :x 3.0}
    :animate {:param :x :from 0.0 :to 3.0 :duration 5.0}
    :objects [(c/graph (p :f) -0.5 3.5)
              (c/accumulation (p :f) (p :a) (p :x))]}))

(def field-figure
  (c/figure
   {:title "line integral: work of F along a circle"
    :view (c/plane [-2.5 2.5] [-2 2] {:x-length 9.0 :y-length 6.0})
    :params {:curl 1.0 :upto 1.0}
    :animate {:param :upto :from 0.0 :to 1.0 :duration 5.0}
    :objects [(c/vector-field (fn [[x y]] [(- y) x]) {:step 0.5})
              (c/line-integral (fn [[x y]] [(- y) x])
                               (fn [t] [(* 1.5 (Math/cos t)) (* 1.5 (Math/sin t))])
                               [0.0 (* 2 Math/PI)] {:upto (p :upto)})]}))

(def revolution-figure
  (c/figure
   {:title "volume of revolution"
    :view (c/space [[-0.5 2.6] [-2.4 2.4] [-2.4 4.6]]
                   {:camera {:target [1.0 0.0 0.2] :distance 12.0 :yaw -1.2 :pitch 0.42
                             :projection :perspective :zoom 1.6}})
    :params {:f :root-like :g :square :method :disk :a 0.0 :b 2.0 :n 6.0 :sweep 1.0}
    :animate {:param :sweep :from 0.05 :to 1.0 :duration 5.0}
    :objects [(c/revolution (p :f) (p :a) (p :b) (p :method)
                            {:g (p :g) :n (p :n) :sweep (p :sweep)})]}))

(def columns-figure
  (c/figure
   {:title "double integral: columns under z = f(x, y)"
    :view (c/space [[-0.2 2.4] [-0.2 2.4] [0.0 3.0]]
                   {:camera {:target [1.0 1.0 0.9] :distance 10.5 :yaw -0.8 :pitch 0.5
                             :projection :perspective :zoom 1.8}})
    :params {:n 6.0 :rise 1.0}
    :animate {:param :rise :from 0.0 :to 1.0 :duration 4.0}
    :objects [(c/double-integral (fn [x y] (+ 1.0 (* 0.5 x y) (* -0.15 x x))) [0.0 2.0] [0.0 2.0]
                                 (p :n) {:rise (p :rise)})]}))

(def demos
  "Key -> {:title :figure :controls}."
  {:calc-area     {:title "area under (sweep)" :figure area-figure
                   :controls (into [fn-select
                                    {:id :b :label "b" :min 0.5 :max 3.0 :value 2.0}
                                    {:id :sweep :label "sweep" :min 0.0 :max 1.0 :value 1.0}] common)}
   :calc-riemann  {:title "Riemann sums" :figure riemann-figure
                   :controls (into [fn-select
                                    {:id :n :label "n" :min 1.0 :max 60.0 :value 4.0}
                                    {:id :rule :kind :select :label "rule" :value :left
                                     :options (mapv (fn [k] {:value k :label (name k)})
                                                    [:left :right :mid :trapezoid])}] common)}
   :calc-between  {:title "area between curves" :figure between-figure
                   :controls (into [{:id :sweep :label "sweep" :min 0.0 :max 1.0 :value 1.0}] common)}
   :calc-tangent  {:title "secant -> tangent" :figure tangent-figure
                   :controls (into [(assoc fn-select :value :cubic)
                                    {:id :x0 :label "x0" :min -2.0 :max 2.0 :value 1.5}
                                    {:id :h0 :label "h" :min 0.1 :max 1.5 :value 1.0}
                                    {:id :approach :label "h -> 0" :min 0.0 :max 1.0 :value 0.0}]
                                   common)}
   :calc-ftc      {:title "accumulation A(x) (FTC)" :figure ftc-figure
                   :controls (into [(assoc fn-select :value :root-like)
                                    {:id :x :label "x" :min 0.0 :max 3.0 :value 3.0}] common)}
   :calc-work     {:title "line integral (work)" :figure field-figure
                   :controls (into [{:id :upto :label "t / 2 pi" :min 0.0 :max 1.0 :value 1.0}] common)}
   :calc-solid    {:title "volume of revolution (3D)" :figure revolution-figure
                   :controls (into [(assoc fn-select :value :root-like)
                                    {:id :method :kind :select :label "method" :value :disk
                                     :options [{:value :disk :label "disk"} {:value :washer :label "washer (f - x^2)"}
                                               {:value :shell :label "shell (about y)"}]}
                                    {:id :n :label "slabs" :min 1.0 :max 24.0 :value 6.0}
                                    {:id :sweep :label "turn" :min 0.05 :max 1.0 :value 1.0}] common)}
   :calc-columns  {:title "double integral columns (3D)" :figure columns-figure
                   :controls (into [{:id :n :label "n" :min 1.0 :max 20.0 :value 6.0}
                                    {:id :rise :label "rise" :min 0.0 :max 1.0 :value 1.0}] common)}})

(defn defaults [k] (into {} (map (juxt :id :value)) (get-in demos [k :controls])))

(defn entry
  "Demo `k` as a source registry entry {:title :source-fn :params :controls}."
  [k]
  (let [{:keys [title figure controls]} (demos k)]
    {:title title :source-fn (figure/source-fn figure) :params (defaults k) :controls controls}))

(def entries
  "Every demo as a source registry entry, by key."
  (into {} (map (juxt identity entry)) (keys demos)))
