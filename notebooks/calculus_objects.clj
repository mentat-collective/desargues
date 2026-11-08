;; # Calculus objects
;;
;; Integrals, tangents and solids as DATA: each object is a map built by a
;; constructor of `desargues.objects.calc.core`, a figure layers objects over
;; a view with params, and each target is one multimethod:
;;
;; - `->shapes` (desargues.objects.calc.shapes): player frames, the same the
;;   JavaFX studio, the browser canvas and the headless PNG host draw;
;; - `->emmy-viewers` (desargues.calc-viewers): Mafs, MathBox and Leva here.
;;
;; Run with `clojure -M:notebooks` (Clerk serves it on port 7788).

^{:nextjournal.clerk/visibility {:code :hide}}
(ns calculus-objects
  {:nextjournal.clerk/toc true}
  (:require [desargues.calc-emmy :as cv]
            [desargues.objects.calc.core :as c :refer [p]]
            [desargues.objects.calc.demos :as demos]
            [desargues.objects.calc.figure :as figure]
            [desargues.objects.calc.numeric :as num]
            [desargues.objects.calc.readout :as readout]
            [desargues.player.headless :as hl]
            [desargues.player.protocols :as pp]
            [desargues.player.space :as space]
            [emmy.clerk :as ec]
            [nextjournal.clerk :as clerk]))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ec/install!)

;; ## Objects are data
;;
;; A Riemann sum whose `n` and rule are params, layered under the graph:

(def riemann demos/riemann-figure)

(c/figure-problems riemann {:n 12.0})

;; Values go through a numeric port (`:jvm` here, `:raster` when its adapter
;; is loaded); the readout puts Emmy's exact value beside the number:

(c/values riemann {:n 12.0 :rule :mid} :jvm)

(figure/readout-text riemann {:n 12.0 :rule :mid :exact true} 100)

;; Convergence: the error of each rule as n doubles.

(clerk/table
 (for [n [2 4 8 16 32 64]]
   (into {"n" n}
         (for [rule [:left :right :mid :trapezoid]]
           [(name rule) (Math/abs (- (c/value (c/riemann :parabola 0 2 n {:rule rule}) :jvm) 16/3))]))))

;; ## The same figure, two renderers
;;
;; Mafs (emmy-viewers), from `->emmy-viewers`:

(cv/figure->mafs riemann {:n 12.0 :rule :trapezoid})

;; The player frame, from `->shapes`, painted by the headless host:

(defn player-png [k params t]
  (let [{:keys [source-fn]} (demos/entry k)
        f (pp/-frame-at (source-fn (merge (demos/defaults k) params)) t)]
    (hl/frame->image (space/->2d f) 720 405)))

(player-png :calc-riemann {:n 12.0 :rule :trapezoid :play false} 0)

;; ## Tangent, live
;;
;; The tangent's `x0` is a param ref, so the viewer draws it from the Leva
;; atom: Emmy differentiates f in the page. Drag the point or the slider.

(cv/figure->viewer demos/tangent-figure {:approach 0.6} (get-in demos/demos [:calc-tangent :controls]))

;; ## Area, between, FTC

(clerk/row
 (cv/figure->mafs demos/area-figure)
 (cv/figure->mafs demos/between-figure))

(cv/figure->mafs demos/ftc-figure {:x 2.5})

(player-png :calc-ftc {:x 2.5 :play false} 0)

;; ## Volumes of revolution
;;
;; Disk, washer and shell are one object with a `:method` slot; exact volumes
;; come from the polynomial bodies through Emmy:

(clerk/table
 (for [m [:disk :washer :shell]
       :let [o (c/revolution :root-like 0 1.28 m {:g :square})]]
   {"method" (name m) "numeric" (c/value o :jvm) "exact" (readout/exact-text o)}))

(cv/figure->mathbox demos/revolution-figure {:method :disk})

(player-png :calc-solid {:method :shell :f :parabola :play false} 0)

;; ## Double integral columns

(cv/figure->mathbox demos/columns-figure {:n 8.0})

(player-png :calc-columns {:n 8.0 :play false} 0)

;; ## Line integral

(cv/figure->mafs demos/field-figure {:upto 0.6})

(c/value (c/line-integral (fn [[x y]] [(- y) x]) (fn [t] [(Math/cos t) (Math/sin t)]) [0 (* 2 Math/PI)])
         (num/resolve-port :jvm))
