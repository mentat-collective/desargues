(ns desargues.lesson.area-under-curve
  "Recorder for the :scene block of lessons/area-under-curve.edn.

   The lesson file is data only, so the recorded scene graph is pasted into
   it as EDN. This namespace is how that EDN was made; re-run it after
   changing the scene and paste the result over the :riemann-scene block's
   :scene value:

     (require 'desargues.lesson.area-under-curve)
     (desargues.lesson.area-under-curve/print-scene)

   Coordinates are rounded to 4 decimals so the pasted EDN stays readable."
  (:require [desargues.scene :as s]
            [desargues.scene.record :as rec]
            [desargues.scene.serialize :as ser]))

(defn- r4 [x] (/ (Math/round (* 10000.0 (double x))) 10000.0))

(defn- sc
  "Math point (x, y) of the unit square -> stage point."
  [x y]
  [(r4 (+ -4.0 (* 6.0 x))) (r4 (+ -2.5 (* 4.5 y)))])

(defn riemann-scene
  "Right Riemann sum of x^2 on [0, 1] with n = 8 rectangles, counting up to
   its value 0.3984 next to the exact area 1/3."
  []
  (rec/record
   "riemann-sum"
   (fn [stage]
     (let [n     8
           title (-> (s/text "Riemann sum of x^2 on [0, 1], n = 8" :font-size 28 :color :white)
                     (s/move-to [0 3.4]))
           xax   (s/line (sc -0.1 0) (sc 1.1 0) :color :white :width 2)
           yax   (s/line (sc 0 -0.05) (sc 0 1.1) :color :white :width 2)
           curve (s/path (mapv (fn [i] (let [x (/ i 40.0)] (sc x (* x x)))) (range 41))
                         :color :blue :width 4)
           rects (vec (for [k (range 1 (inc n))]
                        (let [x0 (/ (dec k) (double n)) x1 (/ k (double n)) h (* x1 x1)]
                          (-> (s/rectangle :width (r4 (/ 6.0 n)) :height (r4 (* 4.5 h)) :color :teal)
                              (s/fill! :teal :opacity 0.5)
                              (s/move-to (sc (/ (+ x0 x1) 2) (/ h 2)))))))
           lbl   (-> (s/text "right sum =" :font-size 30 :color :white) (s/move-to [3.4 0.6]))
           num   (-> (s/decimal 0 :num-decimal-places 4 :color :gold) (s/move-to [5.6 0.6]))
           exact (-> (s/text "exact area: 1/3" :font-size 26 :color :gold) (s/move-to [4.2 -0.4]))]
       (s/play! stage (s/appear title) (s/draw xax) (s/draw yax))
       (s/play! stage (s/draw curve))
       (s/play! stage (s/stagger (mapv s/appear rects)) (s/appear lbl) (s/appear num))
       (s/play! stage (s/count-to num 0.3984))
       (s/play! stage (s/appear exact))
       (s/hold! stage 1.5)))))

(defn print-scene
  "Print the canonical EDN of `riemann-scene`."
  []
  (print (ser/->canonical-edn (riemann-scene))))
