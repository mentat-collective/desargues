(ns desargues.studio.pointer-demos
  "Draggable constructs for the live player. Each is an ordinary parametric
   construct (fn [params] (fn [stage] ...)) on the desargues.scene facade, so
   it still renders an mp4; the stage adds dragging through drag specs
   ({:id :drag {:node name}}, desargues.player.drag).

   REPL:
     (require '[desargues.player :as player] '[desargues.player.javafx :as fx]
              '[desargues.studio.pointer-demos :as pd])
     (def p (player/open! (fx/host) pd/euclid-i-1
                          {:title \"Euclid I.1\" :params pd/euclid-i-1-defaults
                           :controls pd/euclid-i-1-controls}))
     ;; drag A or B with the mouse; or from the REPL:
     (player/set-param! p :a [-2.0 -1.0])"
  (:require [desargues.scene :as s]))

(def euclid-i-1-defaults
  "A and B, the ends of the given segment."
  {:a [-1.5 -1.0] :b [1.5 -1.0]})

(def euclid-i-1-controls
  "Drag specs: the dots named A and B move params :a and :b."
  [{:id :a :drag {:node "A"}}
   {:id :b :drag {:node "B"}}])

(defn apex
  "The third vertex C of the equilateral triangle on AB (Euclid I.1): AB
   turned 60 degrees counter-clockwise about A, which is where the circle
   about A through B meets the circle about B through A."
  [[ax ay] [bx by]]
  (let [dx (- bx ax) dy (- by ay)
        c  0.5 sn (/ (Math/sqrt 3.0) 2.0)]
    [(+ ax (- (* c dx) (* sn dy)))
     (+ ay (* sn dx) (* c dy))]))

(defn- label [s at offset]
  (-> (s/tex s :font-size 36 :color :white)
      (s/move-to (mapv + at offset))))

(defn euclid-i-1
  "Elements I.1: on a given finite straight line AB, construct an equilateral
   triangle. Circles about A and B with radius AB meet at C."
  [{:keys [a b] :or {a [-1.5 -1.0] b [1.5 -1.0]}}]
  (let [r (Math/hypot (- (b 0) (a 0)) (- (b 1) (a 1)))
        c (apex a b)]
    (fn [stage]
      (let [title (-> (s/text "Euclid I.1: drag A or B" :font-size 30 :color :grey)
                      (s/move-to [0 3.5]))
            ab    (s/line a b :color :white :width 4)
            ca    (-> (s/circle :radius r :color :blue :width 2) (s/move-to a))
            cb    (-> (s/circle :radius r :color :teal :width 2) (s/move-to b))
            tri   (-> (s/path [a b c] :closed? true :color :gold :width 4)
                      (s/fill! :gold :opacity 0.15))
            pc    (-> (s/dot :name "C" :color :gold) (s/move-to c))
            pa    (-> (s/dot :name "A" :color :red :radius 0.12) (s/move-to a))
            pb    (-> (s/dot :name "B" :color :red :radius 0.12) (s/move-to b))]
        (s/play! stage (s/together [(s/appear title :run-time 0.3)
                                    (s/draw ab :run-time 0.5)
                                    (s/appear pa :run-time 0.3)
                                    (s/appear pb :run-time 0.3)
                                    (s/appear (label "A" a [-0.3 -0.3]) :run-time 0.3)
                                    (s/appear (label "B" b [0.3 -0.3]) :run-time 0.3)]))
        (s/play! stage (s/together [(s/draw ca :run-time 1.0) (s/draw cb :run-time 1.0)]))
        (s/play! stage (s/together [(s/appear pc :run-time 0.4)
                                    (s/appear (label "C" c [0 0.35]) :run-time 0.4)]))
        (s/play! stage (s/draw tri :run-time 0.8))
        (s/hold! stage 2)))))
