(ns desargues.board.projective
  "Pure layer of boards: projective constructions, registered on the open
   sets of desargues.board.construction. Every one is incidence alone (joins
   and meets of homogeneous coordinates), so each survives any homography.

     {:op :harmonic :of [:A :B :C]}              D with (A, B; C, D) = -1
     {:op :project :of :P :from :O :onto [:A :B]}
                                                 the perspectivity: OP meets AB
     {:op :conic :through [:A :B :C :D :E]}      the conic through five points,
                                                 traced: the point depends on u

   :conic is Braikenridge-Maclaurin, Pascal's theorem run backwards: the
   line l through A at angle pi u meets the conic again at X, where X is
   fixed by asking the hexagon A B C D E X to be Pascal's, so AB.DE, BC.EX
   and CD.XA are collinear. Draw it with [:trace id] on a board whose window
   sweeps u (:n > 1). With :angle expr the line's angle is expr instead
   (radians, over the params), and X is one point riding the conic."
  (:require [desargues.board.construction :as c]
            [desargues.board.linalg :as la]
            [emmy.env :as e]))

(defmethod c/point :harmonic [{[a b c*] :of} env]
  ;; along AB, C at t gives D at t / (2t - 1): (0, 1; t, d) = -1
  (let [A (c/coords-of env a) B (c/coords-of env b) C (c/coords-of env c*)
        d (la/sub B A)
        t (e/divide (la/dot (la/sub C A) d) (la/dot d d))]
    {:xy (la/lerp A B (e/divide t (e/- (e/* 2 t) 1)))}))

(defmethod c/point :project [{:keys [of from] [a b] :onto} env]
  (let [H #(la/h (c/xy-of env %))
        [X Y W] (la/cross (la/cross (H from) (H of)) (la/cross (H a) (H b)))]
    {:xy [(e/divide X W) (e/divide Y W)]}))

(defmethod c/point :conic [{[a b c* d f] :through angle :angle} {:keys [self] :as env}]
  ;; every homogeneous intermediate bound under :let, so each step refers
  ;; to names and the kernel never inlines the whole chain of meets
  (let [H #(la/h (c/xy-of env %))
        [ax ay] (c/xy-of env a)
        th (if angle (c/author-expr angle env) (e/* Math/PI 'u))
        steps (volatile! [])
        bind (fn [k v]
               (let [ss (mapv #(c/local self (str k %)) (range 3))]
                 (vswap! steps into (map vector ss v))
                 ss))
        l (bind "l" (la/cross (H a) [(e/+ ax (e/cos th)) (e/+ ay (e/sin th)) 1]))
        p (bind "p" (la/cross (la/cross (H a) (H b)) (la/cross (H d) (H f))))
        r (bind "r" (la/cross (la/cross (H c*) (H d)) l))
        q (bind "q" (la/cross (la/cross (H b) (H c*)) (la/cross p r)))
        x (bind "x" (la/cross (la/cross (H f) q) l))]
    {:let @steps
     :xy [(e/divide (x 0) (x 2)) (e/divide (x 1) (x 2))]}))
