(ns desargues.board.euclid
  "Pure layer of boards: Euclidean constructions and measures, registered on
   the open sets of desargues.board.construction.

   Points
     {:op :reflect :of :P :in [:A :B]}         P mirrored in the line AB
     {:op :foot :of :P :on [:A :B]}            the foot of the perpendicular from P
     {:op :rotate :of :P :about :O :by expr}   P turned about O by expr radians
     {:op :centroid :of [:A :B :C ...]}
     {:op :circumcenter :of [:A :B :C]}
     {:op :around :center :O :through :P :angle 0.4}
                                               on the circle about O through P,
                                               dragged round it, starting at 0.4
   Checks
     {:area [:A :B :C ...]}                    signed area, positive counterclockwise
     {:perpendicular [[:A :B] [:C :D]]}        cos of the angle between AB and CD
     {:concyclic [:A :B :C :D]}                zero when the four share a circle

   :reflect, :foot and :centroid work in 3D as in the plane; the rest read
   the plane z = 0."
  (:require [desargues.board.construction :as c]
            [desargues.board.linalg :as la]
            [emmy.env :as e]))

(defn- foot [p a b]
  (let [d (la/sub b a)]
    (la/add a (la/scale (e/divide (la/dot (la/sub p a) d) (la/dot d d)) d))))

(defmethod c/point :reflect [{:keys [of] [a b] :in} env]
  (let [p (c/coords-of env of)
        f (foot p (c/coords-of env a) (c/coords-of env b))]
    {:xy (la/sub (la/scale 2 f) p)}))

(defmethod c/point :foot [{:keys [of] [a b] :on} env]
  {:xy (foot (c/coords-of env of) (c/coords-of env a) (c/coords-of env b))})

(defmethod c/point :rotate [{:keys [of about by]} env]
  (let [[px py] (c/xy-of env of) [ox oy] (c/xy-of env about)
        t (c/author-expr by env)
        cs (e/cos t) sn (e/sin t)
        dx (e/- px ox) dy (e/- py oy)]
    {:xy [(e/+ ox (e/- (e/* cs dx) (e/* sn dy))) (e/+ oy (e/+ (e/* sn dx) (e/* cs dy)))]}))

(defmethod c/point :centroid [{ids :of} env]
  (let [ps (map #(c/coords-of env %) ids)]
    {:xy (mapv #(e/divide % (count ids)) (reduce la/add ps))}))

(defmethod c/point :circumcenter [{[a b c*] :of} env]
  (let [[ax ay] (c/xy-of env a) [bx by] (c/xy-of env b) [cx cy] (c/xy-of env c*)
        sq (fn [x y] (e/+ (e/* x x) (e/* y y)))
        d (e/* 2 (e/+ (e/* ax (e/- by cy)) (e/* bx (e/- cy ay)) (e/* cx (e/- ay by))))]
    {:xy [(e/divide (e/+ (e/* (sq ax ay) (e/- by cy)) (e/* (sq bx by) (e/- cy ay)) (e/* (sq cx cy) (e/- ay by))) d)
          (e/divide (e/+ (e/* (sq ax ay) (e/- cx bx)) (e/* (sq bx by) (e/- ax cx)) (e/* (sq cx cy) (e/- bx ax))) d)]}))

(defmethod c/point :around [{:keys [center through angle fixed?] :or {angle 0}} {:keys [self] :as env}]
  ;; X = O + r (cos a, sin a), r = |OP|. Dragged, a is a param the page
  ;; writes from the pointer's angle about O.
  (let [[ox oy] (c/xy-of env center)
        r (c/dist (c/xy-of env center) (c/xy-of env through))
        a (if fixed? angle (c/param-sym self "a"))]
    (cond-> {:xy [(e/+ ox (e/* r (e/cos a))) (e/+ oy (e/* r (e/sin a)))]}
      (not fixed?) (assoc :params [{:id a :min -1e6 :max 1e6 :init angle :control :point}]
                          :handle {:layer :handle :drives {:angle (c/param-key a)}
                                   :around [center through]}))))

(defmethod c/check :area [{ids :area} env]
  (let [ps (mapv #(c/xy-of env %) ids)]
    (e/divide (reduce e/+ (map (fn [[x1 y1] [x2 y2]] (e/- (e/* x1 y2) (e/* x2 y1)))
                               ps (conj (subvec ps 1) (first ps))))
              2)))

(defmethod c/check :perpendicular [{[[a b] [c* d]] :perpendicular} env]
  (let [u (la/sub (c/coords-of env b) (c/coords-of env a))
        v (la/sub (c/coords-of env d) (c/coords-of env c*))]
    (e/divide (la/dot u v) (e/* (la/norm u) (la/norm v)))))

(defmethod c/check :concyclic [{ids :concyclic} env]
  (la/det (mapv (fn [id] (let [[x y] (c/xy-of env id)] [(e/+ (e/* x x) (e/* y y)) x y 1])) ids)))
