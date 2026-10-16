(ns desargues.objects.calc.shapes3d
  "TARGET stratum: the 3D calculus kinds -> desargues.player.space shapes
   (methods of desargues.objects.calc.shapes/->shapes), z up.

     :revolution       :disk / :washer about the x-axis: the profile curve in
                       the y = 0 plane, the surface swept through :sweep of a
                       turn, and :n slabs (disks or washers) at the slab
                       midpoints. :shell about the z-axis (the math y-axis):
                       profile in y = 0, swept surface, :n cylindrical shells.
     :double-integral  the surface z = f(x, y) as a wireframe, and n x n
                       columns (box meshes) of midpoint height, :rise (0..1)
                       growing them from the floor.

   Every solid is a :surface3 of {:vertices :faces}, so the flattening
   (space/->2d) and the JavaFX 3D host both draw it. Host-free .cljc."
  (:require [desargues.objects.calc.core :as calc]
            [desargues.objects.calc.shapes :as sh]
            [desargues.player.space :as space]))

(def ^:private tau (* 2.0 Math/PI))

(defn- mesh [id verts faces color opacity]
  {:id id :node :surface3 :vertices (vec verts) :faces (vec faces)
   :fill {:color color :opacity opacity} :opacity 1.0 :draw 1.0})

(defn- curve [id pts color width]
  {:id id :node :curve3 :points (vec pts) :stroke {:color color :width width} :opacity 1.0 :draw 1.0})

(defn ring-band
  "Quads of a band between rings `p` and `q` (equal-length vectors of
   vertex indices), open (no wrap) unless `closed?`."
  [p q closed?]
  (let [n (count p) m (if closed? n (dec n))]
    (for [k (range m) :let [k2 (mod (inc k) n)]]
      [(p k) (q k) (q k2) (p k2)])))

(defn tube
  "{:vertices :faces} of the solid of revolution of radius r_out (and r_in,
   or 0) about an axis, between stations s0 and s1, through `turn` radians:
   `at` is (fn [s r theta] [x y z])."
  [at s0 s1 r-out r-in turn sides]
  (let [ths  (map #(* turn (/ (double %) sides)) (range (inc sides)))
        ring (fn [s r] (mapv #(at s r %) ths))
        rings [(ring s0 r-out) (ring s1 r-out) (ring s1 r-in) (ring s0 r-in)]
        n     (inc sides)
        verts (vec (apply concat rings))
        idx   (fn [i] (mapv #(+ (* i n) %) (range n)))]
    {:vertices verts
     :faces (vec (concat (ring-band (idx 0) (idx 1) false)
                         (ring-band (idx 1) (idx 2) false)
                         (ring-band (idx 2) (idx 3) false)
                         (ring-band (idx 3) (idx 0) false)))}))

(defn- about-x [s r th] [s (* r (Math/cos th)) (* r (Math/sin th))])
(defn- about-z [s r th] [(* r (Math/cos th)) (* r (Math/sin th)) s])

(defn- swept-surface [o id k turn]
  (let [{:keys [a b method]} o]
    (space/surface (if (= :shell method)
                     (fn [u th] (about-z (calc/f-at o k u) u th))
                     (fn [u th] (about-x u (calc/f-at o k u) th)))
                   [a b] [0.0 turn]
                   {:id id :nu 32 :nv 32 :color (if (= k :g) :maroon :blue) :opacity 0.35})))

(defmethod sh/->shapes :revolution [{:keys [a b method sweep n color] :as o} _ctx]
  (let [turn  (max 1e-3 (* tau (double sweep)))
        a (double a) b (double b) n (max 1 (long n))
        dx    (/ (- b a) n)
        prof  (fn [k] (for [u (sh/sample-xs a b 80)]
                        (if (= :shell method) [u 0.0 (calc/f-at o k u)] [u 0.0 (calc/f-at o k u)])))
        slabs (for [i (range n)
                    :let [l (+ a (* i dx)) r (+ l dx) m (/ (+ l r) 2.0)
                          h (calc/f-at o :f m)]]
                (case method
                  :disk   (tube about-x l r (Math/abs h) 0.0 turn 28)
                  :washer (tube about-x l r (Math/abs h) (Math/abs (calc/f-at o :g m)) turn 28)
                  :shell  (tube about-z 0.0 h r l turn 28)))]
    (cond-> [(swept-surface o (sh/sid o "surface") :f turn)
             (curve (sh/sid o "profile") (prof :f) :gold 5)]
      (= :washer method) (into [(swept-surface o (sh/sid o "inner") :g turn)
                                (curve (sh/sid o "profile-g") (prof :g) :maroon 4)])
      true (into (map-indexed (fn [i {:keys [vertices faces]}]
                                (mesh (sh/sid o "slab" i) vertices faces color 0.85))
                              slabs)))))

(defn box
  "{:vertices :faces} of the axis-aligned box [x0 x1] x [y0 y1] x [z0 z1]."
  [x0 x1 y0 y1 z0 z1]
  {:vertices [[x0 y0 z0] [x1 y0 z0] [x1 y1 z0] [x0 y1 z0]
              [x0 y0 z1] [x1 y0 z1] [x1 y1 z1] [x0 y1 z1]]
   :faces [[0 3 2 1] [4 5 6 7] [0 1 5 4] [1 2 6 5] [2 3 7 6] [3 0 4 7]]})

(defmethod sh/->shapes :double-integral [{:keys [f x-range y-range rise color] :as o} _ctx]
  (let [cells (calc/column-cells o)
        k     (max 0.0 (min 1.0 (double rise)))]
    (into [(assoc (space/surface (fn [x y] [x y (f x y)]) x-range y-range
                                 {:id (sh/sid o "surface") :nu 20 :nv 20 :color :blue
                                  :opacity 0.25 :wire "#58C4DD"})
                  :opacity 0.8)]
          (keep-indexed
           (fn [i {:keys [x0 x1 y0 y1 h]}]
             (when (> (Math/abs (* k h)) 1e-6)
               (let [{:keys [vertices faces]} (box x0 x1 y0 y1 0.0 (* k h))]
                 (mesh (sh/sid o "column" i) vertices faces color 0.9))))
           cells))))

(defmethod sh/->shapes :label3 [{:keys [text at color size] :as o} _ctx]
  [{:id (sh/sid o) :node :label3 :at at :text (str text) :opacity 1.0 :draw 1.0
    :opts {:font-size (or size 28) :color (or color :white)}}])
