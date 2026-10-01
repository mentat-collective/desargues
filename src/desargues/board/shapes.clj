(ns desargues.board.shapes
  "Pure layer of boards: figures as data, for an author to merge into a
   construction.

   Every builder returns a FRAGMENT, {:points [...] :draw [...]}, whose point
   ids are prefixed by the id given, so two fragments never collide;
   `merge-parts` joins fragments and a spec into one construction:

     (merge-parts {:id :cube :kind :construction :view {...} :params [...]}
                  (solid :C :cube {:size 1.2})
                  (axes3 :ax 2))

   Vertices come out pinned (:fixed? true), so a solid is moved by the
   construction's maps, never by a stray drag.")

(defn merge-parts
  "One construction from spec and fragments: vectors concatenate, maps
   merge. The fragments come FIRST, in order, so the spec's own points may
   refer to their vertices and its drawing lies on top of theirs."
  [spec & parts]
  (let [join (fn [acc part]
               (merge-with (fn [a b] (cond (vector? a) (into a b) (map? a) (merge a b) :else b))
                           acc part))]
    (join (reduce join {} parts) spec)))

(defn vertex-id "The id of fragment id's i-th vertex: :C.3." [id i] (keyword (str (name id) "." i)))

(defn regular-polygon
  "n vertices on the circle of radius r about center, the first at angle
   phase; drawn as a filled polygon."
  [id n r & [{:keys [center phase color] :or {center [0 0] phase 0}}]]
  (let [ids (mapv #(vertex-id id %) (range n))]
    {:points (vec (for [i (range n)
                        :let [a (+ phase (/ (* 2 Math/PI i) n))]]
                    {:id (nth ids i) :fixed? true
                     :at [(+ (first center) (* r (Math/cos a))) (+ (second center) (* r (Math/sin a)))]}))
     :draw [(cond-> [:polygon ids] color (conj {:color color}))]}))

;; ---------------------------------------------------------------------------
;; Convex solids, by their vertices: edges and faces are found, not listed.

(def ^:private phi (/ (+ 1 (Math/sqrt 5)) 2))

(defn- perms [[a b c]] [[a b c] [b c a] [c a b]])

(defn- signs [v] ;; every sign pattern of v's non-zero entries
  (reduce (fn [acc i] (if (zero? (nth v i)) acc (into acc (map #(update % i -) acc)))) [v] (range (count v))))

(def ^:private vertices
  {:tetrahedron [[1 1 1] [1 -1 -1] [-1 1 -1] [-1 -1 1]]
   :cube (vec (signs [1 1 1]))
   :octahedron (vec (mapcat signs [[1 0 0] [0 1 0] [0 0 1]]))
   :icosahedron (vec (distinct (mapcat (comp perms) (signs [0 1 phi]))))
   :dodecahedron (vec (distinct (concat (signs [1 1 1])
                                        (mapcat perms (signs [0 (/ 1 phi) phi])))))})

(defn- v- [a b] (mapv - a b))
(defn- vdot [a b] (reduce + (map * a b)))
(defn- vcross [[a b c] [d e f]] [(- (* b f) (* c e)) (- (* c d) (* a f)) (- (* a e) (* b d))])
(defn- vnorm [a] (Math/sqrt (vdot a a)))

(defn- edges [vs]
  (let [d (apply min (for [i (range (count vs)) j (range i) ] (vnorm (v- (vs i) (vs j)))))]
    (vec (for [i (range (count vs)) j (range i)
               :when (< (Math/abs (- d (vnorm (v- (vs i) (vs j))))) 1e-9)]
           [j i]))))

(defn- faces
  "The faces of the convex hull of vs, each a vector of vertex indices in
   order around it."
  [vs]
  (let [n (count vs)
        planes (for [i (range n) j (range i) k (range j)
                     :let [nrm (vcross (v- (vs j) (vs i)) (v- (vs k) (vs i)))]
                     :when (> (vnorm nrm) 1e-9)
                     :let [c (vdot nrm (vs i))
                           side (map #(- (vdot nrm %) c) vs)]
                     :when (or (every? #(<= % 1e-9) side) (every? #(>= % -1e-9) side))]
                 (set (filter #(< (Math/abs (nth side %)) 1e-9) (range n))))]
    (vec (for [face (distinct planes)
               :let [pts (map vs face)
                     ctr (mapv #(/ % (count face)) (apply map + pts))
                     u (v- (first pts) ctr)
                     nrm (vcross (v- (second pts) ctr) u)
                     w (vcross nrm u)]]
           (vec (sort-by #(let [p (v- (vs %) ctr)] (Math/atan2 (vdot p w) (vdot p u))) face))))))

(defn solid
  "A Platonic solid centred at the origin, circumradius size: :tetrahedron
   :cube :octahedron :icosahedron :dodecahedron. opts: :size (1), :faces?
   (true) translucent faces, :edges? (true), :color, :face-opacity."
  [id kind & [{:keys [size faces? edges? color face-opacity]
               :or {size 1 faces? true edges? true face-opacity 0.14}}]]
  (let [raw (or (vertices kind) (throw (ex-info (str "No solid " kind) {:known (keys vertices)})))
        r (vnorm (first raw))
        vs (mapv (fn [v] (mapv #(* size (/ % r)) v)) raw)
        ids (mapv #(vertex-id id %) (range (count vs)))
        opts (cond-> {} color (assoc :color color))]
    {:points (mapv (fn [i v] {:id i :at v :fixed? true}) ids vs)
     :draw (vec (concat
                 (when faces? (for [f (faces vs)] [:polygon (mapv ids f) (assoc opts :fill-opacity face-opacity)]))
                 (when edges? (for [[a b] (edges vs)] [:segment (ids a) (ids b) opts]))))}))

(defn axes3
  "The x, y, z axes from the origin, each len long, colored and labelled."
  [id len]
  (let [o (vertex-id id "o")
        ends {:x [len 0 0] :y [0 len 0] :z [0 0 len]}
        colors {:x :red :y :green :z :blue}]
    {:points (into [{:id o :at [0 0 0] :fixed? true}]
                   (for [[k v] ends] {:id (vertex-id id (name k)) :at v :fixed? true}))
     :draw (vec (for [[k _] ends]
                  [:segment o (vertex-id id (name k)) {:color (colors k) :weight 1.5}]))}))

(defn circle
  "The circle about point center through point through, as the curve point
   id (it depends on u; the board needs a sweep, window :n > 1) and its
   trace."
  [id center through & [{:keys [color]}]]
  (let [c (symbol (name center)) p (symbol (name through))
        r (list 'sqrt (list '+ (list 'square (list '- (list 'x p) (list 'x c)))
                            (list 'square (list '- (list 'y p) (list 'y c)))))]
    {:points [{:id id :op :expr
               :x (list '+ (list 'x c) (list '* r (list 'cos (list '* (* 2 Math/PI) 'u))))
               :y (list '+ (list 'y c) (list '* r (list 'sin (list '* (* 2 Math/PI) 'u))))}]
     :draw [(cond-> [:trace id] color (conj {:color color}))]}))
