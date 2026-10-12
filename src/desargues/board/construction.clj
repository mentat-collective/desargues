(ns desargues.board.construction
  "Pure layer of boards: the :construction kind, geometry you can drag.

   A construction is a figure given as data. Its points are built in order,
   each from earlier ones, by an open set of operations; its maps are the
   group elements that act on it, 3x3 matrices whose entries are
   expressions of the board's slider params; its checks are the quantities
   the figure is about, read out live, so an invariant can be watched
   staying put while everything else moves.

     {:id :ladder :kind :construction
      :params [{:id 't :min 0 :max 1 :init 0 :label \"rotate\"}]
      :maps   {:R [[(cos t) (- (sin t)) 0] [(sin t) (cos t) 0] [0 0 1]]}
      :points [{:id :A :at [-1 0]}                     ; free: drag it
               {:id :B :at [1 0] :fixed? true}         ; free, pinned
               {:id :X :op :on :line [:A :B] :s 0.3}   ; drag it along AB
               {:id :M :op :mid :of [:A :B]}
               {:id :P :op :meet :lines [[:A :B] [:C :D]]}
               {:id :A' :op :map :by [:R] :of :A}      ; R applied to A
               {:id :Q :op :expr :x '(+ (x A) t) :y '(y B)}]
      :draw   [[:polygon [:A :B :C]] [:segment :A :B] [:line :P :Q]
               [:point :A {:label \"A\"}]]
      :checks [{:distance [:A :B] :label \"|AB|\"}
               {:collinear [:P :Q :R] :label \"P, Q, R collinear\"}]}

   Emmy writes every formula (a meet is a cross product of homogeneous
   lines, a map a matrix product); raster compiles the whole figure into one
   kernel that runs once per call (n = 1): its params are the slider values,
   the free points' coordinates and the on-line points' positions; its
   outputs are every point's x and y and every check's value. A drag in the
   page is a param change and one kernel call.

   A map is a matrix or a family form, [:rotation t], [:boost phi],
   [:rotation3 axis t] (desargues.board.groups); :groups names finite groups,
   {:G [:dihedral 4]}, whose element i is the map [:G i]. With a :view the
   figure is 3D: points carry z, the kernel outputs world coordinates, and
   the page projects them (desargues.board.view).

   `point`, `check`, `draw-layer` and `expand-point` are OPEN: a new
   operation, invariant or drawing is a defmethod, never an edit here. The
   vocabulary beyond the core is registered so: desargues.board.euclid,
   desargues.board.projective, desargues.board.symmetry; desargues.board.shapes
   builds solids and polygons as data."
  (:require [clojure.walk :as walk]
            [desargues.board.algebra :as algebra]
            [desargues.board.kernel :as kernel]
            [emmy.env :as e]
            [desargues.board.linalg :as la]
            [desargues.board.groups :as groups]
            [desargues.board.view :as view]))

;; ---------------------------------------------------------------------------
;; Names: every point gets kernel symbols from its position, so an author's
;; ids (:A', :X_1) never have to be legal raster identifiers.

(defn- sym [& parts] (symbol (apply str parts)))

(defn- point-syms
  "Kernel symbols of the i-th point's world coordinates: x and y, and z in
   3D."
  [i dim]
  (cond-> {:x (sym "pt" i "x") :y (sym "pt" i "y")}
    (= 3 dim) (assoc :z (sym "pt" i "z"))))

(defn out-key
  "The output (and kernel array) named for symbol s. Arrays must be named
   apart from the values stored in them: a binding pt0x would shadow an
   array pt0x inside the loop."
  [s] (keyword (str "o-" (name s))))

(defn param-key "The Board param id of kernel param s." [s] (keyword (name s)))

(defn local
  "A kernel symbol private to the point being built (env's :self): its
   intermediate k, for a point op that binds steps under :let."
  [self k] (sym (name (:x self)) "_" (name k)))

(defn param-sym
  "The kernel param a point op owns, named k after the point being built."
  [self k] (sym (name (:x self)) (name k)))

(defn author-expr
  "An author's expression as Emmy: params held symbolic, u (the curve
   parameter the kernel sweeps over [0 1]) too, and (x P) / (y P) / (z P)
   read as point P's coordinates (its kernel symbols, held symbolic as well)."
  [expr {:keys [points param-syms]}]
  (let [used (volatile! #{})
        expr (walk/postwalk
              (fn [f]
                (if (and (seq? f) ('#{x y z} (first f)) (= 2 (count f)))
                  (let [s (get-in points [(keyword (second f)) (keyword (name (first f)))])]
                    (when-not s
                      (throw (ex-info (str "Unknown point " (second f) " in " (pr-str f)) {:form f})))
                    (vswap! used conj s)
                    s)
                  f))
              expr)]
    (algebra/symbolic expr (into (conj (vec param-syms) 'u) (sort-by str @used)))))

;; ---------------------------------------------------------------------------
;; Points, open by :op

(defmulti point
  "point spec, env -> {:xy coords :params [Param ...] :handle layer-or-nil}.
   coords are Emmy expressions over env's symbols (earlier points' kernel
   symbols, params): [x y], or [x y z] in a 3D construction, where a point
   given by two coordinates lies in the plane z = 0. A point that owns
   params (a free point owns its coordinates) returns them, and the handle
   plato drags them by."
  (fn [p _env] (:op p :free)))

(defmethod point :default [p _]
  (throw (ex-info (str "No point operation " (pr-str (:op p))
                       "; register one with (defmethod desargues.board.construction/point "
                       (:op p) " ...)")
                  {:point p :known (keys (methods point))})))

(defn coords-of
  "Point id's world coordinates: [x y], or [x y z] in 3D."
  [env id]
  (let [{:keys [x y z]} (get-in env [:points id])]
    (when-not x (throw (ex-info (str "Point " id " is used before it is built") {:id id})))
    (if z [x y z] [x y])))

(defn xy-of "Point id's world x and y." [env id] (subvec (coords-of env id) 0 2))

(defn map-of
  "The matrix env binds for map name m: an author's map, a composite, or a
   group element [group-id i]."
  [env m]
  (or (get-in env [:maps m])
      (throw (ex-info (str "Unknown map " (pr-str m)) {:map m :known (keys (:maps env))}))))

(defmethod point :free [{:keys [at fixed?]} {:keys [self window]}]
  ;; The params are named apart from the point's own symbols: the kernel
  ;; binds pt<i>x from its param, and raster will not rebind a typed param.
  ;; A point given three coordinates is a 3D vertex, placed, never dragged.
  (if (= 3 (count at))
    {:xy (vec at)}
    (let [[px py] [(sym "in" (:x self)) (sym "in" (:y self))]
          [[x0 x1] [y0 y1]] [(:x window) (:y window)]
          ps [{:id px :min x0 :max x1 :init (first at) :control :point}
              {:id py :min y0 :max y1 :init (second at) :control :point}]]
      {:xy [px py]
       :params ps
       :handle (when-not fixed? {:layer :handle :drives {:x (param-key px) :y (param-key py)}})})))

(defmethod point :on [{:keys [line s fixed?] :or {s 0.5}} {:keys [self] :as env}]
  ;; X = A + s (B - A). Draggable along AB (s is then a param); :fixed? pins
  ;; s, for a point that must stay where it is on the line.
  (let [[a b] line
        s-sym (if fixed? s (sym (:x self) "s"))]
    (cond-> {:xy (la/lerp (coords-of env a) (coords-of env b) s-sym)}
      (not fixed?) (assoc :params [{:id s-sym :min -1e6 :max 1e6 :init s :control :point}]
                          :handle {:layer :handle :drives {:s (param-key s-sym)} :along [a b]}))))

(defmethod point :meet [{:keys [lines]} env]
  (let [[[a b] [c d]] lines
        l (la/cross (la/h (xy-of env a)) (la/h (xy-of env b)))
        m (la/cross (la/h (xy-of env c)) (la/h (xy-of env d)))
        [X Y W] (la/cross l m)]
    {:xy [(e/divide X W) (e/divide Y W)]}))

(defmethod point :mid [{[a b] :of} env]
  {:xy (mapv #(e/divide (e/+ %1 %2) 2) (coords-of env a) (coords-of env b))})

(defmethod point :map [{:keys [by of]} env]
  ;; :by names maps, composed right to left: [:P :E] is P after E. A
  ;; composite is bound once by build (under its :by vector), so a hundred
  ;; images of one group element cost one matrix product, not a hundred.
  ;; An empty :by is the identity.
  (let [p (coords-of env of)]
    (if (empty? by)
      {:xy p}
      (let [H (or (get-in env [:maps by]) (reduce la/mat* (map #(map-of env %) by)))]
        (when-not (= (count H) (inc (count p)))
          (throw (ex-info (str "Map " (pr-str by) " is " (count H) "x" (count H)
                               ", but point " of " has " (count p) " coordinates")
                          {:by by :of of})))
        {:xy (la/apply-mat H p)}))))

(defmethod point :expr [{:keys [x y z]} env]
  {:xy (cond-> [(author-expr x env) (author-expr y env)] z (conj (author-expr z env)))})

;; ---------------------------------------------------------------------------
;; Expansion: a point spec that stands for several points

(defmulti expand-point
  "point spec, ctx -> the point specs it stands for, ctx {:groups :dim}
   (:groups each closed by desargues.board.groups/finite). Open by :op; a
   point stands for itself unless a method says otherwise."
  (fn [p _ctx] (:op p)))

(defmethod expand-point :default [p _] [p])

(defn indexed-id "The id of the i-th point id stands for: :O.3." [id i] (keyword (str (name id) "." i)))

;; ---------------------------------------------------------------------------
;; Checks, open by the key that names them

(declare check)

(defn check-kind
  "Which check a spec names: its :check, else the one registered check whose
   key it carries ({:distance [:A :B]} is a :distance). Read off the
   registered methods, so a new check is found without an edit here."
  [c]
  (or (:check c)
      (first (filter #(and (not= % :default) (contains? c %)) (keys (methods check))))))

(defmulti check
  "check spec, env -> an Emmy expression for its value."
  (fn [c _env] (check-kind c)))

(defmethod check :default [c _]
  (throw (ex-info (str "No check for " (pr-str c)
                       "; register one with (defmethod desargues.board.construction/check ...)")
                  {:check c :known (keys (methods check))})))

(defn dist "The distance between coordinate vectors p and q." [p q] (la/norm (la/sub q p)))

(defmethod check :distance [{[a b] :distance} env]
  (dist (coords-of env a) (coords-of env b)))

(defmethod check :ratio [{[[a b] [c d]] :ratio} env]
  (e/divide (dist (coords-of env a) (coords-of env b)) (dist (coords-of env c) (coords-of env d))))

(defmethod check :angle [{[a o b] :angle} env]
  ;; the angle AOB, in degrees
  (let [o* (coords-of env o)
        u (la/sub (coords-of env a) o*)
        v (la/sub (coords-of env b) o*)]
    (e/* (e/divide 180 Math/PI)
         (e/acos (e/divide (la/dot u v) (e/* (la/norm u) (la/norm v)))))))

(defmethod check :collinear [{[a b c] :collinear} env]
  ;; det [a 1; b 1; c 1]: twice the signed area of abc, zero when collinear
  (let [[ax ay] (xy-of env a) [bx by] (xy-of env b) [cx cy] (xy-of env c)]
    (e/- (e/* (e/- bx ax) (e/- cy ay)) (e/* (e/- by ay) (e/- cx ax)))))

(defmethod check :concurrent [{lines :concurrent} env]
  ;; det of three homogeneous lines, each normalised: zero when they concur
  (let [[l m n] (for [[a b] lines]
                  (let [[p q r] (la/cross (la/h (xy-of env a)) (la/h (xy-of env b)))]
                    (mapv #(e/divide % (e/sqrt (e/+ (e/square p) (e/square q)))) [p q r])))]
    (reduce e/+ (map e/* l (la/cross m n)))))

(defmethod check :cross-ratio [{[a b c d] :cross-ratio} env]
  ;; (A,B;C,D) = (AC·BD)/(AD·BC), signed along the line AB
  (let [[ax ay] (xy-of env a) [bx by] (xy-of env b)
        [ux uy] [(e/- bx ax) (e/- by ay)]
        s (fn [id] (let [[px py] (xy-of env id)] (e/+ (e/* (e/- px ax) ux) (e/* (e/- py ay) uy))))
        [sa sb sc sd] (map s [a b c d])]
    (e/divide (e/* (e/- sc sa) (e/- sd sb)) (e/* (e/- sd sa) (e/- sc sb)))))

(defmethod check :value [{:keys [value]} env] (author-expr value env))

(defmethod check :line-angle [{[[a b] [c d]] :line-angle} env]
  ;; The angle between lines AB and CD, in degrees, 0 when parallel.
  (let [[ax ay] (xy-of env a) [bx by] (xy-of env b)
        [cx cy] (xy-of env c) [dx dy] (xy-of env d)
        [ux uy vx vy] [(e/- bx ax) (e/- by ay) (e/- dx cx) (e/- dy cy)]]
    (e/* (e/divide 180 Math/PI)
         (e/atan (e/abs (e/divide (e/- (e/* ux vy) (e/* uy vx))
                                  (e/+ (e/* ux vx) (e/* uy vy))))))))

;; ---------------------------------------------------------------------------
;; Drawing: the author's :draw as plato.board layers over output keys

(defn at
  "Point id as the output keys of its coordinates: [kx ky], or [kx ky kz]
   in 3D, which the page projects through the board's view."
  [env id]
  (let [{:keys [x y z]} (get-in env [:points id])]
    (when-not x (throw (ex-info (str "Cannot draw unknown point " id) {:id id})))
    (cond-> [(out-key x) (out-key y)] z (conj (out-key z)))))

(defmulti draw-layer
  "A :draw entry [kind & args] -> a plato.board layer, or a vector of
   them. Open by kind."
  (fn [[kind] _env] kind))

(defmethod draw-layer :default [[kind] _]
  (throw (ex-info (str "No drawing for " kind
                       "; register one with (defmethod desargues.board.construction/draw-layer "
                       kind " ...)")
                  {:kind kind :known (keys (methods draw-layer))})))

(defmethod draw-layer :point [[_ id opts] env]
  (merge {:layer :point :at (at env id) :label (name id)} opts))

(defmethod draw-layer :segment [[_ a b opts] env]
  (merge {:layer :segment :a (at env a) :b (at env b)} opts))

(defmethod draw-layer :line [[_ a b opts] env]
  (merge {:layer :line :a (at env a) :b (at env b)} opts))

(defmethod draw-layer :polygon [[_ ids opts] env]
  (merge {:layer :polygon :pts (mapv #(at env %) ids)} opts))

(defmethod draw-layer :path [[_ ids opts] env]
  (merge {:layer :path :pts (mapv #(at env %) ids)} opts))

(defmethod draw-layer :trace [[_ id opts] env]
  ;; Every sample of a point that depends on u: a curve, and under a :map,
  ;; its image.
  (merge {:layer :trace :of (at env id)} opts))

;; ---------------------------------------------------------------------------
;; The plan

(defn- lower
  "An Emmy expression in raster's vocabulary, NOT simplified: every formula
   here is already small, over names bound earlier, and simplify would expand
   each rational function into its canonical polynomial (a cross ratio of
   four bound points grows to a thousand nodes, past the JVM's method limit)."
  [expr]
  (algebra/realize :raster expr))

(defn- bind-matrix
  "Bind the entries of square M under prefix; return [symbol-matrix bindings]."
  [prefix M]
  (let [n (count M)
        entries (for [i (range n) j (range n)] [i j (sym prefix i j) (get-in M [i j])])]
    [(reduce (fn [S [i j s _]] (assoc-in S [i j] s)) (vec (repeat n (vec (repeat n 0)))) entries)
     (mapv (fn [[_ _ s ex]] [s (lower ex)]) entries)]))

(defn- reader
  "Author expressions as Emmy, over the params and the sweep variable u."
  [param-syms]
  #(algebra/symbolic % (conj (vec param-syms) 'u)))

(defn- map-matrix
  "A :maps entry as an Emmy matrix: a family form ([:rotation t], see
   desargues.board.groups) or the rows themselves."
  [form read]
  (if (keyword? (first form))
    (groups/family form read)
    (mapv (fn [row] (mapv read row)) form)))

(defn- bound [syms exprs] (map (fn [s ex] [s (lower ex)]) syms exprs))

(defn- build
  "Walk groups, maps, points, checks in order; collect kernel bindings,
   params and handles. Every intermediate is BOUND to a symbol, so a later
   formula refers to a name and Emmy never expands the whole figure."
  [{:keys [params maps groups points checks window view]}]
  (let [dim (if view 3 2)
        param-syms (mapv (comp symbol name :id) params)
        read (reader param-syms)
        grps (into {} (map (fn [[k g]] [k (groups/finite g)])) groups)
        points (vec (mapcat #(expand-point % {:groups grps :dim dim}) points))
        ;; group elements are numbers, used in place: no bindings
        env0 (reduce (fn [env [gid {:keys [elements]}]]
                       (reduce (fn [env [i M]] (assoc-in env [:maps [gid i]] M))
                               env (map-indexed vector elements)))
                     {:param-syms param-syms :window window :points {} :maps {}
                      :groups grps :dim dim}
                     grps)
        ;; maps: every entry bound once, then referred to by name
        [env binds]
        (reduce (fn [[env binds] [mid form]]
                  (let [[S bs] (bind-matrix (str "m" (name mid)) (map-matrix form read))]
                    [(assoc-in env [:maps mid] S) (into binds bs)]))
                [env0 []]
                (sort-by key maps))
        ;; composites a :map point names (:by [:P :E]), each bound once
        [env binds]
        (reduce (fn [[env binds] [k by]]
                  (let [[S bs] (bind-matrix (str "mc" k "_") (reduce la/mat* (map #(map-of env %) by)))]
                    [(assoc-in env [:maps by] S) (into binds bs)]))
                [env binds]
                (map-indexed vector
                             (distinct (for [p points :when (and (= :map (:op p)) (next (:by p)))] (:by p)))))
        ;; points, in order: each sees only the points before it
        [env binds pparams handles order]
        (reduce (fn [[env binds ps hs order] [i p]]
                  (let [self (point-syms i dim)
                        {:keys [xy params handle] lets :let} (point p (assoc env :self self))
                        wsyms (mapv self (take dim [:x :y :z]))
                        env' (assoc-in env [:points (:id p)] self)]
                    [env'
                     (-> binds
                         (into (bound (map first lets) (map second lets)))
                         (into (bound wsyms (take dim (concat xy (repeat 0))))))
                     (into ps params)
                     (cond-> hs
                       handle (conj (cond-> (assoc handle :at (at env' (:id p)) :label (name (:id p)))
                                      (:along handle) (update :along #(mapv (fn [id] (at env id)) %))
                                      (:around handle) (update :around #(mapv (fn [id] (at env id)) %)))))
                     (conj order self)]))
                [env binds [] [] []]
                (map-indexed vector points))
        ;; checks: each a value, and with :against a reference value it is
        ;; read against (an image's length against the original's)
        cks (vec (map-indexed
                  (fn [i c]
                    (cond-> {:sym (sym "ck" i) :expr (lower (check c env)) :spec c}
                      (:against c) (assoc :ref (sym "ck" i "ref")
                                          :ref-expr (lower (check (:against c) env)))))
                  checks))]
    {:env env
     :bindings (into binds (mapcat (fn [{:keys [sym expr ref ref-expr]}]
                                     (cond-> [[sym expr]] ref (conj [ref ref-expr]))))
                     cks)
     :point-params pparams
     :handles handles
     :order order
     :checks cks}))

(defmethod kernel/defaults :construction [spec]
  ;; One configuration per call (n = 1). A figure with a curve in it (a
  ;; point whose :expr uses u) asks for more samples, and the kernel sweeps
  ;; u over [0 1]: every other point repeats, the curve's point traces. A 3D
  ;; figure has no plane axes to draw.
  {:window (cond-> {:x [-6 6] :y [-3.5 3.5] :n 1 :sweep [0 1]} (:view spec) (assoc :axes :none))
   :params []})

(defn- layers-of [l] (if (map? l) [l] l))

(defmethod kernel/plan :construction [spec]
  (let [{:keys [env bindings point-params handles order checks]} (build spec)
        values (concat (mapcat (fn [{:keys [x y z]}] (cond-> [x y] z (conj z))) order)
                       (mapcat (fn [{:keys [sym ref]}] (cond-> [sym] ref (conj ref))) checks))
        all-params (into (vec (:params spec)) point-params)]
    {:outputs (mapv out-key values)
     :params all-params
     :form (fn [kname]
             (kernel/sweep-form kname {:var 'u :params all-params}
                                (mapv out-key values) bindings (vec values) []))
     :layers (-> []
                 (into (mapcat #(layers-of (draw-layer % env))) (:draw spec))
                 (into (map (fn [{:keys [sym ref spec]}]
                              (cond-> {:layer :value :of (out-key sym)
                                       :label (or (:label spec) (name (check-kind spec)))
                                       :invariant? (boolean (or (:invariant? spec) ref))}
                                ref (assoc :against (out-key ref)))))
                       checks)
                 (into handles))
     :view (some-> (:view spec) view/board-view)
     :probes {}
     :labels {}}))
