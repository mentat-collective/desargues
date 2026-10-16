(ns desargues.objects.calc.core
  "DOMAIN stratum: calculus objects as data, standardized and composable.

   A calculus OBJECT is a map {:kind k :id s ...slots}; one constructor per
   kind builds it and nothing else knows its layout. Slots hold numbers,
   function references (a desargues.objects.calc.functions key, an entry, or
   a bare fn) and PARAM REFERENCES, (p :x0), resolved against a figure's
   params at render time, so a widget or the clock moves the object:

     (area-under :parabola 0 2 {:upto (p :sweep)})
     (riemann :wave -3 3 (p :n) {:rule (p :rule)})
     (tangent-line :cubic (p :x0))

   A FIGURE layers objects over a view with params; it is the composable
   unit every target renders:

     (figure {:view (plane [-1 4] [-1 5]) :objects [...] :params {:n 8}})

   Each target is one multimethod on :kind, open for new kinds (OCP):
     problems      desargues.objects.calc.core      validation (this ns)
     value         desargues.objects.calc.core      numbers through the
                                                    numeric port
     ->shapes      desargues.objects.calc.shapes2d / shapes3d   player frames
     ->emmy-viewers notebooks/desargues/calc_viewers.clj  Mafs / MathBox
     exact         desargues.objects.calc.readout   Emmy closed forms (JVM)
   Host-free .cljc."
  (:require [desargues.objects.calc.functions :as fns]
            [desargues.objects.calc.numeric :as num]))

;; ── Param references ─────────────────────────────────────────────────────────

(defn p
  "A reference to figure param `k` (with an optional default)."
  ([k] {:calc/param k})
  ([k default] {:calc/param k :calc/default default}))

(defn param-ref? [v] (and (map? v) (contains? v :calc/param)))

(defn resolve-params
  "Object (or any data) `x` with every param reference replaced by its value
   in `params` (or its default)."
  [x params]
  (cond
    (param-ref? x) (get params (:calc/param x) (:calc/default x))
    (record? x)    x
    (map? x)       (into {} (map (fn [[k v]] [k (resolve-params v params)])) x)
    (vector? x)    (mapv #(resolve-params % params) x)
    :else          x))

(defn param-keys
  "Every param key referenced in `x`."
  [x]
  (cond (param-ref? x) #{(:calc/param x)}
        (record? x)    #{}
        (map? x)       (reduce into #{} (map param-keys (vals x)))
        (vector? x)    (reduce into #{} (map param-keys x))
        :else          #{}))

;; ── Functions held by objects ────────────────────────────────────────────────

(defn fn-entry [obj k] (fns/as-fn (get obj k)))

(defn f-at
  "(f x) for the function in slot `k` of `obj`, as a double."
  [obj k x]
  (double ((:f (fn-entry obj k)) x)))

(defn df-at
  "f'(x) for slot `k`: Emmy's derivative when the entry has :df, else a
   central difference."
  [obj k x]
  (let [{:keys [f df]} (fn-entry obj k)]
    (if df
      (double (df x))
      (let [h (* 1e-5 (max 1.0 (Math/abs (double x))))]
        (/ (- (double (f (+ x h))) (double (f (- x h)))) (* 2.0 h))))))

;; ── Views and figures ────────────────────────────────────────────────────────

(defn plane
  "A 2D view: math box x-range by y-range drawn in the scene's plot box."
  ([x-range y-range] (plane x-range y-range {}))
  ([x-range y-range opts]
   (merge {:view :plane :x-range x-range :y-range y-range
           :x-length 11.0 :y-length 6.0 :center [0.0 -0.4]}
          opts)))

(defn space
  "A 3D view: axes box `range` [[x0 x1] [y0 y1] [z0 z1]] and a camera."
  ([range] (space range {}))
  ([range opts]
   (merge {:view :space :range range
           :camera {:target [0.0 0.0 0.0] :distance 13.0 :yaw -1.0 :pitch 0.45
                    :projection :perspective}}
          opts)))

(defn figure
  "A figure: {:title :view :objects [obj ...] :params {...} :animate {...}}.
   :animate {:param k :from a :to b :duration s} lets time drive a param."
  [m]
  (merge {:title "" :objects [] :params {}} m))

(defn layer
  "Figure `fig` with `objs` layered on top."
  [fig & objs]
  (update fig :objects into objs))

(defn resolved
  "The objects of figure `fig` at `params` (merged over its own)."
  ([fig] (resolved fig {}))
  ([fig params]
   (let [ps (merge (:params fig) params)]
     (mapv #(resolve-params % ps) (:objects fig)))))

;; ── Constructors (one per kind) ──────────────────────────────────────────────

(defn- obj [kind id m opts] (merge {:kind kind :id (or (:id opts) id)} m (dissoc opts :id)))

(defn area-under
  "The region under y = f over [a b]; :upto (0..1) sweeps it in from a."
  ([f a b] (area-under f a b {}))
  ([f a b opts] (obj :area-under "area" {:f f :a a :b b :upto 1.0 :color :teal} opts)))

(def riemann-rules #{:left :right :mid :trapezoid})

(defn riemann
  "n Riemann pieces of f over [a b], :rule one of riemann-rules."
  ([f a b n] (riemann f a b n {}))
  ([f a b n opts] (obj :riemann "riemann" {:f f :a a :b b :n n :rule :left :color :blue} opts)))

(defn area-between
  "The region between y = f and y = g over [a b]."
  ([f g a b] (area-between f g a b {}))
  ([f g a b opts] (obj :area-between "between" {:f f :g g :a a :b b :upto 1.0 :color :purple} opts)))

(defn graph
  "The graph of y = f over [a b]."
  ([f a b] (graph f a b {}))
  ([f a b opts] (obj :graph "graph" {:f f :a a :b b :color :blue} opts)))

(defn tangent-line
  "The tangent to y = f at x0 (half-length :half in x)."
  ([f x0] (tangent-line f x0 {}))
  ([f x0 opts] (obj :tangent-line "tangent" {:f f :x0 x0 :half 1.2 :color :red} opts)))

(defn secant
  "The secant of y = f through x0 and x0 + h."
  ([f x0 h] (secant f x0 h {}))
  ([f x0 h opts] (obj :secant "secant" {:f f :x0 x0 :h h :color :green} opts)))

(defn secant->tangent
  "Secants of f at x0 with h shrinking along :approach (0 -> h0, 1 -> 0):
   the limit that defines the derivative, the tangent drawn as its target."
  ([f x0 h0] (secant->tangent f x0 h0 {}))
  ([f x0 h0 opts]
   (obj :secant->tangent "limit" {:f f :x0 x0 :h0 h0 :approach 0.0 :color :green} opts)))

(defn accumulation
  "A(x) = integral of f from a to x (FTC): the area swept to x and the graph
   of A traced up to x; A' = f."
  ([f a x] (accumulation f a x {}))
  ([f a x opts] (obj :accumulation "accum" {:f f :a a :x x :b x :color :gold} opts)))

(def revolution-methods #{:disk :washer :shell})

(defn revolution
  "The solid swept by rotating y = f (and y = g, :washer) over [a b]: about
   the x-axis for :disk and :washer, about the y-axis for :shell. :sweep
   (0..1) is how much of the turn is drawn, :n the slabs shown."
  ([f a b method] (revolution f a b method {}))
  ([f a b method opts]
   (obj :revolution "solid" {:f f :g nil :a a :b b :method method :sweep 1.0 :n 8
                             :color :blue} opts)))

(defn double-integral
  "Columns of f(x, y) over x-range by y-range: an n by n Riemann sum of
   prisms under the surface z = f. `f` is (fn [x y] z)."
  ([f x-range y-range n] (double-integral f x-range y-range n {}))
  ([f x-range y-range n opts]
   (obj :double-integral "columns" {:f f :x-range x-range :y-range y-range :n n :rise 1.0
                                    :color :teal} opts)))

(defn vector-field
  "F(x, y) -> [u v] drawn on a grid of `step`."
  ([F] (vector-field F {}))
  ([F opts] (obj :vector-field "field" {:F F :step 0.5 :color :teal} opts)))

(defn line-integral
  "The work of F along r(t), t in [t0 t1]: the path, F at the moving point
   r(t0 + upto (t1 - t0)) and the work done so far."
  ([F r t-range] (line-integral F r t-range {}))
  ([F r t-range opts]
   (obj :line-integral "work" {:F F :r r :t-range t-range :upto 1.0 :color :gold} opts)))

(defn label
  "A text label at math point `at` (2D) or world point (3D)."
  ([text at] (label text at {}))
  ([text at opts] (obj :label (str "label-" text) {:text text :at at :color :white} opts)))

;; ── Validation ───────────────────────────────────────────────────────────────

(defn- finite? [x]
  (and (number? x) #?(:clj (not (or (Double/isNaN (double x)) (Double/isInfinite (double x))))
                      :cljs (js/isFinite x))))

(defn- interval? [a b] (and (finite? a) (finite? b) (< (double a) (double b))))
(defn- fn-ok? [o k] (some? (:f (fn-entry o k))))
(defn- unit? [x] (and (finite? x) (<= 0.0 (double x) 1.0)))

(defmulti problems
  "Problems (keywords) with resolved object `o`; empty when it is well formed."
  :kind)

(defmethod problems :default [o] [(keyword "calc" "unknown-kind")])

(defn- base [o & checks]
  (into [] (keep (fn [[ok? k]] (when-not ok? k))) (partition 2 checks)))

(defmethod problems :area-under [o]
  (base o (fn-ok? o :f) :area/f (interval? (:a o) (:b o)) :area/interval (unit? (:upto o)) :area/upto))
(defmethod problems :graph [o]
  (base o (fn-ok? o :f) :graph/f (interval? (:a o) (:b o)) :graph/interval))
(defmethod problems :riemann [o]
  (base o (fn-ok? o :f) :riemann/f (interval? (:a o) (:b o)) :riemann/interval
        (and (finite? (:n o)) (>= (double (:n o)) 1)) :riemann/n
        (contains? riemann-rules (:rule o)) :riemann/rule))
(defmethod problems :area-between [o]
  (base o (fn-ok? o :f) :between/f (fn-ok? o :g) :between/g
        (interval? (:a o) (:b o)) :between/interval (unit? (:upto o)) :between/upto))
(defmethod problems :tangent-line [o]
  (base o (fn-ok? o :f) :tangent/f (finite? (:x0 o)) :tangent/x0))
(defmethod problems :secant [o]
  (base o (fn-ok? o :f) :secant/f (finite? (:x0 o)) :secant/x0
        (and (finite? (:h o)) (not (zero? (double (:h o))))) :secant/h))
(defmethod problems :secant->tangent [o]
  (base o (fn-ok? o :f) :limit/f (finite? (:x0 o)) :limit/x0
        (and (finite? (:h0 o)) (pos? (double (:h0 o)))) :limit/h0 (unit? (:approach o)) :limit/approach))
(defmethod problems :accumulation [o]
  (base o (fn-ok? o :f) :accum/f (finite? (:a o)) :accum/a (finite? (:x o)) :accum/x))
(defmethod problems :revolution [o]
  (base o (fn-ok? o :f) :solid/f (interval? (:a o) (:b o)) :solid/interval
        (contains? revolution-methods (:method o)) :solid/method
        (or (not= :washer (:method o)) (fn-ok? o :g)) :solid/g
        (or (not= :shell (:method o)) (>= (double (:a o)) 0.0)) :solid/shell-negative-x
        (unit? (:sweep o)) :solid/sweep))
(defmethod problems :double-integral [o]
  (base o (fn? (:f o)) :columns/f (apply interval? (:x-range o)) :columns/x-range
        (apply interval? (:y-range o)) :columns/y-range
        (and (finite? (:n o)) (>= (double (:n o)) 1)) :columns/n))
(defmethod problems :vector-field [o]
  (base o (fn? (:F o)) :field/F (and (finite? (:step o)) (pos? (double (:step o)))) :field/step))
(defmethod problems :line-integral [o]
  (base o (fn? (:F o)) :work/F (fn? (:r o)) :work/r (apply interval? (:t-range o)) :work/t-range
        (unit? (:upto o)) :work/upto))
(defmethod problems :label [o] (base o (string? (:text o)) :label/text))

(defn figure-problems
  "[{:id :problem}] for every object of `fig` at `params`, plus figure-level
   problems (an unknown view, a param an object references but nobody sets)."
  ([fig] (figure-problems fig {}))
  ([fig params]
   (let [ps (merge (:params fig) params)]
     (into (cond-> []
             (not (#{:plane :space} (get-in fig [:view :view])))
             (conj {:problem :figure/view})
             (seq (remove #(contains? ps %) (param-keys (:objects fig))))
             (conj {:problem :figure/unset-param}))
           (for [o (resolved fig params) k (problems o)] {:id (:id o) :problem k})))))

(defn valid? ([fig] (valid? fig {})) ([fig params] (empty? (figure-problems fig params))))

;; ── Values through the numeric port ──────────────────────────────────────────

(defmulti value
  "The number object `o` stands for, computed through numeric port `port`
   (:jvm, :raster or an INumeric): an area, a sum, a slope, a volume, work."
  (fn [o _port] (:kind o)))

(defmethod value :default [_ _] nil)

(defn- integral [port o k a b] (num/integrate port #(f-at o k %) a b))
(defn- upto-x [o] (+ (double (:a o)) (* (double (:upto o 1.0)) (- (double (:b o)) (double (:a o))))))

(defmethod value :area-under [o port] (integral port o :f (:a o) (upto-x o)))
(defmethod value :graph [_ _] nil)
(defmethod value :area-between [o port]
  (num/integrate port #(- (f-at o :f %) (f-at o :g %)) (:a o) (upto-x o)))
(defmethod value :accumulation [o port] (integral port o :f (:a o) (:x o)))
(defmethod value :tangent-line [o _] (df-at o :f (:x0 o)))
(defmethod value :secant [o _]
  (let [{:keys [x0 h]} o] (/ (- (f-at o :f (+ x0 h)) (f-at o :f x0)) (double h))))

(defn approach-h
  "The h of a secant->tangent at its :approach: h0 (1 - u)^2, never 0."
  [{:keys [h0 approach]}]
  (max 1e-3 (* (double h0) (let [w (- 1.0 (double approach))] (* w w)))))

(defmethod value :secant->tangent [o port]
  (value (assoc o :kind :secant :h (approach-h o)) port))

(defn riemann-pieces
  "[{:l :r :hl :hr :h}] of a riemann object: each piece's sides, the heights
   at its ends and its rule height (trapezoid: the mean)."
  [{:keys [a b n rule] :as o}]
  (let [n (max 1 (long n)) a (double a) dx (/ (- (double b) a) n)]
    (for [i (range n)
          :let [l (+ a (* i dx)) r (+ l dx) fl (f-at o :f l) fr (f-at o :f r)]]
      {:l l :r r :hl fl :hr fr
       :h (case rule :right fr :mid (f-at o :f (/ (+ l r) 2.0)) :trapezoid (/ (+ fl fr) 2.0) fl)})))

(defmethod value :riemann [o _]
  (reduce + 0.0 (map (fn [{:keys [l r h]}] (* (- r l) h)) (riemann-pieces o))))

(defmethod value :revolution [{:keys [method a b] :as o} port]
  (case method
    :disk   (* Math/PI (num/integrate port #(let [y (f-at o :f %)] (* y y)) a b))
    :washer (* Math/PI (num/integrate port #(let [y (f-at o :f %) z (f-at o :g %)] (- (* y y) (* z z))) a b))
    :shell  (* 2.0 Math/PI (num/integrate port #(* % (f-at o :f %)) a b))))

(defn column-cells
  "[{:x0 :x1 :y0 :y1 :h}] of a double-integral object: midpoint heights."
  [{:keys [f x-range y-range n]}]
  (let [n (max 1 (long n)) [x0 x1] x-range [y0 y1] y-range
        dx (/ (- (double x1) x0) n) dy (/ (- (double y1) y0) n)]
    (for [i (range n) j (range n)
          :let [xa (+ x0 (* i dx)) ya (+ y0 (* j dy))]]
      {:x0 xa :x1 (+ xa dx) :y0 ya :y1 (+ ya dy)
       :h (double (f (+ xa (/ dx 2)) (+ ya (/ dy 2))))})))

(defn column-sum [o]
  (reduce + 0.0 (map (fn [{:keys [x0 x1 y0 y1 h]}] (* (- x1 x0) (- y1 y0) h)) (column-cells o))))

(defmethod value :double-integral [{:keys [f x-range y-range]} port]
  (num/integrate2 port f x-range y-range))

(defn r-prime
  "Central-difference velocity of path `r` at t."
  [r t]
  (let [h 1e-5 [a b] (r (+ t h)) [c d] (r (- t h))]
    [(/ (- a c) (* 2 h)) (/ (- b d) (* 2 h))]))

(defn work-density
  "F(r(t)) . r'(t), the line-integral integrand."
  [{:keys [F r]} t]
  (let [[u v] (F (r t)) [dx dy] (r-prime r t)] (+ (* u dx) (* v dy))))

(defmethod value :line-integral [{:keys [t-range upto] :as o} port]
  (let [[t0 t1] t-range]
    (num/integrate port #(work-density o %) t0 (+ t0 (* (double upto) (- t1 t0))))))

(defn values
  "{id value} of every object of `fig` at `params` that has a value."
  ([fig params] (values fig params :jvm))
  ([fig params port]
   (into {} (keep (fn [o] (when-let [v (value o port)] [(:id o) v]))) (resolved fig params))))
