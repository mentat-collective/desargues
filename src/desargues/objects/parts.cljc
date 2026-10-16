(ns desargues.objects.parts
  "PURE stratum: the open seam every math object registers into.

   An OBJECT is parametric data, {:object kind ...params}. Its PARTS are a
   vector of PRIMITIVE descriptors in scene units, each with a :key stable
   across parameter changes so a transition can match old to new:

     {:key k :prim :line      :from [x y] :to [x y] :style {...}}
     {:key k :prim :path      :points [[x y] ...] :closed? bool :style {...}}
     {:key k :prim :dot       :at [x y] :style {...}}
     {:key k :prim :rectangle :at [x y] :width w :height h :style {...}}
     {:key k :prim :text      :text s :at [x y] :style {...}}
     {:key k :prim :tex       :tex s :at [x y] :style {...}}

   style: {:color kw :width n :radius r :font-size n :fill kw :fill-opacity a}.

   `parts` is a multimethod on :object (OCP): a new object is one defmethod in
   its own namespace, never an edit here. Primitives are the closed set every
   backend already draws (the scene facade's mobjects plus :tex). Host-free.")

(defmulti parts
  "The primitive parts of object spec `obj`, in draw order."
  :object)

(defmethod parts :default [obj]
  (throw (ex-info (str "desargues.objects: no parts method for " (pr-str (:object obj)))
                  {:object (:object obj)})))

(def prim-kinds
  "The primitives a part may be. Closed: every backend must realize each."
  #{:line :path :dot :rectangle :text :tex})

;; ── Primitive constructors (pure) ─────────────────────────────────────────

(defn line [k from to style]
  {:key k :prim :line :from (vec from) :to (vec to) :style style})

(defn path
  ([k points style] (path k points false style))
  ([k points closed? style]
   {:key k :prim :path :points (mapv vec points) :closed? (boolean closed?) :style style}))

(defn dot [k at style]
  {:key k :prim :dot :at (vec at) :style style})

(defn rectangle [k at width height style]
  {:key k :prim :rectangle :at (vec at) :width (double width) :height (double height)
   :style style})

(defn text [k s at style]
  {:key k :prim :text :text (str s) :at (vec at) :style style})

(defn tex [k s at style]
  {:key k :prim :tex :tex (str s) :at (vec at) :style style})

(defn rekey
  "Prefix every part's key with `prefix`, so a composite can embed another
   object's parts without key clashes."
  [prefix ps]
  (mapv #(update % :key (fn [k] [prefix k])) ps))

(defn part-map
  "Parts keyed by :key."
  [ps]
  (into {} (map (juxt :key identity)) ps))

;; ── Small vector helpers shared by the objects ────────────────────────────

(defn v+ [[a b] [c d]] [(+ (double a) (double c)) (+ (double b) (double d))])
(defn v- [[a b] [c d]] [(- (double a) (double c)) (- (double b) (double d))])
(defn v* [k [a b]] [(* (double k) (double a)) (* (double k) (double b))])
(defn norm [[a b]] (Math/sqrt (+ (* (double a) (double a)) (* (double b) (double b)))))
(defn unit [v] (let [n (norm v)] (if (zero? n) [0.0 0.0] (v* (/ 1.0 n) v))))
(defn rot90 [[a b]] [(- (double b)) (double a)])

(defn format-number
  "A tick label: integers without a decimal point, others with up to 2."
  [x]
  (let [x (double x)]
    (if (== x (Math/rint x))
      (str (long (Math/rint x)))
      (let [s (str (/ (Math/rint (* 100.0 x)) 100.0))] s))))
