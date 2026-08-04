(ns desargues.layout.core
  "Pure, backend-agnostic layout algebra for an elm-ui-inspired, frame-bounded,
   proportional layout DSL.

   ## Design
   Every visual thing is ONE immutable, namespaced tagged map (elm-ui's single
   `Element` type). Leaves and containers share the SAME shape so they nest
   arbitrarily:

     {:desargues.layout/tag :element
      :type     <keyword>     ; :el :box :row :column :text :math :spacer :image
      :attrs    <map>         ; inert data, resolved last-wins (see the attr grammar)
      :children <vector>      ; containers; [] for leaves
      :content  <string|nil>} ; leaves: text / LaTeX / file path

   The resolver ATTACHES two fields (never authored by hand):
     :desargues.layout/box       {:x :y :w :h}  ; layout coords, top-left origin, y-DOWN
     :desargues.layout/intrinsic {:w :h}        ; measured content size (pass-1 cache)

   ## Purity / SOLID
   This namespace is 100% pure Clojure — ZERO libpython. `resolve-layout` takes an
   injected `measure-fn : [node path] -> [w h]` so the whole algebra is unit-testable
   with a stub. The manim backend (measure + realize) lives in
   `desargues.layout.realize` behind that seam.

   All layout math stays in ONE convention: top-left origin, y-DOWN, scene units.
   Conversion to manim (centered, y-UP) happens only in pass-3 (realize)."
  (:refer-clojure :exclude []))

;; ============================================================================
;; Namespaced keys (match the design's `desargues.layout` prefix exactly so the
;; facade ns can share them)
;; ============================================================================

(def tag-key       :desargues.layout/tag)
(def box-key       :desargues.layout/box)
(def intrinsic-key :desargues.layout/intrinsic)

;; ============================================================================
;; Predicates (O(1))
;; ============================================================================

(def ^:private leaf-types      #{:text :math :image :spacer})
(def ^:private container-types #{:el :box :row :column})

(defn element?
  "True when x is a layout element map."
  [x]
  (and (map? x) (= :element (get x tag-key))))

(defn leaf?      [x] (contains? leaf-types (:type x)))
(defn container? [x] (contains? container-types (:type x)))
(defn row?       [x] (= :row (:type x)))
(defn column?    [x] (= :column (:type x)))

(defn child-axis
  "Main axis of a container: :x for row, :y for column (and column-like box/el)."
  [x]
  (if (row? x) :x :y))

;; ============================================================================
;; Constructors (pure; return the Element map)
;; ============================================================================

(defn- ->element [type attrs children content]
  {tag-key   :element
   :type     type
   :attrs    (or attrs {})
   :children (vec children)
   :content  content})

(defn- parse-args
  "Split a leading optional attrs map from children, and flatten a single seq
   child so both `(row attrs k1 k2)` and `(apply row attrs kids)` work.
   `(if (map? (first args)) [attrs (rest args)] [{} args])`."
  [args]
  (let [[attrs raw] (if (map? (first args))
                      [(first args) (rest args)]
                      [{} args])
        kids (if (and (= 1 (count raw))
                      (sequential? (first raw))
                      (not (element? (first raw))))
               (first raw)
               raw)]
    [attrs (vec (remove nil? kids))]))

(defn el
  "Single-child alignment box (elm-ui `el`). attrs optional: (el child) / (el attrs child)."
  [& args]
  (let [[attrs children] (parse-args args)]
    (->element :el attrs (vec (take 1 children)) nil)))

(defn box
  "Decorated frame. 1 child = el; >1 children are auto-wrapped in a column so a
   box always holds exactly one child."
  [& args]
  (let [[attrs children] (parse-args args)
        child (cond
                (empty? children)      []
                (= 1 (count children)) [(first children)]
                :else                  [(->element :column {} children nil)])]
    (->element :box attrs child nil)))

(defn row
  "Container whose main axis is +x (left -> right)."
  [& args]
  (let [[attrs children] (parse-args args)]
    (->element :row attrs children nil)))

(defn column
  "Container whose main axis is +y (top -> bottom)."
  [& args]
  (let [[attrs children] (parse-args args)]
    (->element :column attrs children nil)))

(def col
  "Alias for `column`."
  column)

(defn spacer
  "Flexible gap. Defaults to :fill on the container's MAIN axis (see resolver)."
  [& args]
  (let [[attrs _] (parse-args args)]
    (->element :spacer attrs [] nil)))

(defn text
  "Single-line text leaf (elm-ui `text`, NO wrap). -> manim Text."
  ([s] (text s {}))
  ([s attrs] (->element :text attrs [] s)))

(defn math
  "LaTeX math leaf. -> manim MathTex."
  ([latex] (math latex {}))
  ([latex attrs] (->element :math attrs [] latex)))

(defn image
  "Image leaf (SVG if .svg). -> manim ImageMobject / SVGMobject."
  ([path] (image path {}))
  ([path attrs] (->element :image attrs [] path)))

;; ============================================================================
;; Attr normalizers (closed grammar; unknown keys are ignored data)
;; ============================================================================

(defn normalize-length
  "Canonicalize a :width/:height value to
     {:mode :px :px n} | {:mode :shrink} | {:mode :portion :k k}
   optionally carrying :min / :max clamps. Default (nil) = :shrink."
  [len]
  (cond
    (nil? len)      {:mode :shrink}
    (= len :shrink) {:mode :shrink}
    (= len :fill)   {:mode :portion :k 1}
    (number? len)   {:mode :px :px len}
    (vector? len)
    (let [[t a b c] len]
      (case t
        :px      {:mode :px :px a}
        :portion {:mode :portion :k a}
        :min     (assoc (normalize-length (if (nil? b) :shrink b)) :min a)
        :max     (assoc (normalize-length (if (nil? b) :fill b)) :max a)
        :clamp   (assoc (normalize-length c) :min a :max b)
        {:mode :shrink}))
    :else {:mode :shrink}))

(defn- apply-clamp
  "Apply a length's :min / :max clamp to a resolved scalar."
  [v len]
  (let [lo (:min len)
        hi (:max len)
        v  (if lo (max lo v) v)
        v  (if hi (min hi v) v)]
    v))

(defn normalize-padding
  "Canonicalize :padding to {:top :right :bottom :left}."
  [p]
  (cond
    (nil? p)    {:top 0 :right 0 :bottom 0 :left 0}
    (number? p) {:top p :right p :bottom p :left p}
    (vector? p) (let [[x y] p] {:left x :right x :top y :bottom y})
    (map? p)    (merge {:top 0 :right 0 :bottom 0 :left 0} p)
    :else       {:top 0 :right 0 :bottom 0 :left 0}))

(defn normalize-align
  "Canonicalize :align to {:x <:left|:center|:right> :y <:top|:center|:bottom>}
   (missing axes omitted)."
  [a]
  (cond
    (nil? a)     {}
    (keyword? a) (case a
                   :left     {:x :left}
                   :right    {:x :right}
                   :center-x {:x :center}
                   :top      {:y :top}
                   :bottom   {:y :bottom}
                   :center-y {:y :center}
                   {})
    (set? a)     (reduce (fn [m k] (merge m (normalize-align k))) {} a)
    (map? a)     (select-keys a [:x :y])
    :else        {}))

(defn- x->pack [x] (case x :left :start :center :center :right :end nil))
(defn- y->pack [y] (case y :top :start :center :center :bottom :end nil))

(defn- axis-aligns
  "Resolve a normalized align into axis-relative packing for a container.
   Row: x acts on MAIN, y on CROSS. Column: swapped.
   -> {:main <:start|:center|:end|nil> :cross <:start|:center|:end|nil>}"
  [align row?]
  (let [x (:x align) y (:y align)]
    (if row?
      {:main (x->pack x) :cross (y->pack y)}
      {:main (y->pack y) :cross (x->pack x)})))

;; ============================================================================
;; Spacing / length helpers (axis-aware)
;; ============================================================================

(defn- main-gap
  "Main-axis gap between adjacent children (spacingXY aware)."
  [node]
  (let [s (:spacing (:attrs node))]
    (cond
      (nil? s)    0
      (number? s) s
      (vector? s) (let [[x y] s] (if (row? node) x y))
      :else       0)))

(defn- child-main-length
  "Normalized MAIN-axis length of a child in a container (row? -> width else height).
   Spacers default to :fill on the main axis."
  [node row?]
  (let [attrs (:attrs node)
        raw   (if row? (:width attrs) (:height attrs))
        raw   (if (and (nil? raw) (= :spacer (:type node))) :fill raw)]
    (normalize-length raw)))

(defn- child-cross-length
  "Normalized CROSS-axis length of a child (row? -> height else width)."
  [node row?]
  (normalize-length (if row? (:height (:attrs node)) (:width (:attrs node)))))

(defn- intr [node k] (get-in node [intrinsic-key k]))

;; ============================================================================
;; Inherited style channel (pure pre-walk; path -> effective style)
;; ============================================================================

(def style-keys
  "Attr keys that inherit down the tree (child-wins, else nearest ancestor)."
  [:font-size :color :weight :slant :font])

(defn collect-styles
  "path -> effective {:font-size :color :weight :slant :font}, inherited from the
   nearest ancestor that sets each key (elm-ui: child wins, else inherit)."
  ([tree] (collect-styles tree [] {} (atom {})))
  ([node path inherited acc]
   (let [eff (merge inherited (select-keys (:attrs node) style-keys))]
     (swap! acc assoc path eff)
     (doseq [[i c] (map-indexed vector (:children node))]
       (collect-styles c (conj path i) eff acc))
     @acc)))

;; ============================================================================
;; PASS 1 — MEASURE INTRINSIC (bottom-up, pure; memoizes ::intrinsic)
;; ============================================================================

(defn- shrink-contrib
  "The size a child contributes to a parent's intrinsic on an axis: :px -> n,
   :shrink -> content, :fill collapses to its shrink (content) size."
  [len content]
  (apply-clamp
   (case (:mode len)
     :px      (:px len)
     :shrink  content
     :portion content)
   len))

(defn- measure-intrinsic
  "Annotate node (and its subtree) with ::intrinsic {:w :h}. `measure-fn` is
   called only for measurable leaves as (measure-fn node path) -> [w h]."
  [node path measure-fn]
  (case (:type node)
    (:text :math :image)
    (let [[w h] (measure-fn node path)]
      (assoc node intrinsic-key {:w w :h h}))

    :spacer
    (let [wl (normalize-length (:width (:attrs node)))
          hl (normalize-length (:height (:attrs node)))]
      (assoc node intrinsic-key
             {:w (if (= :px (:mode wl)) (:px wl) 0)
              :h (if (= :px (:mode hl)) (:px hl) 0)}))

    ;; containers (:row :column :box :el)
    (let [children (vec (map-indexed
                         (fn [i c] (measure-intrinsic c (conj path i) measure-fn))
                         (:children node)))
          node     (assoc node :children children)
          pad      (normalize-padding (:padding (:attrs node)))
          pad-x    (+ (:left pad) (:right pad))
          pad-y    (+ (:top pad) (:bottom pad))
          gap      (main-gap node)
          n        (count children)
          row-node? (row? node)
          ;; box/el behave like a vertical (column) stack of their lone child
          mains    (map (fn [c] (shrink-contrib (child-main-length c row-node?)
                                                (intr c (if row-node? :w :h))))
                        children)
          crosses  (map (fn [c] (shrink-contrib (child-cross-length c row-node?)
                                                (intr c (if row-node? :h :w))))
                        children)
          main-total (+ (reduce + 0.0 mains) (* gap (max 0 (dec n))))
          cross-max  (if (seq crosses) (apply max crosses) 0.0)]
      (assoc node intrinsic-key
             (if row-node?
               {:w (+ main-total pad-x) :h (+ cross-max pad-y)}
               {:w (+ cross-max pad-x)  :h (+ main-total pad-y)})))))

;; ============================================================================
;; PASS 2 — RESOLVE + POSITION (top-down, pure; parent box known)
;; ============================================================================

(declare resolve-node)

(defn- resolve-size
  "Resolve a length to a concrete size given the child's intrinsic content size
   on that axis and the parent's available extent (for fill)."
  [len content extent]
  (apply-clamp
   (case (:mode len)
     :px      (:px len)
     :shrink  content
     :portion extent)
   len))

(defn- resolve-flow
  "Resolve a row (row?=true) or column (row?=false). Returns the placed children.
   Guarantees each child ::box lies within the container's content region."
  [node box row?]
  (let [children (:children node)
        n        (count children)]
    (if (zero? n)
      []
      (let [attrs   (:attrs node)
            pad     (normalize-padding (:padding attrs))
            cx0     (+ (:x box) (:left pad))
            cy0     (+ (:y box) (:top pad))
            CW      (max 0.0 (- (:w box) (:left pad) (:right pad)))
            CH      (max 0.0 (- (:h box) (:top pad) (:bottom pad)))
            gap     (main-gap node)
            gaps    (* gap (max 0 (dec n)))
            main-start (if row? cx0 cy0)
            M       (if row? CW CH)
            cross-start (if row? cy0 cx0)
            Xc      (if row? CH CW)
            calign  (axis-aligns (normalize-align (:align attrs)) row?)
            cont-main  (or (:main calign) :start)
            cont-cross (or (:cross calign) :center)
            ;; (a) MAIN sizes -----------------------------------------------
            main-lens (mapv #(child-main-length % row?) children)
            main-con  (mapv #(intr % (if row? :w :h)) children)
            fill?     (mapv #(= :portion (:mode %)) main-lens)
            fixed     (mapv (fn [len con f]
                              (when-not f
                                (apply-clamp (case (:mode len) :px (:px len) :shrink con) len)))
                            main-lens main-con fill?)
            avail     (max 0.0 (- M gaps))
            Sfixed    (reduce + 0.0 (keep identity fixed))
            leftover0 (max 0.0 (- avail Sfixed))
            fills     (vec (keep-indexed (fn [i f] (when f i)) fill?))
            P0        (reduce + 0.0 (map (fn [i] (:k (nth main-lens i))) fills))
            raw-fill  (fn [i leftover P]
                        (if (pos? P) (* leftover (/ (:k (nth main-lens i)) P)) 0.0))
            fs0       (into {} (map (fn [i] [i (apply-clamp (raw-fill i leftover0 P0) (nth main-lens i))]) fills))
            ;; ONE redistribution pass over the fills whose clamp bit (documented approx)
            clamped   (filterv (fn [i] (not= (raw-fill i leftover0 P0) (get fs0 i))) fills)
            fill-final
            (if (seq clamped)
              (let [cset (set clamped)
                    used (reduce + 0.0 (map #(get fs0 %) clamped))
                    left1 (max 0.0 (- leftover0 used))
                    rem   (filterv (complement cset) fills)
                    P1    (reduce + 0.0 (map (fn [i] (:k (nth main-lens i))) rem))
                    fs1   (into {} (map (fn [i] [i (apply-clamp (raw-fill i left1 P1) (nth main-lens i))]) rem))]
                (merge fs0 fs1))
              fs0)
            main-sizes0 (mapv (fn [i f] (if f (get fill-final i) (nth fixed i))) (range n) fill?)
            ;; safety net: no single child exceeds avail; proportional squeeze on overflow
            main-sizes1 (mapv #(min % avail) main-sizes0)
            sum1        (reduce + 0.0 main-sizes1)
            squeeze     (if (> (+ sum1 gaps) M)
                          (/ avail (max 1.0e-9 sum1))
                          1.0)
            main-sizes  (mapv #(* % squeeze) main-sizes1)
            ;; (b) CROSS sizes ----------------------------------------------
            cross-lens (mapv #(child-cross-length % row?) children)
            cross-con  (mapv #(intr % (if row? :h :w)) children)
            cross-sizes (mapv (fn [len con] (min Xc (resolve-size len con Xc))) cross-lens cross-con)
            ;; (c) MAIN packing (free space distribution + elm-ui push) ------
            sum-main   (reduce + 0.0 main-sizes)
            free       (max 0.0 (- M (+ sum-main gaps)))
            base-off   (case cont-main :start 0.0 :center (/ free 2) :end free)
            self-mains (mapv #(:main (axis-aligns (normalize-align (:align (:attrs %))) row?)) children)
            push-idx   (first (keep-indexed (fn [i a] (when (#{:center :end} a) i)) self-mains))
            inject     (if push-idx (case (nth self-mains push-idx) :center (/ free 2) :end free 0.0) 0.0)
            offset     (if push-idx 0.0 base-off)]
        ;; (d)+(e) place & recurse
        (loop [i 0, cursor (+ main-start offset), acc []]
          (if (= i n)
            acc
            (let [cur   (if (and push-idx (= i push-idx)) (+ cursor inject) cursor)
                  msize (nth main-sizes i)
                  csize (nth cross-sizes i)
                  child (nth children i)
                  center-main (+ cur (/ msize 2))
                  self-cross  (or (:cross (axis-aligns (normalize-align (:align (:attrs child))) row?))
                                  cont-cross)
                  cross-pos   (case self-cross
                                :start  cross-start
                                :center (+ cross-start (/ (- Xc csize) 2))
                                :end    (+ cross-start (- Xc csize)))
                  center-cross (+ cross-pos (/ csize 2))
                  [cx cy w h] (if row?
                                [center-main center-cross msize csize]
                                [center-cross center-main csize msize])
                  child-box {:x (- cx (/ w 2)) :y (- cy (/ h 2)) :w w :h h}]
              (recur (inc i)
                     (+ cur msize gap)
                     (conj acc (resolve-node child child-box))))))))))

(defn- resolve-boxel
  "Position a box/el's lone child inside the content region by its self-align
   (default :center on both axes). bg/border spans the full box (drawn in pass 3)."
  [node box]
  (let [child (first (:children node))]
    (if (nil? child)
      []
      (let [pad (normalize-padding (:padding (:attrs node)))
            cx0 (+ (:x box) (:left pad))
            cy0 (+ (:y box) (:top pad))
            CW  (max 0.0 (- (:w box) (:left pad) (:right pad)))
            CH  (max 0.0 (- (:h box) (:top pad) (:bottom pad)))
            wl  (normalize-length (:width (:attrs child)))
            hl  (normalize-length (:height (:attrs child)))
            cw  (min CW (resolve-size wl (intr child :w) CW))
            ch  (min CH (resolve-size hl (intr child :h) CH))
            ca  (normalize-align (:align (:attrs child)))
            ax  (or (:x ca) :center)
            ay  (or (:y ca) :center)
            x   (case ax :left cx0 :center (+ cx0 (/ (- CW cw) 2)) :right (+ cx0 (- CW cw)))
            y   (case ay :top cy0 :center (+ cy0 (/ (- CH ch) 2)) :bottom (+ cy0 (- CH ch)))]
        [(resolve-node child {:x x :y y :w cw :h ch})]))))

(defn- resolve-node
  "Assign a node its ::box and recurse into its children."
  [node box]
  (let [node (assoc node box-key box)]
    (case (:type node)
      (:text :math :image :spacer) node
      :row       (assoc node :children (resolve-flow node box true))
      :column    (assoc node :children (resolve-flow node box false))
      (:box :el) (assoc node :children (resolve-boxel node box)))))

;; ============================================================================
;; Public resolver (pass 1 + pass 2), fully pure
;; ============================================================================

(defn resolve-layout
  "Resolve `tree` inside `root-box` {:x :y :w :h} using injected
   `measure-fn : [node path] -> [w h]`. Returns the tree annotated with
   ::intrinsic (pass 1) and ::box (pass 2). No libpython, no I/O."
  [tree root-box measure-fn]
  (-> tree
      (measure-intrinsic [] measure-fn)
      (resolve-node root-box)))

;; ============================================================================
;; Tree utilities (used by tests / realize)
;; ============================================================================

(defn walk-boxes
  "Depth-first seq of [path node] for every node carrying a ::box."
  ([node] (walk-boxes node []))
  ([node path]
   (cons [path node]
         (mapcat (fn [i c] (walk-boxes c (conj path i)))
                 (range) (:children node)))))

(defn content-box
  "The inner content region of a node's ::box after its padding."
  [node]
  (let [b   (get node box-key)
        pad (normalize-padding (:padding (:attrs node)))]
    {:x (+ (:x b) (:left pad))
     :y (+ (:y b) (:top pad))
     :w (max 0.0 (- (:w b) (:left pad) (:right pad)))
     :h (max 0.0 (- (:h b) (:top pad) (:bottom pad)))}))

(defn within?
  "True when inner box lies within outer box (with a small epsilon)."
  ([inner outer] (within? inner outer 1.0e-6))
  ([inner outer eps]
   (and (>= (:x inner) (- (:x outer) eps))
        (>= (:y inner) (- (:y outer) eps))
        (<= (+ (:x inner) (:w inner)) (+ (:x outer) (:w outer) eps))
        (<= (+ (:y inner) (:h inner)) (+ (:y outer) (:h outer) eps)))))
