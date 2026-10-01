(ns desargues.logic.scene.grid
  "The grid flow: a relation R on a finite class A as an n x n grid whose
   cell (x, y) (row x, column y) is filled when x R y.

     (beats {:relation R :carrier A :name 'R})

   Phases: :draw (grid, labels, filled pairs), :reflexive (the diagonal, gaps
   red), :symmetric (each pair against its mirror, missing mirrors red),
   :transitive (a path x R y R z, its closing cell (x, z) red when missing,
   teal when present), and, for an equivalence, :classes (rows and columns
   reordered so each class is a block on the diagonal, blocks coloured) and
   the quotient A/R. Values come from desargues.logic.model; beats are
   desargues.logic.scene.beats data, played by its `play!`."
  (:require [desargues.logic.model :as m]
            [desargues.logic.scene.beats :as b]
            [desargues.logic.scene.figure :as fig]
            [desargues.logic.tex :as lt]
            [desargues.scene :as s]
            [desargues.scene.data :as rec]))

;; =============================================================================
;; Values
;; =============================================================================

(defn- holds? [op r a] (m/denote (list op 'R 'A) {'R r 'A a}))

(defn transitivity-failures
  "Every [x y z] with x R y, y R z and not x R z, in order."
  [r]
  (vec (for [[x y] (sort-by pr-str r) [y' z] (sort-by pr-str r)
             :when (and (= y y') (not (contains? r [x z])))]
         [x y z])))

(defn analyse
  "Facts of relation r on class a: {:reflexive? :missing-diagonal
   :symmetric? :missing-mirrors :transitive? :failures :witness :equivalence?
   :classes (an ordered vector of classes, for an equivalence)}."
  [{:keys [relation carrier]}]
  (let [r (set relation) a (set carrier)
        es (fig/ordered a)
        failures (transitivity-failures r)
        equivalence? (holds? 'equivalence r a)
        quotient (when equivalence? (m/denote '(quotient A R) {'R r 'A a}))]
    {:reflexive? (holds? 'reflexive r a)
     :missing-diagonal (filterv #(not (contains? r [% %])) es)
     :symmetric? (m/denote '(symmetric R) {'R r})
     :missing-mirrors (vec (for [[x y] (sort-by pr-str r) :when (not (contains? r [y x]))] [y x]))
     :transitive? (empty? failures)
     :failures failures
     :witness (first (sort-by (fn [[x _ z]] (if (= x z) 1 0))
                              (for [[x y] (sort-by pr-str r) [y' z] (sort-by pr-str r)
                                    :when (and (= y y') (not= x y) (not= y z))]
                                [x y z])))
     :equivalence? equivalence?
     :classes (when quotient
                (vec (sort-by #(.indexOf ^java.util.List es (first (fig/ordered %)))
                              (map set quotient))))}))

;; =============================================================================
;; Layout (world units)
;; =============================================================================

(def layout
  "Geometry of the flow: the grid sits left of centre, notes on the right."
  {:title-y 3.45 :caption-y -3.45 :center [-2.2 -0.2] :max-cell 0.8 :span 5.4
   :label-size 26 :note-x 3.9 :note-w 5.6})

(defn grid
  "Geometry of an n x n grid over the ordered elements es:
   {:cell s :x0 :y0 :index {elem k}} with cell (row i, col j) centred at
   (x0 + s (j + 1/2), y0 - s (i + 1/2))."
  [es]
  (let [n (max 1 (count es))
        {:keys [center max-cell span]} layout
        s (min max-cell (/ span n))
        [cx cy] center]
    {:cell s :n n :es (vec es)
     :x0 (- cx (/ (* s n) 2)) :y0 (+ cy (/ (* s n) 2))
     :index (zipmap es (range))}))

(defn cell-at
  "Centre of cell (row of x, column of y)."
  [{:keys [cell x0 y0 index]} [x y]]
  [(+ x0 (* cell (+ (index y) 0.5))) (- y0 (* cell (+ (index x) 0.5)))])

(defn- row-label-at [{:keys [cell x0 y0 index]} x]
  [(- x0 0.35) (- y0 (* cell (+ (index x) 0.5)))])

(defn- col-label-at [{:keys [cell x0 y0 index]} y]
  [(+ x0 (* cell (+ (index y) 0.5))) (+ y0 0.32)])

;; =============================================================================
;; Items and beats (pure)
;; =============================================================================

(def class-colors
  "Colours of the equivalence classes, cycled."
  [:gold :teal :orange :green :purple :pink])

(defn- pair-id [p] [:pair p])
(defn- mark-id [tag p] [:mark tag p])

(defn- grid-lines [{:keys [cell n x0 y0]}]
  (let [x1 (+ x0 (* cell n)) y1 (- y0 (* cell n))]
    (vec (concat (for [i (range (inc n))]
                   (fig/line [:hline i] [x0 (- y0 (* cell i))] [x1 (- y0 (* cell i))] :grey 1))
                 (for [j (range (inc n))]
                   (fig/line [:vline j] [(+ x0 (* cell j)) y0] [(+ x0 (* cell j)) y1] :grey 1))))))

(defn- labels [g es]
  (vec (for [e es
             it [(fig/tex [:row e] (fig/elem-tex e) (row-label-at g e) (:label-size layout) :white)
                 (fig/tex [:col e] (fig/elem-tex e) (col-label-at g e) (:label-size layout) :white)]]
         it)))

(defn- pair-box [g p color] (fig/box (pair-id p) (cell-at g p) (let [s (* 0.8 (:cell g))] [s s]) color true))

(defn- mark-box [g tag p color]
  (fig/box (mark-id tag p) (cell-at g p) (let [s (* 0.92 (:cell g))] [s s]) color false))

(defn- recolor [ids color] {:beat :recolor :ids (vec ids) :color color})

(defn- title [latex]
  {:beat :swap :ids [:title] :items [(fig/tex :title latex [0.0 (:title-y layout)] 34 :white)]})

(defn- say [t color] (fig/caption t (:caption-y layout) color))

(defn- note [t y color]
  (let [{:keys [note-x note-w]} layout] (fig/note [:note y] t [note-x y] note-w color)))

(defn- verdict-note [y ok? yes no] (note (if ok? yes no) y (if ok? :teal :red)))

(defn- clear-marks [ids] (if (seq ids) [{:beat :hide :ids (vec ids)}] []))

(defn- reflexive-phase [g es {:keys [reflexive? missing-diagonal]}]
  (let [diag (map (fn [e] [e e]) es)
        marks (map (fn [p] (mark-box g :diag p (if (some #{(first p)} missing-diagonal) :red :gold))) diag)]
    (concat
     [(say "reflexive: every diagonal cell (x, x) is filled" :white)
      {:beat :show :items (vec marks)}
      (verdict-note 2.2 reflexive? "reflexive" "not reflexive")
      {:beat :hold :seconds 1.2}]
     (clear-marks (map :id marks)))))

(defn- symmetric-phase [g r {:keys [symmetric? missing-mirrors]}]
  (let [{:keys [x0 y0 cell n]} g
        axis (fig/line :axis [x0 y0] [(+ x0 (* cell n)) (- y0 (* cell n))] :gold 2)
        off (remove (fn [[x y]] (= x y)) r)
        marks (map #(mark-box g :mirror % :red) missing-mirrors)]
    (concat
     [(say "symmetric: every filled cell has its mirror across the diagonal" :white)
      {:beat :show :items [axis]}
      (recolor (map pair-id off) :teal)]
     (when (seq marks) [{:beat :show :items (vec marks)}])
     [(verdict-note 1.5 symmetric? "symmetric" "not symmetric")
      {:beat :hold :seconds 1.2}
      (recolor (map pair-id off) :blue)]
     (clear-marks (cons :axis (map :id marks))))))

(defn- transitive-phase [g {:keys [transitive? failures witness]}]
  (let [[x y z :as path] (or (first failures) witness)
        closing (when path [x z])
        marks (when path
                [(mark-box g :path [x y] :gold)
                 (mark-box g :path [y z] :gold)
                 (mark-box g :close closing (if transitive? :teal :red))])]
    (concat
     [(say "transitive: x R y and y R z force x R z" :white)]
     (when path
       [{:beat :show :items (subvec marks 0 2)}
        {:beat :show :items [(peek marks)]}])
     [(verdict-note 0.8 transitive? "transitive" "not transitive")
      {:beat :hold :seconds 1.2}]
     (clear-marks (map :id marks)))))

(defn- classes-phase [g0 r a-name {:keys [classes]}]
  (let [es (vec (mapcat fig/ordered classes))
        g (grid es)
        color-of (into {} (for [[k c] (map-indexed vector classes) e c]
                            [e (class-colors (mod k (count class-colors)))]))
        blocks (for [[k c] (map-indexed vector classes)
                     :let [cs (fig/ordered c)
                           [x0 y0] (cell-at g [(first cs) (first cs)])
                           [x1 y1] (cell-at g [(peek cs) (peek cs)])
                           side (* (:cell g) (count cs))]]
                 (fig/box [:block k] [(/ (+ x0 x1) 2) (/ (+ y0 y1) 2)] [side side]
                          (class-colors (mod k (count class-colors))) false))]
    [(say "an equivalence: reorder A so each class is a block" :white)
     {:beat :move :to (into {} (concat (for [p r] [(pair-id p) (cell-at g p)])
                                       (for [e es] [[:row e] (row-label-at g e)])
                                       (for [e es] [[:col e] (col-label-at g e)])))}
     {:beat :show :items (vec blocks)}
     {:beat :par :beats (vec (for [[k c] (map-indexed vector classes)]
                               (recolor (for [p r :when (contains? c (first p))] (pair-id p))
                                        (class-colors (mod k (count class-colors))))))}
     {:beat :par :beats (vec (for [e es] (recolor [[:row e] [:col e]] (color-of e))))}
     (fig/tex-caption (str "A/" (lt/->TeX a-name) " = " (fig/elem-tex (set classes)))
                      (:caption-y layout) :white)
     {:beat :hold :seconds 2.0}]))

(defn beats
  "The grid flow of {:relation :carrier :name} as beats (see ns doc)."
  [{:keys [relation carrier name] :or {name 'R} :as spec}]
  (let [r (set relation)
        es (fig/ordered carrier)
        g (grid es)
        facts (analyse spec)]
    (vec
     (concat
      [(title (str (lt/->TeX name) " \\subseteq A \\times A"))
       {:beat :show :items (grid-lines g)}
       {:beat :show :items (labels g es)}
       {:beat :show :items (mapv #(pair-box g % :blue) (sort-by pr-str r))}
       {:beat :hold :seconds 0.8}]
      (reflexive-phase g es facts)
      (symmetric-phase g r facts)
      (transitive-phase g facts)
      (if (:equivalence? facts)
        (cons (note "an equivalence relation" 0.1 :teal) (classes-phase g r name facts))
        [(note "not an equivalence relation" 0.1 :red)
         {:beat :hold :seconds 1.5}])))))

;; =============================================================================
;; Boundary
;; =============================================================================

(defn construct
  "A construct fn playing the grid flow of spec."
  [spec]
  (let [bs (beats spec)] (fn [stage] (b/play! stage bs))))

(defn record
  "The grid flow of spec recorded as an EDN scene graph."
  [scene-name spec]
  (s/with-backend (rec/recording-backend)
    (s/render! scene-name (construct spec))))
