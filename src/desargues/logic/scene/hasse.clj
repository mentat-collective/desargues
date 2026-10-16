(ns desargues.logic.scene.hasse
  "The Hasse flow: a finite partial order as a Hasse diagram, layered by rank
   with only the covering pairs drawn.

     (beats {:carrier A :order <=} {:subset B})

   Phases: :draw (layers bottom up, then the covers), :extremes (maximal and
   minimal elements, greatest and least), :bounds for a subset B (upper
   bounds, sup or the clash of minimal upper bounds in red; lower bounds, inf),
   :lattice (the first pair without a join or meet, its minimal upper or
   maximal lower bounds red; or, for a lattice, its join and meet tables).
   Values come from desargues.logic.model (the order is the model letter <=);
   beats are desargues.logic.scene.beats data, played by its `play!`."
  (:require [desargues.logic.model :as m]
            [desargues.logic.scene.beats :as b]
            [desargues.logic.scene.figure :as fig]
            [desargues.scene :as s]
            [desargues.scene.data :as rec]))

;; =============================================================================
;; Values
;; =============================================================================

(defn reflexive-closure
  "The order of spec with every (x, x) added."
  [{:keys [carrier order]}]
  (into (set order) (map (fn [x] [x x])) carrier))

(defn- env [{:keys [carrier] :as spec} bindings]
  (merge {::m/universe (set carrier) m/order-letter (reflexive-closure spec)} bindings))

(defn value
  "The value of form in the model of poset spec with extra letter bindings."
  [spec form bindings]
  (m/denote form (env spec bindings)))

(defn- lt? [r x y] (and (not= x y) (contains? r [x y])))

(defn covers
  "The covering pairs [x y] (x < y, nothing strictly between), ordered."
  [{:keys [carrier] :as spec}]
  (let [r (reflexive-closure spec)]
    (vec (for [x (fig/ordered carrier) y (fig/ordered carrier)
               :when (and (lt? r x y) (not-any? #(and (lt? r x %) (lt? r % y)) carrier))]
           [x y]))))

(defn ranks
  "{x rank}: 0 for a minimal element, else one more than its highest lower
   cover."
  [{:keys [carrier] :as spec}]
  (let [below (group-by second (covers spec))]
    (reduce (fn [acc x]
              (letfn [(rk [acc x]
                        (if (contains? acc x)
                          acc
                          (let [acc (reduce rk acc (map first (below x)))]
                            (assoc acc x (if-let [ls (seq (below x))]
                                           (inc (apply max (map #(acc (first %)) ls)))
                                           0)))))]
                (rk acc x)))
            {} (fig/ordered carrier))))

(defn- extremal [spec op xs]
  (filterv #(value spec (list op 'm 'S) {'m % 'S (set xs)}) (fig/ordered xs)))

(defn bounds
  "Bounds of subset B: {:upper :lower :sup :inf :minimal-upper :maximal-lower},
   :sup / :inf nil when undefined."
  [spec bs]
  (let [bs (set bs)
        up (value spec '(upper-bounds B) {'B bs})
        lo (value spec '(lower-bounds B) {'B bs})
        sup (value spec '(sup B) {'B bs})
        inf (value spec '(inf B) {'B bs})]
    {:upper (fig/ordered up) :lower (fig/ordered lo)
     :sup (when (m/defined? sup) sup) :inf (when (m/defined? inf) inf)
     :minimal-upper (extremal spec 'minimal up)
     :maximal-lower (extremal spec 'maximal lo)}))

(defn analyse
  "Facts of poset spec: {:maximal :minimal :greatest :least :lattice?
   :failure ({:pair [x y] :missing :join|:meet :culprits [..]} or nil)}."
  [{:keys [carrier] :as spec}]
  (let [es (fig/ordered carrier)
        failure (first (for [[i x] (map-indexed vector es) y (drop (inc i) es)
                             :let [{:keys [sup inf minimal-upper maximal-lower]} (bounds spec [x y])]
                             :when (or (nil? sup) (nil? inf))]
                         (if (nil? sup)
                           {:pair [x y] :missing :join :culprits minimal-upper}
                           {:pair [x y] :missing :meet :culprits maximal-lower})))]
    {:maximal (extremal spec 'maximal carrier)
     :minimal (extremal spec 'minimal carrier)
     :greatest (first (filter #(value spec '(greatest m S) {'m % 'S (set carrier)}) es))
     :least (first (filter #(value spec '(least m S) {'m % 'S (set carrier)}) es))
     :lattice? (nil? failure)
     :failure failure}))

(defn op-table
  "{[x y] (join x y)} or meet, over the carrier of a lattice."
  [{:keys [carrier] :as spec} op]
  (into {} (for [x carrier y carrier] [[x y] (value spec (list op 'x 'y) {'x x 'y y})])))

;; =============================================================================
;; Layout (world units)
;; =============================================================================

(def layout
  "Geometry: the diagram on the left, notes and tables on the right."
  {:title-y 3.45 :caption-y -3.45 :x-mid -3.6 :width 5.4 :top 2.55 :bottom -2.75
   :max-dy 1.3 :max-dx 1.4 :dot-r 0.1 :label-size 26 :note-x 3.5 :note-w 6.6
   :table-top 2.6 :table-w 3.25 :table-x [0.0 3.75]})

(defn positions
  "{x [px py]}: layers by rank bottom up, each layer ordered by the mean
   position of its lower covers, then by `fig/ordered`."
  [spec]
  (let [rk (ranks spec)
        below (group-by second (covers spec))
        layers (->> (group-by rk (keys rk)) (sort-by key) (mapv (comp fig/ordered val)))
        {:keys [x-mid width top bottom max-dy max-dx]} layout
        nl (count layers)
        dy (if (< nl 2) 0 (min max-dy (/ (- top bottom) (dec nl))))
        y0 (- (/ (+ top bottom) 2) (* dy (/ (dec nl) 2)))]
    (reduce (fn [pos [i layer]]
              (let [bary (fn [x] (let [ls (map #(first (pos (first %))) (below x))]
                                   (if (seq ls) (/ (reduce + ls) (count ls)) x-mid)))
                    layer (vec (sort-by bary layer))
                    k (count layer)
                    dx (if (< k 2) 0 (min max-dx (/ width (dec k))))
                    x0 (- x-mid (* dx (/ (dec k) 2)))]
                (into pos (map-indexed (fn [j x] [x [(+ x0 (* j dx)) (+ y0 (* i dy))]]) layer))))
            {} (map-indexed vector layers))))

;; =============================================================================
;; Items and beats (pure)
;; =============================================================================

(defn- dot-id [x] [:dot x])
(defn- label-id [x] [:label x])
(defn- recolor [ids color] {:beat :recolor :ids (vec ids) :color color})

(defn- title [latex]
  {:beat :swap :ids [:title] :items [(fig/tex :title latex [0.0 (:title-y layout)] 34 :white)]})

(defn- say [t color] (fig/caption t (:caption-y layout) color))

(defn- note [k t color]
  (let [{:keys [note-x note-w]} layout] (fig/note [:note k] t [note-x (- 2.3 (* 0.6 k))] note-w color)))

(defn- clear-notes [ks] {:beat :hide :ids (mapv #(vector :note %) ks)})

(defn- list-text [xs] (if (seq xs) (apply str (interpose ", " (map str xs))) "none"))

(defn- draw-phase [spec pos]
  (let [rk (ranks spec)
        layers (->> (group-by rk (keys rk)) (sort-by key) (map (comp fig/ordered val)))]
    (concat
     (for [layer layers]
       {:beat :show
        :items (vec (for [x layer :let [[px py] (pos x)]
                          it [(fig/dot (dot-id x) [px py] (:dot-r layout) :white)
                              (fig/tex (label-id x) (fig/elem-tex x) [(+ px 0.32) (+ py 0.12)]
                                       (:label-size layout) :white)]]
                      it))})
     [{:beat :show :items (vec (mapcat (fn [[x y]] (fig/polyline [:cover x y] [(pos x) (pos y)] :grey 2))
                                       (covers spec)))}
      {:beat :hold :seconds 0.8}])))

(defn- restore [carrier] (recolor (map dot-id (fig/ordered carrier)) :white))

(defn- extremes-phase [{:keys [carrier]} {:keys [maximal minimal greatest least]}]
  [(say "maximal: nothing above it; minimal: nothing below it" :white)
   (recolor (map dot-id maximal) :gold)
   (note 0 (str "maximal: " (list-text maximal)) :gold)
   (note 1 (str "greatest: " (if (some? greatest) greatest "none")) (if (some? greatest) :gold :red))
   (recolor (map dot-id minimal) :teal)
   (note 2 (str "minimal: " (list-text minimal)) :teal)
   (note 3 (str "least: " (if (some? least) least "none")) (if (some? least) :teal :red))
   {:beat :hold :seconds 1.5}
   (restore carrier)
   (clear-notes [0 1 2 3])])

(defn- bounds-phase [{:keys [carrier] :as spec} bs]
  (let [{:keys [upper lower sup inf minimal-upper maximal-lower]} (bounds spec bs)
        bs (fig/ordered bs)]
    [(say (str "the subset B = {" (list-text bs) "}") :gold)
     (recolor (map dot-id bs) :gold)
     (recolor (map dot-id (remove (set bs) upper)) :teal)
     (note 0 (str "upper bounds: " (list-text upper)) :teal)
     (if sup
       (recolor [(dot-id sup)] :green)
       (recolor (map dot-id minimal-upper) :red))
     (note 1 (if sup (str "sup B = " sup) (str "no sup: minimal upper bounds " (list-text minimal-upper)))
           (if sup :green :red))
     {:beat :hold :seconds 1.2}
     (restore carrier)
     (recolor (map dot-id bs) :gold)
     (recolor (map dot-id (remove (set bs) lower)) :orange)
     (note 2 (str "lower bounds: " (list-text lower)) :orange)
     (if inf
       (recolor [(dot-id inf)] :green)
       (recolor (map dot-id maximal-lower) :red))
     (note 3 (if inf (str "inf B = " inf) (str "no inf: maximal lower bounds " (list-text maximal-lower)))
           (if inf :green :red))
     {:beat :hold :seconds 1.5}
     (restore carrier)
     (clear-notes [0 1 2 3])]))

(defn table-items
  "Items of the op table of lattice spec at left edge x0: header row and
   column, then one row of cells per element (ids [:table op ...])."
  [spec op x0]
  (let [es (fig/ordered (:carrier spec))
        n (count es)
        {:keys [table-top table-w]} layout
        c (/ table-w (inc n))
        tbl (op-table spec op)
        size (fn [latex] (b/fit-size {:kind :tex :latex latex :size 24} (* 0.92 c)))
        at (fn [i j] [(+ x0 (* c (+ j 0.5))) (- table-top (* c (+ i 0.5)))])
        cell (fn [id latex i j color] (fig/tex id latex (at i j) (size latex) color))
        sym (if (= op 'join) "\\vee" "\\wedge")]
    {:frame (into [(cell [:table op :corner] sym 0 0 :gold)]
                  (concat (for [[j x] (map-indexed vector es)] (cell [:table op :col x] (fig/elem-tex x) 0 (inc j) :gold))
                          (for [[i x] (map-indexed vector es)] (cell [:table op :row x] (fig/elem-tex x) (inc i) 0 :gold))))
     :rows (vec (for [[i x] (map-indexed vector es)]
                  (vec (for [[j y] (map-indexed vector es)]
                         (cell [:table op x y] (fig/elem-tex (tbl [x y])) (inc i) (inc j) :white)))))}))

(defn- lattice-phase [spec {:keys [lattice? failure]}]
  (if-not lattice?
    (let [{[x y] :pair :keys [missing culprits]} failure
          word (if (= :join missing) "join" "meet")]
      [(say "a lattice: every pair has a join (sup) and a meet (inf)" :white)
       (recolor (map dot-id [x y]) :gold)
       (recolor (map dot-id culprits) :red)
       (note 0 (str x " and " y " have no " word ": " (list-text culprits)
                    (if (= :join missing) " are minimal upper bounds" " are maximal lower bounds"))
             :red)
       (note 1 "not a lattice" :red)
       {:beat :hold :seconds 2.0}])
    (let [[xj xm] (:table-x layout)
          j (table-items spec 'join xj)
          mt (table-items spec 'meet xm)]
      (concat
       [(say "a lattice: every pair has a join and a meet" :teal)
        {:beat :show :items (into (:frame j) (:frame mt))}]
       (map (fn [rj rm] {:beat :show :items (into rj rm)}) (:rows j) (:rows mt))
       [{:beat :hold :seconds 2.0}]))))

(defn beats
  "The Hasse flow of poset {:carrier :order} as beats (see ns doc).
   opts: :subset B for the bounds phase."
  ([spec] (beats spec {}))
  ([spec {:keys [subset]}]
   (let [pos (positions spec)
         facts (analyse spec)]
     (vec
      (concat
       [(title "(A, \\le)")]
       (draw-phase spec pos)
       (extremes-phase spec facts)
       (when (seq subset) (bounds-phase spec subset))
       (lattice-phase spec facts))))))

;; =============================================================================
;; Boundary
;; =============================================================================

(defn construct
  "A construct fn playing the Hasse flow of spec."
  ([spec] (construct spec {}))
  ([spec opts] (let [bs (beats spec opts)] (fn [stage] (b/play! stage bs)))))

(defn record
  "The Hasse flow of spec recorded as an EDN scene graph."
  ([scene-name spec] (record scene-name spec {}))
  ([scene-name spec opts]
   (s/with-backend (rec/recording-backend)
     (s/render! scene-name (construct spec opts)))))
