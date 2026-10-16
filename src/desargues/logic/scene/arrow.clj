(ns desargues.logic.scene.arrow
  "The arrow flow: a function or relation between finite classes drawn as
   columns of labelled dots with an arrow per pair.

     (beats {:graph g :dom A :cod B :name 'f} opts)  ; A -> B, two columns
     (beats {:graph r :dom A :name 'R} opts)         ; a relation on A: a ring
     (compose-beats {:f f :g g :A A :B B :C C})      ; g o f, three columns

   opts: :image C (f[C] in gold), :preimage D (the preimage of D in teal).
   Phases: :function (every dot of A sends exactly one arrow), :injective (two
   arrows into one dot, red), :surjective (dots of B no arrow reaches, red),
   then the image and preimage. Values come from desargues.logic.model; beats
   are desargues.logic.scene.beats data, played by its `play!`.

     (record :f {:graph #{[1 :a] [2 :a]} :dom #{1 2} :cod #{:a :b} :name 'f})"
  (:require [desargues.logic.model :as m]
            [desargues.logic.scene.beats :as b]
            [desargues.logic.scene.figure :as fig]
            [desargues.logic.tex :as lt]
            [desargues.scene :as s]
            [desargues.scene.data :as rec]))

;; =============================================================================
;; Values
;; =============================================================================

(defn value
  "The value of form with letters bound by env, through desargues.logic.model."
  [form env]
  (m/denote form env))

(defn analyse
  "Facts of graph g from A to B (B defaults to A):
   {:function? :lonely (A dots sending no arrow) :forked (A dots sending
    several) :collisions {y [x ...]} (B dots several arrows reach) :gaps (B
    dots no arrow reaches) :injective? :surjective?}."
  [{:keys [graph dom cod]}]
  (let [cod (or cod dom)
        env {'f graph 'A dom 'B cod}
        out (group-by first graph)
        in (group-by second graph)
        function? (value '(maps f A B) env)]
    {:function? function?
     :lonely (filterv #(empty? (out %)) (fig/ordered dom))
     :forked (filterv #(< 1 (count (out %))) (fig/ordered dom))
     :collisions (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
                       (for [[y ps] in :when (< 1 (count ps))] [y (fig/ordered (map first ps))]))
     :gaps (filterv #(empty? (in %)) (fig/ordered cod))
     :injective? (and function? (value '(injective f) env))
     :surjective? (and function? (value '(surjective f A B) env))}))

;; =============================================================================
;; Layout (world units)
;; =============================================================================

(def layout
  "Geometry of the flow."
  {:title-y 3.45 :caption-y -3.45 :top 2.15 :bottom -2.45 :max-gap 0.95
   :dot-r 0.09 :label-dx 0.42 :label-size 26 :box-w 1.5 :ring-r 2.0 :ring-center [0.0 -0.15]})

(defn- column-ys [n]
  (let [{:keys [top bottom max-gap]} layout
        gap (if (< n 2) 0 (min max-gap (/ (- top bottom) (dec n))))
        mid (/ (+ top bottom) 2)
        y0 (+ mid (* gap (/ (dec n) 2)))]
    (mapv #(- y0 (* gap %)) (range n))))

(defn column
  "Positions of class xs in a column at x: {:x :side :points {elem [x y]}
   :box {:at :size}}. side is :left, :right or :mid (where labels go)."
  [xs x side]
  (let [es (fig/ordered xs)
        ys (column-ys (count es))
        span (if (seq ys) (- (first ys) (peek ys)) 0)]
    {:x x :side side
     :elems es
     :points (zipmap es (map (fn [y] [x y]) ys))
     :box {:at [x (if (seq ys) (/ (+ (first ys) (peek ys)) 2) 0.0)]
           :size [(:box-w layout) (+ span 0.9)]}}))

(defn ring
  "Positions of class xs on a circle (a relation on one class)."
  [xs]
  (let [es (fig/ordered xs)
        n (count es)
        {:keys [ring-r ring-center]} layout
        [cx cy] ring-center]
    {:side :ring :elems es :center ring-center
     :points (zipmap es (map (fn [k] (let [t (+ (/ Math/PI 2) (* -2 Math/PI (/ k (max n 1))))]
                                       [(+ cx (* ring-r (Math/cos t))) (+ cy (* ring-r (Math/sin t)))]))
                             (range n)))}))

(defn- outward [{:keys [center]} [x y]]
  (let [[cx cy] center dx (- x cx) dy (- y cy) n (max 1e-9 (Math/hypot dx dy))]
    [(/ dx n) (/ dy n)]))

(defn- label-at [{:keys [side] :as col} [x y]]
  (let [d (:label-dx layout)]
    (case side
      :left [(- x d) y]
      :right [(+ x d) y]
      :mid [x (+ y 0.27)]
      :ring (let [[ux uy] (outward col [x y])] [(+ x (* 0.4 ux)) (+ y (* 0.4 uy))]))))

;; =============================================================================
;; Items and beats (pure)
;; =============================================================================

(defn- dot-id [k x] [:dot k x])

(defn- column-items
  "Box and set name of column k (no box on a ring)."
  [k col set-latex]
  (if (= :ring (:side col))
    [(fig/tex [:set k] set-latex [-4.6 2.4] 34 :white)]
    (let [{:keys [at size]} (:box col)
          [x y] at]
      [(fig/box [:box k] at size :grey false)
       (fig/tex [:set k] set-latex [x (+ y (/ (second size) 2) 0.32)] 32 :white)])))

(defn- point-items [k col]
  (vec (for [e (:elems col) :let [p (get-in col [:points e])]
             it [(fig/dot (dot-id k e) p (:dot-r layout) :white)
                 (fig/tex [:label k e] (fig/elem-tex e) (label-at col p) (:label-size layout) :white)]]
         it)))

(defn- arrow-id [k [x y]] [:arrow k x y])

(defn- arrow-items
  "Line items of the arrow for pair [x y] from column a to column c."
  [k a c [x y] color]
  (let [p (get-in a [:points x]) q (get-in c [:points y])]
    (cond
      (or (nil? p) (nil? q)) []
      (and (= :ring (:side a)) (= x y)) (fig/loop-arrow (arrow-id k [x y]) p (outward a p) color)
      (= :ring (:side a))
      (let [[px py] p [qx qy] q dx (- qx px) dy (- qy py) n (max 1e-9 (Math/hypot dx dy))
            off [(* 0.07 (/ (- dy) n)) (* 0.07 (/ dx n))]]
        (fig/arrow (arrow-id k [x y]) (mapv + p off) (mapv + q off) color :gap 0.16))
      :else (fig/arrow (arrow-id k [x y]) p q color :gap 0.16))))

(defn- ids-of [its] (mapv :id its))

(defn- arrows-ids [k a c pairs]
  (vec (mapcat #(ids-of (arrow-items k a c % :white)) pairs)))

(defn- recolor [ids color] {:beat :recolor :ids (vec ids) :color color})

(defn- sorted-pairs [g] (sort-by (juxt (comp pr-str first) (comp pr-str second)) g))

(defn- title [latex]
  {:beat :swap :ids [:title] :items [(fig/tex :title latex [0.0 (:title-y layout)] 34 :white)]})

(defn- say [t color] (fig/caption t (:caption-y layout) color))
(defn- say-tex [l color] (fig/tex-caption l (:caption-y layout) color))

(defn- draw-phase
  "Columns, dots, then the arrows of each dot of the source column."
  [cols set-names k a c graph]
  (let [by-x (group-by first (sorted-pairs graph))]
    (concat
     [{:beat :show :items (vec (mapcat (fn [[kk col] l] (column-items kk col l)) cols set-names))}
      {:beat :par :beats (mapv (fn [[kk col]] {:beat :show :items (point-items kk col)}) cols)}]
     (for [x (:elems a) :let [ps (by-x x)] :when ps]
       {:beat :show :items (vec (mapcat #(arrow-items k a c % :white) ps))}))))

(defn- restore [k a c graph a-key c-key]
  {:beat :par
   :beats [(recolor (arrows-ids k a c graph) :white)
           (recolor (concat (map #(dot-id a-key %) (:elems a)) (map #(dot-id c-key %) (:elems c))) :white)]})

(defn- function-phase [{:keys [lonely forked function?]} k a c graph name]
  (let [by-x (group-by first graph)
        bad (concat lonely forked)]
    (concat
     [(say (str "a function sends exactly one arrow from every dot of the domain") :white)]
     (when (seq bad)
       [{:beat :par :beats [(recolor (map #(dot-id :dom %) bad) :red)
                            (recolor (arrows-ids k a c (mapcat by-x forked)) :red)]}])
     [(say (if function?
             (str name " is a function")
             (str name " is not a function: " (count lonely) " dots send no arrow, "
                  (count forked) " send several"))
           (if function? :teal :red))
      {:beat :hold :seconds 1.2}])))

(defn- injective-phase [{:keys [collisions injective?]} k a c graph name]
  (let [into-y (group-by second graph)]
    (concat
     [(say "injective: no two arrows end at the same dot" :white)]
     (when (seq collisions)
       [{:beat :par :beats [(recolor (arrows-ids k a c (mapcat into-y (keys collisions))) :red)
                            (recolor (map #(dot-id :cod %) (keys collisions)) :red)]}])
     [(say (if injective?
             (str name " is injective")
             (str name " is not injective: two arrows meet"))
           (if injective? :teal :red))
      {:beat :hold :seconds 1.2}])))

(defn- surjective-phase [{:keys [gaps surjective?]} name]
  (concat
   [(say "surjective: every dot of the codomain is reached" :white)]
   (when (seq gaps) [(recolor (map #(dot-id :cod %) gaps) :red)])
   [(say (if surjective?
           (str name " is surjective")
           (str name " is not surjective: the red dots are never reached"))
         (if surjective? :teal :red))
    {:beat :hold :seconds 1.2}]))

(defn- image-phase [k a c graph name img-of]
  (let [img (value '(image f C) {'f graph 'C img-of})
        ps (filter #(contains? img-of (first %)) graph)]
    [{:beat :par :beats [(recolor (map #(dot-id :dom %) (fig/ordered img-of)) :gold)
                         (recolor (arrows-ids k a c ps) :gold)
                         (recolor (map #(dot-id :cod %) (fig/ordered img)) :gold)]}
     (say-tex (str (lt/->TeX (list 'image name 'C)) " = " (fig/class-tex img)
                   "\\quad C = " (fig/class-tex img-of)) :gold)
     {:beat :hold :seconds 1.5}]))

(defn- preimage-phase [k a c graph name pre-of]
  (let [pre (value '(preimage f D) {'f graph 'D pre-of})
        ps (filter #(contains? pre-of (second %)) graph)]
    [{:beat :par :beats [(recolor (map #(dot-id :cod %) (fig/ordered pre-of)) :teal)
                         (recolor (arrows-ids k a c ps) :teal)
                         (recolor (map #(dot-id :dom %) (fig/ordered pre)) :teal)]}
     (say-tex (str (lt/->TeX (list 'preimage name 'D)) " = " (fig/class-tex pre)
                   "\\quad D = " (fig/class-tex pre-of)) :teal)
     {:beat :hold :seconds 1.5}]))

(defn beats
  "The arrow flow of {:graph :dom :cod :name} as beats (see ns doc). Without
   :cod the graph is a relation on :dom, drawn on a ring."
  ([spec] (beats spec {}))
  ([{:keys [graph dom cod name] :or {name 'f} :as spec} {:keys [image preimage]}]
   (let [ring? (nil? cod)
         a (if ring? (ring dom) (column dom -2.6 :left))
         c (if ring? a (column cod 2.6 :right))
         cols (if ring? [[:dom a]] [[:dom a] [:cod c]])
         set-names (if ring? ["A"] ["A" "B"])
         facts (analyse spec)
         k :f
         restore* (fn [] (restore k a c graph :dom (if ring? :dom :cod)))]
     (vec
      (concat
       [(title (if ring?
                 (str (lt/->TeX name) " \\subseteq A \\times A")
                 (str (lt/->TeX name) " : A \\to B")))]
       (draw-phase cols set-names k a c graph)
       (when-not ring?
         (concat
          (function-phase facts k a c graph name)
          [(restore*)]
          (when (:function? facts)
            (concat (injective-phase facts k a c graph name) [(restore*)]
                    (surjective-phase facts name) [(restore*)]))))
       (when image (concat (image-phase k a c graph name (set image)) [(restore*)]))
       (when preimage (concat (preimage-phase k a c graph name (set preimage)) [(restore*)]))
       [{:beat :hold :seconds 1.0}])))))

(defn compose-beats
  "The composition flow of f : A -> B and g : B -> C: three columns, the
   arrows of f and g, the path of each x through f(x), then the composed
   arrows of g o f with B and the inner arrows cleared."
  [{:keys [f g A B C]}]
  (let [a (column A -4.4 :left)
        mid (column B 0.0 :mid)
        c (column C 4.4 :right)
        cols [[:dom a] [:mid mid] [:cod c]]
        gf (value '(compose g f) {'g g 'f f})
        g-of (into {} (map vec g))
        f-ids (arrows-ids :f a mid f)
        g-ids (arrows-ids :g mid c g)]
    (vec
     (concat
      [(title "g \\circ f : A \\to C")]
      [{:beat :show :items (vec (mapcat (fn [[kk col] l] (column-items kk col l)) cols ["A" "B" "C"]))}
       {:beat :par :beats (mapv (fn [[kk col]] {:beat :show :items (point-items kk col)}) cols)}
       {:beat :show :items (vec (mapcat #(arrow-items :f a mid % :white) (sorted-pairs f)))}
       {:beat :show :items (vec (mapcat #(arrow-items :g mid c % :white) (sorted-pairs g)))}
       (say-tex "(g \\circ f)(x) = g(f(x))" :white)]
      (mapcat (fn [[x y]]
                (let [z (g-of y)
                      path (concat (arrows-ids :f a mid [[x y]])
                                   (when (some? z) (arrows-ids :g mid c [[y z]])))
                      dots (cond-> [(dot-id :dom x) (dot-id :mid y)] (some? z) (conj (dot-id :cod z)))]
                  [{:beat :par :beats [(recolor path :gold) (recolor dots :gold)]}
                   {:beat :hold :seconds 0.6}
                   {:beat :par :beats [(recolor path :grey) (recolor dots :white)]}]))
              (sorted-pairs f))
      [{:beat :hide :ids (vec (concat f-ids g-ids [[:box :mid] [:set :mid]]
                                      (mapcat (fn [e] [(dot-id :mid e) [:label :mid e]]) (:elems mid))))}
       {:beat :show :items (vec (mapcat #(arrow-items :gf a c % :teal) (sorted-pairs gf)))}
       (say-tex (str "g \\circ f = " (fig/class-tex gf)) :teal)
       {:beat :hold :seconds 1.5}]))))

;; =============================================================================
;; Boundary
;; =============================================================================

(defn construct
  "A construct fn playing the arrow flow of spec."
  ([spec] (construct spec {}))
  ([spec opts] (let [bs (beats spec opts)] (fn [stage] (b/play! stage bs)))))

(defn compose-construct
  "A construct fn playing the composition flow."
  [spec]
  (let [bs (compose-beats spec)] (fn [stage] (b/play! stage bs))))

(defn record
  "The arrow flow of spec recorded as an EDN scene graph."
  ([scene-name spec] (record scene-name spec {}))
  ([scene-name spec opts]
   (s/with-backend (rec/recording-backend)
     (s/render! scene-name (construct spec opts)))))

(defn record-compose
  "The composition flow recorded as an EDN scene graph."
  [scene-name spec]
  (s/with-backend (rec/recording-backend)
    (s/render! scene-name (compose-construct spec))))
