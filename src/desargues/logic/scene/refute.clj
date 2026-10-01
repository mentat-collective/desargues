(ns desargues.logic.scene.refute
  "The refutation flow: the truth table run backwards, as an animation.

   The formula stands at the top; the flow assumes it is F, and every forcing
   step of desargues.logic.refute/refute adds the newly pinned subformulas as
   TeX lines `sub = T/F` growing downward, narrated by the connective's
   essential case. A branch splits the column into side-by-side cases; a clash
   turns the clashing line red and closes its column with a red x; an open
   column ends with its counterexample in gold. A verdict caption ends the flow.

   `beats` is pure data (desargues.logic.scene.beats); `construct` plays it on
   a stage; `record` runs construct against the RecordingBackend."
  (:require [clojure.string :as str]
            [desargues.logic.connective :as c]
            [desargues.logic.formula :as f]
            [desargues.logic.refute :as r]
            [desargues.logic.scene.beats :as b]
            [desargues.logic.tex :as lt]
            [desargues.scene :as s]
            [desargues.scene.data :as rec]))

(def layout
  "Geometry of the flow, in world units and px."
  {:title-y 3.4 :title-size 36
   :top 2.6 :bottom -2.85
   :caption-y -3.55 :caption-size 22
   :row-max 0.62 :size-max 28 :gap 0.4 :margin 6.8})

;; ---------------------------------------------------------------------------
;; Words and TeX for values

(defn- value-word [v] (if v "T" "F"))
(defn- value-color [v] (if v :teal :orange))

(defn- assignment-tex
  "TeX for `g = v`, a compound g parenthesized."
  [g v]
  (let [t (lt/->TeX g)]
    (str (if (f/compound? g) (str "\\left(" t "\\right)") t) " = " (b/value-tex v))))

(defn env-tex
  "TeX for an env {atom bool}: P = F, Q = T."
  [env]
  (str/join ",\\ " (map (fn [[a v]] (assignment-tex a v)) env)))

(defn- shortcut [g] (:shortcut (c/spec (f/op g))))

(defn- glyph
  "The connective's symbol, the first word of its shortcut."
  [g]
  (first (str/split (shortcut g) #" ")))

(defn- row-words
  "A candidate row in words: T ∨ F, ¬ T."
  [g row]
  (if (= 1 (count row))
    (str (glyph g) " " (value-word (first row)))
    (str/join (str " " (glyph g) " ") (map value-word row))))

;; ---------------------------------------------------------------------------
;; Sizing (pure): rows per step, depth of the tree, columns per leaf

(defmulti step-rows
  "How many line rows a refutation step adds to its column."
  :kind)

(defmethod step-rows :assume [_] 1)
(defmethod step-rows :force [{:keys [forced]}] (count forced))
(defmethod step-rows :clash [_] 1)
(defmethod step-rows :default [_] 0)

(defn- depth
  "[rows gaps] of the deepest column path of tree."
  [tree]
  (let [own (reduce + (map step-rows (:steps tree)))]
    (case (:status tree)
      :branch (let [[rs gs] (apply max-key (fn [[rs gs]] (+ rs gs)) (map depth (:children tree)))]
                [(+ own rs) (inc gs)])
      :open [(inc own) 0]
      [own 0])))

(defn- row-height
  [tree]
  (let [{:keys [top bottom row-max gap]} layout
        [rs gs] (depth tree)]
    (min row-max (/ (- top bottom (* gs gap)) (max 1 rs)))))

(defn- caption
  [text color]
  (let [{:keys [caption-y caption-size margin]} layout]
    (b/caption {:kind :text :text text :at [0 caption-y] :size caption-size :color color}
               (* 2 margin))))

(defn- tex-caption
  [latex color]
  (let [{:keys [caption-y caption-size margin]} layout]
    (b/caption {:kind :tex :latex latex :at [0 caption-y] :size caption-size :color color}
               (* 2 margin))))

;; ---------------------------------------------------------------------------
;; The walk: context -> beats

(defn- centre [{:keys [x0 x1]}] (/ (+ x0 x1) 2.0))

(defn- row-item
  "The item for a new line in ctx's column, and ctx moved one row down."
  [ctx latex color]
  (let [{:keys [x0 x1 y h path k size]} ctx
        it {:id [:row path k] :kind :tex :latex latex :at [(centre ctx) y] :size size :color color}]
    [(-> ctx (update :y - h) (update :k inc))
     (assoc it :size (b/fit-size it (- x1 x0 0.3)))]))

(defn- add-lines
  "Lines `sub = v` for every entry of assignments not already shown."
  [ctx assignments]
  (reduce (fn [[ctx its] [g v]]
            (if (contains? (:line-of ctx) g)
              [ctx its]
              (let [[ctx' it] (row-item ctx (assignment-tex g v) (value-color v))]
                [(assoc-in ctx' [:line-of g] {:id (:id it) :color (value-color v)}) (conj its it)])))
          [ctx []] assignments))

(defn- line-id [ctx g] (get-in ctx [:line-of g :id]))

(defmulti narrate
  "[ctx step] -> [ctx' beats] for one refutation step."
  (fn [_ctx step] (:kind step)))

(defmethod narrate :assume [ctx {:keys [node value]}]
  (let [[ctx' its] (add-lines ctx [[node value]])]
    [ctx' [(caption (str "assume it is " (value-word value)) :white)
           {:beat :show :items its}
           {:beat :hold :seconds 0.6}]]))

(defmethod narrate :force [ctx {:keys [node value rows forced]}]
  (let [said (if-let [{:keys [i n]} (:case ctx)]
               (str "case " i " of " n ": " (row-words node (first rows))
                    " gives " (value-word value))
               (str (shortcut node) ", so"))
        [ctx' its] (add-lines (dissoc ctx :case) (sort-by (comp str key) forced))
        id (line-id ctx node)
        back (get-in ctx [:line-of node :color])]
    [ctx' [{:beat :par :beats [(caption said :white)
                               {:beat :recolor :ids [id] :color :gold}]}
           {:beat :show :items its}
           {:beat :recolor :ids [id] :color back}]]))

(defmethod narrate :clash [ctx {:keys [node value]}]
  (let [[ctx' cross] (row-item ctx "\\times" :red)]
    [ctx' [{:beat :par :beats [(caption (str "clash: no row of " (glyph node)
                                             " gives " (value-word value)) :red)
                               {:beat :recolor :ids [(line-id ctx node)] :color :red}]}
           {:beat :show :items [(assoc cross :size (min 36 (* 1.4 (:size ctx))))]}
           {:beat :hold :seconds 0.5}]]))

(defmethod narrate :default [ctx _] [ctx []])

(defn- narrate-all [ctx steps]
  (reduce (fn [[ctx bs] st] (let [[ctx' more] (narrate ctx st)] [ctx' (into bs more)]))
          [ctx []] steps))

(defn- columns
  "Split [x0 x1] among children in proportion to their leaf counts."
  [{:keys [x0 x1]} children]
  (let [ls (map (comp count r/leaves) children)
        total (reduce + ls)
        w (- x1 x0)]
    (map (fn [start l] [(+ x0 (* w (/ start total))) (+ x0 (* w (/ (+ start l) total)))])
         (reductions + 0 ls) ls)))

(declare walk)

(defn- branch-beats [ctx {:keys [split children]}]
  (let [{:keys [node value]} split
        {:keys [gap]} layout
        {:keys [y h]} ctx
        regions (columns ctx children)
        n (count children)
        top (+ y (* 0.3 h))
        kids (map-indexed (fn [i [x0 x1]]
                            (assoc ctx :x0 x0 :x1 x1 :y (- y gap) :path (conj (:path ctx) i)
                                   :k 0 :case {:i (inc i) :n n}))
                          regions)
        links (map-indexed (fn [i kid]
                             {:id [:link (:path ctx) i] :kind :line :color :grey :width 2
                              :from [(centre ctx) top] :to [(centre kid) (- top gap)]})
                           kids)
        id (line-id ctx node)]
    (into [{:beat :par :beats [(caption (str (shortcut node) "; " (count (:rows split))
                                             " rows give " (value-word value) ": split into cases")
                                        :white)
                               {:beat :recolor :ids [id] :color :gold}]}
           {:beat :show :items (vec links)}
           {:beat :recolor :ids [id] :color (get-in ctx [:line-of node :color])}]
          (mapcat (fn [kid child] (second (walk kid child))) kids children))))

(defn- walk
  "[ctx beats] for tree drawn in ctx's column."
  [ctx tree]
  (let [[ctx' bs] (narrate-all ctx (:steps tree))]
    (case (:status tree)
      :branch [ctx' (into bs (branch-beats ctx' tree))]
      :open (let [[ctx'' it] (row-item ctx' (env-tex (:counterexample tree)) :gold)]
              [ctx'' (into bs [{:beat :par :beats [(caption "no clash: this branch stays open" :gold)
                                                   {:beat :show :items [it]}]}
                               {:beat :emphasize :ids [(:id it)]}])])
      [ctx' bs])))

(defn verdict
  "The closing caption beat for tree."
  [tree]
  (if-let [ce (some :counterexample (r/leaves tree))]
    (tex-caption (str "\\text{counterexample: }" (env-tex ce)) :gold)
    (caption "every branch closes: tautology" :teal)))

(defn beats
  "The refutation flow of formula as beats."
  [formula]
  (let [tree (r/refute formula)
        {:keys [title-y title-size top margin size-max]} layout
        h (row-height tree)
        ctx {:x0 (- margin) :x1 margin :y top :h h :path [] :k 0 :line-of {}
             :size (min size-max (Math/floor (* 45 h)))}
        title {:id :title :kind :tex :latex (lt/->TeX formula) :at [0 title-y] :size title-size :color :white}
        [_ bs] (walk ctx tree)]
    (-> [{:beat :show :items [(assoc title :size (b/fit-size title (* 2 margin)))]}
         {:beat :hold :seconds 0.6}]
        (into bs)
        (conj (verdict tree) {:beat :hold :seconds 1.5}))))

(defn construct
  "A construct fn playing the refutation flow of formula on a stage."
  [formula]
  (let [bs (beats formula)]
    (fn [stage] (b/play! stage bs))))

(defn record
  "Record the refutation flow of formula against a fresh RecordingBackend."
  [scene-name formula]
  (s/with-backend (rec/recording-backend)
    (s/render! scene-name (construct formula))))
