(ns desargues.logic.scene.table
  "The truth-table flow: a desargues.logic.table animated as dynamic
   programming.

     (beats formula)                 ; pure: the flow as a vector of beat maps
     (construct formula & opts)      ; 1-arg construct fn for desargues.scene/render!
     (record scene-name formula & opts) ; the scene graph under the RecordingBackend

   Layout (`layout`) and beats are plain data in world units (centred, y up,
   14.2 x 8), played by desargues.logic.scene.beats/play!. A beat is

     {:beat   :show | :recolor | :caption | :mark
      :phase  :intro :headers :frame :atoms :focus :shortcut :fill :mark
              :unfocus :outline :verdict :final        ; what it means
      :col    compound column index (column beats only)
      :items  [item]      ; :show, :caption (replaces the caption in :slot)
      :slot   :top | :bottom
      :ids    [id] :color kw   ; :recolor, :mark
      :cells  [{:row :col}]}   ; :mark: the essential cells

   An item is {:id :kind (:tex | :text | :line) :at [x y] :size px :color kw
   :latex | :text | :from :to :width} plus, for cells, :row :col :value
   :essential?. Ids: [:header c], [:cell row c] (c a column of the whole
   table, atoms first), [:rule k], [:outline k], [:caption slot]."
  (:require [desargues.logic.connective :as c]
            [desargues.logic.formula :as f]
            [desargues.logic.table :as t]
            [desargues.logic.tex :as lt]
            [desargues.scene :as s]
            [desargues.scene.data :as rec]
            [desargues.logic.scene.beats :as b]))

;; =============================================================================
;; Layout (pure)
;; =============================================================================

(def ^:private margin 0.35)
(def ^:private caption-top-y 3.55)
(def ^:private caption-bottom-y -3.6)
(def ^:private table-top 3.1)
(def ^:private table-bottom -3.15)

(def ^:private true-tex (b/value-tex true))

(defn layout
  "Geometry of tbl's table in the world: {:columns :n-atoms :xs :widths
   :header-y :header-size :row-ys :row-h :cell-size :rule-y :bottom-y :top-y
   :left :right :split-x :legible?}. Column j of :columns is atom j, then the
   compounds. The header font is the largest (<= 30 px) at which every column
   fits; :legible? is false when that is under 12 px."
  [{:keys [atoms columns rows]}]
  (let [cols (vec (concat atoms columns))
        n (count rows)
        avail-w (- (* 2 b/half-width) (* 2 margin))
        pad 0.4
        header-h 0.75
        header-y (- table-top (/ header-h 2))
        rule-y (- table-top header-h)
        rows-top (- rule-y 0.12)
        row-h (min 0.62 (/ (- rows-top table-bottom) (max n 1)))
        cell-size0 (-> (* row-h b/px-per-unit 0.6) (min 30.0) (max 12.0))
        tex30 (mapv #(b/tex-width (lt/->TeX %) 30) cols)
        widths-at (fn [s cs]
                    (let [cw (b/tex-width true-tex cs)]
                      (mapv #(+ pad (max (* % (/ s 30.0)) cw)) tex30)))
        fits? (fn [s] (<= (reduce + (widths-at s cell-size0)) avail-w))
        header-size (or (first (filter fits? (range 30.0 7.9 -0.5))) 8.0)
        w0 (widths-at header-size cell-size0)
        shrink (min 1.0 (/ avail-w (reduce + w0)))
        cell-size (* cell-size0 shrink)
        header-size (* header-size shrink)
        widths (mapv #(* shrink %) w0)
        total-w (reduce + widths)
        left (- (/ total-w 2))
        xs (mapv (fn [x w] (+ x (/ w 2))) (reductions + left widths) widths)
        row-ys (mapv #(- rows-top (* row-h (+ % 0.5))) (range n))]
    {:columns cols
     :n-atoms (count atoms)
     :xs xs
     :widths widths
     :header-y header-y
     :header-size header-size
     :legible? (<= 12.0 header-size)
     :row-ys row-ys
     :row-h row-h
     :cell-size cell-size
     :rule-y rule-y
     :bottom-y (- rows-top (* row-h n))
     :top-y table-top
     :left left
     :right (+ left total-w)
     :split-x (+ left (reduce + (take (count atoms) widths)))}))

;; =============================================================================
;; Beats (pure)
;; =============================================================================

(def ^:private plain-colors {true :white false :grey})

(def ^:private verdict-caption
  {:tautology {:color :green :text "tautology: the last column is T in every row"}
   :contradiction {:color :red :text "contradiction: the last column is F in every row"}
   :contingent {:color :orange :text "contingent: the red rows are counterexamples"}})

(defn- tex-item [id latex at size color & {:as extra}]
  (merge {:id id :kind :tex :latex latex :at at :size size :color color} extra))

(defn- text-item [id text at size color]
  {:id id :kind :text :text text :at at :size size :color color})

(defn- line-item [id from to color width]
  {:id id :kind :line :from from :to to :color color :width width})

(defn- caption [slot text color]
  (let [y (if (= slot :top) caption-top-y caption-bottom-y)
        size (min 24 (/ (* 2 (- b/half-width margin)) (* (b/em 1) 0.52 (max 1 (count text)))))]
    (text-item [:caption slot] text [0.0 y] size color)))

(defn- caption-beat [phase slot text color & {:as extra}]
  (merge {:beat :caption :phase phase :slot slot :hold 0.6 :items [(caption slot text color)]} extra))

(defn- show-beat [phase items & {:as extra}]
  (merge {:beat :show :phase phase :items items :lag (min 0.3 (/ 3.0 (max 1 (count items))))} extra))

(defn- cell-item [{:keys [xs row-ys cell-size]} row col v extra]
  (tex-item [:cell row col] (b/value-tex v) [(xs col) (row-ys row)] cell-size
            (plain-colors v) :row row :col col :value v extra))

(defn- column-beats
  "The beats that fill compound column j of tbl (whole-table column col)."
  [lay j col g col-cells]
  (let [header [:header col]
        essential (filterv :essential? col-cells)]
    (cond-> [{:beat :recolor :phase :focus :col j :ids [header] :color :gold}
             (caption-beat :shortcut :top (:shortcut (c/spec (f/op g))) :gold :col j)
             (show-beat :fill (mapv (fn [{:keys [row value essential?]}]
                                      (cell-item lay row col value {:essential? essential?}))
                                    col-cells)
                        :col j)]
      (seq essential)
      (conj {:beat :mark :phase :mark :col j :color :gold
             :ids (mapv (fn [{:keys [row]}] [:cell row col]) essential)
             :cells (mapv (fn [{:keys [row]}] {:row row :col j}) essential)})
      true
      (conj {:beat :recolor :phase :unfocus :col j :ids [header] :color :white}))))

(defn- outline-items [{:keys [xs widths top-y bottom-y]} col]
  (let [x0 (- (xs col) (/ (widths col) 2) -0.04)
        x1 (- (+ (xs col) (/ (widths col) 2)) 0.04)
        y0 (- bottom-y 0.02)
        y1 (- top-y 0.02)
        corners [[x0 y0] [x1 y0] [x1 y1] [x0 y1]]]
    (mapv (fn [k a b] (line-item [:outline k] a b :gold 3))
          (range) corners (concat (rest corners) [(first corners)]))))

(defn beats
  "The truth-table flow of formula as a vector of beats (see ns doc), in play
   order: intro, headers, frame, atom rows, then each compound column children
   first (focus, shortcut, fill, mark, unfocus), outline of the last column,
   verdict, final recolour."
  [formula]
  (let [tbl (t/table formula)
        {:keys [columns n-atoms xs header-y header-size left right rule-y
                split-x top-y bottom-y] :as lay} (layout tbl)
        cells (t/cells tbl)
        by-col (group-by :col cells)
        last-col (dec (count columns))
        final-cells (for [[i {:keys [values env]}] (map-indexed vector (:rows tbl))]
                      [i (if (f/compound? formula) (get values formula) (get env formula))])
        verdict (:verdict tbl)
        {vcolor :color vtext :text} (verdict-caption verdict)]
    (vec
     (concat
      [(caption-beat :intro :top "each column asks one question of the columns before it" :white)
       (show-beat :headers (mapv (fn [col g] (tex-item [:header col] (lt/->TeX g) [(xs col) header-y]
                                                       header-size :white))
                                 (range) columns))
       (show-beat :frame [(line-item [:rule 0] [left rule-y] [right rule-y] :grey 2)
                          (line-item [:rule 1] [split-x top-y] [split-x bottom-y] :grey 2)])
       (show-beat :atoms (vec (for [[i {:keys [env]}] (map-indexed vector (:rows tbl))
                                    [col a] (map-indexed vector (:atoms tbl))]
                                (cell-item lay i col (get env a) {}))))]
      (mapcat (fn [j g] (column-beats lay j (+ n-atoms j) g (by-col j)))
              (range) (:columns tbl))
      [(show-beat :outline (outline-items lay last-col))
       (caption-beat :verdict :bottom vtext vcolor)
       {:beat :recolor :phase :final :verdict verdict
        :color (if (= verdict :tautology) :green :red)
        :ids (vec (for [[i v] final-cells
                        :when (or (= verdict :tautology) (false? v))]
                    [:cell i last-col]))}]))))

;; =============================================================================
;; Boundary: beats -> desargues.scene, through desargues.logic.scene.beats/play!
;; =============================================================================

(defn construct
  "A 1-arg construct fn for desargues.scene/render! that plays the truth-table
   flow of formula. opts: :pace."
  [formula & {:as opts}]
  (let [bs (conj (beats formula) {:beat :hold :seconds 2})]
    (fn [stage] (b/play! stage bs opts) nil)))

(defn record
  "The scene graph of formula's truth-table flow under a fresh
   RecordingBackend."
  [scene-name formula & {:as opts}]
  (s/with-backend (rec/recording-backend)
    (s/render! scene-name (construct formula opts))))
