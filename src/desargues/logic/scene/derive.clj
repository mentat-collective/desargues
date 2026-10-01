(ns desargues.logic.scene.derive
  "The derivation flow: a class statement unfolded, one definition at a time,
   into the propositional formula it means.

   A vertical chain of TeX lines. Before each step the subterm about to change
   is highlighted gold; the next line appears below with the definition used
   at the right margin (def. of X ∪ Y), and the line it came from fades to
   grey. At most (:visible layout) lines stay on screen: older ones scroll up
   and vanish. The last line, the propositional formula, turns teal and a
   caption hands it to the next flow (a truth table or a refutation).

   `chain` and `beats` are pure data (desargues.logic.scene.beats); `construct`
   plays them on a stage; `record` runs construct against the
   RecordingBackend."
  (:require [desargues.logic.classes :as cl]
            [desargues.logic.formula :as f]
            [desargues.logic.scene.beats :as b]
            [desargues.logic.tex :as lt]
            [desargues.scene :as s]
            [desargues.scene.data :as rec]))

(def layout
  "Geometry of the flow, in world units and px."
  {:title-y 3.45 :title-size 22
   :top 2.55 :h 0.88 :visible 6
   :x -1.4 :width 10.4 :size 30
   :rule-x 5.4 :rule-width 2.9 :rule-size 20
   :caption-y -3.55 :caption-size 22 :margin 6.8})

(def handoffs
  "Handoff keyword -> how the closing caption names the next flow."
  {:truth-table "a truth table" :refutation "a refutation"})

;; ---------------------------------------------------------------------------
;; The chain (pure)

(defn- generic
  "The class term's operator applied to placeholder letters: (union X Y)."
  [c]
  (if (seq? c) (cons (first c) (take (count (rest c)) '[X Y Z W])) c))

(defn rule-tex
  "TeX naming the definition that unfolds class term c."
  [c]
  (str "\\text{def. of }" (lt/->TeX (generic c))))

(defn chain
  "The lines of the derivation of class statement s:
   [{:formula g :label latex-or-nil :path path-of-next-change-or-nil}]."
  [s]
  (let [st (cl/statement s)
        steps (cl/unfold-steps st)
        labels (cons nil (map (fn [{g :formula p :path}] (rule-tex (nth (f/subterm g p) 2)))
                              (butlast steps)))
        unfolded (mapv (fn [{g :formula p :path} l] {:formula g :label l :path p}) steps labels)]
    (if (= s st)
      unfolded
      (into [{:formula s :label nil :path []}]
            (update unfolded 0 assoc :label "\\text{extent}")))))

;; ---------------------------------------------------------------------------
;; Beats (pure)

(defn- slot-y [k] (- (:top layout) (* k (:h layout))))

(defn- shown-slot
  "The slot of line i while line n is the newest."
  [i n]
  (- i (max 0 (- n (dec (:visible layout))))))

(defn- line-item
  [id latex y color]
  (let [{:keys [x width size]} layout
        it {:id id :kind :tex :latex latex :at [x y] :size size :color color}]
    (assoc it :size (b/fit-size it width))))

(defn- rule-item
  [i latex y]
  (let [{:keys [rule-x rule-width rule-size]} layout
        it {:id [:rule i] :kind :tex :latex latex :at [rule-x y] :size rule-size :color :grey}]
    (assoc it :size (b/fit-size it rule-width))))

(defn- caption
  [text color]
  (let [{:keys [caption-y caption-size margin]} layout]
    (b/caption {:kind :text :text text :at [0 caption-y] :size caption-size :color color}
               (* 2 margin))))

(defn- new-line
  "Items showing line i of lines at slot k."
  [lines i k]
  (let [{:keys [formula label]} (nth lines i)
        y (slot-y k)]
    (cond-> [(line-item [:line i] (lt/->TeX formula) y :white)]
      label (conj (rule-item i label y)))))

(defn- scroll
  "Beat making room for line n: older lines glide up a slot, the oldest goes."
  [lines n current]
  (let [{:keys [x rule-x visible]} layout
        gone (- n visible)
        kept (range (inc gone) n)]
    {:beat :par
     :beats [{:beat :hide :ids (cond-> [(current gone)] (:label (nth lines gone)) (conj [:rule gone]))}
             {:beat :move :to (into {} (mapcat (fn [i]
                                                 (let [y (slot-y (shown-slot i n))]
                                                   (cond-> [[(current i) [x y]]]
                                                     (:label (nth lines i)) (conj [[:rule i] [rule-x y]])))))
                                     kept)}]}))

(defn beats
  "The derivation flow of class statement s as beats. opts: :handoff, a key
   of `handoffs` (default :truth-table)."
  [s & {:keys [handoff] :or {handoff :truth-table}}]
  (let [lines (chain s)
        last-i (dec (count lines))
        {:keys [title-y title-size visible]} layout
        title {:id :title :kind :text :text "unfold the definitions, one membership at a time"
               :at [0 title-y] :size title-size :color :grey}
        step (fn [[bs current] i]
               (let [{g :formula p :path} (nth lines i)
                     n (inc i)
                     y (slot-y (shown-slot i i))
                     hl (line-item [:line-hl i] (lt/->TeX (lt/highlight g p :gold)) y :white)
                     current (assoc current i [:line-hl i])
                     moved? (>= n visible)
                     bs (cond-> (conj bs
                                      {:beat :par :beats [(caption "the gold part unfolds by its definition" :white)
                                                          {:beat :swap :ids [[:line i]] :items [hl]}]}
                                      {:beat :hold :seconds 0.4})
                          moved? (conj (scroll lines n current)))]
                 [(conj bs {:beat :par :beats [{:beat :show :items (new-line lines n (shown-slot n n))}
                                               {:beat :recolor :ids [[:line-hl i]] :color :grey}]})
                  (assoc current n [:line n])]))
        [bs _] (reduce step
                       [[{:beat :show :items (into [title] (new-line lines 0 0))}
                         {:beat :hold :seconds 0.6}]
                        {0 [:line 0]}]
                       (range last-i))]
    (conj bs
          {:beat :par :beats [{:beat :recolor :ids [[:line last-i]] :color :teal}
                              (caption (str "a propositional formula: decide it by "
                                            (get handoffs handoff (name handoff)))
                                       :teal)]}
          {:beat :emphasize :ids [[:line last-i]]}
          {:beat :hold :seconds 1.5})))

(defn construct
  "A construct fn playing the derivation flow of class statement s."
  [s & opts]
  (let [bs (apply beats s opts)]
    (fn [stage] (b/play! stage bs))))

(defn record
  "Record the derivation flow of class statement s against a fresh
   RecordingBackend."
  [scene-name s & opts]
  (s/with-backend (rec/recording-backend)
    (s/render! scene-name (apply construct s opts))))
