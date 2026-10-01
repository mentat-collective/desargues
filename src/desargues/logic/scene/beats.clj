(ns desargues.logic.scene.beats
  "An animation as plain data (BEATS), and the one boundary that plays it.

   A beat is a map {:beat kw ...}; `animate` is OPEN on :beat. An item is a
   node description {:id any :kind kw ...}; `realize` (backend node) and
   `extent` (estimated bounding box) are OPEN on :kind.

   Beats:
     {:beat :show :items [item]}            items appear together
     {:beat :hide :ids [id]}                nodes vanish
     {:beat :swap :ids [id] :items [item]}  old nodes vanish as new ones appear
     {:beat :recolor :ids [id] :color kw}
     {:beat :move :to {id [x y]}}           nodes glide to new centres
     {:beat :emphasize :ids [id]}
     {:beat :par :beats [beat]}             sub-beats played as one step
     {:beat :hold :seconds s}

   Items:
     {:id :kind :tex  :latex s :at [x y] :size px :color kw}
     {:id :kind :text :text s  :at [x y] :size px :color kw}
     {:id :kind :line :from [x y] :to [x y] :color kw :width w}

   World: centred, y up, 14.2 x 8 units, 60 px per unit. Every footprint of a
   well-formed flow lies inside it (`inside?`)."
  (:require [clojure.string :as str]
            [desargues.scene :as s]
            [desargues.tex.label :as tl]))

;; ---------------------------------------------------------------------------
;; World, values and size estimates (pure), shared by every logic flow

(def half-width 7.1)
(def half-height 4.0)
(def px-per-unit 60.0)

(defn em
  "One em at font size px, in world units."
  [size]
  (/ (double size) px-per-unit))

(defn value-tex
  "TeX for a truth value: \\mathrm{T} or \\mathrm{F}."
  [v]
  (if v "\\mathrm{T}" "\\mathrm{F}"))

(defn tex-width
  "Estimated rendered width in world units of latex at font size px
   (checked against latex+dvisvgm extents: never more than about 5% under)."
  [latex size]
  (let [src (-> latex
                (str/replace #"\\textcolor\{[^}]*\}" "")
                (str/replace #"\\(left|right|mathrm)\b" " "))
        arrows (count (re-seq #"\\(Rightarrow|Leftrightarrow|Leftarrow|implies|iff)\b" src))
        cmds (- (count (re-seq #"\\[a-zA-Z]+" src)) arrows)
        rest-src (str/replace src #"\\[a-zA-Z]+|[{}\s]" "")
        delims (count (re-seq #"[()\[\]|]" rest-src))
        ops (count (re-seq #"[-+=<>]" rest-src))
        glyphs (- (count rest-src) delims)]
    (* (em size) (+ (* 0.72 glyphs) (* 0.5 ops) (* 0.33 delims) (* 1.1 cmds) (* 1.8 arrows)))))

(defn text-width
  "Estimated rendered width in world units of plain text at font size px."
  [text size]
  (* (em size) 0.52 (count text)))

(defmulti width
  "Estimated width in world units of an item at its :size px. Open on :kind;
   kinds without a width (lines) give 0."
  :kind)

(defmethod width :default [_] 0.0)
(defmethod width :tex [{:keys [latex size]}] (tex-width latex size))
(defmethod width :text [{:keys [text size]}] (text-width text size))

(defn fit-size
  "The largest size <= size at which item fits in w world units."
  [item w]
  (let [at-1 (width (assoc item :size 1))]
    (if (pos? at-1) (min (:size item) (Math/floor (/ w at-1))) (:size item))))

(defn caption
  "The beat replacing the :caption node by item (any kind with :size), shrunk
   to fit width w."
  [item w]
  {:beat :swap :ids [:caption] :items [(assoc item :id :caption :size (fit-size item w))]})


(defmulti extent
  "Item -> [[xmin ymin] [xmax ymax]], its estimated footprint."
  :kind)

(defn- box [{[x y] :at size :size :as item}]
  (let [hw (/ (width item) 2) hh (/ (* 0.6 size) px-per-unit)]
    [[(- x hw) (- y hh)] [(+ x hw) (+ y hh)]]))

(defmethod extent :tex [item] (box item))
(defmethod extent :text [item] (box item))
(defmethod extent :line [{[x0 y0] :from [x1 y1] :to}]
  [[(min x0 x1) (min y0 y1)] [(max x0 x1) (max y0 y1)]])

(defn- moved [item to]
  (if (:at item) (assoc item :at to) item))

(defn footprints
  "Every footprint any item occupies over the beats, moves included."
  [beats]
  (letfn [(walk [[items acc] b]
            (case (:beat b)
              (:show :swap) [(into items (map (juxt :id identity)) (:items b))
                             (into acc (map extent) (:items b))]
              :move (let [ms (keep (fn [[id to]] (some-> (items id) (moved to))) (:to b))]
                      [(into items (map (juxt :id identity)) ms) (into acc (map extent) ms)])
              :par (reduce walk [items acc] (:beats b))
              [items acc]))]
    (second (reduce walk [{} []] beats))))

(defn inside?
  "True when footprint [[xmin ymin] [xmax ymax]] lies in the world."
  [[[x0 y0] [x1 y1]]]
  (and (<= (- half-width) x0 x1 half-width) (<= (- half-height) y0 y1 half-height)))

(defn items
  "Every item the beats introduce, in order."
  [beats]
  (mapcat (fn [b] (if (= :par (:beat b)) (items (:beats b)) (:items b))) beats))

;; ---------------------------------------------------------------------------
;; Boundary: items to scene nodes, beats to played animations

(defmulti realize
  "Item -> a scene node built through desargues.scene."
  :kind)

(defmethod realize :tex [{:keys [latex at size color]}]
  (tl/label latex at :size size :color color))

(defmethod realize :text [{:keys [text at size color]}]
  (-> (s/text text :font-size size :color color) (s/move-to at)))

(defmethod realize :line [{:keys [from to color width] :or {width 2}}]
  (s/line from to :color color :width width))

(def ^:private appear-of {:line s/draw})

(defn- reveal [item node] ((get appear-of (:kind item) s/appear) node :run-time 0.5))

(defmulti animate
  "[nodes beat] -> [nodes' animations]; nodes maps item id -> scene node."
  (fn [_nodes beat] (:beat beat)))

(defmethod animate :show [nodes {:keys [items]}]
  (reduce (fn [[ns as] it]
            (let [nd (realize it)] [(assoc ns (:id it) nd) (conj as (reveal it nd))]))
          [nodes []] items))

(defmethod animate :hide [nodes {:keys [ids]}]
  [(apply dissoc nodes ids) (vec (keep #(some-> (nodes %) (s/vanish :run-time 0.4)) ids))])

(defmethod animate :swap [nodes {:keys [ids items]}]
  (let [[n1 a1] (animate nodes {:beat :hide :ids ids})
        [n2 a2] (animate n1 {:beat :show :items items})]
    [n2 (into a1 a2)]))

(defmethod animate :recolor [nodes {:keys [ids color]}]
  [nodes (vec (keep #(some-> (nodes %) (s/recolor color :run-time 0.4)) ids))])

(defmethod animate :move [nodes {:keys [to]}]
  [nodes (vec (keep (fn [[id p]] (some-> (nodes id) (s/glide p :run-time 0.5))) to))])

(defmethod animate :emphasize [nodes {:keys [ids]}]
  [nodes (vec (keep #(some-> (nodes %) (s/emphasize :run-time 0.5)) ids))])

(defmethod animate :par [nodes {:keys [beats]}]
  (reduce (fn [[ns as] b] (let [[ns' as'] (animate ns b)] [ns' (into as as')]))
          [nodes []] beats))

(defmethod animate :hold [nodes _] [nodes []])

(defn play!
  "Boundary: play beats on stage through desargues.scene, in order."
  [stage beats]
  (reduce (fn [nodes b]
            (if (= :hold (:beat b))
              (do (s/hold! stage (:seconds b 1)) nodes)
              (let [[nodes' anims] (animate nodes b)]
                (when (seq anims) (apply s/play! stage [(s/together anims)]))
                nodes')))
          {} beats))
