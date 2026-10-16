(ns desargues.logic.scene.beats
  "An animation as plain data (BEATS), and the one interpreter that plays it.

   A beat is a map {:beat kw ...}; `animate` is OPEN on :beat. An item is a
   node description {:id any :kind kw ...}; `realize` (backend node),
   `extent` (estimated bounding box), `reveal` (how it comes in) and `width`
   are OPEN on :kind. `placed` (which items a beat puts where) is OPEN on
   :beat, so `footprints` bounds every flow.

   Beats:
     {:beat :show :items [item] :lag r? :run-time s? :groups {gid [id]}?}
     {:beat :hide :ids [id] | :all}             ids may name groups
     {:beat :swap :ids [id] :items [item]}      old nodes vanish as new appear
     {:beat :caption :items [item] :hold s?}    each item replaces the node
                                                with its id (a slot)
     {:beat :recolor :ids [id] :color kw}
     {:beat :mark :ids [id] :color kw}          recolour, then emphasize
     {:beat :move :to {id [x y]}}               nodes glide to new centres
     {:beat :emphasize :ids [id]}
     {:beat :diagram :items [item] :groups {}}  outlines drawn, then the rest
     {:beat :shade :items [item] :groups {}}    dots fade in together
     {:beat :par :beats [beat]}                 sub-beats played as one step
     {:beat :hold :seconds s}

   Items:
     {:id :kind :tex    :latex s :at [x y] :size px :color kw}
     {:id :kind :text   :text s  :at [x y] :size px :color kw}
     {:id :kind :line   :from [x y] :to [x y] :color kw :width w}
     {:id :kind :rect   :center [x y] :size [w h] :color kw :width w}
     {:id :kind :circle :center [x y] :radius r :color kw :width w}
     {:id :kind :dot    :at [x y] :radius r :color kw}

   The registry the interpreter threads is {:nodes {id node} :groups {gid
   [id]} :pace p}; a group id stands for its members wherever ids are read.

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
   kinds without a width (lines, shapes) give 0."
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
  {:beat :caption :ids [:caption] :items [(assoc item :id :caption :size (fit-size item w))]})

;; ---------------------------------------------------------------------------
;; Footprints (pure): the one bounds checker

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
(defmethod extent :rect [{[x y] :center [w h] :size}]
  [[(- x (/ w 2)) (- y (/ h 2))] [(+ x (/ w 2)) (+ y (/ h 2))]])
(defmethod extent :circle [{[x y] :center r :radius}]
  [[(- x r) (- y r)] [(+ x r) (+ y r)]])
(defmethod extent :dot [{[x y] :at r :radius}]
  [[(- x r) (- y r)] [(+ x r) (+ y r)]])

(defn moved
  "item with its centre at p: :at or :center replaced, a line translated so
   its midpoint is p."
  [item [px py :as p]]
  (cond (:at item) (assoc item :at p)
        (:center item) (assoc item :center p)
        (:from item) (let [[x0 y0] (:from item) [x1 y1] (:to item)
                           dx (- px (/ (+ x0 x1) 2.0)) dy (- py (/ (+ y0 y1) 2.0))]
                       (assoc item :from [(+ x0 dx) (+ y0 dy)] :to [(+ x1 dx) (+ y1 dy)]))
        :else item))

(defmulti placed
  "[items-by-id beat] -> the items beat puts on screen, where it puts them."
  (fn [_by-id beat] (:beat beat)))

(defmethod placed :default [_ beat] (:items beat))

(defmethod placed :move [by-id {:keys [to]}]
  (keep (fn [[id p]] (some-> (by-id id) (moved p))) to))

(defn footprints
  "Every footprint any item occupies over the beats, moves included."
  [beats]
  (letfn [(walk [[by-id acc] b]
            (if (= :par (:beat b))
              (reduce walk [by-id acc] (:beats b))
              (let [its (placed by-id b)]
                [(into by-id (map (juxt :id identity)) its) (into acc (map extent) its)])))]
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
;; Boundary: items to scene nodes

(defmulti realize
  "Item -> a scene node built through desargues.scene."
  :kind)

(defmethod realize :tex [{:keys [latex at size color]}]
  (tl/label latex at :size size :color color))

(defmethod realize :text [{:keys [text at size color]}]
  (-> (s/text text :font-size size :color color) (s/move-to at)))

(defmethod realize :line [{:keys [from to color width] :or {width 2}}]
  (s/line from to :color color :width width))

(defmethod realize :rect [{:keys [center size color width] :or {width 2}}]
  (let [[w h] size]
    (-> (s/rectangle :width w :height h) (s/stroke! :color color :width width) (s/move-to center))))

(defmethod realize :circle [{:keys [center radius color width] :or {width 3}}]
  (-> (s/circle :radius radius) (s/stroke! :color color :width width) (s/move-to center)))

(defmethod realize :dot [{:keys [at radius color]}]
  (-> (s/dot :color color :radius radius) (s/move-to at)))

(defmulti reveal
  "[item node run-time] -> the animation bringing node in. Open on :kind."
  (fn [item _node _run-time] (:kind item)))

(defmethod reveal :default [_ node t] (s/appear node :run-time t))
(defmethod reveal :line [_ node t] (s/draw node :run-time t))
(defmethod reveal :rect [_ node t] (s/draw node :run-time t))
(defmethod reveal :circle [_ node t] (s/draw node :run-time t))

(defn outline?
  "True for items revealed by drawing (lines and shapes)."
  [item]
  (not= (get-method reveal :default) (get-method reveal (:kind item))))

;; ---------------------------------------------------------------------------
;; The interpreter: [registry beat] -> [registry' steps]
;;
;; A step is {:anims [animation] :lag r?} (played together, or staggered by
;; lag) or {:hold seconds}.

(def empty-registry {:nodes {} :groups {} :pace 1.0})

(defn- rt [{:keys [pace]} seconds] (* seconds pace))

(defn- member-ids
  "ids with every group id replaced by its members; :all is every node."
  [{:keys [nodes groups]} ids]
  (if (= :all ids)
    (keys nodes)
    (distinct (mapcat (fn [id] (if-let [ms (get groups id)] ms [id])) ids))))

(defn nodes-of
  "The nodes registry holds for ids (group ids resolved)."
  [reg ids]
  (keep #(get-in reg [:nodes %]) (member-ids reg ids)))

(defn- build [reg items]
  (reduce (fn [[r ns] it] (let [nd (realize it)] [(assoc-in r [:nodes (:id it)] nd) (conj ns [it nd])]))
          [reg []] items))

(defn- forget [reg ids]
  (let [gone (set (member-ids reg ids))
        named (if (= :all ids) #{} (set ids))]
    (-> reg
        (update :nodes #(apply dissoc % gone))
        (update :groups (fn [gs] (if (= :all ids)
                                   {}
                                   (into {} (remove (fn [[g ms]] (or (named g) (every? gone ms)))) gs)))))))

(defn- step [anims & {:keys [lag]}]
  (when (seq anims) (cond-> {:anims (vec anims)} lag (assoc :lag lag))))

(defn- steps [& ss] (vec (keep identity ss)))

(defmulti animate
  "[registry beat] -> [registry' steps]. Open on :beat."
  (fn [_reg beat] (:beat beat)))

(defn- show [reg {:keys [items groups lag run-time]} default-run-time]
  (let [[reg' built] (build reg items)
        t (rt reg (or run-time default-run-time))]
    [(update reg' :groups merge groups)
     (steps (step (map (fn [[it nd]] (reveal it nd t)) built) :lag lag))]))

(defmethod animate :show [reg beat] (show reg beat 0.5))

(defmethod animate :shade [reg beat] (show reg beat 0.8))

(defmethod animate :hide [reg {:keys [ids]}]
  [(forget reg ids)
   (steps (step (map #(s/vanish % :run-time (rt reg 0.4)) (nodes-of reg ids))))])

(defmethod animate :swap [reg {:keys [ids items]}]
  (let [[r1 a1] (animate reg {:beat :hide :ids ids})
        [r2 a2] (animate r1 {:beat :show :items items})]
    [r2 (steps (step (mapcat :anims (concat a1 a2))))]))

(defmethod animate :caption [reg {:keys [items hold]}]
  (let [[reg' ss] (animate reg {:beat :swap :ids (mapv :id items) :items items})]
    [reg' (cond-> ss hold (conj {:hold (rt reg hold)}))]))

(defmethod animate :recolor [reg {:keys [ids color]}]
  [reg (steps (step (map #(s/recolor % color :run-time (rt reg 0.4)) (nodes-of reg ids))))])

(defmethod animate :mark [reg {:keys [ids color]}]
  (let [nds (nodes-of reg ids)]
    [reg (steps (step (map #(s/recolor % color :run-time (rt reg 0.4)) nds))
                (step (map #(s/emphasize % :run-time (rt reg 0.5)) nds)))]))

(defmethod animate :move [reg {:keys [to]}]
  [reg (steps (step (keep (fn [[id p]] (some-> (get-in reg [:nodes id]) (s/glide p :run-time (rt reg 0.5))))
                          to)))])

(defmethod animate :emphasize [reg {:keys [ids]}]
  [reg (steps (step (map #(s/emphasize % :run-time (rt reg 0.5)) (nodes-of reg ids))))])

(defmethod animate :diagram [reg {:keys [items groups]}]
  (let [[reg' built] (build reg items)
        {outlines true others false} (group-by (comp outline? first) built)]
    [(update reg' :groups merge groups)
     (steps (step (map (fn [[it nd]] (reveal it nd (rt reg 0.8))) outlines))
            (step (map (fn [[_ nd]] (s/appear nd :run-time (rt reg 0.5))) others)))]))

(defmethod animate :par [reg {:keys [beats]}]
  (let [[reg' ss] (reduce (fn [[r ss] b] (let [[r' ss'] (animate r b)] [r' (into ss ss')]))
                          [reg []] beats)]
    [reg' (steps (step (mapcat :anims ss)))]))

(defmethod animate :hold [reg {:keys [seconds]}]
  [reg [{:hold (rt reg (or seconds 1))}]])

(defn- run-step! [stage {:keys [anims lag hold]}]
  (cond hold (s/hold! stage hold)
        lag (s/play! stage (s/stagger anims :lag-ratio lag))
        :else (s/play! stage (s/together anims))))

(defn play!
  "Boundary: play beats on stage through desargues.scene, in order. opts:
   :pace (run-time multiplier, default 1). Returns the final registry."
  [stage beats & {:keys [pace] :or {pace 1.0}}]
  (reduce (fn [reg b]
            (let [[reg' ss] (animate reg b)]
              (doseq [st ss] (run-step! stage st))
              reg'))
          (assoc empty-registry :pace (double pace))
          beats))
