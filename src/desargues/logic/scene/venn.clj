(ns desargues.logic.scene.venn
  "The Venn flow: a class statement (= L R) or (subset L R) shown as shaded
   regions of a Venn diagram of 0 to 3 circles inside a universe rectangle.

   Strata, bottom up:
     regions   a region is the set of class letters an element is in; an
               expression DENOTES the regions where its membership formula
               (desargues.logic.classes) evaluates true.
     verdict   open on the statement operator: does the statement hold, and
               which regions witness its failure.
     layout    pure geometry in world units (centred, y up, 14.2 x 8): circle
               placement open on the circle count, a hex grid of shading dots
               keeping clear of outlines and letters.
     beats     the flow as plain data, every position already computed.
     boundary  play-beats! interprets beats on a desargues.scene stage; the
               interpretation is open on the beat kind.

   A region is shaded by dots placed only where the region test holds.

     (record :de-morgan '(= (compl (union A B)) (inter (compl A) (compl B))))"
  (:require [clojure.set :as set]
            [desargues.logic.classes :as classes]
            [desargues.logic.formula :as f]
            [desargues.logic.tex :as tex]
            [desargues.scene :as s]
            [desargues.scene.data :as rec]
            [desargues.tex.label :as label]
            [desargues.logic.scene.beats :as b]))

;; =============================================================================
;; Regions
;; =============================================================================

(defn membership-of
  "The propositional formula for x in expr, atoms (in x A) with A a letter."
  [expr]
  (classes/membership (list 'in classes/element expr)))

(defn letters
  "The class letters a class statement or membership formula mentions, sorted
   by name."
  [formula]
  (->> (f/atoms (classes/membership formula))
       (keep (fn [a] (when (and (seq? a) (= 'in (first a))) (nth a 2))))
       distinct
       (sort-by str)
       vec))

(defn regions
  "Every region over letters: the set of letters an element is in, all 2^n."
  [letters]
  (mapv (fn [v] (set (filter v letters))) (f/valuations letters)))

(defn region-env
  "The valuation of the membership atoms an element of region gives."
  [region letters]
  (into {} (for [l letters] [(list 'in classes/element l) (contains? region l)])))

(defn denotes
  "The set of regions over letters where x in expr holds."
  [expr letters]
  (let [m (membership-of expr)]
    (set (filter #(f/evaluate m (region-env % letters)) (regions letters)))))

;; =============================================================================
;; Verdict: open on the statement operator
;; =============================================================================

(defmulti verdict
  "For a statement (op L R) with region sets lhs and rhs:
   {:holds? bool :offending #{regions} :caption string}."
  (fn [op _lhs _rhs] op))

(defmethod verdict '= [_ lhs rhs]
  (let [off (set/union (set/difference lhs rhs) (set/difference rhs lhs))]
    {:holds? (empty? off)
     :offending off
     :caption (if (empty? off)
                "same regions: the identity holds"
                "the regions differ: the identity fails")}))

(defmethod verdict 'subset [_ lhs rhs]
  (let [off (set/difference lhs rhs)]
    {:holds? (empty? off)
     :offending off
     :caption (if (empty? off)
                "every left region lies on the right: the inclusion holds"
                "left regions outside the right: the inclusion fails")}))

(defmethod verdict :default [op _ _]
  (throw (ex-info "No Venn verdict for this statement operator" {:op op})))

(defn analyse
  "Statement (op L R) -> {:statement :op :lhs-expr :rhs-expr :letters :lhs
   :rhs :verdict}, :lhs and :rhs the region sets of each side."
  [statement]
  (let [[op l r] statement
        ls (letters statement)]
    (when (> (count ls) 3)
      (throw (ex-info "A Venn diagram draws at most 3 class letters" {:letters ls})))
    (let [lhs (denotes l ls) rhs (denotes r ls)]
      {:statement statement :op op :lhs-expr l :rhs-expr r :letters ls
       :lhs lhs :rhs rhs :verdict (verdict op lhs rhs)})))

;; =============================================================================
;; Layout (world units)
;; =============================================================================

(def world
  "The world frame: x and y bounds of the centred, y-up scene."
  {:x [(- b/half-width) b/half-width] :y [(- b/half-height) b/half-height]})

(def universe-size
  "Universe rectangle [w h] in diagram units (scale 1)."
  [6.0 4.6])

(defmulti circle-placement
  "{:centres [[x y] ...] :radius r} in diagram units for n circles."
  identity)

(defmethod circle-placement 0 [_] {:centres [] :radius 0})
(defmethod circle-placement 1 [_] {:centres [[0.0 0.0]] :radius 1.6})
(defmethod circle-placement 2 [_] {:centres [[-0.8 0.0] [0.8 0.0]] :radius 1.4})
(defmethod circle-placement 3 [_] {:centres [[-0.75 0.6] [0.75 0.6] [0.0 -0.7]] :radius 1.35})

(defn- v+ [a b] (mapv + a b))
(defn- v* [k a] (mapv #(* k %) a))
(defn- dist [a b] (Math/hypot (- (a 0) (b 0)) (- (a 1) (b 1))))

(defn- outward
  "Unit vector from the diagram centre towards c; up-left for c at the centre."
  [c]
  (let [n (Math/hypot (c 0) (c 1))]
    (if (< n 1e-9) [-0.7071 0.7071] (v* (/ 1.0 n) c))))

(defn diagram
  "World geometry of a Venn diagram of letters placed by frame
   {:center [x y] :scale k}:
   {:center :scale :universe {:center :size} :circles [{:letter :center :radius}]
    :labels [{:latex :at}]}. Letter labels sit inside each circle's own region;
   the universe label sits in the top-left corner."
  [letters {:keys [center scale] :or {center [0.0 0.0] scale 1.0}}]
  (let [{:keys [centres radius]} (circle-placement (count letters))
        world-pt (fn [p] (v+ center (v* scale p)))
        [w h] universe-size
        circles (mapv (fn [l c] {:letter l :center (world-pt c) :radius (* scale radius)})
                      letters centres)
        labels (mapv (fn [l c] {:latex (tex/->TeX l)
                                :at (world-pt (v+ c (v* (* 0.62 radius) (outward c))))})
                     letters centres)]
    {:center center :scale scale
     :universe {:center center :size [(* scale w) (* scale h)]}
     :circles circles
     :labels (conj labels {:latex (tex/->TeX 'universe)
                           :at (world-pt [(+ (/ w -2) 0.32) (- (/ h 2) 0.32)])})}))

(defn region-at
  "The region of world point p in diagram: the letters whose circle holds p."
  [{:keys [circles]} p]
  (set (for [{:keys [letter center radius]} circles
             :when (< (dist p center) radius)]
         letter)))

(def shading-defaults
  "Hex-grid step, dot radius, gaps (diagram units, scaled with the diagram)."
  {:step 0.17 :dot-radius 0.05 :outline-gap 0.09 :label-gap 0.3 :edge-gap 0.12})

(defn- clear?
  [{:keys [circles labels scale]} p {:keys [outline-gap label-gap]}]
  (and (not-any? #(< (Math/abs (- (dist p (:center %)) (:radius %))) (* scale outline-gap)) circles)
       (not-any? #(< (dist p (:at %)) (* scale label-gap)) labels)))

(defn cells
  "Every shading dot position of diagram on a hex grid, with its region:
   [{:at [x y] :region #{...}}], kept clear of outlines, labels and the
   universe edge."
  ([d] (cells d {}))
  ([{:keys [universe scale] :as d} opts]
   (let [{:keys [step edge-gap] :as o} (merge shading-defaults opts)
         step (* scale step)
         [cx cy] (:center universe)
         [w h] (:size universe)
         inset (* scale edge-gap)
         x0 (+ (- cx (/ w 2)) inset) x1 (- (+ cx (/ w 2)) inset)
         y0 (+ (- cy (/ h 2)) inset) y1 (- (+ cy (/ h 2)) inset)
         dy (* step (Math/sqrt 0.75))]
     (vec (for [i (range (inc (int (/ (- y1 y0) dy))))
                :let [y (+ y0 (* i dy))
                      off (if (odd? i) (/ step 2) 0.0)]
                j (range (inc (int (/ (- x1 x0 off) step))))
                :let [p [(+ x0 off (* j step)) y]]
                :when (clear? d p o)]
            {:at p :region (region-at d p)})))))

(defn shading
  "{region [[x y] ...]} for the regions of diagram in region-set."
  ([d region-set] (shading d region-set {}))
  ([d region-set opts]
   (let [by (group-by :region (cells d opts))]
     (into {} (for [r region-set] [r (mapv :at (get by r []))])))))

;; =============================================================================
;; Beats: the flow as data
;; =============================================================================

(def frames
  "Where each diagram of the flow sits."
  {:main {:center [0.0 0.1] :scale 1.05}
   :left {:center [-3.45 -0.1] :scale 0.95}
   :right {:center [3.45 -0.1] :scale 0.95}})

(defn- shade-beat [key d region-set color opts]
  {:beat :shade :key key :color color
   :radius (* (:scale d) (:dot-radius (merge shading-defaults opts)))
   :dots (shading d region-set opts)})

(defn- caption [key latex at size color]
  {:beat :caption :key key :latex latex :at at :size size :color color})

(defn- side-phase
  "Beats shading one side on the main diagram: the statement with that side
   highlighted, the side's regions, its membership test underneath."
  [{:keys [statement]} d path expr region-set color opts]
  [(caption :statement (tex/->TeX (tex/highlight statement path color)) [0.0 3.45] 30 :white)
   (shade-beat :main-shade d region-set color opts)
   (caption :test (tex/->TeX (membership-of expr)) [0.0 -3.2] 26 color)
   {:beat :hold :seconds 1.5}
   {:beat :clear :keys [:main-shade :test :statement]}])

(defn beats
  "The Venn flow for statement as plain data. opts: shading-defaults keys.
   Beats: :diagram :caption :shade :recolor :text :clear :hold."
  ([statement] (beats statement {}))
  ([statement opts]
   (let [{:keys [letters lhs-expr rhs-expr lhs rhs verdict] :as a} (analyse statement)
         main (diagram letters (:main frames))
         left (diagram letters (:left frames))
         right (diagram letters (:right frames))
         {:keys [holds? offending]} verdict]
     (vec
      (concat
       [{:beat :diagram :key :main :geometry main}]
       (side-phase a main [0] lhs-expr lhs :gold opts)
       (side-phase a main [1] rhs-expr rhs :teal opts)
       [{:beat :clear :keys :all}
        {:beat :diagram :key :left :geometry left}
        {:beat :diagram :key :right :geometry right}
        (caption :left-tex (tex/->TeX lhs-expr) [-3.45 2.75] 30 :gold)
        (caption :right-tex (tex/->TeX rhs-expr) [3.45 2.75] 30 :teal)
        (shade-beat :left-shade left lhs :gold opts)
        (shade-beat :right-shade right rhs :teal opts)
        {:beat :hold :seconds 1.0}]
       (when-not holds?
         [{:beat :recolor :color :red
           :targets (vec (for [r (sort-by #(vec (sort-by str %)) offending)
                               k [:left-shade :right-shade]]
                           [k r]))}])
       [{:beat :text :key :verdict :text (:caption verdict) :at [0.0 -3.25] :size 26
         :color (if holds? :teal :red)}
        {:beat :hold :seconds 2.0}])))))

(defmulti beat-points
  "Every world point a beat draws at, widened by its radii."
  :beat)

(defmethod beat-points :default [_] [])

(defmethod beat-points :diagram [{{:keys [universe circles labels]} :geometry}]
  (let [[cx cy] (:center universe) [w h] (:size universe)]
    (concat [[(- cx (/ w 2)) (- cy (/ h 2))] [(+ cx (/ w 2)) (+ cy (/ h 2))]]
            (for [{[x y] :center r :radius} circles
                  p [[(- x r) y] [(+ x r) y] [x (- y r)] [x (+ y r)]]]
              p)
            (map :at labels))))

(defmethod beat-points :shade [{:keys [dots radius]}]
  (for [[x y] (apply concat (vals dots))
        p [[(- x radius) (- y radius)] [(+ x radius) (+ y radius)]]]
    p))

(defmethod beat-points :caption [{:keys [at]}] [at])
(defmethod beat-points :text [{:keys [at]}] [at])

(defn inside-world?
  "True when every point the beats draw at lies inside the world frame."
  [bs]
  (let [{[x0 x1] :x [y0 y1] :y} world]
    (every? (fn [[x y]] (and (<= x0 x x1) (<= y0 y y1))) (mapcat beat-points bs))))

;; =============================================================================
;; Boundary: beats -> desargues.scene
;; =============================================================================

(defn- dot-node [p color r]
  (-> (s/dot :color color :radius r) (s/move-to p)))

(defmulti stage-beat!
  "Play one beat on stage; registry maps a key (or [key region] for shading)
   to the nodes it made. Returns the new registry."
  (fn [_stage _registry beat] (:beat beat)))

(defmethod stage-beat! :diagram [stage registry {:keys [key geometry]}]
  (let [{:keys [universe circles labels]} geometry
        [w h] (:size universe)
        u (-> (s/rectangle :width w :height h)
              (s/stroke! :color :grey :width 2)
              (s/move-to (:center universe)))
        cs (mapv (fn [{:keys [center radius]}]
                   (-> (s/circle :radius radius)
                       (s/stroke! :color :white :width 3)
                       (s/move-to center)))
                 circles)
        ls (mapv (fn [{:keys [latex at]}] (label/label latex at :size 30 :color :white)) labels)]
    (s/play! stage (s/together (concat [(s/draw u :run-time 0.8)]
                                       (map #(s/draw % :run-time 0.8) cs))))
    (s/play! stage (s/together (map #(s/appear % :run-time 0.5) ls)))
    (assoc registry key (into [u] (concat cs ls)))))

(defmethod stage-beat! :caption [stage registry {:keys [key latex at size color]}]
  (let [n (label/label latex at :size size :color color)]
    (s/play! stage (s/appear n :run-time 0.6))
    (assoc registry key [n])))

(defmethod stage-beat! :text [stage registry {:keys [key text at size color]}]
  (let [n (-> (s/text text :font-size size :color color) (s/move-to at))]
    (s/play! stage (s/appear n :run-time 0.6))
    (assoc registry key [n])))

(defmethod stage-beat! :shade [stage registry {:keys [key dots color radius]}]
  (let [by-region (into {} (for [[r ps] dots] [r (mapv #(dot-node % color radius) ps)]))
        all (vec (mapcat val by-region))]
    (when (seq all)
      (s/play! stage (s/together (map #(s/appear % :run-time 0.8) all))))
    (reduce-kv (fn [reg r ns] (assoc reg [key r] ns))
               (assoc registry key all)
               by-region)))

(defmethod stage-beat! :recolor [stage registry {:keys [targets color]}]
  (let [ns (mapcat #(get registry %) targets)]
    (when (seq ns)
      (s/play! stage (s/together (map #(s/recolor % color :run-time 0.6) ns))))
    registry))

(defn- owned-by? [k entry-key]
  (or (= k entry-key) (and (vector? entry-key) (= k (first entry-key)))))

(defmethod stage-beat! :clear [stage registry {:keys [keys]}]
  (let [gone? (if (= :all keys)
                (constantly true)
                (fn [ek] (some #(owned-by? % ek) keys)))
        ns (->> registry (filter (fn [[k _]] (and (gone? k) (not (vector? k)))))
                (mapcat val) distinct)]
    (when (seq ns)
      (s/play! stage (s/together (map #(s/vanish % :run-time 0.5) ns))))
    (into {} (remove (fn [[k _]] (gone? k)) registry))))

(defmethod stage-beat! :hold [stage registry {:keys [seconds]}]
  (s/hold! stage seconds)
  registry)

(defn play-beats!
  "Interpret beats in order on stage. Returns the final registry."
  [stage bs]
  (reduce #(stage-beat! stage %1 %2) {} bs))

(defn construct
  "A desargues construct fn playing the Venn flow of statement."
  ([statement] (construct statement {}))
  ([statement opts]
   (let [bs (beats statement opts)]
     (fn [stage] (play-beats! stage bs)))))

(defn record
  "The Venn flow of statement recorded as an EDN scene graph (no Python)."
  ([scene-name statement] (record scene-name statement {}))
  ([scene-name statement opts]
   (s/with-backend (rec/recording-backend)
     (s/render! scene-name (construct statement opts)))))
