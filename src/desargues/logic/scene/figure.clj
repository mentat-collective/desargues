(ns desargues.logic.scene.figure
  "Figure items shared by the arrow, grid and Hasse flows, registered on the
   open item kinds of desargues.logic.scene.beats, plus pure helpers.

   Items (besides the beats kinds :tex :text :line :rect :dot):
     {:id :kind :block :center [x y] :size [w h] :color kw}   a filled rectangle

   `arrow` and `polyline` build :line items (a shaft plus two head strokes
   for an arrow), so they need nothing beyond the facade's line.
   for an arrow), so they need nothing beyond the facade's line.
   `elem-tex` renders a model value (element, class, pair) as TeX;
   `ordered` sorts a class of model values deterministically."
  (:require [clojure.string :as str]
            [desargues.logic.scene.beats :as b]
            [desargues.logic.tex :as lt]
            [desargues.scene :as s]))

;; ---------------------------------------------------------------------------
;; Values (pure)

(defn- cmp [x y]
  (let [kx (if (number? x) 0 1) ky (if (number? y) 0 1)]
    (cond (not= kx ky) (compare kx ky)
          (number? x) (compare x y)
          :else (compare (pr-str x) (pr-str y)))))

(defn ordered
  "The members of class xs as a vector: numbers ascending, then the rest by
   printed form."
  [xs]
  (vec (sort cmp xs)))

(defn elem-tex
  "TeX for a model value: a class as \\{...\\}, a pair as (a, b), anything
   else through desargues.logic.tex."
  [x]
  (cond (set? x) (str "\\{" (str/join ", " (map elem-tex (ordered x))) "\\}")
        (vector? x) (str "(" (str/join ", " (map elem-tex x)) ")")
        (or (number? x) (symbol? x)) (lt/->TeX x)
        (keyword? x) (str "\\mathrm{" (name x) "}")
        :else (str "\\mathrm{" (str/replace (str x) #"[{}\\]" "") "}")))

(defn class-tex
  "TeX for the class xs written out: \\{a, b\\}, or \\varnothing."
  [xs]
  (if (empty? xs) "\\varnothing" (elem-tex (set xs))))

;; ---------------------------------------------------------------------------
;; Items (pure)

(defn tex
  "A :tex item."
  [id latex at size color]
  {:id id :kind :tex :latex latex :at at :size size :color color})

(defn text
  "A :text item."
  [id t at size color]
  {:id id :kind :text :text t :at at :size size :color color})

(defn dot
  "A :dot item."
  [id at radius color]
  {:id id :kind :dot :at at :radius radius :color color})

(defn box
  "A rectangle item centred at at: a filled :block, or a beats :rect outline."
  [id at [w h] color fill?]
  (if fill?
    {:id id :kind :block :center at :size [w h] :color color}
    {:id id :kind :rect :center at :size [w h] :color color :width 2}))

(defn line
  "A :line item."
  [id from to color width]
  {:id id :kind :line :from from :to to :color color :width width})

(defn- v- [a c] (mapv - a c))
(defn- v+ [a c] (mapv + a c))
(defn- v* [k a] (mapv #(* k %) a))
(defn- norm [[x y]] (Math/hypot x y))

(defn arrow
  "Line items drawing an arrow from p to q, both ends pulled in by gap:
   ids [id :shaft], [id :head 0], [id :head 1]."
  [id p q color & {:keys [gap head width] :or {gap 0.12 head 0.18 width 2}}]
  (let [d (v- q p)
        n (max 1e-9 (norm d))
        u (v* (/ 1.0 n) d)
        gap (min gap (* 0.3 n))
        a (v+ p (v* gap u))
        z (v- q (v* gap u))
        [ux uy] u
        back (v* (- head) u)
        side (v* (* 0.55 head) [(- uy) ux])]
    [(line [id :shaft] a z color width)
     (line [id :head 0] z (v+ z (v+ back side)) color width)
     (line [id :head 1] z (v+ z (v- back side)) color width)]))

(defn arrow-ids
  "The ids of the line items of arrow id."
  [id]
  [[id :shaft] [id :head 0] [id :head 1]])

(defn polyline
  "Line items joining points ps in order: ids [id k]."
  [id ps color width]
  (mapv (fn [k a c] (line [id k] a c color width)) (range) ps (rest ps)))

(defn loop-arrow
  "Line items for a small loop at a dot centred at c, bulging in unit
   direction dir, closed by an arrow head: ids [id k] and [id :head j]."
  [id c dir color & {:keys [r] :or {r 0.22}}]
  (let [[dx dy] dir
        o (v+ c (v* (* 1.25 r) dir))
        ang0 (Math/atan2 dy dx)
        ps (for [k (range 9)
                 :let [t (+ ang0 Math/PI -1.05 (* k (/ (- (* 2 Math/PI) 2.1) 8)))]]
             (v+ o (v* r [(Math/cos t) (Math/sin t)])))
        segs (polyline id ps color 2)
        [p q] (take-last 2 ps)
        head (drop 1 (arrow id p q color :gap 0.0 :head 0.14))]
    (into segs head)))

;; ---------------------------------------------------------------------------
;; Registration on the open item kinds

(defmethod b/extent :block [item] (b/extent (assoc item :kind :rect)))

(defmethod b/realize :block [{:keys [center size color]}]
  (let [[w h] size]
    (-> (s/rectangle :width w :height h)
        (s/stroke! :color color :width 2)
        (s/fill! color :opacity 0.75)
        (s/move-to center))))

;; ---------------------------------------------------------------------------
;; Beat helpers (pure)

(defn caption
  "The beat putting text (plain) in the caption slot at y, fitted to the world
   width."
  [t y color]
  (b/caption (text nil t [0.0 y] 26 color) (* 2 (- b/half-width 0.3))))

(defn tex-caption
  "The beat putting latex in the caption slot at y, fitted to the world width."
  [latex y color]
  (b/caption (tex nil latex [0.0 y] 32 color) (* 2 (- b/half-width 0.3))))

(defn note
  "The beat replacing the node id by plain text at [x y], fitted to width w."
  [id t [x y] w color]
  (let [it (text id t [x y] 24 color)]
    {:beat :swap :ids [id] :items [(assoc it :size (b/fit-size it w))]}))

(defn inside-world?
  "True when every footprint of beats lies inside the world."
  [beats]
  (every? b/inside? (b/footprints beats)))
