(ns desargues.bench.chart
  "SVG charts from a desargues.bench report. `charts` is pure (report -> file
   name -> SVG hiccup); `write-all!` is the only side effect."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def theme
  {:bg "#0b0e13" :fg "#e8e6e3" :muted "#8a8f98" :grid "#232833"
   :series ["#4fd1c5" "#f0ac5f" "#7aa2f7" "#f7768e" "#9ece6a" "#bb9af7"]})

(defn- color [i] (nth (:series theme) (mod i (count (:series theme)))))

;; ── hiccup -> SVG text ──────────────────────────────────────────────────────

(defn- esc [s]
  (-> (str s)
      (str/replace "&" "&amp;") (str/replace "<" "&lt;")
      (str/replace ">" "&gt;") (str/replace "\"" "&quot;")))

(defn- attrs->str [m]
  (apply str (for [[k v] (sort-by (comp name key) m) :when (some? v)]
               (str " " (name k) "=\"" (esc v) "\""))))

(defn svg->str
  "Render hiccup (keyword tag, optional attr map, children; seqs splice) to SVG text."
  [x]
  (cond
    (nil? x)        ""
    (string? x)     (esc x)
    (and (sequential? x) (keyword? (first x)))
    (let [[tag & more] x
          [attrs kids] (if (map? (first more)) [(first more) (rest more)] [{} more])]
      (str "<" (name tag) (attrs->str attrs) ">"
           (apply str (map svg->str kids))
           "</" (name tag) ">"))
    (sequential? x) (apply str (map svg->str x))
    :else           (esc x)))

;; ── number formatting and ticks ─────────────────────────────────────────────

(defn fmt-ms
  "Milliseconds (any number) -> a short human label (µs / ms / s)."
  [ms]
  (let [ms (double ms)]
    (cond
      (< ms 1)    (format "%.1f µs" (* ms 1000))
      (< ms 1000) (format "%.2f ms" ms)
      :else       (format "%.2f s" (/ ms 1000)))))

(defn- nice-step [range n]
  (let [raw (/ range (max 1 n))
        mag (Math/pow 10 (Math/floor (Math/log10 raw)))
        r   (/ raw mag)]
    (* mag (cond (<= r 1) 1 (<= r 2) 2 (<= r 5) 5 :else 10))))

(defn- ticks [lo hi n]
  (let [step (nice-step (- hi lo) n)]
    (take-while #(<= % (+ hi (* 0.5 step))) (iterate #(+ % step) (* step (Math/floor (/ lo step)))))))

;; ── charts ──────────────────────────────────────────────────────────────────

(defn bar-chart
  "Horizontal bars. bars: [{:label s :value ms :err ms}]."
  [{:keys [title subtitle bars width] :or {width 760}}]
  (let [{:keys [bg fg muted grid]} theme
        n      (count bars)
        row-h  36
        top    70
        left   210
        right  110
        h      (+ top (* n row-h) 36)
        plot-w (- width left right)
        vmax   (reduce max 1e-9 (map (fn [b] (+ (:value b) (or (:err b) 0))) bars))
        x      (fn [v] (+ left (* plot-w (/ v vmax))))]
    [:svg {:xmlns "http://www.w3.org/2000/svg" :width width :height h
           :viewBox (str "0 0 " width " " h) :font-family "system-ui, -apple-system, Segoe UI, sans-serif"}
     [:rect {:x 0 :y 0 :width width :height h :fill bg :rx 8}]
     [:text {:x 24 :y 32 :fill fg :font-size 18 :font-weight 700} title]
     [:text {:x 24 :y 52 :fill muted :font-size 12} subtitle]
     (for [t (ticks 0 vmax 5)]
       [:g
        [:line {:x1 (x t) :x2 (x t) :y1 top :y2 (- h 30) :stroke grid}]
        [:text {:x (x t) :y (- h 12) :fill muted :font-size 11 :text-anchor "middle"} (fmt-ms t)]])
     (map-indexed
      (fn [i {:keys [label value err]}]
        (let [y  (+ top (* i row-h) 7)
              bh (- row-h 14)
              cy (+ y (/ bh 2))]
          [:g
           [:text {:x (- left 12) :y (+ cy 4) :fill fg :font-size 12 :text-anchor "end"} label]
           [:rect {:x left :y y :width (max 1 (- (x value) left)) :height bh :fill (color i) :rx 3}]
           (when (and err (pos? err))
             [:line {:x1 (x (max 0 (- value err))) :x2 (x (+ value err)) :y1 cy :y2 cy
                     :stroke fg :stroke-width 1.5}])
           [:text {:x (+ (x (+ value (or err 0))) 8) :y (+ cy 4) :fill fg :font-size 11} (fmt-ms value)]]))
      bars)]))

(defn line-chart
  "Log-log lines. series: [{:label s :points [[n ms] ...]}]. X ticks closer than
   6% (in log10) to the previous one are not labelled."
  [{:keys [title subtitle series x-label y-label width height]
    :or {width 760 height 440 x-label "n" y-label "ms"}}]
  (let [{:keys [bg fg muted grid]} theme
        left 80 right 40 top 70 bottom 56
        pw   (- width left right)
        ph   (- height top bottom)
        pts  (mapcat :points series)
        lg   (fn [v] (Math/log10 (max 1e-9 v)))
        xmin (lg (reduce min (map first pts)))
        xmax (lg (reduce max (map first pts)))
        ymin (Math/floor (lg (reduce min (map second pts))))
        ymax (Math/ceil (lg (reduce max (map second pts))))
        X    (fn [v] (+ left (* pw (/ (- (lg v) xmin) (max 1e-9 (- xmax xmin))))))
        Y    (fn [v] (- (+ top ph) (* ph (/ (- (lg v) ymin) (max 1e-9 (- ymax ymin))))))
        xt   (reduce (fn [acc x] (if (and (seq acc) (< (- (lg x) (lg (peek acc))) 0.06)) acc (conj acc x)))
                     [] (sort (distinct (map first pts))))
        yt   (map #(Math/pow 10 %) (range (long ymin) (inc (long ymax))))]
    [:svg {:xmlns "http://www.w3.org/2000/svg" :width width :height height
           :viewBox (str "0 0 " width " " height) :font-family "system-ui, -apple-system, Segoe UI, sans-serif"}
     [:rect {:x 0 :y 0 :width width :height height :fill bg :rx 8}]
     [:text {:x 24 :y 32 :fill fg :font-size 18 :font-weight 700} title]
     [:text {:x 24 :y 52 :fill muted :font-size 12} subtitle]
     (for [t yt]
       [:g
        [:line {:x1 left :x2 (+ left pw) :y1 (Y t) :y2 (Y t) :stroke grid}]
        [:text {:x (- left 10) :y (+ (Y t) 4) :fill muted :font-size 11 :text-anchor "end"} (fmt-ms t)]])
     (for [t xt]
       [:g
        [:line {:x1 (X t) :x2 (X t) :y1 top :y2 (+ top ph) :stroke grid}]
        [:text {:x (X t) :y (+ top ph 18) :fill muted :font-size 11 :text-anchor "middle"} (str t)]])
     [:text {:x (+ left (/ pw 2)) :y (- height 10) :fill muted :font-size 12 :text-anchor "middle"} x-label]
     [:text {:x 18 :y (+ top (/ ph 2)) :fill muted :font-size 12 :text-anchor "middle"
             :transform (str "rotate(-90 18 " (+ top (/ ph 2)) ")")} y-label]
     (map-indexed
      (fn [i {:keys [label points]}]
        (let [c (color i)
              d (str/join " " (map (fn [[x y]] (str (X x) "," (Y y))) points))]
          [:g
           [:polyline {:points d :fill "none" :stroke c :stroke-width 2.5 :stroke-linejoin "round"}]
           (for [[x y] points] [:circle {:cx (X x) :cy (Y y) :r 4 :fill c}])
           [:g {:transform (str "translate(" (+ left 12) "," (+ top 14 (* i 18)) ")")}
            [:line {:x1 0 :x2 22 :y1 0 :y2 0 :stroke c :stroke-width 3}]
            [:text {:x 30 :y 4 :fill fg :font-size 12} label]]]))
      series)]))

;; ── report -> charts ────────────────────────────────────────────────────────

(defn charts
  "Bench report -> {file-name svg-hiccup}. Only groups present in the report
   produce a chart."
  [{:keys [results jvm clojure cpu]}]
  (let [by   (group-by :group results)
        sub  (str "mean per call ± σ · JDK " jvm " · Clojure " clojure " · " cpu)
        bars (fn [rows] (mapv (fn [r] {:label (or (:id r) (str "n = " (:n r)))
                                       :value (:mean-ms r) :err (:std-ms r)}) rows))
        scaling (keep (fn [[g label]]
                        (when-let [rows (seq (by g))]
                          {:label label :points (mapv (juxt :n :mean-ms) (sort-by :n rows))}))
                      [[:layout "resolve-layout (elements)"]
                       [:scene-graph "layout->scene-graph (elements)"]
                       [:recording "recording render! (animations)"]])]
    (cond-> {}
      (by :emmy)
      (assoc "emmy-conveyor.svg"
             (bar-chart {:title "Emmy conveyor: f → f′ → LaTeX (both)" :subtitle sub :bars (bars (by :emmy))}))
      (by :solver)
      (assoc "solvers.svg"
             (bar-chart {:title "Pendulum trajectory, 10 s at dt = 0.01" :subtitle sub :bars (bars (by :solver))}))
      (seq scaling)
      (assoc "scaling.svg"
             (line-chart {:title "Pure backend: time vs size (log–log)" :subtitle sub
                          :x-label "size" :y-label "time" :series (vec scaling)}))
      (by :manim)
      (assoc "manim-render.svg"
             (bar-chart {:title "Manim render of a 16-cell layout (wall time)" :subtitle sub :bars (bars (by :manim))})))))

(defn write-all!
  "Write every chart of `report` into `dir`; returns the file names written."
  [report dir]
  (let [cs (charts report)]
    (.mkdirs (io/file dir))
    (doseq [[f svg] cs] (spit (io/file dir f) (svg->str svg)))
    (vec (keys cs))))
