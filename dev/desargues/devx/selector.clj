(ns desargues.devx.selector
  "Interactive Segment Selector for REPL Development.
   
   Provides a simple terminal menu for selecting segments to preview,
   render, or inspect. Shows segment status and dependency graph.
   
   ## Quick Start
   
   ```clojure
   (require '[desargues.devx.selector :as sel])
   
   ;; Show segment menu and select one
   (sel/select! graph)
   
   ;; Select and preview
   (sel/select-and-preview! graph)
   
   ;; Select and render
   (sel/select-and-render! graph)
   
   ;; Show dependency graph
   (sel/show-graph graph)
   ```
   
   ## Menu Navigation
   
   - Enter number to select segment
   - Enter 'q' to quit/cancel
   - Enter 'r' to refresh list
   - Enter 'g' to show dependency graph
   - Enter 'd' to select dirty segments only"
  (:require [desargues.devx.segment :as seg]
            [desargues.devx.graph :as graph]
            [clojure.string :as str]))

;; =============================================================================
;; Status Indicators
;; =============================================================================

(def status-symbols
  "Unicode symbols for segment status."
  {:pending "○" ; Empty circle
   :dirty "◐" ; Half-filled circle
   :rendering "◔" ; Quarter circle
   :cached "●" ; Filled circle
   :error "✗"}) ; X mark

(def status-colors
  "ANSI color codes for segment status."
  {:pending "\u001b[90m" ; Gray
   :dirty "\u001b[33m" ; Yellow
   :rendering "\u001b[36m" ; Cyan
   :cached "\u001b[32m" ; Green
   :error "\u001b[31m" ; Red
   :reset "\u001b[0m"}) ; Reset

(defn- colorize
  "Wrap text with ANSI color code."
  [text status]
  (str (get status-colors status (:reset status-colors))
       text
       (:reset status-colors)))

(defn- status-indicator
  "Get colored status indicator for a segment."
  [segment]
  (let [state (seg/render-state segment)]
    (colorize (get status-symbols state "?") state)))

;; =============================================================================
;; Segment List Display
;; =============================================================================

(defn- format-segment-line
  "Format a single segment line for display."
  [idx segment & {:keys [show-deps?] :or {show-deps? false}}]
  (let [seg-id (seg/segment-id segment)
        state (seg/render-state segment)
        deps (seg/dependencies segment)
        hash-preview (when-let [h (seg/content-hash segment)]
                       (subs h 0 (min 8 (count h))))]
    (str (format "%3d. " (inc idx))
         (status-indicator segment)
         " "
         (colorize (format "%-20s" (name seg-id)) state)
         (when hash-preview
           (colorize (format " [%s]" hash-preview) :pending))
         (when (and show-deps? (seq deps))
           (str " → " (str/join ", " (map name deps)))))))

(defn print-segment-list
  "Print a formatted list of segments with status."
  [graph & {:keys [show-deps? filter-fn title]
            :or {show-deps? false
                 filter-fn identity
                 title "Segments"}}]
  (let [segments (->> (graph/render-order graph)
                      (map #(graph/get-segment graph %))
                      (filter filter-fn)
                      vec)]
    (println)
    (println (str title " (" (count segments) ")"))
    (println (apply str (repeat (+ (count title) 5) "─")))

    (if (empty? segments)
      (println "  (no segments)")
      (doseq [[idx seg] (map-indexed vector segments)]
        (println (format-segment-line idx seg :show-deps? show-deps?))))

    (println)
    (println "Legend: ○ pending  ◐ dirty  ◔ rendering  ● cached  ✗ error")
    (println)

    segments))

(defn print-dirty-segments
  "Print only dirty segments."
  [graph]
  (print-segment-list graph
                      :filter-fn #(= :dirty (seg/render-state %))
                      :title "Dirty Segments"))

;; =============================================================================
;; Dependency Graph Visualization
;; =============================================================================

(defn- build-tree-lines
  "Build ASCII tree lines for a segment and its dependents."
  [graph seg-id visited depth max-depth]
  (when (< depth max-depth)
    (let [segment (graph/get-segment graph seg-id)
          dependents (graph/dependents graph seg-id)
          prefix (if (zero? depth) "" (str (apply str (repeat (* 2 depth) " ")) "└─ "))]
      (cons
       (str prefix
            (status-indicator segment)
            " "
            (name seg-id))
       (when-not (contains? @visited seg-id)
         (swap! visited conj seg-id)
         (mapcat #(build-tree-lines graph % visited (inc depth) max-depth)
                 dependents))))))

(defn show-graph
  "Display the segment dependency graph as ASCII art."
  [graph & {:keys [max-depth] :or {max-depth 5}}]
  (println)
  (println "Segment Dependency Graph")
  (println "========================")
  (println)

  ;; Find root segments (no dependencies)
  (let [root-ids (filter #(empty? (seg/dependencies (graph/get-segment graph %)))
                         (graph/render-order graph))]
    (if (empty? root-ids)
      (println "(no root segments found)")
      (doseq [root-id root-ids]
        (let [visited (atom #{})]
          (doseq [line (build-tree-lines graph root-id visited 0 max-depth)]
            (println line))
          (println)))))

  (println "Legend: ○ pending  ◐ dirty  ◔ rendering  ● cached  ✗ error")
  (println))

;; =============================================================================
;; Interactive Selection
;; =============================================================================

(defn- read-selection
  "Read user selection from terminal."
  [prompt valid-range]
  (print prompt)
  (flush)
  (let [input (str/trim (read-line))]
    (cond
      (= input "q") :quit
      (= input "r") :refresh
      (= input "g") :graph
      (= input "d") :dirty
      (= input "a") :all

      :else
      (try
        (let [n (Integer/parseInt input)]
          (if (and (>= n 1) (<= n valid-range))
            (dec n) ; Convert to 0-indexed
            (do
              (println (format "Please enter a number between 1 and %d" valid-range))
              nil)))
        (catch NumberFormatException _
          (println "Invalid input. Enter a number, 'q' to quit, or 'g' for graph.")
          nil)))))

(defn select!
  "Interactive segment selector. Returns selected segment or nil.
   
   Commands:
   - Number: Select segment by number
   - q: Quit/cancel
   - r: Refresh list
   - g: Show dependency graph
   - d: Show only dirty segments
   - a: Show all segments"
  [graph]
  (loop [filter-fn identity
         title "Segments"]
    (let [segments (print-segment-list graph
                                       :filter-fn filter-fn
                                       :show-deps? true
                                       :title title)]
      (when (seq segments)
        (let [selection (read-selection
                         "Select segment (number, q=quit, g=graph, d=dirty): "
                         (count segments))]
          (case selection
            :quit nil
            :refresh (recur filter-fn title)
            :graph (do (show-graph graph)
                       (recur filter-fn title))
            :dirty (recur #(= :dirty (seg/render-state %)) "Dirty Segments")
            :all (recur identity "All Segments")

            (if (number? selection)
              (nth segments selection)
              (recur filter-fn title))))))))

(defn select-id!
  "Interactive segment selector that returns the segment ID."
  [graph]
  (when-let [segment (select! graph)]
    (seg/segment-id segment)))

(defn select-multiple!
  "Select multiple segments interactively.
   Returns a vector of selected segments."
  [graph]
  (println "Select segments (enter numbers separated by spaces, or 'all' for all):")
  (let [segments (vec (print-segment-list graph :show-deps? true))]
    (print "Selection: ")
    (flush)
    (let [input (str/trim (read-line))]
      (cond
        (= input "all") segments
        (= input "q") []
        (= input "") []

        :else
        (try
          (->> (str/split input #"\s+")
               (map #(Integer/parseInt %))
               (map dec) ; Convert to 0-indexed
               (filter #(and (>= % 0) (< % (count segments))))
               (map #(nth segments %))
               vec)
          (catch NumberFormatException _
            (println "Invalid input")
            []))))))

;; =============================================================================
;; Selection Actions
;; =============================================================================

(defn select-and-preview!
  "Select a segment and preview it.
   Returns the preview path or nil."
  [graph preview-fn]
  (when-let [segment (select! graph)]
    (preview-fn segment)))

(defn select-and-render!
  "Select a segment and render it.
   Returns the updated segment or nil."
  [graph render-fn]
  (when-let [segment (select! graph)]
    (render-fn segment)))

(defn select-and-mark-dirty!
  "Select segments and mark them as dirty.
   Returns vector of marked segment IDs."
  [graph mark-fn]
  (let [segments (select-multiple! graph)]
    (doseq [seg segments]
      (mark-fn (seg/segment-id seg)))
    (mapv seg/segment-id segments)))

;; =============================================================================
;; Quick Status
;; =============================================================================

(defn quick-status
  "Print a quick one-line status summary."
  [graph]
  (let [segments (vals (:segments graph))
        by-state (group-by seg/render-state segments)]
    (println
     (format "Segments: %d total | %s pending | %s dirty | %s cached | %s error"
             (count segments)
             (colorize (str (count (:pending by-state))) :pending)
             (colorize (str (count (:dirty by-state))) :dirty)
             (colorize (str (count (:cached by-state))) :cached)
             (colorize (str (count (:error by-state))) :error)))))

(defn status-bar
  "Generate a visual status bar string."
  [graph & {:keys [width] :or {width 40}}]
  (let [segments (vals (:segments graph))
        total (count segments)
        by-state (group-by seg/render-state segments)

        ;; Calculate proportions
        cached-pct (if (zero? total) 0 (/ (count (:cached by-state)) total))
        dirty-pct (if (zero? total) 0 (/ (count (:dirty by-state)) total))
        error-pct (if (zero? total) 0 (/ (count (:error by-state)) total))
        pending-pct (- 1 cached-pct dirty-pct error-pct)

        ;; Convert to characters
        cached-chars (int (* cached-pct width))
        dirty-chars (int (* dirty-pct width))
        error-chars (int (* error-pct width))
        pending-chars (- width cached-chars dirty-chars error-chars)]

    (str "["
         (colorize (apply str (repeat cached-chars "█")) :cached)
         (colorize (apply str (repeat dirty-chars "▓")) :dirty)
         (colorize (apply str (repeat error-chars "░")) :error)
         (apply str (repeat pending-chars "·"))
         "]")))
