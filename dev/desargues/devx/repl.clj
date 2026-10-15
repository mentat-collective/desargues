(ns desargues.devx.repl
  "REPL workflow helpers for devx system.
   
   This namespace provides stateful REPL convenience functions that
   operate on a 'current scene graph'. This separation follows SRP:
   - renderer.clj: Pure rendering operations
   - repl.clj: Stateful REPL workflow (including hot-reload)
   
   ## Quick Start
   
   ```clojure
   (require '[desargues.devx.repl :as repl])
   
   ;; Load a scene graph
   (repl/set-graph! my-scene)
   
   ;; Check status
   (repl/status)
   
   ;; Render dirty segments
   (repl/render!)
   
   ;; Preview a specific segment
   (repl/preview! :intro)
   
   ;; Mark segment dirty
   (repl/dirty! :step-1)
   
   ;; Export final video
   (repl/combine! \"output.mp4\")
   ```
   
   ## Hot Reload Workflow
   
   ```clojure
   ;; Start watching for file changes
   (repl/watch! :auto-render? true)
   
   ;; Check watcher status
   (repl/watch-status)
   
   ;; Manual reload
   (repl/reload!)
   
   ;; Stop watching
   (repl/unwatch!)
   ```
   
   ## Preview Workflow (Phase 3)
   
   ```clojure
   ;; Enhanced last-frame preview
   (repl/preview! :intro)
   
   ;; OpenGL live preview
   (repl/live! :intro)
   
   ;; Interactive segment selector
   (repl/select!)
   
   ;; Show segment dependency graph
   (repl/show-graph)
   ```
   
   ## Parallel Rendering (Phase 5)
   
   ```clojure
   ;; Analyze parallelization potential
   (repl/waves!)
   
   ;; Render with wave-based parallel execution
   (repl/rp!)  ; or (repl/render-parallel!)
   
   ;; Estimate speedup
   (repl/estimate-speedup 10.0)  ; avg 10s per segment
   ```"
  (:require [desargues.devx.segment :as seg]
            [desargues.devx.graph :as graph]
            [desargues.devx.renderer :as renderer]
            [desargues.devx.watcher :as watcher]
            [desargues.devx.reload :as reload]
            [desargues.devx.ns-tracker :as tracker]
            [desargues.devx.preview :as preview]
            [desargues.devx.opengl :as opengl]
            [desargues.devx.selector :as selector]
            [desargues.devx.quality :as quality]
            [desargues.devx.parallel :as parallel]))

;; =============================================================================
;; State Management
;; =============================================================================

(defonce ^:private graph-atom
  (atom nil))

(defn set-graph!
  "Set the current working graph for REPL convenience functions.
   Returns the graph for chaining."
  [graph]
  (reset! graph-atom graph)
  graph)

(defn get-graph
  "Get the current working graph. Returns nil if no graph loaded."
  []
  @graph-atom)

(defn clear-graph!
  "Clear the current working graph."
  []
  (reset! graph-atom nil))

;; =============================================================================
;; Status & Inspection
;; =============================================================================

(defn status
  "Print status of current graph."
  []
  (if-let [g @graph-atom]
    (graph/print-status g)
    (println "No graph loaded. Use (set-graph! g) first.")))

(defn stats
  "Get statistics about current graph."
  []
  (when-let [g @graph-atom]
    (graph/stats g)))

(defn segments
  "List all segment IDs in current graph."
  []
  (when-let [g @graph-atom]
    (graph/segment-ids g)))

(defn dirty-segments
  "List dirty segment IDs in current graph."
  []
  (when-let [g @graph-atom]
    (graph/dirty-segment-ids g)))

;; =============================================================================
;; Rendering Operations
;; =============================================================================

(defn render!
  "Render dirty segments in current graph.
   Updates the stored graph with results.
   
   Options:
   - :quality - :low, :medium, :high (default :low)
   - :parallel? - use parallel rendering (default false)"
  [& {:keys [quality parallel?] :as opts}]
  (if-let [g @graph-atom]
    (let [result (renderer/render-dirty! g
                                         :quality (or quality :low)
                                         :parallel? (or parallel? false))]
      (reset! graph-atom result)
      (status)
      result)
    (do (println "No graph loaded. Use (set-graph! g) first.")
        nil)))

(defn render-all!
  "Force re-render of all segments in current graph."
  [& {:keys [quality] :or {quality :low}}]
  (if-let [g @graph-atom]
    (let [result (renderer/render-all! g :quality quality)]
      (reset! graph-atom result)
      (status)
      result)
    (do (println "No graph loaded. Use (set-graph! g) first.")
        nil)))

(defn render-segment!
  "Render a specific segment by ID."
  [seg-id & {:keys [quality] :or {quality :low}}]
  (if-let [g @graph-atom]
    (let [result (renderer/render-segment-by-id! g seg-id :quality quality)]
      (reset! graph-atom result)
      (println "Rendered:" seg-id)
      result)
    (do (println "No graph loaded. Use (set-graph! g) first.")
        nil)))

;; =============================================================================
;; Preview Operations
;; =============================================================================

(defn preview!
  "Preview a segment by ID (renders last frame, opens image viewer).
   
   Options:
   - :quality - :low (default), :medium, :high
   - :open? - open image viewer (default true)"
  [seg-id & {:keys [quality open?] :or {quality :low open? true}}]
  (if-let [g @graph-atom]
    (if-let [seg (graph/get-segment g seg-id)]
      (preview/preview! seg :quality quality :open? open?)
      (do (println "Segment not found:" seg-id)
          nil))
    (do (println "No graph loaded. Use (set-graph! g) first.")
        nil)))

(defn live!
  "Start OpenGL live preview for a segment.
   
   Opens a GPU-accelerated preview window that shows the scene
   in real-time. Close the window to return to REPL.
   
   Options:
   - :quality - :low (default), :medium, :high
   - :interactive? - enable IPython interactive mode (default false)"
  [seg-id & {:keys [quality interactive?] :or {quality :low interactive? false}}]
  (if-let [g @graph-atom]
    (if-let [seg (graph/get-segment g seg-id)]
      (if interactive?
        (opengl/live-interactive! seg :quality quality)
        (opengl/live! seg :quality quality))
      (do (println "Segment not found:" seg-id)
          nil))
    (do (println "No graph loaded. Use (set-graph! g) first.")
        nil)))

(defn select!
  "Interactive segment selector menu.
   
   Displays segments with status and allows selection by number.
   Returns the selected segment or nil.
   
   Commands in menu:
   - Number: select segment
   - q: quit
   - g: show dependency graph
   - d: filter dirty segments only"
  []
  (if-let [g @graph-atom]
    (selector/select! g)
    (do (println "No graph loaded. Use (set-graph! g) first.")
        nil)))

(defn select-and-preview!
  "Interactive segment selector, then preview selected segment."
  []
  (when-let [seg (select!)]
    (preview/preview! seg)))

(defn select-and-render!
  "Interactive segment selector, then render selected segment."
  []
  (when-let [seg (select!)]
    (let [result (renderer/render-segment! seg)]
      (swap! graph-atom graph/update-segment (seg/segment-id seg) (constantly result))
      result)))

(defn select-and-live!
  "Interactive segment selector, then start live preview."
  []
  (when-let [seg (select!)]
    (opengl/live! seg)))

(defn show-graph
  "Display segment dependency graph as ASCII art."
  []
  (if-let [g @graph-atom]
    (selector/show-graph g)
    (println "No graph loaded. Use (set-graph! g) first.")))

(defn quick-status
  "Print one-line status summary."
  []
  (if-let [g @graph-atom]
    (selector/quick-status g)
    (println "No graph loaded.")))

;; =============================================================================
;; Change Management
;; =============================================================================

(defn dirty!
  "Mark a segment (and its dependents) as dirty.
   Prints status after marking."
  [seg-id]
  (if-let [g @graph-atom]
    (do
      (swap! graph-atom graph/mark-dirty seg-id)
      (status)
      @graph-atom)
    (do (println "No graph loaded.")
        nil)))

(defn dirty-all!
  "Mark all segments as dirty."
  []
  (if-let [g @graph-atom]
    (do
      (swap! graph-atom graph/mark-all-dirty)
      (status)
      @graph-atom)
    (do (println "No graph loaded.")
        nil)))

;; =============================================================================
;; Export Operations
;; =============================================================================

(defn combine!
  "Combine cached segments into final video."
  [output-path]
  (if-let [g @graph-atom]
    (renderer/combine-partials! g output-path)
    (do (println "No graph loaded.")
        nil)))

(defn export!
  "Export current scene: render dirty segments then combine.
   
   Options:
   - :quality - :low, :medium, :high (default :high for export)"
  [output-path & {:keys [quality] :or {quality :high}}]
  (when-let [_ (render! :quality quality)]
    (combine! output-path)))

;; =============================================================================
;; Parallel Rendering (Phase 5)
;; =============================================================================

(defn render-parallel!
  "Render dirty segments using wave-based parallel execution.
   
   Segments are partitioned into waves based on dependencies.
   Within each wave, segments are rendered in parallel.
   
   Options:
   - :max-workers - Maximum parallel workers (default: 4)
   - :quality - Quality preset (default: :low)
   - :on-progress - Callback fn for progress events"
  [& {:keys [max-workers quality on-progress]
      :or {max-workers 4 quality :low}}]
  (if-let [g @graph-atom]
    (let [result (parallel/render-parallel! g
                                            :max-workers max-workers
                                            :quality quality
                                            :on-progress on-progress)]
      (reset! graph-atom result)
      (status)
      result)
    (do (println "No graph loaded. Use (set-graph! g) first.")
        nil)))

(defn waves!
  "Print wave analysis for current graph.
   Shows how segments would be partitioned for parallel execution."
  []
  (if-let [g @graph-atom]
    (parallel/print-wave-analysis g)
    (do (println "No graph loaded. Use (set-graph! g) first.")
        nil)))

(defn estimate-speedup
  "Estimate parallel speedup for current graph.
   
   Arguments:
   - avg-segment-time-s: Average time to render one segment in seconds
   - max-workers: Number of parallel workers (default: 4)"
  [avg-segment-time-s & {:keys [max-workers] :or {max-workers 4}}]
  (if-let [g @graph-atom]
    (let [estimate (parallel/estimate-parallel-time g avg-segment-time-s max-workers)]
      (println "\n=== Parallel Speedup Estimate ===")
      (println (format "Sequential time: %.1fs" (:sequential-estimate-s estimate)))
      (println (format "Parallel time:   %.1fs" (:parallel-estimate-s estimate)))
      (println (format "Speedup factor:  %.2fx" (:speedup-factor estimate)))
      (println (format "Waves: %d, Max parallelism: %d"
                       (:total-waves (:stats estimate))
                       (:max-parallelism (:stats estimate))))
      estimate)
    (do (println "No graph loaded. Use (set-graph! g) first.")
        nil)))

;; Shortcut
(def rp!
  "Shortcut for (render-parallel!)"
  render-parallel!)

;; =============================================================================
;; Short Aliases (for quick REPL use)
;; =============================================================================

(def r!
  "Alias for render!"
  render!)

(def p!
  "Alias for preview!"
  preview!)

(def d!
  "Alias for dirty!"
  dirty!)

(def s!
  "Alias for status"
  status)

(def c!
  "Alias for combine!"
  combine!)

(def L!
  "Alias for live! (OpenGL preview)"
  live!)

(def sel!
  "Alias for select!"
  select!)

;; =============================================================================
;; Help
;; =============================================================================

(defn help
  "Print help for REPL workflow."
  []
  (println "
DevX REPL Workflow Commands
===========================

State Management:
  (set-graph! g)      - Set current working graph
  (get-graph)         - Get current graph
  (clear-graph!)      - Clear current graph

Status:
  (status) or (s!)    - Print graph status
  (quick-status)      - One-line status summary
  (stats)             - Get statistics map
  (segments)          - List all segment IDs
  (dirty-segments)    - List dirty segment IDs
  (show-graph)        - Display dependency graph (ASCII)

Rendering:
  (render!) or (r!)   - Render dirty segments
  (render-all!)       - Force re-render all
  (render-segment! :id) - Render specific segment

Preview (Phase 3):
  (preview! :id) or (p! :id) - Last-frame preview (opens image)
  (live! :id) or (L! :id)    - OpenGL live preview window
  (live! :id :interactive? true) - Interactive with IPython
  (select!)           - Interactive segment menu
  (select-and-preview!) - Select then preview
  (select-and-live!)  - Select then live preview

Changes:
  (dirty! :id) or (d! :id) - Mark segment dirty
  (dirty-all!)        - Mark all segments dirty

Export:
  (combine! \"out.mp4\") or (c! \"out.mp4\") - Combine to video
  (export! \"out.mp4\")  - Render + combine

Hot Reload:
  (watch!)            - Start file watcher
  (watch! :auto-render? true) - Watch with auto-render
  (unwatch!) or (uw!) - Stop file watcher
  (watching?)         - Check if watcher is running
  (reload!)           - Manual reload namespaces
  (watch-status)      - Show watcher status
  (tracking-report)   - Show namespace tracking
  (w!)                - Quick: watch + auto-render
  (W!)                - Quick: watch + auto-render + auto-combine

Quality:
  Most functions accept :quality (:low :medium :high :auto)
  Example: (render! :quality :high)
  (quality/print-presets)    - Show quality presets
  (quality/print-quality-info) - Current quality context
"))

;; =============================================================================
;; Hot Reload Workflow
;; =============================================================================

(defn watch!
  "Start file watching for hot reload.
   
   Options:
   - :paths - source paths to watch (default: [\"src\"])
   - :auto-render? - auto-render dirty segments (default: false)
   - :auto-combine? - auto-combine after render (default: false)
   
   Usage:
   (watch!)                        ; Manual render mode
   (watch! :auto-render? true)     ; Auto-render on file change
   (watch! :paths [\"src\" \"dev\"]) ; Watch multiple paths"
  [& {:keys [paths auto-render? auto-combine?]
      :or {paths ["src"]
           auto-render? false
           auto-combine? false}}]
  (reload/init-reload! :source-paths paths)
  (reload/set-auto-render! auto-render?)
  (reload/set-auto-combine! auto-combine?)
  (watcher/start-watcher!
   {:paths paths
    :on-change (fn [event]
                 (reload/handle-file-change!
                  graph-atom event
                  :auto-render? auto-render?
                  :auto-combine? auto-combine?))}))

(defn unwatch!
  "Stop file watching."
  []
  (watcher/stop-watcher!))

(defn watching?
  "Check if file watcher is currently running."
  []
  (watcher/watching?))

(defn reload!
  "Manually reload changed namespaces and re-render dirty segments.
   
   Options:
   - :render? - whether to render dirty segments (default: true)
   - :combine? - whether to combine after render (default: false)"
  [& {:keys [render? combine?]
      :or {render? true combine? false}}]
  (when @graph-atom
    (reload/reload! graph-atom :render? render? :combine? combine?)))

(defn watch-status
  "Show current watcher status and statistics."
  []
  (let [stats (watcher/watcher-stats)]
    (println "\n=== File Watcher Status ===")
    (println (format "Running: %s" (:running? stats)))
    (when (:running? stats)
      (println (format "Watching: %s" (pr-str (:paths stats))))
      (println (format "Changes detected: %d" (:changes-detected stats)))
      (when-let [last (:last-change stats)]
        (println (format "Last change: %s at %s"
                         (:namespace last)
                         (:time last)))))
    stats))

(defn tracking-report
  "Show namespace tracking report for current scene."
  []
  (when-let [sg @graph-atom]
    (tracker/print-tracking-report sg)))

;; Hot reload shortcuts
(def w!
  "Quick watch - start watching with auto-render enabled."
  (fn [] (watch! :auto-render? true)))

(def W!
  "Quick watch all - start watching with auto-render and auto-combine."
  (fn [] (watch! :auto-render? true :auto-combine? true)))

(def uw!
  "Quick unwatch - stop file watching."
  unwatch!)
