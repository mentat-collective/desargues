(ns desargues.devx.core
  "Developer Experience - Pure API for incremental rendering.
   
   This is the primary entry point for the devx system. All functions
   are pure (take scene-graph, return scene-graph) except for I/O operations.
   
   For stateful REPL workflow (current scene, hot-reload), use:
     (require '[desargues.devx.repl :as repl])
   
   ## Quick Start
   
   ```clojure
   (require '[desargues.devx.core :as devx])
   
   ;; Initialize
   (devx/init!)
   
   ;; Define segments
   (def intro 
     (devx/segment :intro
       (fn [scene]
         (let [title (create-text \"Hello\")]
           (play! scene (write title))))))
   
   (def main-content
     (devx/segment :main
       :deps #{:intro}
       (fn [scene]
         ...)))
   
   ;; Build scene graph
   (def my-scene (devx/scene [intro main-content]))
   
   ;; Render and export (pure API - pass graph, get graph back)
   (-> my-scene
       (devx/render! :quality :high)
       (devx/export! \"output.mp4\"))
   ```
   
   ## REPL Workflow
   
   For interactive development with hot-reload, use the repl namespace:
   ```clojure
   (require '[desargues.devx.repl :as repl])
   (repl/set-graph! my-scene)
   (repl/render!)
   (repl/watch! :auto-render? true)
   ```"
  (:require [desargues.devx.segment :as seg]
            [desargues.devx.graph :as graph]
            [desargues.devx.renderer :as renderer]
            [desargues.manim.core :as manim]))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize the devx system (and Manim)."
  []
  (manim/init!)
  (seg/ensure-partial-dir!)
  (println "DevX system initialized."))

;; =============================================================================
;; Segment Creation DSL
;; =============================================================================

(defn segment
  "Create a segment with the given ID and construct function.
   
   Usage:
   (segment :intro
     (fn [scene]
       (play! scene (create title))))
   
   With dependencies:
   (segment :step-2
     :deps #{:step-1}
     (fn [scene] ...))"
  [id & args]
  (let [[opts construct-fn] (if (keyword? (first args))
                              [(apply hash-map (butlast args)) (last args)]
                              [{} (first args)])
        deps (get opts :deps #{})
        metadata (dissoc opts :deps)]
    (seg/create-segment
     :id id
     :construct-fn construct-fn
     :deps deps
     :metadata metadata)))

(defmacro defsegment
  "Define a segment as a var.
   
   Usage:
   (defsegment intro
     \"Introduction sequence\"
     [scene]
     (play! scene (create title)))
   
   With dependencies:
   (defsegment main-content
     \"Main animation\"
     :deps #{intro}
     [scene]
     (play! scene ...))"
  [name & body]
  (let [[docstring body] (if (string? (first body))
                           [(first body) (rest body)]
                           [nil body])
        [opts body] (loop [opts {} remaining body]
                      (if (keyword? (first remaining))
                        (recur (assoc opts (first remaining) (second remaining))
                               (drop 2 remaining))
                        [opts remaining]))
        [args & body] body
        deps (get opts :deps #{})]
    `(def ~name
       (seg/create-segment
        :id ~(keyword name)
        :construct-fn (fn ~args ~@body)
        :deps ~deps
        :metadata {:description ~docstring
                   :source-ns '~(ns-name *ns*)}))))

;; =============================================================================
;; Scene Graph Construction
;; =============================================================================

(defn scene
  "Create a scene graph from a collection of segments.
   
   Usage:
   (scene [intro step-1 step-2 conclusion])
   
   With metadata:
   (scene [intro step-1]
     :title \"My Animation\"
     :output \"my-video.mp4\")"
  [segments & {:as metadata}]
  (-> (graph/create-graph :metadata (or metadata {}))
      (graph/add-segments segments)))

(defn add-segment
  "Add a segment to an existing scene.
   Returns updated scene graph."
  [scene-graph segment]
  (graph/add-segment scene-graph segment))

;; =============================================================================
;; Render Functions (Pure API)
;; =============================================================================

(defn render!
  "Render dirty segments in the scene.
   
   Options:
   - :quality - :low, :medium, :high (default :low)
   - :parallel? - use parallel rendering (default false)
   
   Returns updated scene graph."
  [scene-graph & {:keys [quality parallel?]
                  :or {quality :low parallel? false}}]
  (if parallel?
    (renderer/render-parallel! scene-graph :quality quality)
    (renderer/render-dirty! scene-graph :quality quality)))

(defn render-all!
  "Force re-render of all segments.
   Returns updated scene graph."
  [scene-graph & opts]
  (apply renderer/render-all! scene-graph opts))

(defn render-segment!
  "Render a specific segment by ID.
   Returns updated scene graph."
  [scene-graph seg-id & opts]
  (apply renderer/render-segment-by-id! scene-graph seg-id opts))

;; =============================================================================
;; Preview Functions
;; =============================================================================

(defn preview!
  "Quick preview of a segment (renders last frame only).
   Returns updated scene graph.
   
   Usage:
   (preview! my-scene :step-3)"
  [scene-graph seg-id]
  (if-let [seg (graph/get-segment scene-graph seg-id)]
    (let [result (renderer/preview-segment! seg)]
      (graph/update-segment scene-graph seg-id (constantly result)))
    (throw (ex-info "Segment not found" {:segment seg-id}))))

;; =============================================================================
;; Export Functions  
;; =============================================================================

(defn export!
  "Export scene to final video file.
   Returns the output path on success.
   
   Usage:
   (export! my-scene \"output.mp4\")
   (export! my-scene \"output.mp4\" :quality :high)"
  [scene-graph output-path & {:keys [quality render-missing?]
                              :or {quality :high render-missing? true}}]
  (let [scene-graph (if render-missing?
                      (render! scene-graph :quality quality)
                      scene-graph)]
    (renderer/combine-partials! scene-graph output-path)))

;; =============================================================================
;; Status & Inspection
;; =============================================================================

(defn status
  "Print status of the scene graph."
  [scene-graph]
  (graph/print-status scene-graph))

(defn stats
  "Get statistics about the scene graph."
  [scene-graph]
  (graph/stats scene-graph))

(defn dirty?
  "Check if any segments need rendering."
  [scene-graph]
  (boolean (seq (graph/dirty-segments scene-graph))))

(defn dirty-segments
  "Get list of segments that need rendering."
  [scene-graph]
  (graph/dirty-segment-ids scene-graph))

;; =============================================================================
;; Change Management (Pure Functions)
;; =============================================================================

(defn mark-dirty
  "Mark a segment (and its dependents) as dirty.
   Returns updated graph."
  [scene-graph seg-id]
  (graph/mark-dirty scene-graph seg-id))

(defn mark-all-dirty
  "Mark all segments as dirty.
   Returns updated graph."
  [scene-graph]
  (graph/mark-all-dirty scene-graph))

(defn invalidate
  "Alias for mark-dirty - invalidate a segment's cache.
   Returns updated graph."
  [scene-graph seg-id]
  (mark-dirty scene-graph seg-id))

;; =============================================================================
;; Graph Visualization
;; =============================================================================

(defn ->dot
  "Generate DOT representation for graphviz."
  [scene-graph]
  (graph/->dot scene-graph))

(defn save-dot!
  "Save DOT representation to file."
  [scene-graph path]
  (spit path (->dot scene-graph))
  (println "Saved to:" path))

;; =============================================================================
;; Example Usage
;; =============================================================================

(comment
  ;; Initialize
  (init!)

  ;; Define segments
  (def intro
    (segment :intro
             (fn [scene]
               (let [m (manim/manim)
                     Text (manim/get-class "Text")
                     title (Text "Hello World")]
                 (manim/play! scene (manim/get-class "Write") title)
                 (manim/wait! scene 1)))))

  (def step-1
    (segment :step-1
             :deps #{:intro}
             (fn [scene]
               (let [Circle (manim/get-class "Circle")
                     circle (Circle)]
                 (manim/play! scene (manim/get-class "Create") circle)))))

  ;; Create scene graph
  (def my-scene (scene [intro step-1]))

  ;; Pure API workflow
  (-> my-scene
      (render! :quality :low)
      (export! "output.mp4"))

  ;; Or use REPL workflow for interactive development:
  ;; (require '[desargues.devx.repl :as repl])
  ;; (repl/set-graph! my-scene)
  ;; (repl/render!)
  ;; (repl/watch! :auto-render? true)
  )
