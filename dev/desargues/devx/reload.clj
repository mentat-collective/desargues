(ns desargues.devx.reload
  "Hot reload coordination - namespace reloading and incremental re-rendering.
   
   ## Reload Cycle
   
   1. File watcher detects .clj file change
   2. clj-reload reloads the affected namespace (and dependents)
   3. Find segments with matching :source-ns metadata
   4. Recompute segment hashes
   5. Mark segments dirty if hash changed
   6. Optionally re-render dirty segments
   7. Optionally recombine partial files"
  (:require [desargues.devx.segment :as seg]
            [desargues.devx.graph :as graph]
            [desargues.devx.ns-tracker :as tracker]
            [desargues.devx.renderer :as renderer]
            [clj-reload.core :as reload]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def ^:dynamic *auto-render*
  "When true, automatically render dirty segments after reload."
  false)

(def ^:dynamic *auto-combine*
  "When true, automatically combine partial files after rendering."
  false)

(def ^:dynamic *notify-fn*
  "Function called to notify user of events. 
   Signature: (fn [event-type message & data])"
  (fn [event-type message & _data]
    (println (format "[%s] %s" (name event-type) message))))

;; =============================================================================
;; clj-reload Configuration
;; =============================================================================

(defonce ^:private reload-initialized? (atom false))

(defn init-reload!
  "Initialize clj-reload with source paths.
   
   Protected namespaces (not unloaded on reload):
   - desargues.manim.core (Python state)
   - desargues.manim-quickstart (Python init)
   - user (REPL state)"
  [& {:keys [source-paths]
      :or {source-paths ["src" "dev"]}}]
  (when-not @reload-initialized?
    (reload/init {:dirs source-paths
                  :no-unload '#{desargues.manim.core
                                desargues.manim-quickstart
                                user}})
    (reset! reload-initialized? true)
    (*notify-fn* :info "clj-reload initialized" {:paths source-paths})))

;; =============================================================================
;; Namespace Reloading
;; =============================================================================

(defn reload-namespace!
  "Reload changed namespaces using clj-reload.
   Returns result map with :success?, :reloaded, :unloaded, :error keys."
  []
  (try
    (let [result (reload/reload)]
      {:success? true
       :reloaded (:loaded result)
       :unloaded (:unloaded result)
       :error nil})
    (catch Exception e
      {:success? false
       :reloaded []
       :unloaded []
       :error e})))

(defn safe-reload-namespace!
  "Reload namespace with error handling and notification."
  [ns-sym]
  (*notify-fn* :reload (format "Reloading %s..." ns-sym) {:namespace ns-sym})
  (let [result (reload-namespace!)]
    (if (:success? result)
      (do
        (when (seq (:reloaded result))
          (*notify-fn* :success
                       (format "Reloaded: %s" (pr-str (:reloaded result)))
                       result))
        result)
      (do
        (*notify-fn* :error
                     (format "Reload failed: %s" (.getMessage (:error result)))
                     result)
        result))))

;; =============================================================================
;; Hash Recomputation
;; =============================================================================

(defn recompute-segment-hash
  "Recompute hash for a segment and check if it changed."
  [scene-graph seg-id]
  (let [segment (graph/get-segment scene-graph seg-id)
        old-hash (:hash segment)
        dep-hashes (into {}
                         (map (fn [dep-id]
                                [dep-id (:hash (graph/get-segment scene-graph dep-id))])
                              (:deps segment)))
        new-segment (seg/with-hash segment dep-hashes)
        new-hash (:hash new-segment)]
    {:segment-id seg-id
     :old-hash old-hash
     :new-hash new-hash
     :changed? (not= old-hash new-hash)}))

(defn recompute-affected-hashes
  "Recompute hashes for all affected segments in topological order.
   Returns vector of hash check results and the updated graph."
  [scene-graph affected-seg-ids]
  (let [render-order (graph/render-order scene-graph)
        ordered-affected (filter (set affected-seg-ids) render-order)]
    (loop [remaining ordered-affected
           current-graph scene-graph
           results []]
      (if (empty? remaining)
        {:results results :graph current-graph}
        (let [seg-id (first remaining)
              result (recompute-segment-hash current-graph seg-id)
              updated-graph (if (:changed? result)
                              (graph/update-segment
                               current-graph seg-id
                               #(assoc % :hash (:new-hash result)))
                              current-graph)]
          (recur (rest remaining)
                 updated-graph
                 (conj results result)))))))

;; =============================================================================
;; Dirty Marking
;; =============================================================================

(defn mark-changed-segments-dirty
  "Mark segments as dirty if their hash changed.
   Returns updated graph (pure function)."
  [scene-graph hash-results]
  (let [changed-ids (->> hash-results
                         (filter :changed?)
                         (map :segment-id))]
    (reduce
     (fn [g seg-id]
       (graph/mark-dirty g seg-id))
     scene-graph
     changed-ids)))

;; =============================================================================
;; Main Reload Handler
;; =============================================================================

(defn handle-file-change!
  "Main handler for file change events.
   
   Called by watcher when a .clj file changes. Coordinates full reload cycle.
   
   Arguments:
   - scene-graph-atom: atom containing the current SceneGraph
   - event: map with :namespace, :file, :event-type keys
   
   Options:
   - :auto-render? - whether to auto-render dirty segments
   - :auto-combine? - whether to auto-combine after render"
  [scene-graph-atom event & {:keys [auto-render? auto-combine?]
                             :or {auto-render? *auto-render*
                                  auto-combine? *auto-combine*}}]
  (let [ns-sym (:namespace event)
        file (:file event)]
    (*notify-fn* :change (format "File changed: %s" file) event)

    ;; Step 1: Reload namespace
    (let [reload-result (safe-reload-namespace! ns-sym)]
      (if-not (:success? reload-result)
        {:success? false
         :phase :reload
         :error (:error reload-result)
         :namespace ns-sym}

        ;; Step 2: Find affected segments
        (let [scene-graph @scene-graph-atom
              affected-ids (tracker/affected-segments scene-graph ns-sym)]
          (if (empty? affected-ids)
            (do
              (*notify-fn* :info "No segments affected by this change"
                           {:namespace ns-sym})
              {:success? true
               :reloaded (:reloaded reload-result)
               :affected []
               :dirty []})

            ;; Steps 3 & 4: Recompute hashes and mark dirty
            (let [_ (*notify-fn* :info
                                 (format "Checking %d affected segment(s)..."
                                         (count affected-ids))
                                 {:affected affected-ids})
                  {:keys [results graph]} (recompute-affected-hashes scene-graph affected-ids)
                  changed-count (count (filter :changed? results))
                  new-graph (mark-changed-segments-dirty graph results)
                  _ (reset! scene-graph-atom new-graph)
                  dirty-ids (graph/dirty-segment-ids new-graph)]

              (*notify-fn* :info
                           (format "%d segment(s) marked dirty" changed-count)
                           {:dirty dirty-ids})

              ;; Step 5: Optional auto-render
              (when (and auto-render? (seq dirty-ids))
                (*notify-fn* :render "Auto-rendering dirty segments...")
                (try
                  (let [rendered-graph (renderer/render-dirty! new-graph)]
                    (reset! scene-graph-atom rendered-graph)

                    ;; Step 6: Optional auto-combine
                    (when auto-combine?
                      (renderer/combine-partials! rendered-graph "media/videos/output.mp4")))
                  (catch Exception e
                    (*notify-fn* :error
                                 (format "Render failed: %s" (.getMessage e))
                                 {:error e}))))

              {:success? true
               :reloaded (:reloaded reload-result)
               :affected (vec affected-ids)
               :dirty (vec dirty-ids)
               :hash-changes (filterv :changed? results)})))))))

;; =============================================================================
;; Manual Reload Commands
;; =============================================================================

(defn reload!
  "Manually trigger a reload cycle for the current scene.
   
   Options:
   - :render? - whether to render dirty segments (default: true)
   - :combine? - whether to combine after render (default: false)"
  [scene-graph-atom & {:keys [render? combine?]
                       :or {render? true combine? false}}]
  (let [scene-graph @scene-graph-atom]
    (*notify-fn* :reload "Manual reload triggered...")

    (let [ns-index (tracker/build-ns-index scene-graph)
          namespaces (keys (:by-ns ns-index))]

      ;; Reload all tracked namespaces
      (doseq [ns-sym namespaces]
        (safe-reload-namespace! ns-sym))

      (let [all-seg-ids (graph/segment-ids scene-graph)
            {:keys [results graph]} (recompute-affected-hashes scene-graph all-seg-ids)
            new-graph (mark-changed-segments-dirty graph results)
            _ (reset! scene-graph-atom new-graph)
            dirty-ids (graph/dirty-segment-ids new-graph)]

        (*notify-fn* :info
                     (format "Reload complete. %d dirty segment(s)" (count dirty-ids))
                     {:dirty dirty-ids})

        (when (and render? (seq dirty-ids))
          (let [rendered-graph (renderer/render-dirty! new-graph)]
            (reset! scene-graph-atom rendered-graph)

            (when combine?
              (renderer/combine-partials! rendered-graph "media/videos/output.mp4"))))

        {:dirty dirty-ids
         :rendered? render?}))))

;; =============================================================================
;; Configuration Helpers
;; =============================================================================

(defn set-auto-render!
  "Enable or disable auto-rendering after file changes."
  [enabled?]
  (alter-var-root #'*auto-render* (constantly enabled?))
  (*notify-fn* :config
               (format "Auto-render %s" (if enabled? "enabled" "disabled"))
               {:auto-render enabled?}))

(defn set-auto-combine!
  "Enable or disable auto-combining after rendering."
  [enabled?]
  (alter-var-root #'*auto-combine* (constantly enabled?))
  (*notify-fn* :config
               (format "Auto-combine %s" (if enabled? "enabled" "disabled"))
               {:auto-combine enabled?}))

(defn set-notify-fn!
  "Set a custom notification function."
  [f]
  (alter-var-root #'*notify-fn* (constantly f)))
