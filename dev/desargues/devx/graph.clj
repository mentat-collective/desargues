(ns desargues.devx.graph
  "Scene Graph - DAG of segments with dependency tracking.
   
   The SceneGraph maintains:
   - Collection of segments indexed by ID
   - Dependency edges between segments
   - Topologically sorted render order
   - Change propagation (dirty marking)
   
   ## Key Operations
   
   - add-segment: Add segment to graph
   - mark-dirty: Mark segment and dependents as dirty (pure)
   - render-order: Get topologically sorted segment IDs
   - dirty-segments: Get segments that need rendering
   - independent-segments: Get segments with no unrendered dependencies"
  (:require [desargues.devx.segment :as seg]))

;; =============================================================================
;; Scene Graph Record
;; =============================================================================

(defrecord SceneGraph
           [segments ; map of keyword -> Segment
            edges ; map of keyword -> set of keywords (reverse deps: who depends on me)
            render-order ; vector of keywords in topological order
            metadata]) ; graph-level metadata

;; =============================================================================
;; Graph Construction
;; =============================================================================

(defn create-graph
  "Create an empty scene graph."
  [& {:keys [metadata] :or {metadata {}}}]
  (->SceneGraph {} {} [] metadata))

(defn- compute-reverse-edges
  "Compute reverse dependency edges (who depends on each segment)."
  [segments]
  (reduce
   (fn [edges [seg-id segment]]
     (reduce
      (fn [e dep-id]
        (update e dep-id (fnil conj #{}) seg-id))
      edges
      (seg/dependencies segment)))
   {}
   segments))

(defn- topological-sort
  "Return segment IDs in topological order (dependencies first).
   Throws if cycle detected."
  [segments]
  (let [in-degree (atom (into {} (map (fn [[id seg]]
                                        [id (count (seg/dependencies seg))])
                                      segments)))
        reverse-edges (compute-reverse-edges segments)
        queue (atom (vec (filter #(zero? (get @in-degree %))
                                 (keys segments))))
        result (atom [])]
    (while (seq @queue)
      (let [node (first @queue)]
        (swap! queue rest)
        (swap! result conj node)
        (doseq [dependent (get reverse-edges node #{})]
          (swap! in-degree update dependent dec)
          (when (zero? (get @in-degree dependent))
            (swap! queue conj dependent)))))
    (if (= (count @result) (count segments))
      @result
      (throw (ex-info "Cycle detected in segment dependencies"
                      {:segments (keys segments)
                       :processed @result})))))

(defn add-segment
  "Add a segment to the graph. Updates edges and render order."
  [graph segment]
  (let [seg-id (seg/segment-id segment)
        new-segments (assoc (:segments graph) seg-id segment)
        ;; Validate dependencies exist
        missing-deps (remove #(contains? new-segments %)
                             (seg/dependencies segment))]
    (when (seq missing-deps)
      (throw (ex-info "Segment has missing dependencies"
                      {:segment seg-id
                       :missing missing-deps})))
    (-> graph
        (assoc :segments new-segments)
        (assoc :edges (compute-reverse-edges new-segments))
        (assoc :render-order (topological-sort new-segments)))))

(defn add-segments
  "Add multiple segments to the graph."
  [graph segments]
  (reduce add-segment graph segments))

(defn remove-segment
  "Remove a segment from the graph."
  [graph seg-id]
  (let [;; Check no other segments depend on this one
        dependents (get (:edges graph) seg-id #{})]
    (when (seq dependents)
      (throw (ex-info "Cannot remove segment with dependents"
                      {:segment seg-id
                       :dependents dependents})))
    (let [new-segments (dissoc (:segments graph) seg-id)]
      (-> graph
          (assoc :segments new-segments)
          (assoc :edges (compute-reverse-edges new-segments))
          (assoc :render-order (topological-sort new-segments))))))

;; =============================================================================
;; Segment Access
;; =============================================================================

(defn get-segment
  "Get segment by ID."
  [graph seg-id]
  (get (:segments graph) seg-id))

(defn all-segments
  "Get all segments as a sequence."
  [graph]
  (vals (:segments graph)))

(defn segment-ids
  "Get all segment IDs."
  [graph]
  (keys (:segments graph)))

(defn segment-count
  "Get number of segments in graph."
  [graph]
  (count (:segments graph)))

;; =============================================================================
;; State Updates
;; =============================================================================

(defn update-segment
  "Update a segment in the graph."
  [graph seg-id f & args]
  (if-let [seg (get-segment graph seg-id)]
    (assoc-in graph [:segments seg-id] (apply f seg args))
    (throw (ex-info "Segment not found" {:segment seg-id}))))

(defn mark-dirty
  "Mark a segment and all its dependents as dirty.
   Returns updated graph (pure function - no side effects)."
  [graph seg-id]
  (let [dependents (get (:edges graph) seg-id #{})
        ;; Mark this segment dirty
        graph (update-segment graph seg-id seg/mark-dirty)]
    ;; Recursively mark dependents
    (reduce mark-dirty graph dependents)))

(defn mark-all-dirty
  "Mark all segments as dirty."
  [graph]
  (reduce
   (fn [g seg-id]
     (update-segment g seg-id seg/mark-dirty))
   graph
   (segment-ids graph)))

;; =============================================================================
;; Dependency Queries
;; =============================================================================

(defn dependents
  "Get direct dependents of a segment (segments that depend on it)."
  [graph seg-id]
  (get (:edges graph) seg-id #{}))

(defn dependencies
  "Get direct dependencies of a segment."
  [graph seg-id]
  (when-let [seg (get-segment graph seg-id)]
    (seg/dependencies seg)))

(defn transitive-dependents
  "Get all transitive dependents of a segment."
  [graph seg-id]
  (loop [to-visit #{seg-id}
         visited #{}
         result #{}]
    (if (empty? to-visit)
      (disj result seg-id) ; exclude the original segment
      (let [current (first to-visit)
            deps (dependents graph current)]
        (recur (into (disj to-visit current)
                     (remove visited deps))
               (conj visited current)
               (into result deps))))))

(defn transitive-dependencies
  "Get all transitive dependencies of a segment."
  [graph seg-id]
  (loop [to-visit #{seg-id}
         visited #{}
         result #{}]
    (if (empty? to-visit)
      (disj result seg-id)
      (let [current (first to-visit)
            deps (dependencies graph current)]
        (recur (into (disj to-visit current)
                     (remove visited deps))
               (conj visited current)
               (into result deps))))))

;; =============================================================================
;; Render Planning
;; =============================================================================

(defn render-order
  "Get segment IDs in render order (dependencies first)."
  [graph]
  (:render-order graph))

(defn dirty-segments
  "Get segments that need rendering, in render order."
  [graph]
  (->> (render-order graph)
       (map #(get-segment graph %))
       (filter seg/needs-render?)))

(defn dirty-segment-ids
  "Get IDs of segments that need rendering, in render order."
  [graph]
  (map seg/segment-id (dirty-segments graph)))

(defn cached-segments
  "Get segments that are cached."
  [graph]
  (filter seg/is-cached? (all-segments graph)))

(defn independent-dirty-segments
  "Get dirty segments whose dependencies are all cached.
   These can be rendered in parallel."
  [graph]
  (let [cached-ids (set (map seg/segment-id (cached-segments graph)))]
    (->> (dirty-segments graph)
         (filter (fn [seg]
                   (every? #(contains? cached-ids %)
                           (seg/dependencies seg)))))))

(defn next-render-batch
  "Get the next batch of segments that can be rendered in parallel.
   Returns segments whose dependencies are all satisfied."
  [graph]
  (let [cached-ids (set (map seg/segment-id (cached-segments graph)))
        rendering-ids (set (map seg/segment-id
                                (filter #(= :rendering (seg/render-state %))
                                        (all-segments graph))))]
    (->> (dirty-segments graph)
         (filter (fn [seg]
                   (let [deps (seg/dependencies seg)]
                     (every? #(or (contains? cached-ids %)
                                  (contains? rendering-ids %))
                             deps)))))))

;; =============================================================================
;; Hash Management
;; =============================================================================

(defn recompute-hashes
  "Recompute hashes for all segments based on current dependencies.
   Returns updated graph with segments marked dirty if hash changed."
  [graph]
  (reduce
   (fn [g seg-id]
     (let [seg (get-segment g seg-id)
           dep-hashes (into {}
                            (map (fn [dep-id]
                                   [dep-id (:hash (get-segment g dep-id))])
                                 (seg/dependencies seg)))
           new-seg (seg/with-hash seg dep-hashes)
           hash-changed? (not= (:hash seg) (:hash new-seg))]
       (if hash-changed?
         (update-segment g seg-id (constantly (seg/mark-dirty new-seg)))
         g)))
   graph
   (render-order graph)))

;; =============================================================================
;; Graph Statistics
;; =============================================================================

(defn stats
  "Get statistics about the graph."
  [graph]
  (let [segs (all-segments graph)]
    {:total (count segs)
     :pending (count (filter #(= :pending (seg/render-state %)) segs))
     :rendering (count (filter #(= :rendering (seg/render-state %)) segs))
     :cached (count (filter #(= :cached (seg/render-state %)) segs))
     :dirty (count (filter #(= :dirty (seg/render-state %)) segs))
     :error (count (filter #(= :error (seg/render-state %)) segs))
     :max-depth (if (empty? segs) 0
                    (apply max (map #(count (transitive-dependencies graph
                                                                     (seg/segment-id %)))
                                    segs)))}))

;; =============================================================================
;; Visualization
;; =============================================================================

(defn ->dot
  "Generate DOT graph representation for visualization."
  [graph]
  (let [segments (:segments graph)]
    (str "digraph SceneGraph {\n"
         "  rankdir=LR;\n"
         "  node [shape=box];\n"
         (apply str
                (for [[id seg] segments]
                  (let [color (case (seg/render-state seg)
                                :pending "gray"
                                :rendering "yellow"
                                :cached "green"
                                :dirty "orange"
                                :error "red"
                                "white")]
                    (str "  " (name id) " [style=filled, fillcolor=" color "];\n"))))
         (apply str
                (for [[id seg] segments
                      dep (seg/dependencies seg)]
                  (str "  " (name dep) " -> " (name id) ";\n")))
         "}\n")))

(defn print-status
  "Print human-readable status of the graph."
  [graph]
  (println "\n=== Scene Graph Status ===")
  (let [s (stats graph)]
    (println (format "Total: %d segments" (:total s)))
    (println (format "  Cached:    %d" (:cached s)))
    (println (format "  Dirty:     %d" (:dirty s)))
    (println (format "  Pending:   %d" (:pending s)))
    (println (format "  Rendering: %d" (:rendering s)))
    (println (format "  Error:     %d" (:error s))))
  (println "\nRender Order:")
  (doseq [seg-id (render-order graph)]
    (let [seg (get-segment graph seg-id)
          state-icon (case (seg/render-state seg)
                       :pending "○"
                       :rendering "◐"
                       :cached "●"
                       :dirty "◑"
                       :error "✗"
                       "?")]
      (println (format "  %s %s" state-icon (name seg-id))))))

;; =============================================================================
;; Printing
;; =============================================================================

(defmethod print-method SceneGraph [g ^java.io.Writer w]
  (let [s (stats g)]
    (.write w (str "#SceneGraph{:segments " (:total s)
                   " :cached " (:cached s)
                   " :dirty " (:dirty s) "}"))))
