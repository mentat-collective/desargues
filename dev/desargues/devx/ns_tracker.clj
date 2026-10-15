(ns desargues.devx.ns-tracker
  "Namespace dependency tracking for segments.
   
   Tracks which segments depend on which namespaces by examining
   the :source-ns metadata field in segments. When a namespace changes,
   this module identifies all affected segments that need re-rendering."
  (:require [desargues.devx.segment :as seg]
            [desargues.devx.graph :as graph]))

;; =============================================================================
;; Segment -> Namespace Queries
;; =============================================================================

(defn segment-source-ns
  "Get the source namespace for a segment.
   Returns nil if not specified in metadata."
  [segment]
  (get-in segment [:metadata :source-ns]))

(defn segments-by-namespace
  "Group all segments in a graph by their source namespace.
   Returns map of {namespace-symbol -> [segment-ids]}"
  [scene-graph]
  (->> (graph/all-segments scene-graph)
       (filter segment-source-ns)
       (group-by segment-source-ns)
       (reduce-kv (fn [m ns segs]
                    (assoc m ns (mapv seg/segment-id segs)))
                  {})))

(defn segments-for-namespace
  "Find all segment IDs whose source namespace matches the given namespace."
  [scene-graph ns-sym]
  (->> (graph/all-segments scene-graph)
       (filter #(= ns-sym (segment-source-ns %)))
       (mapv seg/segment-id)))

;; =============================================================================
;; Affected Segment Computation
;; =============================================================================

(defn affected-segments
  "Given a changed namespace, find ALL segments that need re-rendering.
   
   This includes:
   1. Segments whose :source-ns matches the changed namespace
   2. All transitive dependents of those segments
   
   Returns a set of segment IDs."
  [scene-graph changed-ns]
  (let [direct (segments-for-namespace scene-graph changed-ns)]
    (if (empty? direct)
      #{}
      (reduce
       (fn [affected seg-id]
         (into affected
               (conj (graph/transitive-dependents scene-graph seg-id)
                     seg-id)))
       #{}
       direct))))

(defn affected-segments-for-namespaces
  "Find all affected segments for multiple changed namespaces."
  [scene-graph changed-namespaces]
  (reduce
   (fn [all-affected ns-sym]
     (into all-affected (affected-segments scene-graph ns-sym)))
   #{}
   changed-namespaces))

;; =============================================================================
;; Namespace Index
;; =============================================================================

(defn build-ns-index
  "Build an index for fast namespace -> segments lookup."
  [scene-graph]
  (let [all-segs (graph/all-segments scene-graph)]
    {:by-ns (reduce
             (fn [m seg]
               (if-let [ns-sym (segment-source-ns seg)]
                 (update m ns-sym (fnil conj #{}) (seg/segment-id seg))
                 m))
             {}
             all-segs)
     :by-segment (reduce
                  (fn [m seg]
                    (if-let [ns-sym (segment-source-ns seg)]
                      (assoc m (seg/segment-id seg) ns-sym)
                      m))
                  {}
                  all-segs)
     :untracked (->> all-segs
                     (remove segment-source-ns)
                     (map seg/segment-id)
                     set)}))

;; =============================================================================
;; Reporting
;; =============================================================================

(defn report-tracking
  "Generate a report of namespace tracking status for a scene graph."
  [scene-graph]
  (let [index (build-ns-index scene-graph)
        total (graph/segment-count scene-graph)
        tracked (- total (count (:untracked index)))]
    {:total-segments total
     :tracked-segments tracked
     :untracked-segments (count (:untracked index))
     :namespaces-involved (count (:by-ns index))
     :tracking-coverage (if (pos? total)
                          (double (/ tracked total))
                          1.0)
     :untracked-ids (:untracked index)
     :namespace-distribution (:by-ns index)}))

(defn print-tracking-report
  "Print a human-readable namespace tracking report."
  [scene-graph]
  (let [r (report-tracking scene-graph)]
    (println "\n=== Namespace Tracking Report ===")
    (println (format "Total segments: %d" (:total-segments r)))
    (println (format "Tracked: %d (%.0f%%)"
                     (:tracked-segments r)
                     (* 100 (:tracking-coverage r))))
    (println (format "Untracked: %d" (:untracked-segments r)))
    (println (format "Namespaces: %d" (:namespaces-involved r)))
    (when (seq (:untracked-ids r))
      (println "\nUntracked segment IDs:")
      (doseq [id (:untracked-ids r)]
        (println (str "  - " id))))
    (println "\nNamespace -> Segments:")
    (doseq [[ns-sym seg-ids] (sort-by key (:namespace-distribution r))]
      (println (format "  %s: %s" ns-sym (vec seg-ids))))))
