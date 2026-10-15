(ns desargues.devx.parallel
  "Parallel Execution System for Segment Rendering.
   
   This module provides parallel rendering of independent segments
   using wave-based execution. Segments are partitioned into waves
   where each wave contains segments that can be rendered in parallel.
   
   ## Limitations
   
   Note: Due to Python's GIL (Global Interpreter Lock), true CPU-parallel
   rendering within a single process is limited. However, this module still
   provides benefits:
   1. Wave analysis for understanding parallelization potential
   2. Concurrent I/O operations (file writing, ffmpeg)
   3. Future support for subprocess-based true parallelism
   
   ## Architecture
   
   ```
   ┌─────────────────────────────────────────────────────────┐
   │                    Orchestrator                         │
   │  • Analyzes segment dependencies                        │
   │  • Partitions into waves                                │
   │  • Coordinates rendering                                │
   └────────────────────────┬────────────────────────────────┘
                            │
          Wave 0: [A, B]    │    Wave 1: [C, D]    Wave 2: [E]
          (independent)     │    (depend on 0)      (depends on 1)
   ```
   
   ## Usage
   
   ```clojure
   (require '[desargues.devx.parallel :as par])
   
   ;; Analyze parallelization potential
   (par/print-wave-analysis graph)
   
   ;; Render with wave-based execution
   (par/render-parallel! graph :max-workers 4)
   
   ;; With progress callback
   (par/render-parallel! graph
     :on-progress (fn [event] (println event)))
   ```"
  (:require [desargues.devx.segment :as seg]
            [desargues.devx.graph :as graph]
            [desargues.devx.quality :as quality]
            [desargues.devx.events :as events]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]])
  (:import [java.util.concurrent
            Executors
            ExecutorService
            Future
            TimeUnit
            Callable]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def ^:dynamic *max-workers*
  "Maximum number of parallel workers."
  4)

(def ^:dynamic *render-timeout-ms*
  "Timeout for a single segment render in milliseconds."
  300000) ; 5 minutes

(def ^:dynamic *progress-fn*
  "Function called for progress events."
  nil)

;; =============================================================================
;; Progress Reporting
;; =============================================================================

(defn- emit-progress!
  "Emit a progress event."
  [event-type data]
  (let [event (merge {:type event-type
                      :timestamp (java.util.Date.)}
                     data)]
    ;; Emit to callback if set
    (when *progress-fn*
      (*progress-fn* event))
    ;; Also emit to events system
    (events/emit! event)))

;; =============================================================================
;; Dependency Analysis
;; =============================================================================

(defn analyze-waves
  "Analyze segment graph and partition into waves of parallelizable segments.
   
   Returns a vector of waves, where each wave is a set of segment IDs
   that can be rendered in parallel (their dependencies are satisfied
   by previous waves).
   
   Example:
   [{:intro}                    ; Wave 0: independent segments
    {:main-1 :main-2}           ; Wave 1: depend on intro
    {:conclusion}]              ; Wave 2: depends on main-*"
  [graph dirty-seg-ids]
  (let [dirty-set (set dirty-seg-ids)
        ;; Build dependency map for dirty segments only
        dep-map (into {}
                      (map (fn [seg-id]
                             (let [seg (graph/get-segment graph seg-id)]
                               [seg-id (set (filter dirty-set (:deps seg)))]))
                           dirty-seg-ids))]
    (loop [remaining dirty-set
           satisfied #{}
           waves []]
      (if (empty? remaining)
        waves
        (let [;; Find segments whose deps are all satisfied
              ready (set (filter (fn [seg-id]
                                   (every? satisfied (get dep-map seg-id)))
                                 remaining))]
          (if (empty? ready)
            ;; Cycle detected or bug - render remaining sequentially
            (conj waves remaining)
            (recur (set/difference remaining ready)
                   (set/union satisfied ready)
                   (conj waves ready))))))))

(defn wave-stats
  "Get statistics about the wave analysis."
  [waves]
  {:total-waves (count waves)
   :total-segments (reduce + (map count waves))
   :max-parallelism (apply max 0 (map count waves))
   :wave-sizes (mapv count waves)})

;; =============================================================================
;; Render Task Execution (runs in worker thread)
;; =============================================================================

(defn- render-task
  "Execute a single render task using the standard renderer.
   Returns result map."
  [graph seg-id quality-settings]
  (let [segment (graph/get-segment graph seg-id)
        start-time (System/currentTimeMillis)]
    (try
      ;; Use the standard renderer
      (require '[desargues.devx.renderer :as renderer])
      (let [render-segment! (resolve 'desargues.devx.renderer/render-segment!)
            result (render-segment! segment :quality quality-settings)
            elapsed (- (System/currentTimeMillis) start-time)]
        (if (= :cached (seg/render-state result))
          {:success true
           :segment-id seg-id
           :output-path (:partial-file result)
           :elapsed-ms elapsed
           :segment result}
          {:success false
           :segment-id seg-id
           :error "Render did not complete"
           :elapsed-ms elapsed
           :segment result}))
      (catch Exception e
        {:success false
         :segment-id seg-id
         :error (.getMessage e)
         :exception (str (type e))
         :elapsed-ms (- (System/currentTimeMillis) start-time)}))))

;; =============================================================================
;; Thread Pool Worker
;; =============================================================================

(defonce ^:private executor-atom (atom nil))

(defn- get-executor
  "Get or create the thread pool executor."
  ^ExecutorService [max-workers]
  (if-let [exec @executor-atom]
    exec
    (let [exec (Executors/newFixedThreadPool max-workers)]
      (reset! executor-atom exec)
      exec)))

(defn shutdown-executor!
  "Shutdown the thread pool executor."
  []
  (when-let [exec @executor-atom]
    (.shutdown exec)
    (.awaitTermination exec 30 TimeUnit/SECONDS)
    (reset! executor-atom nil)))

(defn- submit-render-task
  "Submit a render task to the executor."
  ^Future [^ExecutorService executor graph seg-id quality-settings]
  (.submit executor
           (reify Callable
             (call [_]
               (render-task graph seg-id quality-settings)))))

;; =============================================================================
;; Wave-Based Parallel Rendering
;; =============================================================================

(defn render-wave!
  "Render a single wave of segments in parallel.
   Returns vector of results."
  [graph seg-ids quality-settings max-workers]
  (let [executor (get-executor max-workers)
        ;; Submit all tasks
        futures (mapv (fn [seg-id]
                        (emit-progress! :segment-start {:segment-id seg-id})
                        [seg-id (submit-render-task executor graph seg-id quality-settings)])
                      seg-ids)
        ;; Collect results with timeout
        results (mapv (fn [[seg-id ^Future fut]]
                        (try
                          (let [result (.get fut *render-timeout-ms* TimeUnit/MILLISECONDS)]
                            (emit-progress!
                             (if (:success result) :segment-complete :segment-error)
                             result)
                            result)
                          (catch java.util.concurrent.TimeoutException _
                            (.cancel fut true)
                            (let [result {:success false
                                          :segment-id seg-id
                                          :error "Render timeout"
                                          :timeout true}]
                              (emit-progress! :segment-error result)
                              result))
                          (catch Exception e
                            (let [result {:success false
                                          :segment-id seg-id
                                          :error (.getMessage e)}]
                              (emit-progress! :segment-error result)
                              result))))
                      futures)]
    results))

(defn- apply-results-to-graph
  "Apply render results to the graph, updating segment states."
  [graph results]
  (reduce
   (fn [g result]
     (let [seg-id (:segment-id result)]
       (if (:success result)
         ;; Use the rendered segment if available
         (if-let [rendered-seg (:segment result)]
           (graph/update-segment g seg-id (constantly rendered-seg))
           (graph/update-segment g seg-id
                                 #(-> %
                                      (seg/mark-cached (:output-path result))
                                      (update :metadata merge
                                              {:render-time (/ (:elapsed-ms result) 1000.0)
                                               :rendered-at (java.util.Date.)}))))
         (graph/update-segment g seg-id
                               #(seg/mark-error % {:error (:error result)})))))
   graph
   results))

;; =============================================================================
;; Main Parallel Render API
;; =============================================================================

(defn render-parallel!
  "Render dirty segments in parallel using wave-based execution.
   
   Segments are partitioned into waves based on dependencies.
   Within each wave, segments are rendered truly in parallel using
   a thread pool.
   
   Options:
   - :max-workers - Maximum parallel workers (default: 4)
   - :quality - Quality preset (default: :low)
   - :on-progress - Callback fn for progress events
   - :timeout-ms - Per-segment timeout (default: 300000)
   
   Returns updated graph with all segments rendered or marked error."
  [graph & {:keys [max-workers quality on-progress timeout-ms]
            :or {max-workers *max-workers*
                 quality :low
                 timeout-ms *render-timeout-ms*}}]
  (binding [*progress-fn* on-progress
            *render-timeout-ms* timeout-ms]
    (let [dirty-ids (graph/dirty-segment-ids graph)
          quality-settings (quality/resolve-quality quality)]

      (if (empty? dirty-ids)
        (do
          (emit-progress! :all-complete {:rendered 0 :waves 0})
          graph)

        (let [waves (analyze-waves graph dirty-ids)
              stats (wave-stats waves)]

          (println (format "\n=== Parallel Render: %d segments in %d waves (max %d parallel) ==="
                           (:total-segments stats)
                           (:total-waves stats)
                           (min max-workers (:max-parallelism stats))))

          (emit-progress! :render-start stats)

          (let [start-time (System/currentTimeMillis)
                final-graph
                (loop [g graph
                       wave-idx 0
                       [wave & remaining] waves
                       total-rendered 0]
                  (if-not wave
                    g
                    (let [wave-size (count wave)]
                      (println (format "\n--- Wave %d: %d segment(s) ---"
                                       wave-idx wave-size))
                      (emit-progress! :wave-start {:wave wave-idx
                                                   :segments (vec wave)
                                                   :size wave-size})

                      (let [results (render-wave! g (vec wave) quality-settings max-workers)
                            success-count (count (filter :success results))
                            g-updated (apply-results-to-graph g results)]

                        (emit-progress! :wave-complete {:wave wave-idx
                                                        :success success-count
                                                        :failed (- wave-size success-count)})

                        (recur g-updated
                               (inc wave-idx)
                               remaining
                               (+ total-rendered success-count))))))

                elapsed (/ (- (System/currentTimeMillis) start-time) 1000.0)]

            (println (format "\n=== Parallel render complete in %.2fs ===" elapsed))

            (emit-progress! :all-complete
                            {:rendered (count (filter #(:success %)
                                                      (mapcat identity
                                                              (map #(render-wave! graph (vec %) quality-settings max-workers)
                                                                   waves))))
                             :waves (count waves)
                             :elapsed-s elapsed})

            final-graph))))))

;; =============================================================================
;; Convenience Functions
;; =============================================================================

(defn parallel-preview!
  "Preview multiple segments in parallel.
   Useful for getting a quick overview of all segment last-frames."
  [graph seg-ids & {:keys [max-workers] :or {max-workers 4}}]
  (println (format "Previewing %d segments in parallel..." (count seg-ids)))
  (let [executor (get-executor max-workers)
        futures (mapv (fn [seg-id]
                        [seg-id
                         (.submit executor
                                  (reify Callable
                                    (call [_]
                                      (require '[desargues.devx.preview :as preview])
                                      ((resolve 'desargues.devx.preview/render-preview!)
                                       (graph/get-segment graph seg-id)
                                       :quality :low))))])
                      seg-ids)
        results (mapv (fn [[seg-id ^Future fut]]
                        (try
                          {:segment-id seg-id
                           :success true
                           :result (.get fut 60000 TimeUnit/MILLISECONDS)}
                          (catch Exception e
                            {:segment-id seg-id
                             :success false
                             :error (.getMessage e)})))
                      futures)]
    {:previewed (count (filter :success results))
     :failed (count (filter (complement :success) results))
     :results results}))

(defn estimate-parallel-time
  "Estimate total render time with parallel execution.
   Assumes average segment render time."
  [graph avg-segment-time-s max-workers]
  (let [dirty-ids (graph/dirty-segment-ids graph)
        waves (analyze-waves graph dirty-ids)
        stats (wave-stats waves)]
    {:sequential-estimate-s (* (count dirty-ids) avg-segment-time-s)
     :parallel-estimate-s (* (count waves)
                             (Math/ceil (/ (:max-parallelism stats) max-workers))
                             avg-segment-time-s)
     :speedup-factor (if (pos? (count waves))
                       (/ (count dirty-ids)
                          (* (count waves)
                             (Math/ceil (/ (:max-parallelism stats) max-workers))))
                       1.0)
     :stats stats}))

;; =============================================================================
;; REPL Helpers
;; =============================================================================

(defn print-wave-analysis
  "Print wave analysis for a graph."
  [graph]
  (let [dirty-ids (graph/dirty-segment-ids graph)
        waves (analyze-waves graph dirty-ids)
        stats (wave-stats waves)]
    (println "\n=== Wave Analysis ===")
    (println (format "Total segments: %d" (:total-segments stats)))
    (println (format "Total waves: %d" (:total-waves stats)))
    (println (format "Max parallelism: %d" (:max-parallelism stats)))
    (println "\nWaves:")
    (doseq [[idx wave] (map-indexed vector waves)]
      (println (format "  Wave %d (%d segments): %s"
                       idx (count wave) (vec wave))))
    stats))

(comment
  ;; Example usage
  (require '[desargues.devx.core :as devx])
  (require '[desargues.devx.parallel :as par])

  ;; Initialize
  (devx/init!)

  ;; Create scene with segments
  (def my-scene (devx/scene [...]))

  ;; Analyze waves
  (par/print-wave-analysis my-scene)

  ;; Render in parallel
  (def rendered-scene
    (par/render-parallel! my-scene
                          :max-workers 4
                          :quality :low
                          :on-progress println))

  ;; Estimate speedup
  (par/estimate-parallel-time my-scene 10.0 4))
