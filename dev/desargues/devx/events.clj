(ns desargues.devx.events
  "Observer Pattern for Segment State Changes.
   
   This module provides a publish-subscribe system for segment events,
   enabling loose coupling between rendering logic and observers
   (logging, metrics, UI updates, etc.).
   
   ## Event Types
   - :state-change - Segment render state changed
   - :render-start - Segment rendering started
   - :render-complete - Segment rendering finished
   - :render-error - Segment rendering failed
   - :graph-change - Scene graph structure modified
   
   ## Usage
   
   ```clojure
   ;; Register an observer
   (register-observer! :my-logger
     (fn [event]
       (println \"Event:\" (:type event) (:segment-id event))))
   
   ;; Register filtered observer (only specific events)
   (register-observer! :render-tracker
     (fn [event] (track-render event))
     :filter (fn [e] (= :render-complete (:type e))))
   
   ;; Emit an event (usually done by renderer)
   (emit! {:type :state-change
           :segment-id :intro
           :old-state :dirty
           :new-state :rendering})
   
   ;; Remove observer
   (unregister-observer! :my-logger)
   ```
   
   ## Built-in Observers
   - `logging-observer` - Prints events to stdout
   - `metrics-observer` - Collects render statistics")

;; =============================================================================
;; Observer Registry
;; =============================================================================

(defonce ^:private observer-registry
  (atom {}))

(defn register-observer!
  "Register an observer function for segment events.
   
   Parameters:
   - observer-id: Unique keyword identifier
   - handler-fn: Function called with event map
   - :filter (optional): Predicate to filter events
   
   Handler receives event maps with keys:
   - :type - Event type keyword
   - :segment-id - Affected segment (if applicable)
   - :timestamp - Event time
   - Additional keys based on event type"
  [observer-id handler-fn & {:keys [filter]}]
  {:pre [(keyword? observer-id)
         (fn? handler-fn)]}
  (swap! observer-registry assoc observer-id
         {:handler handler-fn
          :filter (or filter (constantly true))})
  observer-id)

(defn unregister-observer!
  "Remove an observer by ID."
  [observer-id]
  (swap! observer-registry dissoc observer-id)
  observer-id)

(defn list-observers
  "List all registered observer IDs."
  []
  (keys @observer-registry))

(defn clear-observers!
  "Remove all observers."
  []
  (reset! observer-registry {})
  nil)

;; =============================================================================
;; Event Emission
;; =============================================================================

(defn emit!
  "Emit an event to all registered observers.
   
   Event map should contain at minimum:
   - :type - Event type keyword
   
   Automatically adds :timestamp if not present."
  [event]
  {:pre [(map? event)
         (keyword? (:type event))]}
  (let [event (assoc event :timestamp (or (:timestamp event)
                                          (System/currentTimeMillis)))]
    (doseq [[_id {:keys [handler filter]}] @observer-registry]
      (when (filter event)
        (try
          (handler event)
          (catch Exception e
            (println (format "Observer error: %s" (.getMessage e)))))))
    event))

(defn emit-state-change!
  "Convenience function to emit a state change event."
  [segment-id old-state new-state & {:as extra}]
  (emit! (merge {:type :state-change
                 :segment-id segment-id
                 :old-state old-state
                 :new-state new-state}
                extra)))

(defn emit-render-start!
  "Emit event when segment rendering starts."
  [segment-id & {:keys [quality] :as extra}]
  (emit! (merge {:type :render-start
                 :segment-id segment-id
                 :quality quality}
                extra)))

(defn emit-render-complete!
  "Emit event when segment rendering completes successfully."
  [segment-id & {:keys [duration output-file] :as extra}]
  (emit! (merge {:type :render-complete
                 :segment-id segment-id
                 :duration duration
                 :output-file output-file}
                extra)))

(defn emit-render-error!
  "Emit event when segment rendering fails."
  [segment-id error & {:as extra}]
  (emit! (merge {:type :render-error
                 :segment-id segment-id
                 :error error}
                extra)))

(defn emit-graph-change!
  "Emit event when scene graph structure changes."
  [change-type & {:keys [segment-ids] :as extra}]
  (emit! (merge {:type :graph-change
                 :change-type change-type
                 :segment-ids segment-ids}
                extra)))

;; =============================================================================
;; Built-in Observers
;; =============================================================================

(defn logging-observer
  "Simple logging observer that prints events to stdout.
   
   Usage:
   (register-observer! :logger (logging-observer))
   (register-observer! :logger (logging-observer :verbose true))"
  [& {:keys [verbose] :or {verbose false}}]
  (fn [event]
    (let [{:keys [type segment-id]} event]
      (case type
        :state-change
        (println (format "[%s] %s -> %s"
                         (name segment-id)
                         (name (:old-state event))
                         (name (:new-state event))))

        :render-start
        (println (format "[%s] Rendering started (%s)"
                         (name segment-id)
                         (name (or (:quality event) :default))))

        :render-complete
        (println (format "[%s] Rendered in %.2fs -> %s"
                         (name segment-id)
                         (or (:duration event) 0.0)
                         (or (:output-file event) "?")))

        :render-error
        (println (format "[%s] ERROR: %s"
                         (name segment-id)
                         (:error event)))

        :graph-change
        (when verbose
          (println (format "[graph] %s: %s"
                           (name (:change-type event))
                           (pr-str (:segment-ids event)))))

        ;; Default
        (when verbose
          (println (format "[event] %s" (pr-str event))))))))

(defn metrics-observer
  "Observer that collects render statistics.
   
   Returns a function that also provides access to collected metrics.
   
   Usage:
   (def metrics (metrics-observer))
   (register-observer! :metrics metrics)
   ;; ... render some segments ...
   (metrics :stats)  ; Get statistics
   (metrics :reset)  ; Reset counters"
  []
  (let [state (atom {:render-count 0
                     :error-count 0
                     :total-duration 0.0
                     :by-segment {}})]
    (fn [event-or-command]
      (cond
        ;; Command: get stats
        (= :stats event-or-command)
        @state

        ;; Command: reset
        (= :reset event-or-command)
        (reset! state {:render-count 0
                       :error-count 0
                       :total-duration 0.0
                       :by-segment {}})

        ;; Event handling
        (map? event-or-command)
        (let [{:keys [type segment-id duration]} event-or-command]
          (case type
            :render-complete
            (swap! state (fn [s]
                           (-> s
                               (update :render-count inc)
                               (update :total-duration + (or duration 0))
                               (update-in [:by-segment segment-id :count] (fnil inc 0))
                               (update-in [:by-segment segment-id :total-duration]
                                          (fnil + 0) (or duration 0)))))

            :render-error
            (swap! state (fn [s]
                           (-> s
                               (update :error-count inc)
                               (update-in [:by-segment segment-id :error-count] (fnil inc 0)))))

            nil))

        :else nil))))

;; =============================================================================
;; Event Type Helpers
;; =============================================================================

(def event-types
  "Set of valid event types."
  #{:state-change
    :render-start
    :render-complete
    :render-error
    :graph-change})

(defn state-change-filter
  "Create a filter for specific state transitions.
   
   Usage:
   (register-observer! :dirty-watcher handler
     :filter (state-change-filter :to :dirty))"
  [& {:keys [from to segment-id]}]
  (fn [event]
    (and (= :state-change (:type event))
         (or (nil? from) (= from (:old-state event)))
         (or (nil? to) (= to (:new-state event)))
         (or (nil? segment-id) (= segment-id (:segment-id event))))))

(defn segment-filter
  "Create a filter for specific segment IDs.
   
   Usage:
   (register-observer! :intro-watcher handler
     :filter (segment-filter #{:intro :outro}))"
  [segment-ids]
  (let [ids (if (set? segment-ids) segment-ids (set segment-ids))]
    (fn [event]
      (contains? ids (:segment-id event)))))
