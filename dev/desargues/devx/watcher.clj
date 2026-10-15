(ns desargues.devx.watcher
  "File watcher integration using Beholder.
   
   Watches .clj files in source paths and notifies the reload
   system when files change. Supports multiple watch modes:
   - Auto-render: automatically re-render dirty segments on change
   - Manual: mark dirty but wait for explicit render command"
  (:require [nextjournal.beholder :as beholder]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; =============================================================================
;; State
;; =============================================================================

(defonce ^:private watcher-state
  (atom {:watcher nil
         :config nil
         :stats {:changes-detected 0
                 :last-change nil
                 :started-at nil}}))

;; =============================================================================
;; File -> Namespace Mapping
;; =============================================================================

(defn- clj-file?
  "Check if path is a Clojure source file."
  [path]
  (let [path-str (str path)]
    (or (str/ends-with? path-str ".clj")
        (str/ends-with? path-str ".cljc"))))

(defn- path->namespace
  "Convert a file path to a namespace symbol.
   
   Example: 'src/desargues/videos/physics.clj' -> 'desargues.videos.physics"
  [path source-paths]
  (let [path-str (str path)
        source-path (first (filter #(str/starts-with? path-str %) source-paths))]
    (when source-path
      (-> path-str
          (subs (count source-path))
          (str/replace #"^/" "")
          (str/replace #"\.cljc?$" "")
          (str/replace "/" ".")
          (str/replace "_" "-")
          symbol))))

(defn- resolve-source-paths
  "Resolve source paths to absolute paths."
  [paths]
  (mapv #(.getCanonicalPath (io/file %)) paths))

;; =============================================================================
;; Watcher Core
;; =============================================================================

(defn- make-change-handler
  "Create a Beholder-compatible change handler.
   
   Beholder calls handler with: type (:create, :modify, :delete) and path"
  [{:keys [source-paths on-change debounce-ms]
    :or {debounce-ms 100}}]
  (let [last-event (atom nil)
        resolved-paths (resolve-source-paths source-paths)]
    (fn [event]
      (let [{:keys [type path]} event]
        (when (and (clj-file? path)
                   (#{:modify :create} type))
          ;; Simple debouncing
          (let [now (System/currentTimeMillis)
                last @last-event]
            (when (or (nil? last)
                      (not= (:path last) path)
                      (> (- now (:time last)) debounce-ms))
              (reset! last-event {:path path :time now})

              (when-let [ns-sym (path->namespace (str path) resolved-paths)]
                ;; Update stats
                (swap! watcher-state update-in [:stats :changes-detected] inc)
                (swap! watcher-state assoc-in [:stats :last-change]
                       {:namespace ns-sym
                        :file (str path)
                        :time (java.util.Date.)})

                ;; Invoke callback
                (try
                  (on-change {:namespace ns-sym
                              :file (str path)
                              :event-type type})
                  (catch Exception e
                    (println "[watcher] Error in change handler:" (.getMessage e))))))))))))

(defn start-watcher!
  "Start the file watcher.
   
   Options:
   - :paths - vector of source paths to watch (default: [\"src\"])
   - :on-change - callback fn receiving {:namespace ns-sym :file path :event-type type}
   - :debounce-ms - milliseconds to debounce rapid changes (default: 100)
   
   Returns the watcher instance, or nil if already running."
  [{:keys [paths on-change debounce-ms]
    :or {paths ["src"]
         debounce-ms 100}
    :as config}]
  (when-not on-change
    (throw (ex-info "on-change callback is required" {:config config})))

  (if (:watcher @watcher-state)
    (do
      (println "[watcher] Watcher already running. Stop it first with (stop-watcher!)")
      nil)
    (let [handler (make-change-handler {:source-paths paths
                                        :on-change on-change
                                        :debounce-ms debounce-ms})
          ;; beholder/watch expects string paths, not File objects
          watcher (apply beholder/watch handler paths)]
      (swap! watcher-state assoc
             :watcher watcher
             :config (assoc config :resolved-paths (resolve-source-paths paths))
             :stats {:changes-detected 0
                     :last-change nil
                     :started-at (java.util.Date.)})
      (println (format "[watcher] Started on: %s" (str/join ", " paths)))
      watcher)))

(defn stop-watcher!
  "Stop the file watcher."
  []
  (when-let [watcher (:watcher @watcher-state)]
    (beholder/stop watcher)
    (swap! watcher-state assoc :watcher nil)
    (println "[watcher] Stopped.")
    true))

(defn watching?
  "Check if the watcher is currently running."
  []
  (some? (:watcher @watcher-state)))

(defn watcher-stats
  "Get statistics about the watcher."
  []
  (let [{:keys [config stats]} @watcher-state]
    {:running? (watching?)
     :paths (:paths config)
     :changes-detected (:changes-detected stats)
     :last-change (:last-change stats)
     :started-at (:started-at stats)}))

(defn restart-watcher!
  "Restart the watcher with the same configuration."
  []
  (when-let [config (:config @watcher-state)]
    (stop-watcher!)
    (Thread/sleep 100)
    (start-watcher! config)))
