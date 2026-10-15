(ns desargues.devx.backend
  "Render Backend Strategy Pattern.
   
   This module defines the protocol for rendering backends, allowing
   different implementations (Manim, FFmpeg-only, Mock for testing).
   
   ## Built-in Backends
   - :manim - Full Manim rendering (default)
   - :mock - Mock backend for testing (no actual rendering)
   
   ## Custom Backends
   ```clojure
   (defrecord MyBackend []
     IRenderBackend
     (init-backend! [this] ...)
     (render-segment [this segment opts] ...)
     (preview-segment [this segment] ...)
     (combine-videos [this files output-path] ...))
   
   (set-backend! (->MyBackend))
   ```
   
   ## Usage
   ```clojure
   ;; Use mock backend for testing
   (with-backend :mock
     (render-segment! my-segment))
   
   ;; Or set globally
   (set-backend! :mock)
   ```")

;; =============================================================================
;; Render Backend Protocol
;; =============================================================================

(defprotocol IRenderBackend
  "Protocol for rendering backends.
   Implement this to create custom rendering strategies."

  (backend-name [this]
    "Return the backend name as a keyword.")

  (init-backend! [this]
    "Initialize the backend. Called once before first render.")

  (render-segment [this segment opts]
    "Render a segment with given options.
     Options: {:quality :low/:medium/:high, :output-file \"path\"}
     Returns updated segment with :state and :partial-file.")

  (preview-segment [this segment]
    "Render only the last frame of a segment for preview.
     Returns updated segment.")

  (combine-videos [this video-files output-path]
    "Combine multiple video files into one.
     Returns output-path on success, nil on failure."))

;; =============================================================================
;; Backend Registry
;; =============================================================================

(defonce ^:private backend-registry
  (atom {}))

(defonce ^:private current-backend
  (atom nil))

(defn register-backend!
  "Register a backend implementation by keyword."
  [backend-key backend-impl]
  {:pre [(keyword? backend-key)
         (satisfies? IRenderBackend backend-impl)]}
  (swap! backend-registry assoc backend-key backend-impl)
  backend-key)

(defn get-backend
  "Get a registered backend by keyword, or return the implementation directly."
  [backend]
  (cond
    (keyword? backend)
    (or (get @backend-registry backend)
        (throw (ex-info (str "Unknown backend: " backend
                             ". Available: " (keys @backend-registry))
                        {:backend backend
                         :available (keys @backend-registry)})))

    (satisfies? IRenderBackend backend)
    backend

    :else
    (throw (ex-info "Invalid backend" {:backend backend}))))

(defn set-backend!
  "Set the current rendering backend."
  [backend]
  (let [impl (get-backend backend)]
    (reset! current-backend impl)
    (backend-name impl)))

(defn current-backend-impl
  "Get the current backend implementation."
  []
  @current-backend)

(defn set-backend-impl!
  "Internal: Set the current backend directly (for use by with-backend)."
  [impl]
  (reset! current-backend impl))

(defn list-backends
  "List all registered backend keywords."
  []
  (keys @backend-registry))

;; =============================================================================
;; Mock Backend (for testing)
;; =============================================================================

(defrecord MockBackend [render-delay-ms]
  IRenderBackend

  (backend-name [_] :mock)

  (init-backend! [_]
    (println "[MockBackend] Initialized"))

  (render-segment [_ segment opts]
    (let [seg-id (:id segment)
          output (or (:output-file opts)
                     (str "mock/segment_" (name seg-id) ".mp4"))
          quality (or (:quality opts) :low)]
      (println (format "[MockBackend] Rendering %s (%s)" (name seg-id) (name quality)))
      (when (pos? render-delay-ms)
        (Thread/sleep render-delay-ms))
      (-> segment
          (assoc :state :cached
                 :partial-file output)
          (update :metadata assoc
                  :render-time (/ render-delay-ms 1000.0)
                  :rendered-at (java.util.Date.)
                  :backend :mock))))

  (preview-segment [this segment]
    (render-segment this segment {:quality :low :preview true}))

  (combine-videos [_ video-files output-path]
    (println (format "[MockBackend] Combining %d files -> %s"
                     (count video-files) output-path))
    output-path))

(defn mock-backend
  "Create a mock backend for testing.
   
   Options:
   - :render-delay-ms - Simulated render time (default 0)"
  [& {:keys [render-delay-ms] :or {render-delay-ms 0}}]
  (->MockBackend render-delay-ms))

;; =============================================================================
;; Backend Context Macro
;; =============================================================================

(defmacro with-backend
  "Execute body with a specific backend.
   Restores previous backend after execution.
   
   Usage:
   (with-backend :mock
     (render-segment! my-segment))"
  [backend & body]
  `(let [prev# (current-backend-impl)
         impl# (get-backend ~backend)]
     (try
       (set-backend-impl! impl#)
       ~@body
       (finally
         (set-backend-impl! prev#)))))

;; =============================================================================
;; Initialize Mock Backend
;; =============================================================================

;; Register mock backend by default
(register-backend! :mock (mock-backend))
