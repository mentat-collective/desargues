(ns desargues.devx.quality
  "Extensible Quality Settings Registry (OCP).
   
   This module provides an extensible system for render quality presets.
   Users can register custom presets without modifying source code.
   
   ## Built-in Presets
   - :low     - 480p @ 15fps (fast iteration)
   - :medium  - 720p @ 30fps (balanced)
   - :high    - 1080p @ 60fps (production)
   
   ## Custom Presets
   ```clojure
   (register-preset! :4k
     {:quality \"fourk_quality\"
      :fps 60
      :height 2160
      :description \"4K Ultra HD\"})
   
   (register-preset! :preview
     {:quality \"low_quality\"
      :fps 10
      :height 240
      :description \"Ultra-fast preview\"})
   ```
   
   ## Protocol Extension
   For complex quality logic, implement IQualityProvider:
   ```clojure
   (extend-type MyCustomConfig
     IQualityProvider
     (quality-settings [this] {...}))
   ```")

;; =============================================================================
;; Quality Provider Protocol
;; =============================================================================

(defprotocol IQualityProvider
  "Protocol for objects that provide quality settings.
   Implement this to create custom quality configuration sources."
  (quality-settings [this] "Return quality settings map with :quality, :fps, :height keys"))

;; =============================================================================
;; Quality Registry
;; =============================================================================

(defonce ^:private preset-registry
  (atom {:low {:quality "low_quality"
               :fps 15
               :height 480
               :description "Fast iteration - 480p @ 15fps"}
         :medium {:quality "medium_quality"
                  :fps 30
                  :height 720
                  :description "Balanced - 720p @ 30fps"}
         :high {:quality "high_quality"
                :fps 60
                :height 1080
                :description "Production - 1080p @ 60fps"}}))

(defn register-preset!
  "Register a custom quality preset.
   
   Parameters:
   - preset-key: Keyword identifier (e.g., :4k, :preview, :draft)
   - settings: Map with :quality, :fps, :height keys
   
   Example:
   (register-preset! :4k {:quality \"fourk_quality\"
                          :fps 60
                          :height 2160
                          :description \"4K Ultra HD\"})"
  [preset-key settings]
  {:pre [(keyword? preset-key)
         (map? settings)
         (string? (:quality settings))
         (number? (:fps settings))
         (number? (:height settings))]}
  (swap! preset-registry assoc preset-key settings)
  preset-key)

(defn unregister-preset!
  "Remove a custom quality preset."
  [preset-key]
  (swap! preset-registry dissoc preset-key)
  preset-key)

(defn list-presets
  "List all registered quality presets."
  []
  (keys @preset-registry))

(defn get-preset
  "Get settings for a quality preset.
   Returns nil if preset not found."
  [preset-key]
  (get @preset-registry preset-key))

(defn preset-exists?
  "Check if a quality preset is registered."
  [preset-key]
  (contains? @preset-registry preset-key))

;; =============================================================================
;; Quality Resolution
;; =============================================================================

(defn resolve-quality
  "Resolve quality to settings map.
   
   Accepts:
   - Keyword preset: :low, :medium, :high, or custom registered preset
   - Map with settings: {:quality \"...\" :fps N :height N}
   - IQualityProvider implementation
   
   Returns settings map or throws if invalid."
  [quality]
  (cond
    ;; Keyword preset lookup
    (keyword? quality)
    (if-let [settings (get-preset quality)]
      settings
      (throw (ex-info (str "Unknown quality preset: " quality
                           ". Available: " (pr-str (list-presets)))
                      {:preset quality
                       :available (list-presets)})))

    ;; Direct settings map
    (map? quality)
    (do
      (assert (:quality quality) "Quality map must have :quality key")
      (assert (:fps quality) "Quality map must have :fps key")
      (assert (:height quality) "Quality map must have :height key")
      quality)

    ;; Protocol implementation
    (satisfies? IQualityProvider quality)
    (quality-settings quality)

    :else
    (throw (ex-info "Invalid quality specification"
                    {:quality quality
                     :type (type quality)}))))

;; =============================================================================
;; Convenience Functions
;; =============================================================================

(defn quality-string
  "Get the Manim quality string for a preset."
  [quality]
  (:quality (resolve-quality quality)))

(defn fps
  "Get the FPS for a quality preset."
  [quality]
  (:fps (resolve-quality quality)))

(defn height
  "Get the height (resolution) for a quality preset."
  [quality]
  (:height (resolve-quality quality)))

(defn describe-preset
  "Get human-readable description of a preset."
  [preset-key]
  (when-let [settings (get-preset preset-key)]
    (or (:description settings)
        (format "%dp @ %dfps" (:height settings) (:fps settings)))))

(defn print-presets
  "Print all registered presets with descriptions."
  []
  (println "\nQuality Presets")
  (println "===============")
  (doseq [[k settings] (sort-by (comp :height val) @preset-registry)]
    (println (format "  %-10s %s"
                     (name k)
                     (or (:description settings)
                         (format "%dp @ %dfps" (:height settings) (:fps settings))))))
  (println))

;; =============================================================================
;; Default Quality
;; =============================================================================

(def ^:dynamic *default-quality*
  "Default quality preset for rendering."
  :low)

(defn set-default-quality!
  "Set the default quality preset."
  [preset-key]
  {:pre [(preset-exists? preset-key)]}
  (alter-var-root #'*default-quality* (constantly preset-key)))

;; =============================================================================
;; Auto-Quality Mode
;; =============================================================================

(def ^:dynamic *context*
  "Current quality context. Used by auto-quality to select appropriate preset.
   
   Contexts:
   - :development - Fast iteration, use :low quality
   - :preview - Quick preview, use :low quality  
   - :watch - File watching mode, use :low quality
   - :render - Explicit render, use :medium quality
   - :export - Final export, use :high quality
   - nil - No context, use *default-quality*"
  nil)

;; Rules for auto-quality selection based on context.
;; Map of context -> quality preset.
(defonce auto-quality-rules
  (atom {:development :low
         :preview :low
         :watch :low
         :render :medium
         :export :high}))

(defn set-auto-rule!
  "Set the quality preset for a specific context."
  [context preset-key]
  {:pre [(keyword? context)
         (preset-exists? preset-key)]}
  (swap! auto-quality-rules assoc context preset-key))

(defn get-auto-quality
  "Get quality preset based on current context.
   
   If *context* is set, uses auto-quality rules.
   Otherwise returns *default-quality*."
  []
  (if-let [ctx *context*]
    (get @auto-quality-rules ctx *default-quality*)
    *default-quality*))

(defn resolve-auto-quality
  "Like resolve-quality, but handles :auto keyword.
   
   When quality is :auto, uses get-auto-quality to determine preset.
   Otherwise delegates to resolve-quality."
  [quality]
  (if (= quality :auto)
    (resolve-quality (get-auto-quality))
    (resolve-quality quality)))

(defmacro with-context
  "Execute body with a specific quality context.
   
   Example:
   (with-context :export
     (render! graph))  ; Uses :high quality"
  [context & body]
  `(binding [*context* ~context]
     ~@body))

(defmacro with-development
  "Execute body in development context (low quality)."
  [& body]
  `(with-context :development ~@body))

(defmacro with-preview
  "Execute body in preview context (low quality)."
  [& body]
  `(with-context :preview ~@body))

(defmacro with-export
  "Execute body in export context (high quality)."
  [& body]
  `(with-context :export ~@body))

;; =============================================================================
;; Per-Segment Quality
;; =============================================================================

(defn segment-quality
  "Get quality for a segment, checking metadata overrides.
   
   Priority (highest to lowest):
   1. Explicit quality parameter
   2. Segment :quality metadata
   3. Auto-quality based on context
   4. Default quality"
  [segment & {:keys [quality]}]
  (or quality
      (get-in segment [:metadata :quality])
      (get-auto-quality)))

(defn with-segment-quality
  "Add quality metadata to a segment.
   
   Example:
   (-> my-segment
       (with-segment-quality :high))  ; Always render in high quality"
  [segment quality]
  (assoc-in segment [:metadata :quality] quality))

;; =============================================================================
;; Quality Info
;; =============================================================================

(defn current-quality-info
  "Get info about current quality settings."
  []
  {:context *context*
   :auto-quality (get-auto-quality)
   :default-quality *default-quality*
   :resolved (resolve-quality (get-auto-quality))})

(defn print-quality-info
  "Print current quality settings."
  []
  (let [info (current-quality-info)]
    (println "\nCurrent Quality Settings")
    (println "========================")
    (println (format "  Context:  %s" (or (:context info) "(none)")))
    (println (format "  Auto:     %s" (name (:auto-quality info))))
    (println (format "  Default:  %s" (name (:default-quality info))))
    (println (format "  Resolved: %s" (describe-preset (:auto-quality info))))
    (println)))

(defn print-auto-rules
  "Print auto-quality rules."
  []
  (println "\nAuto-Quality Rules")
  (println "==================")
  (doseq [[ctx preset] (sort-by key @auto-quality-rules)]
    (println (format "  %-15s → %s" (name ctx) (name preset))))
  (println))

