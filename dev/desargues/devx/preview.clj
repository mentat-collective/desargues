(ns desargues.devx.preview
  "Enhanced Preview System for Segments.
   
   Provides fast visual feedback during development by rendering
   only the last frame of a segment. Supports multiple image viewers
   and quality settings.
   
   ## Quick Start
   
   ```clojure
   (require '[desargues.devx.preview :as preview])
   
   ;; Preview a segment (renders last frame, opens viewer)
   (preview/preview! segment)
   
   ;; Preview without opening viewer
   (preview/preview! segment :open? false)
   
   ;; Preview with specific quality
   (preview/preview! segment :quality :medium)
   
   ;; Get preview image path
   (preview/preview-path segment)
   ```
   
   ## Configuration
   
   ```clojure
   ;; Set preferred image viewer
   (preview/set-viewer! :feh)  ; or :xdg-open, :eog, :custom
   
   ;; Custom viewer command
   (preview/set-viewer! :custom \"firefox\")
   ```"
  (:require [desargues.devx.segment :as seg]
            [desargues.devx.quality :as quality]
            [desargues.manim.core :as manim]
            [libpython-clj2.python :as py]
            [libpython-clj2.python.class :as py-class]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def ^:dynamic *preview-dir* "media/previews")
(def ^:dynamic *viewer* :xdg-open)
(def ^:dynamic *custom-viewer-cmd* nil)

(defonce viewer-config
  (atom {:viewer :xdg-open
         :custom-cmd nil}))

(def viewer-commands
  "Map of viewer keywords to shell commands."
  {:xdg-open ["xdg-open"]
   :feh ["feh" "--scale-down" "--auto-zoom"]
   :eog ["eog"]
   :sxiv ["sxiv"]
   :imv ["imv"]
   :firefox ["firefox"]
   :chromium ["chromium"]})

(defn set-viewer!
  "Set the preferred image viewer.
   
   Accepts:
   - :xdg-open (default, uses system default)
   - :feh (lightweight, good for tiling WMs)
   - :eog (GNOME image viewer)
   - :sxiv (simple X image viewer)
   - :imv (Wayland-compatible)
   - :firefox, :chromium (browser)
   - :custom with command string"
  ([viewer-key]
   (if (contains? viewer-commands viewer-key)
     (swap! viewer-config assoc :viewer viewer-key :custom-cmd nil)
     (throw (ex-info "Unknown viewer" {:viewer viewer-key
                                       :available (keys viewer-commands)}))))
  ([viewer-key custom-cmd]
   (when (= viewer-key :custom)
     (swap! viewer-config assoc :viewer :custom :custom-cmd custom-cmd))))

(defn get-viewer-command
  "Get the shell command for the current viewer."
  []
  (let [{:keys [viewer custom-cmd]} @viewer-config]
    (if (= viewer :custom)
      [custom-cmd]
      (get viewer-commands viewer ["xdg-open"]))))

;; =============================================================================
;; Preview Directory Management
;; =============================================================================

(defn ensure-preview-dir!
  "Ensure preview directory exists."
  []
  (let [dir (io/file *preview-dir*)]
    (when-not (.exists dir)
      (.mkdirs dir))
    (.getAbsolutePath dir)))

(defn preview-path
  "Get the preview image path for a segment."
  [segment]
  (let [seg-id (seg/segment-id segment)
        hash-prefix (subs (or (seg/content-hash segment) "unknown") 0 8)]
    (str (ensure-preview-dir!) "/" (name seg-id) "_" hash-prefix ".png")))

(defn clean-old-previews!
  "Remove preview images older than max-age-hours."
  [& {:keys [max-age-hours] :or {max-age-hours 24}}]
  (let [dir (io/file *preview-dir*)
        cutoff (- (System/currentTimeMillis) (* max-age-hours 60 60 1000))]
    (when (.exists dir)
      (doseq [f (.listFiles dir)]
        (when (and (.isFile f)
                   (str/ends-with? (.getName f) ".png")
                   (< (.lastModified f) cutoff))
          (.delete f)
          (println "Cleaned:" (.getName f)))))))

;; =============================================================================
;; Preview Rendering
;; =============================================================================

(defn- create-preview-scene
  "Create a Manim scene class configured for last-frame preview."
  [segment]
  (let [construct-fn (:construct-fn segment)
        m (manim/manim)
        Scene (py/get-attr m "Scene")
        wrapped-construct (py-class/make-tuple-instance-fn
                           (fn [self] (construct-fn self)))]
    (py/create-class
     (str "Preview_" (name (seg/segment-id segment)))
     [Scene]
     {"construct" wrapped-construct})))

(defn render-preview!
  "Render a preview image (last frame) of a segment.
   
   Returns the path to the rendered image, or nil on error.
   
   Options:
   - :quality - :low (default), :medium, :high
   - :output-path - override output path"
  [segment & {:keys [quality output-path]
              :or {quality :low}}]
  (manim/init!)
  (ensure-preview-dir!)

  (let [seg-id (seg/segment-id segment)
        output (or output-path (preview-path segment))
        config (py/get-attr (manim/manim) "config")
        q-settings (quality/resolve-quality quality)]

    (println (format "[preview] Rendering last frame: %s" (name seg-id)))

    ;; Configure for preview (last frame only)
    (py/set-attr! config "quality" (:quality q-settings))
    (py/set-attr! config "save_last_frame" true)
    (py/set-attr! config "write_to_movie" false)
    (py/set-attr! config "output_file" output)

    ;; Disable progress bar for cleaner output
    (py/set-attr! config "progress_bar" "none")

    (try
      (let [scene-class (create-preview-scene segment)
            scene (scene-class)
            start-time (System/currentTimeMillis)]

        (py/call-attr scene "render")

        (let [elapsed (/ (- (System/currentTimeMillis) start-time) 1000.0)]
          (println (format "[preview] Completed in %.2fs: %s" elapsed output))
          output))

      (catch Exception e
        (println (format "[preview] ERROR: %s" (.getMessage e)))
        nil))))

;; =============================================================================
;; Image Viewing
;; =============================================================================

(defn open-image!
  "Open an image file with the configured viewer.
   Returns true on success, false on failure."
  [image-path]
  (when (and image-path (.exists (io/file image-path)))
    (let [cmd (get-viewer-command)
          full-cmd (conj cmd image-path)]
      (try
        ;; Run in background (don't block REPL)
        (future
          (apply sh full-cmd))
        (println (format "[preview] Opened: %s" image-path))
        true
        (catch Exception e
          (println (format "[preview] Could not open image: %s" (.getMessage e)))
          false)))))

;; =============================================================================
;; Main Preview API
;; =============================================================================

(defn preview!
  "Preview a segment by rendering its last frame and optionally opening it.
   
   This is the main entry point for quick visual feedback during development.
   
   Options:
   - :quality   - :low (default), :medium, :high
   - :open?     - open image viewer (default true)
   - :output    - override output path
   
   Returns the preview image path, or nil on error.
   
   Examples:
   ```clojure
   ;; Basic preview
   (preview! my-segment)
   
   ;; Preview without opening viewer
   (preview! my-segment :open? false)
   
   ;; High quality preview
   (preview! my-segment :quality :high)
   ```"
  [segment & {:keys [quality open? output]
              :or {quality :low open? true}}]
  (when-let [path (render-preview! segment :quality quality :output-path output)]
    (when open?
      (open-image! path))
    path))

(defn preview-segment-by-id!
  "Preview a segment from a graph by its ID."
  [graph seg-id & opts]
  (if-let [segment (get-in graph [:segments seg-id])]
    (apply preview! segment opts)
    (do
      (println (format "[preview] Segment not found: %s" seg-id))
      nil)))

;; =============================================================================
;; Batch Preview
;; =============================================================================

(defn preview-all-dirty!
  "Generate previews for all dirty segments in a graph.
   Does not open viewers, just generates images.
   Returns map of segment-id to preview-path."
  [graph & {:keys [quality] :or {quality :low}}]
  (let [dirty (filter #(= :dirty (seg/render-state %))
                      (vals (:segments graph)))]
    (if (empty? dirty)
      (do
        (println "[preview] No dirty segments to preview.")
        {})
      (do
        (println (format "[preview] Generating %d previews..." (count dirty)))
        (into {}
              (for [segment dirty]
                [(seg/segment-id segment)
                 (render-preview! segment :quality quality)]))))))

;; =============================================================================
;; Preview Comparison
;; =============================================================================

(defn compare-previews!
  "Generate side-by-side preview comparison.
   Useful for before/after visualization.
   
   Returns path to comparison image."
  [segment-a segment-b & {:keys [quality] :or {quality :low}}]
  (let [path-a (render-preview! segment-a :quality quality)
        path-b (render-preview! segment-b :quality quality)]
    (when (and path-a path-b)
      (let [output (str (ensure-preview-dir!) "/compare_"
                        (name (seg/segment-id segment-a)) "_"
                        (name (seg/segment-id segment-b)) ".png")]
        ;; Use ImageMagick to create side-by-side comparison
        (let [result (sh "convert" "+append" path-a path-b output)]
          (if (zero? (:exit result))
            (do
              (println (format "[preview] Comparison saved: %s" output))
              (open-image! output)
              output)
            (do
              (println "[preview] ImageMagick not available for comparison")
              ;; Fall back to opening both
              (open-image! path-a)
              (open-image! path-b)
              nil)))))))

;; =============================================================================
;; Info & Status
;; =============================================================================

(defn preview-info
  "Get info about existing preview for a segment."
  [segment]
  (let [path (preview-path segment)
        file (io/file path)]
    (if (.exists file)
      {:path path
       :exists? true
       :size (.length file)
       :modified (java.util.Date. (.lastModified file))}
      {:path path
       :exists? false})))

(defn list-previews
  "List all preview images in the preview directory."
  []
  (let [dir (io/file *preview-dir*)]
    (when (.exists dir)
      (->> (.listFiles dir)
           (filter #(str/ends-with? (.getName %) ".png"))
           (map (fn [f]
                  {:name (.getName f)
                   :path (.getAbsolutePath f)
                   :size (.length f)
                   :modified (java.util.Date. (.lastModified f))}))
           (sort-by :modified)
           reverse))))

(defn print-previews
  "Print a formatted list of preview images."
  []
  (let [previews (list-previews)]
    (if (empty? previews)
      (println "No preview images found.")
      (do
        (println "\nPreview Images")
        (println "==============")
        (doseq [{:keys [name size modified]} previews]
          (println (format "  %-40s %6.1f KB  %s"
                           name
                           (/ size 1024.0)
                           modified)))
        (println (format "\nTotal: %d previews" (count previews)))))))
