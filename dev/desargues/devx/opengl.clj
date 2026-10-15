(ns desargues.devx.opengl
  "OpenGL Live Preview for Real-Time Development.
   
   Provides GPU-accelerated live preview using Manim's OpenGL renderer.
   The preview window updates in real-time as you make changes to your code.
   
   ## Quick Start
   
   ```clojure
   (require '[desargues.devx.opengl :as gl])
   
   ;; Start live preview of a segment
   (gl/live! segment)
   
   ;; Start with auto-refresh on file changes
   (gl/live! segment :watch? true)
   
   ;; Interactive mode (get IPython prompt)
   (gl/live-interactive! segment)
   ```
   
   ## Keyboard Controls (in preview window)
   
   - `r` - Refresh/re-render the scene
   - `q` - Quit the preview window
   - `+` / `-` - Zoom in/out
   - Arrow keys - Pan camera
   
   ## Configuration
   
   ```clojure
   ;; Enable OpenGL globally
   (gl/enable-opengl!)
   
   ;; Disable (back to Cairo)
   (gl/disable-opengl!)
   
   ;; Check current renderer
   (gl/opengl-enabled?)
   ```
   
   ## Notes
   
   - OpenGL renderer requires a display (X11/Wayland)
   - Performance is significantly better for complex scenes
   - Interactive embed requires IPython 8.0.1 (see docs)"
  (:require [desargues.devx.segment :as seg]
            [desargues.devx.quality :as quality]
            [desargues.manim.core :as manim]
            [libpython-clj2.python :as py]
            [libpython-clj2.python.class :as py-class]
            [clojure.java.io :as io]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def ^:dynamic *opengl-enabled* false)
(def ^:dynamic *force-window* true)
(def ^:dynamic *enable-gui* false)

(defonce opengl-config
  (atom {:enabled false
         :force-window true
         :enable-gui false
         :quality :low}))

(defn enable-opengl!
  "Enable OpenGL renderer globally.
   
   Options:
   - :force-window - force preview window to open (default true)
   - :enable-gui - enable GUI controls (default false)"
  [& {:keys [force-window enable-gui]
      :or {force-window true enable-gui false}}]
  (swap! opengl-config assoc
         :enabled true
         :force-window force-window
         :enable-gui enable-gui)
  (println "[opengl] OpenGL renderer enabled"))

(defn disable-opengl!
  "Disable OpenGL renderer, switch back to Cairo."
  []
  (swap! opengl-config assoc :enabled false)
  (println "[opengl] OpenGL renderer disabled (using Cairo)"))

(defn opengl-enabled?
  "Check if OpenGL renderer is enabled."
  []
  (:enabled @opengl-config))

(defn set-opengl-quality!
  "Set default quality for OpenGL preview."
  [quality]
  (swap! opengl-config assoc :quality quality))

;; =============================================================================
;; Manim Config Helpers
;; =============================================================================

(defn- configure-opengl!
  "Configure Manim for OpenGL rendering."
  [& {:keys [force-window enable-gui quality]
      :or {force-window true enable-gui false quality :low}}]
  (manim/init!)
  (let [config (py/get-attr (manim/manim) "config")
        q-settings (quality/resolve-quality quality)]

    ;; Set renderer to OpenGL
    (py/set-attr! config "renderer" "opengl")
    (py/set-attr! config "force_window" force-window)
    (py/set-attr! config "enable_gui" enable-gui)

    ;; Quality settings
    (py/set-attr! config "quality" (:quality q-settings))

    ;; Don't write to movie for live preview
    (py/set-attr! config "write_to_movie" false)

    config))

(defn- configure-cairo!
  "Reset Manim to Cairo rendering."
  []
  (manim/init!)
  (let [config (py/get-attr (manim/manim) "config")]
    (py/set-attr! config "renderer" "cairo")
    (py/set-attr! config "force_window" false)
    config))

;; =============================================================================
;; Scene Creation for OpenGL
;; =============================================================================

(defn- create-opengl-scene
  "Create a Manim scene class for OpenGL preview.
   
   The scene includes:
   - The segment's construct function
   - Optional interactive embed for REPL control
   - Keyboard handlers for refresh/quit"
  [segment & {:keys [interactive?] :or {interactive? false}}]
  (let [construct-fn (:construct-fn segment)
        m (manim/manim)
        Scene (py/get-attr m "Scene")]

    (if interactive?
      ;; Interactive scene with embed
      (let [wrapped-construct
            (py-class/make-tuple-instance-fn
             (fn [self]
               (construct-fn self)
               ;; Call interactive_embed for REPL control
               (py/call-attr self "interactive_embed")))]
        (py/create-class
         (str "OpenGLInteractive_" (name (seg/segment-id segment)))
         [Scene]
         {"construct" wrapped-construct}))

      ;; Non-interactive scene
      (let [wrapped-construct
            (py-class/make-tuple-instance-fn
             (fn [self]
               (construct-fn self)))]
        (py/create-class
         (str "OpenGLPreview_" (name (seg/segment-id segment)))
         [Scene]
         {"construct" wrapped-construct})))))

;; =============================================================================
;; Live Preview API
;; =============================================================================

(defn live!
  "Start a live OpenGL preview of a segment.
   
   Opens a GPU-accelerated preview window that shows the scene.
   The window stays open until closed manually.
   
   Options:
   - :quality     - :low (default), :medium, :high
   - :force-window - force window to open (default true)
   
   Returns the scene instance for further manipulation.
   
   Example:
   ```clojure
   (live! my-segment)
   (live! my-segment :quality :medium)
   ```"
  [segment & {:keys [quality force-window]
              :or {quality :low force-window true}}]
  (println (format "[opengl] Starting live preview: %s"
                   (name (seg/segment-id segment))))

  (configure-opengl! :quality quality :force-window force-window)

  (try
    (let [scene-class (create-opengl-scene segment)
          scene (scene-class)]

      (println "[opengl] Rendering scene...")
      (py/call-attr scene "render")

      (println "[opengl] Preview window opened. Close window to return.")
      scene)

    (catch Exception e
      (println (format "[opengl] ERROR: %s" (.getMessage e)))
      (println "[opengl] Hint: Ensure DISPLAY is set and OpenGL is available")
      nil)

    (finally
      ;; Reset to Cairo after preview
      (configure-cairo!))))

(defn live-interactive!
  "Start an interactive OpenGL preview with IPython control.
   
   Opens the preview window and drops into an IPython prompt where
   you can continue to manipulate the scene:
   
   - Add more animations: self.play(...)
   - Modify objects: circle.set_color(RED)
   - Type 'exit' or Ctrl+D to close
   
   IMPORTANT: Requires IPython 8.0.1 for compatibility.
   Install with: pip install IPython==8.0.1
   
   Example:
   ```clojure
   (live-interactive! my-segment)
   ;; In IPython prompt:
   ;; >>> self.play(FadeOut(circle))
   ;; >>> exit
   ```"
  [segment & {:keys [quality] :or {quality :low}}]
  (println (format "[opengl] Starting interactive preview: %s"
                   (name (seg/segment-id segment))))
  (println "[opengl] Type 'exit' or Ctrl+D to return to Clojure REPL")

  (configure-opengl! :quality quality :force-window true)

  (try
    (let [scene-class (create-opengl-scene segment :interactive? true)
          scene (scene-class)]

      (py/call-attr scene "render")

      (println "[opengl] Interactive session ended")
      scene)

    (catch Exception e
      (println (format "[opengl] ERROR: %s" (.getMessage e)))
      (when (re-find #"(?i)ipython" (.getMessage e))
        (println "[opengl] Hint: Install IPython 8.0.1: pip install IPython==8.0.1"))
      nil)

    (finally
      (configure-cairo!))))

(defn live-segment-by-id!
  "Start live preview of a segment from a graph by its ID."
  [graph seg-id & opts]
  (if-let [segment (get-in graph [:segments seg-id])]
    (apply live! segment opts)
    (do
      (println (format "[opengl] Segment not found: %s" seg-id))
      nil)))

;; =============================================================================
;; Watch Integration
;; =============================================================================

(defonce live-session
  (atom {:segment nil
         :scene nil
         :active? false}))

(defn start-live-session!
  "Start a persistent live preview session.
   
   The session keeps the preview window open and refreshes
   when `refresh-live!` is called.
   
   This is designed to integrate with the file watcher for
   automatic refresh on code changes."
  [segment & {:keys [quality] :or {quality :low}}]
  (when (:active? @live-session)
    (println "[opengl] Stopping existing live session..."))

  (reset! live-session {:segment segment
                        :scene nil
                        :active? true})

  (println (format "[opengl] Starting live session: %s"
                   (name (seg/segment-id segment))))

  ;; Initial render
  (let [scene (live! segment :quality quality)]
    (swap! live-session assoc :scene scene)
    scene))

(defn refresh-live!
  "Refresh the current live preview session.
   
   Call this after code changes to see updates in the preview window.
   Used by the hot-reload system for automatic refresh."
  []
  (when-let [{:keys [segment active?]} @live-session]
    (when active?
      (println "[opengl] Refreshing live preview...")
      (let [scene (live! segment)]
        (swap! live-session assoc :scene scene)
        scene))))

(defn stop-live-session!
  "Stop the current live preview session."
  []
  (when (:active? @live-session)
    (println "[opengl] Stopping live session")
    (reset! live-session {:segment nil :scene nil :active? false})))

(defn live-session-info
  "Get info about the current live session."
  []
  (let [{:keys [segment active?]} @live-session]
    {:active? active?
     :segment-id (when segment (seg/segment-id segment))}))

;; =============================================================================
;; Utility Functions
;; =============================================================================

(defn check-opengl-support
  "Check if OpenGL rendering is available on this system.
   Returns a map with :supported? and diagnostic info."
  []
  (manim/init!)
  (try
    (let [config (py/get-attr (manim/manim) "config")]
      ;; Try to set renderer to opengl
      (py/set-attr! config "renderer" "opengl")
      (let [renderer (py/get-attr config "renderer")]
        {:supported? (= (str renderer) "opengl")
         :display (System/getenv "DISPLAY")
         :wayland (System/getenv "WAYLAND_DISPLAY")
         :renderer (str renderer)}))
    (catch Exception e
      {:supported? false
       :error (.getMessage e)
       :display (System/getenv "DISPLAY")
       :wayland (System/getenv "WAYLAND_DISPLAY")})
    (finally
      (configure-cairo!))))

(defn print-opengl-status
  "Print diagnostic information about OpenGL support."
  []
  (let [status (check-opengl-support)]
    (println "\nOpenGL Status")
    (println "=============")
    (println (format "  Supported: %s" (if (:supported? status) "Yes" "No")))
    (println (format "  DISPLAY:   %s" (or (:display status) "(not set)")))
    (println (format "  WAYLAND:   %s" (or (:wayland status) "(not set)")))
    (when (:error status)
      (println (format "  Error:     %s" (:error status))))
    (println)))
