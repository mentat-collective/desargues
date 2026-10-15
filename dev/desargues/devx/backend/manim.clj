(ns desargues.devx.backend.manim
  "Manim Rendering Backend.
   
   This is the default backend that uses Manim Community Edition
   for rendering mathematical animations via Python interop."
  (:require [desargues.devx.backend :as backend]
            [desargues.devx.segment :as seg]
            [desargues.devx.quality :as quality]
            [desargues.manim.core :as manim]
            [libpython-clj2.python :as py]
            [libpython-clj2.python.class :as py-class]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]))

;; =============================================================================
;; Manim Scene Creation
;; =============================================================================

(defn- create-segment-scene
  "Create a Manim scene class for a segment."
  [segment]
  (let [construct-fn (:construct-fn segment)
        m (manim/manim)
        Scene (py/get-attr m "Scene")
        wrapped-construct (py-class/make-tuple-instance-fn
                           (fn [self] (construct-fn self)))]
    (py/create-class
     (str "Segment_" (name (seg/segment-id segment)))
     [Scene]
     {"construct" wrapped-construct})))

;; =============================================================================
;; Manim Backend Implementation
;; =============================================================================

(defrecord ManimBackend [initialized?]
  backend/IRenderBackend

  (backend-name [_] :manim)

  (init-backend! [this]
    (when-not @initialized?
      (manim/init!)
      (seg/ensure-partial-dir!)
      (reset! initialized? true)
      (println "[ManimBackend] Initialized")))

  (render-segment [this segment opts]
    (backend/init-backend! this)
    (let [seg-id (seg/segment-id segment)
          output (or (:output-file opts) (seg/partial-file-path segment))
          q-settings (quality/resolve-quality (or (:quality opts) :low))
          preview? (:preview opts)
          config (py/get-attr (manim/manim) "config")]

      (println (format "Rendering segment: %s (%s)"
                       (name seg-id)
                       (if (keyword? (:quality opts))
                         (name (:quality opts))
                         "custom")))

      ;; Configure Manim
      (py/set-attr! config "quality" (:quality q-settings))
      (py/set-attr! config "output_file" output)

      (when preview?
        (py/set-attr! config "save_last_frame" true)
        (py/set-attr! config "write_to_movie" false))

      (try
        ;; Create and render scene
        (let [scene-class (create-segment-scene segment)
              scene (scene-class)
              start-time (System/currentTimeMillis)]

          (py/call-attr scene "render")

          (let [elapsed (/ (- (System/currentTimeMillis) start-time) 1000.0)]
            (println (format "  Completed in %.2fs" elapsed))

            ;; Return updated segment
            (-> segment
                (seg/mark-cached output)
                (update :metadata assoc
                        :render-time elapsed
                        :rendered-at (java.util.Date.)
                        :backend :manim))))

        (catch Exception e
          (println (format "  ERROR: %s" (.getMessage e)))
          (seg/mark-error segment {:exception (str e)
                                   :message (.getMessage e)})))))

  (preview-segment [this segment]
    (backend/render-segment this segment {:quality :low :preview true}))

  (combine-videos [_ video-files output-path]
    (if (empty? video-files)
      (do (println "No video files to combine.")
          nil)
      (let [;; Create concat file for ffmpeg
            concat-file (str seg/*partial-dir* "/concat.txt")
            concat-content (clojure.string/join
                            "\n"
                            (map #(str "file '" (.getAbsolutePath (io/file %)) "'")
                                 video-files))]
        (spit concat-file concat-content)

        (println (format "Combining %d segments into %s"
                         (count video-files) output-path))

        (let [result (sh "ffmpeg" "-y" "-f" "concat" "-safe" "0"
                         "-i" concat-file
                         "-c" "copy" output-path)]
          (if (zero? (:exit result))
            (do
              (println "Successfully created:" output-path)
              output-path)
            (do
              (println "ffmpeg error:" (:err result))
              nil)))))))

;; =============================================================================
;; Factory Function
;; =============================================================================

(defn manim-backend
  "Create a Manim rendering backend."
  []
  (->ManimBackend (atom false)))

;; =============================================================================
;; Register as Default Backend
;; =============================================================================

(defn register!
  "Register the Manim backend and set it as default."
  []
  (let [backend (manim-backend)]
    (backend/register-backend! :manim backend)
    (backend/set-backend! :manim)
    :manim))
