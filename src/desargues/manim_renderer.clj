(ns desargues.manim-renderer
  (:require [libpython-clj2.python :as py]
            [libpython-clj2.require :refer [require-python]]
            [desargues.config :as config]))

(defn init! []
  "Initialize Python with the manim conda environment"
  (let [{:keys [python-exe library-path site-packages]} (config/manim-config)]
    (py/initialize!
     :python-executable python-exe
     :library-path library-path)

    ;; Add conda environment's site-packages to Python path
    (let [sys (py/import-module "sys")]
      (py/call-attr (py/get-attr sys "path") "insert" 0
                    site-packages))))

(defn setup-manim! []
  "Import necessary manim modules"
  (require-python '[manim :as m])
  (require-python '[manim.utils.file_ops]))

(defn create-circle-scene-fn []
  "Returns a function that constructs the CreateCircle scene"
  (fn [self]
    (let [circle (py/call-attr (py/get-attr (py/->py-dict {:manim (py/import-module "manim")})
                                            :manim)
                               "Circle")]
      ;; Set fill color (PINK) and opacity
      (py/call-attr circle "set_fill" "#FF69B4" :opacity 0.5)
      ;; Play the Create animation
      (py/call-attr self "play"
                    (py/call-attr (py/import-module "manim") "Create" circle)))))

(defn render-scene
  "Render a manim scene with the given quality settings
   quality: 'l' (low), 'm' (medium), 'h' (high)
   preview: true to open video after rendering"
  [scene-class & {:keys [quality preview output-file]
                  :or {quality "l" preview true}}]
  (let [manim (py/import-module "manim")
        config (py/get-attr manim "config")]

    ;; Configure quality
    (case quality
      "l" (do (py/set-attr! config "pixel_height" 480)
              (py/set-attr! config "pixel_width" 854)
              (py/set-attr! config "frame_rate" 15))
      "m" (do (py/set-attr! config "pixel_height" 720)
              (py/set-attr! config "pixel_width" 1280)
              (py/set-attr! config "frame_rate" 30))
      "h" (do (py/set-attr! config "pixel_height" 1080)
              (py/set-attr! config "pixel_width" 1920)
              (py/set-attr! config "frame_rate" 60)))

    (when output-file
      (py/set-attr! config "output_file" output-file))

    (py/set-attr! config "preview" preview)

    ;; Render the scene
    (let [scene-instance (py/call-attr scene-class)]
      (py/call-attr scene-instance "render"))))

(defn create-and-render-circle! []
  "Complete example: create and render the CreateCircle scene"
  (let [manim (py/import-module "manim")
        Scene (py/get-attr manim "Scene")
        Circle (py/get-attr manim "Circle")
        Create (py/get-attr manim "Create")
        PINK (py/get-attr manim "PINK")

        ;; Create the scene class
        CreateCircle
        (py/create-class
         "CreateCircle"
         [Scene]
         {"construct"
          (fn [self]
            (let [circle (Circle)]
              (py/call-attr-kw circle "set_fill" [PINK] {:opacity 0.5})
              (py/call-attr self "play" (Create circle))))})]

    ;; Render it
    (let [scene (CreateCircle)]
      (py/call-attr scene "render")
      (println "Rendering complete! Check the media folder for output.")
      scene)))

(comment
  ;; REPL Usage:

  ;; 1. Initialize Python
  (init!)

  ;; 2. Setup manim
  (setup-manim!)

  ;; 3. Create and render the circle animation
  (create-and-render-circle!)

  ;; The video will be saved in the media/ folder
  )
