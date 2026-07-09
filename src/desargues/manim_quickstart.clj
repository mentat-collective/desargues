(ns desargues.manim-quickstart
  "Complete quickstart example - reproduces the Manim tutorial"
  (:require [libpython-clj2.python :as py]
            [libpython-clj2.python.class :as py-class]
            [desargues.config :as config]))

;; ============================================================================
;; Setup Functions
;; ============================================================================

(defn init!
  "Initialize Python with the manim conda environment.
   Paths come from desargues.config (override via DESARGUES_MANIM_* env vars).
   Call this ONCE at the start of your REPL session."
  []
  (let [{:keys [python-exe library-path site-packages]} (config/manim-config)]
    (py/initialize!
     :python-executable python-exe
     :library-path library-path)
    (let [sys (py/import-module "sys")]
      (py/call-attr (py/get-attr sys "path") "insert" 0 site-packages))
    (println "Python initialized:" python-exe)))

(defn get-manim-module
  "Get the manim module. Call after init!"
  []
  (py/import-module "manim"))

;; ============================================================================
;; Scene Creation Helpers
;; ============================================================================

(defn create-scene-class
  "Create a Manim Scene subclass with a custom construct function.

   The construct-fn should accept a single argument (self).
   Uses make-tuple-instance-fn to properly handle the self parameter.

   Example:
   (create-scene-class
     \"MyScene\"
     (fn [self]
       (let [circle (Circle)]
         (.play self (Create circle)))))"
  [class-name construct-fn]
  (let [manim (get-manim-module)
        Scene (py/get-attr manim "Scene")
        ;; Wrap the construct function to properly receive self
        wrapped-construct (py-class/make-tuple-instance-fn construct-fn)]
    (py/create-class class-name [Scene] {"construct" wrapped-construct})))

;; ============================================================================
;; Quickstart Example: CreateCircle
;; ============================================================================

(defn create-circle-construct
  "The construct function for CreateCircle scene (from manim quickstart)"
  [self]
  (let [manim (get-manim-module)
        Circle (py/get-attr manim "Circle")
        Create (py/get-attr manim "Create")
        PINK (py/get-attr manim "PINK")

        ;; Create a circle
        circle (Circle)]

    ;; Set fill color and transparency
    (py/call-attr-kw circle "set_fill" [PINK] {:opacity 0.5})

    ;; Play the Create animation
    (py/call-attr self "play" (Create circle))))

(defn make-create-circle-scene
  "Create the CreateCircle scene class by importing from manim_examples.py"
  []
  ;; Add the project directory to Python path
  (config/add-project-to-syspath!)

  ;; Import the manim_examples module and get CreateCircle class
  (let [examples (py/import-module "manim_examples")]
    (py/get-attr examples "CreateCircle")))

;; ============================================================================
;; Additional Examples
;; ============================================================================

(defn square-to-circle-construct
  "Transform a square into a circle"
  [self]
  (let [manim (get-manim-module)
        Circle (py/get-attr manim "Circle")
        Square (py/get-attr manim "Square")
        Create (py/get-attr manim "Create")
        Transform (py/get-attr manim "Transform")
        FadeOut (py/get-attr manim "FadeOut")
        BLUE (py/get-attr manim "BLUE")
        BLUE_E (py/get-attr manim "BLUE_E")
        PI (py/get-attr manim "PI")

        circle (Circle)
        square (Square)]

    ;; Style the circle
    (py/call-attr-kw circle "set_fill" [BLUE] {:opacity 0.5})
    (py/call-attr-kw circle "set_stroke" [BLUE_E] {:width 4})

    ;; Rotate the square
    (py/call-attr square "rotate" (/ PI 4))

    ;; Animate
    (py/call-attr self "play" (Create square))
    (py/call-attr self "play" (Transform square circle))
    (py/call-attr self "play" (FadeOut square))))

(defn make-square-to-circle-scene
  "Create the SquareToCircle scene class by importing from manim_examples.py"
  []
  ;; Add the project directory to Python path
  (config/add-project-to-syspath!)

  ;; Import the manim_examples module and get SquareToCircle class
  (let [examples (py/import-module "manim_examples")]
    (py/get-attr examples "SquareToCircle")))

(defn get-example-scene
  "Get any scene class from manim_examples.py by name"
  [scene-name]
  ;; Add the project directory to Python path
  (config/add-project-to-syspath!)

  ;; Import the manim_examples module and get the scene
  (let [examples (py/import-module "manim_examples")]
    (py/get-attr examples scene-name)))

;; ============================================================================
;; Rendering
;; ============================================================================

(defn render-scene!
  "Render a scene instance. The video will be saved to media/videos/

   Example:
   (let [scene (make-create-circle-scene)]
     (render-scene! (scene)))"
  [scene-instance]
  (py/call-attr scene-instance "render")
  (println "✓ Rendering complete! Check the media/videos/ directory."))

;; ============================================================================
;; Complete Workflow
;; ============================================================================

(defn quickstart!
  "Complete quickstart: initialize, create scene, and render.
   This reproduces the exact example from the Manim quickstart tutorial.
   Uses pure Clojure - no Python files needed."
  []
  (println "\n=== Manim Quickstart Example ===\n")

  ;; Step 1: Initialize Python
  (try
    (init!)
    (catch Exception e
      ;; Already initialized, that's fine
      (println "Python already initialized")))

  ;; Step 2: Create the scene class using pure Clojure construct function
  (println "Creating CreateCircle scene...")
  (let [CreateCircle (create-scene-class "CreateCircle" create-circle-construct)

        ;; Step 3: Instantiate the scene
        scene (CreateCircle)]

    (println "Rendering animation...")

    ;; Step 4: Render
    (render-scene! scene))

  (println "\n=== Done! ===")
  (println "Your video should be in: media/videos/\n"))

;; ============================================================================
;; REPL Usage Examples
;; ============================================================================

(comment
  ;; === Quick Start (One Command) ===
  ;; This runs everything automatically:
  (quickstart!)

  ;; === Step-by-Step Workflow ===

  ;; 1. Initialize Python (do this once per REPL session)
  (init!)

  ;; 2. Create and render the quickstart example
  (let [CreateCircle (make-create-circle-scene)
        scene (CreateCircle)]
    (render-scene! scene))

  ;; 3. Try the square to circle example
  (let [SquareToCircle (make-square-to-circle-scene)
        scene (SquareToCircle)]
    (render-scene! scene))

  ;; 4. Use any scene from manim_examples.py
  (let [SquareAndCircle (get-example-scene "SquareAndCircle")
        scene (SquareAndCircle)]
    (render-scene! scene))

  ;; 5. Try the animated square to circle
  (let [AnimatedSquareToCircle (get-example-scene "AnimatedSquareToCircle")
        scene (AnimatedSquareToCircle)]
    (render-scene! scene))

  ;; 6. Try the different rotations example
  (let [DifferentRotations (get-example-scene "DifferentRotations")
        scene (DifferentRotations)]
    (render-scene! scene))

  ;; === Custom Scene (Advanced) ===
  ;; NOTE: Creating custom scenes directly in Clojure using create-class
  ;; is tricky due to method wrapping issues. The recommended approach is
  ;; to define your scenes in Python and import them, like we do above.
  ;;
  ;; For custom scenes, you can:
  ;; 1. Add your scene to manim_examples.py
  ;; 2. Use (get-example-scene "YourSceneName")
  ;; 3. Render it with (render-scene! (scene))

  ;; === Access Manim objects directly ===

  (init!)

  ;; Get manim colors
  (let [manim (get-manim-module)]
    {:pink (py/get-attr manim "PINK")
     :blue (py/get-attr manim "BLUE")
     :red (py/get-attr manim "RED")
     :green (py/get-attr manim "GREEN")})

  ;; Create objects (without rendering)
  (let [manim (get-manim-module)
        Circle (py/get-attr manim "Circle")
        circle (Circle)]
    (println "Created circle:" circle)
    circle))
