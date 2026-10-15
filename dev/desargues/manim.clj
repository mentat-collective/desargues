(ns desargues.manim
  "Legacy Manim bindings - kept for backward compatibility.
   
   For new code, use the desargues.manim.* namespaces instead:
   - desargues.manim.core
   - desargues.manim.mobjects
   - desargues.manim.animations
   - desargues.manim.scenes
   - desargues.manim.constants
   - desargues.manim.all (convenience namespace)"
  (:require [libpython-clj2.python :as py]
            [libpython-clj2.require :refer [require-python]]
            [desargues.config :as config]))

;; Initialize Python and set up the conda environment
(defn init-python! []
  ;; Point to your conda manim environment
  ;; Adjust the path if your conda installation is elsewhere
  (let [{:keys [python-exe library-path site-packages]} (config/manim-config)]
    (py/initialize!
     :python-executable python-exe
     :library-path library-path)

    ;; Add conda environment's site-packages to Python path
    (let [sys (py/import-module "sys")]
      (py/call-attr (py/get-attr sys "path") "insert" 0
                    site-packages))))

;; Import manim dynamically
(defn import-manim! []
  (py/import-module "manim"))

;; CreateCircle scene - Clojure version
(defn create-circle-scene []
  ;; Define a Scene subclass dynamically
  (let [manim (import-manim!)
        Scene (py/get-attr manim "Scene")
        Circle (py/get-attr manim "Circle")
        Create (py/get-attr manim "Create")
        PINK (py/get-attr manim "PINK")]
    (py/create-class
     "CreateCircle"
     [Scene]
     {"construct"
      (fn [self]
        (let [circle (Circle)]
          ;; Set fill color and opacity
          (py/call-attr-kw circle "set_fill" [PINK] {:opacity 0.5})
          ;; Play the Create animation
          (py/call-attr self "play" (Create circle))))})))

;; Alternative approach using defclass-like pattern
(defn make-create-circle-scene []
  (let [manim (import-manim!)
        Scene (py/get-attr manim "Scene")
        Circle (py/get-attr manim "Circle")
        Create (py/get-attr manim "Create")
        PINK (py/get-attr manim "PINK")]
    (py/create-class
     "CreateCircle"
     [Scene]
     {"construct"
      (fn [self]
        (let [circle (Circle)]
          (py/call-attr-kw circle "set_fill" [PINK] {:opacity 0.5})
          (py/call-attr self "play" (Create circle))))})))

(comment
  ;; Usage instructions:
  ;; 1. Start a REPL
  ;; 2. Initialize Python with the manim environment:
  (init-python!)

  ;; 3. Import manim:
  (def manim (import-manim!))

  ;; 4. To render a scene, you would typically use manim's CLI
  ;; But you can also interact with manim objects directly:
  (let [Circle (py/get-attr manim "Circle")
        PINK (py/get-attr manim "PINK")
        circle (Circle)]
    (py/call-attr-kw circle "set_fill" [PINK] {:opacity 0.5})
    circle)

  ;; For new code, prefer the new bindings:
  ;; (require '[desargues.manim.all :as m])
  ;; (m/init!)
  ;; (m/run-scene
  ;;   (let [c (m/circle :color @m/PINK)]
  ;;     (m/play! self (m/create c))))
  )
