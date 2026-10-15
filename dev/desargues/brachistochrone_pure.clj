(ns desargues.brachistochrone-pure
  "Pure Clojure brachistochrone implementation using libpython-clj to create Manim objects.

  No Python files needed - everything is done in Clojure by manipulating Python objects directly."
  (:require [emmy.env :as e :refer [->TeX D simplify sin cos square sqrt]]
            [libpython-clj2.python :as py]
            [desargues.manim-quickstart :as mq]
            [desargues.brachistochrone :as brach]))

;; ============================================================================
;; Manim Object Creation Helpers
;; ============================================================================

(defn manim-array
  "Convert Clojure vector [x y z] to numpy array for Manim"
  [[x y z]]
  (let [np (py/import-module "numpy")]
    (py/call-attr np "array" [x y z])))

(defn create-text
  "Create a Manim Text object"
  [text & {:keys [font-size color]
           :or {font-size 36 color nil}}]
  (let [manim (mq/get-manim-module)
        Text (py/get-attr manim "Text")
        kwargs (cond-> {:font_size font-size}
                 color (assoc :color color))]
    (apply Text (apply concat [:text text] (seq kwargs)))))

(defn create-math-tex
  "Create a Manim MathTex object"
  [latex & {:keys [font-size color]
            :or {font-size 36 color nil}}]
  (let [manim (mq/get-manim-module)
        MathTex (py/get-attr manim "MathTex")
        kwargs (cond-> {:font_size font-size}
                 color (assoc :color color))]
    (apply MathTex (apply concat [latex] (seq kwargs)))))

(defn create-dot
  "Create a Manim Dot"
  [point & {:keys [color radius]
            :or {radius 0.08 color nil}}]
  (let [manim (mq/get-manim-module)
        Dot (py/get-attr manim "Dot")
        kwargs (cond-> {:radius radius}
                 color (assoc :color color))]
    (apply Dot (apply concat [:point (manim-array point)] (seq kwargs)))))

(defn create-line
  "Create a Manim Line"
  [start end & {:keys [color]
                :or {color nil}}]
  (let [manim (mq/get-manim-module)
        Line (py/get-attr manim "Line")
        kwargs (cond-> {}
                 color (assoc :color color))]
    (apply Line (apply concat [:start (manim-array start)
                               :end (manim-array end)]
                       (seq kwargs)))))

;;============================================================================
;; Simple Brachistochrone Scene
;; ============================================================================

(defn simple-brach-construct
  "Simple brachistochrone scene showing problem setup and physics results"
  [self]
  (let [manim (mq/get-manim-module)

        ;; Get animation classes
        Write (py/get-attr manim "Write")
        Create (py/get-attr manim "Create")
        FadeOut (py/get-attr manim "FadeOut")

        ;; Get constants/colors
        UP (py/get-attr manim "UP")
        DOWN (py/get-attr manim "DOWN")
        GREEN (py/get-attr manim "GREEN")
        RED (py/get-attr manim "RED")
        BLUE (py/get-attr manim "BLUE")
        YELLOW (py/get-attr manim "YELLOW")

        ;; Compute physics
        physics (brach/compare-paths -4.0 2.0 4.0 -2.0)
        lagrangian-tex (brach/generate-lagrangian-latex)]

    ;; Title
    (let [title (create-text "The Brachistochrone Problem" :font-size 42)]
      (py/call-attr title "to_edge" UP)
      (py/call-attr self "play" (Write title))
      (py/call-attr self "wait" 1))

    ;; Show points A and B
    (let [pt-a (create-dot [-4 2 0] :color GREEN :radius 0.12)
          pt-b (create-dot [4 -2 0] :color RED :radius 0.12)
          label-a (create-math-tex "A" :color GREEN :font-size 36)
          label-b (create-math-tex "B" :color RED :font-size 36)]

      (py/call-attr label-a "next_to" pt-a UP)
      (py/call-attr label-b "next_to" pt-b DOWN)

      (py/call-attr self "play" (Create pt-a) (Create pt-b))
      (py/call-attr self "play" (Write label-a) (Write label-b))
      (py/call-attr self "wait" 1))

    ;; Draw three paths
    (let [straight (create-line [-4 2 0] [4 -2 0] :color BLUE)
          question (create-text "Which path is fastest under gravity?" :font-size 32)]
      (py/call-attr question "shift" (manim-array [0 -3 0]))
      (py/call-attr self "play" (Create straight))
      (py/call-attr self "wait" 0.5)
      (py/call-attr self "play" (Write question))
      (py/call-attr self "wait" 1.5)
      (py/call-attr self "play" (FadeOut question)))

    ;; Show Lagrangian
    (let [lag-label (create-text "Lagrangian:" :font-size 32)
          lag-eq (create-math-tex lagrangian-tex :font-size 30)]
      (py/call-attr lag-label "shift" (manim-array [0 1 0]))
      (py/call-attr lag-eq "next_to" lag-label DOWN)

      (py/call-attr self "play" (Write lag-label))
      (py/call-attr self "wait" 0.5)
      (py/call-attr self "play" (Write lag-eq))
      (py/call-attr self "wait" 2)
      (py/call-attr self "play" (FadeOut lag-label) (FadeOut lag-eq)))

    ;; Show results
    (let [results-title (create-text "Descent Times:" :font-size 32)
          time-straight (format "Straight: %.2fs" (:time (:straight physics)))
          time-parabola (format "Parabola: %.2fs" (:time (:parabola physics)))
          time-cycloid (format "Cycloid: %.2fs (fastest!)" (:time (:cycloid physics)))

          text1 (create-text time-straight :font-size 28 :color BLUE)
          text2 (create-text time-parabola :font-size 28 :color YELLOW)
          text3 (create-text time-cycloid :font-size 28 :color GREEN)

          VGroup (py/get-attr manim "VGroup")
          results-group (VGroup)
          LEFT (py/get-attr manim "LEFT")]

      (py/call-attr results-title "shift" (manim-array [0 1.5 0]))
      (py/call-attr results-group "add" text1)
      (py/call-attr results-group "add" text2)
      (py/call-attr results-group "add" text3)
      (py/call-attr results-group "arrange" DOWN :buff 0.4 :aligned_edge LEFT)
      (py/call-attr results-group "shift" (manim-array [0 -0.5 0]))

      (py/call-attr self "play" (Write results-title))
      (py/call-attr self "wait" 0.5)
      (py/call-attr self "play" (Write text1))
      (py/call-attr self "wait" 0.5)
      (py/call-attr self "play" (Write text2))
      (py/call-attr self "wait" 0.5)
      (py/call-attr self "play" (Write text3))
      (py/call-attr self "wait" 2))

    ;; Show actions
    (let [action-title (create-text "Action Integrals:" :font-size 32)
          action-straight (format "S(straight) = %.1f" (:action (:straight physics)))
          action-parabola (format "S(parabola) = %.1f" (:action (:parabola physics)))
          action-cycloid (format "S(cycloid) = %.1f (minimum!)" (:action (:cycloid physics)))

          text1 (create-text action-straight :font-size 28 :color BLUE)
          text2 (create-text action-parabola :font-size 28 :color YELLOW)
          text3 (create-text action-cycloid :font-size 28 :color GREEN)

          VGroup (py/get-attr manim "VGroup")
          action-group (VGroup)
          LEFT (py/get-attr manim "LEFT")]

      (py/call-attr action-title "shift" (manim-array [0 1.5 0]))
      (py/call-attr action-group "add" text1)
      (py/call-attr action-group "add" text2)
      (py/call-attr action-group "add" text3)
      (py/call-attr action-group "arrange" DOWN :buff 0.4 :aligned_edge LEFT)
      (py/call-attr action-group "shift" (manim-array [0 -0.5 0]))

      (py/call-attr self "play" (Write action-title))
      (py/call-attr self "wait" 0.5)
      (py/call-attr self "play" (Write text1))
      (py/call-attr self "wait" 0.5)
      (py/call-attr self "play" (Write text2))
      (py/call-attr self "wait" 0.5)
      (py/call-attr self "play" (Write text3))
      (py/call-attr self "wait" 3))

    ;; Final message
    (let [winner (create-text "The Cycloid is the Brachistochrone!" :font-size 36 :color GREEN)]
      (py/call-attr self "play" (Write winner))
      (py/call-attr self "wait" 3))))

(defn render-simple-brachistochrone
  "Render simple brachistochrone scene"
  []
  (mq/init!)

  (println "\n=== Rendering Simple Brachistochrone ===")
  (println "Computing physics with Emmy...")

  (let [physics (brach/compare-paths -4.0 2.0 4.0 -2.0)]
    (println "\nResults:")
    (println "  Straight:" (:time (:straight physics)) "s, Action:" (:action (:straight physics)))
    (println "  Parabola:" (:time (:parabola physics)) "s, Action:" (:action (:parabola physics)))
    (println "  Cycloid:" (:time (:cycloid physics)) "s, Action:" (:action (:cycloid physics)))
    (println "\nLagrangian:" (brach/generate-lagrangian-latex)))

  ;; Create scene class
  (let [manim (mq/get-manim-module)
        Scene (py/get-attr manim "Scene")

        SimpleBrachistochrone (py/create-class
                               "SimpleBrachistochrone"
                               [Scene]
                               {"construct" simple-brach-construct})

        scene (SimpleBrachistochrone)]

    (println "\nRendering...")
    (mq/render-scene! scene)
    (println "Done! Check media/videos/ directory.")))

;; ============================================================================
;; REPL Examples
;; ============================================================================

(comment
  ;; Initialize Python/Manim
  (mq/init!)

  ;; Test physics calculations
  (brach/compare-paths -4.0 2.0 4.0 -2.0)

  ;; Test LaTeX generation
  (brach/generate-lagrangian-latex)

  ;; Render the simple scene
  (render-simple-brachistochrone)

  ;; The animation will show:
  ;; 1. Problem setup (points A and B)
  ;; 2. Question about fastest path
  ;; 3. Lagrangian equation (from Emmy)
  ;; 4. Descent times for each curve (computed from physics)
  ;; 5. Action integrals for each curve
  ;; 6. Winner announcement (cycloid is the brachistochrone!)
  )
