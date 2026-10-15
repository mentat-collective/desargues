(ns desargues.manim.examples
  "Comprehensive examples demonstrating Manim bindings.
   
   Run these examples in the REPL after (require '[desargues.manim.examples :as ex])
   Then: (ex/run-circle-example) etc."
  (:require [desargues.manim.core :as core]
            [desargues.manim.mobjects :as m]
            [desargues.manim.animations :as a]
            [desargues.manim.scenes :as s]
            [desargues.manim.constants :as c]))

;; ============================================================================
;; Basic Examples
;; ============================================================================

(defn circle-example
  "Basic example: Create a circle with animation."
  []
  (s/create-scene "CircleExample"
                  (fn [self]
                    (let [circle (-> (m/circle :radius 2)
                                     (m/set-fill @c/BLUE :opacity 0.5)
                                     (m/set-stroke :color @c/BLUE_E :width 4))]
                      (s/play! self (a/create circle))
                      (s/wait! self 2)))))

(defn run-circle-example
  "Run the circle example."
  []
  (core/init!)
  (let [scene ((circle-example))]
    (s/render! scene {:quality "low_quality"})))

(defn square-to-circle-example
  "Transform a square into a circle."
  []
  (s/create-scene "SquareToCircle"
                  (fn [self]
                    (let [square (-> (m/square :side_length 2)
                                     (m/set-fill @c/YELLOW :opacity 0.5)
                                     (m/rotate (/ Math/PI 4)))
                          circle (-> (m/circle :radius 1)
                                     (m/set-fill @c/BLUE :opacity 0.5))]
                      (s/play! self (a/create square))
                      (s/wait! self 1)
                      (s/play! self (a/transform square circle))
                      (s/wait! self 2)))))

(defn run-square-to-circle
  "Run the square to circle example."
  []
  (core/init!)
  (let [scene ((square-to-circle-example))]
    (s/render! scene {:quality "low_quality"})))

;; ============================================================================
;; Text and LaTeX Examples
;; ============================================================================

(defn latex-example
  "Display LaTeX mathematical equations."
  []
  (s/create-scene "LaTeXExample"
                  (fn [self]
                    (let [eq1 (m/math-tex "e^{i\\pi} + 1 = 0")
                          eq2 (m/math-tex "\\int_0^\\infty e^{-x^2} dx = \\frac{\\sqrt{\\pi}}{2}")
                          eq3 (m/math-tex "\\sum_{n=1}^{\\infty} \\frac{1}{n^2} = \\frac{\\pi^2}{6}")]
                      ;; Position equations
                      (m/move-to eq1 [0 2 0])
                      (m/move-to eq2 [0 0 0])
                      (m/move-to eq3 [0 -2 0])

                      ;; Animate
                      (s/play! self (a/write eq1))
                      (s/wait! self 0.5)
                      (s/play! self (a/write eq2))
                      (s/wait! self 0.5)
                      (s/play! self (a/write eq3))
                      (s/wait! self 2)))))

(defn run-latex-example []
  (core/init!)
  (let [scene ((latex-example))]
    (s/render! scene {:quality "low_quality"})))

(defn text-example
  "Display styled text."
  []
  (s/create-scene "TextExample"
                  (fn [self]
                    (let [title (-> (m/text "Welcome to Manim!" :font_size 72)
                                    (m/set-color @c/BLUE)
                                    (m/to-edge @c/UP))
                          subtitle (-> (m/text "Mathematical Animations" :font_size 48)
                                       (m/set-color @c/GREEN))]
                      (s/play! self (a/write title))
                      (s/wait! self 1)
                      (s/play! self (a/fade-in subtitle :shift @c/UP))
                      (s/wait! self 2)))))

;; ============================================================================
;; Graph and Coordinate System Examples
;; ============================================================================

(defn axes-example
  "Plot a function on coordinate axes."
  []
  (s/create-scene "AxesExample"
                  (fn [self]
                    (let [ax (m/axes :x_range [-3 3 1]
                                     :y_range [-2 2 1]
                                     :x_length 6
                                     :y_length 4)
                          ;; Note: For function plotting, we need Python callable
                          ;; This is a simplified version
                          graph-label (-> (m/math-tex "y = \\sin(x)")
                                          (m/to-corner @c/UR))]
                      (s/play! self (a/create ax))
                      (s/wait! self 1)
                      (s/play! self (a/write graph-label))
                      (s/wait! self 2)))))

(defn number-plane-example
  "Show a number plane with transformations."
  []
  (s/create-scene "NumberPlaneExample"
                  (fn [self]
                    (let [plane (m/number-plane :x_range [-5 5 1]
                                                :y_range [-5 5 1]
                                                :x_length 10
                                                :y_length 10)]
                      (s/play! self (a/create plane :run_time 2))
                      (s/wait! self 1)
                      ;; Apply a transformation
                      (s/play! self (a/apply-complex-function
                                     plane
                                     (fn [z] (* z z))))
                      (s/wait! self 2)))))

;; ============================================================================
;; Animation Composition Examples
;; ============================================================================

(defn lagged-animation-example
  "Show multiple objects with staggered animations."
  []
  (s/create-scene "LaggedExample"
                  (fn [self]
                    (let [circles (for [i (range 5)]
                                    (-> (m/circle :radius 0.5)
                                        (m/set-fill (c/color (nth [:red :blue :green :yellow :purple] i))
                                                    :opacity 0.7)
                                        (m/move-to [(- (* i 2) 4) 0 0])))]
                      ;; Add all circles
                      (apply s/add! self circles)
                      ;; Animate with stagger
                      (s/play! self (apply a/lagged-start
                                           (map a/grow-from-center circles)
                                           [:lag_ratio 0.2]))
                      (s/wait! self 1)
                      ;; Fade out with stagger
                      (s/play! self (apply a/lagged-start
                                           (map a/fade-out circles)
                                           [:lag_ratio 0.1]))))))

(defn succession-example
  "Play animations one after another."
  []
  (s/create-scene "SuccessionExample"
                  (fn [self]
                    (let [shapes [(m/circle :radius 1 :color @c/RED)
                                  (m/square :side_length 2 :color @c/BLUE)
                                  (m/triangle :color @c/GREEN)]]
                      ;; Position shapes
                      (m/move-to (nth shapes 0) [-3 0 0])
                      (m/move-to (nth shapes 1) [0 0 0])
                      (m/move-to (nth shapes 2) [3 0 0])

                      ;; Play creation animations in succession
                      (s/play! self (a/succession
                                     (a/create (nth shapes 0))
                                     (a/create (nth shapes 1))
                                     (a/create (nth shapes 2))))
                      (s/wait! self 2)))))

;; ============================================================================
;; 3D Examples
;; ============================================================================

(defn sphere-example
  "3D sphere with camera rotation."
  []
  (s/create-3d-scene "SphereExample"
                     (fn [self]
                       (let [sphere (-> (m/sphere :radius 2)
                                        (m/set-color @c/BLUE))]
                         ;; Set initial camera angle
                         (s/set-camera-orientation! self :phi (/ Math/PI 4) :theta (/ Math/PI 4))

                         (s/play! self (a/create sphere))
                         (s/wait! self 1)

                         ;; Start rotating camera
                         (s/begin-ambient-camera-rotation! self :rate 0.1)
                         (s/wait! self 4)
                         (s/stop-ambient-camera-rotation! self)))))

(defn cube-example
  "3D cube rotating."
  []
  (s/create-3d-scene "CubeExample"
                     (fn [self]
                       (let [cube (-> (m/cube :side_length 2)
                                      (m/set-fill @c/BLUE :opacity 0.7)
                                      (m/set-stroke :color @c/WHITE :width 2))]
                         (s/set-camera-orientation! self :phi (/ Math/PI 3) :theta (/ Math/PI 4))

                         (s/play! self (a/create cube))
                         (s/play! self (a/rotating cube :radians (* 2 Math/PI) :run_time 3))
                         (s/wait! self 1)))))

;; ============================================================================
;; Indication Examples
;; ============================================================================

(defn indication-example
  "Various indication animations."
  []
  (s/create-scene "IndicationExample"
                  (fn [self]
                    (let [formula (m/math-tex "E = mc^2")
                          circle (-> (m/circle :radius 1)
                                     (m/move-to [-3 0 0]))
                          square (-> (m/square :side_length 2)
                                     (m/move-to [3 0 0]))]
                      ;; Add objects
                      (s/add! self formula circle square)
                      (s/wait! self 1)

                      ;; Indicate formula
                      (s/play! self (a/indicate formula :color @c/YELLOW))
                      (s/wait! self 0.5)

                      ;; Circumscribe circle
                      (s/play! self (a/circumscribe circle :color @c/RED))
                      (s/wait! self 0.5)

                      ;; Wiggle square
                      (s/play! self (a/wiggle square))
                      (s/wait! self 1)))))

;; ============================================================================
;; Updater Example
;; ============================================================================

(defn updater-example
  "Use updaters to create dynamic relationships."
  []
  (s/create-scene "UpdaterExample"
                  (fn [self]
                    (let [dot1 (-> (m/dot :color @c/BLUE)
                                   (m/move-to [-2 0 0]))
                          dot2 (m/dot :color @c/RED)
                          line-between (m/line [-2 0 0] [0 0 0])]

                      ;; Add updater to keep dot2 to the right of dot1
                      (m/add-updater dot2
                                     (fn [mob]
                                       (m/move-to mob [(+ 2 (first (m/get-center dot1))) 0 0])))

                      ;; Add updater to keep line connecting the dots
                      (m/add-updater line-between
                                     (fn [mob]
                                       (let [start (m/get-center dot1)
                                             end (m/get-center dot2)]
                                         ;; Update line endpoints
                                         (core/call-method mob "put_start_and_end_on"
                                                           (core/->py-list start)
                                                           (core/->py-list end)))))

                      (s/add! self dot1 dot2 line-between)
                      (s/wait! self 1)

                      ;; Animate dot1 moving, dot2 and line follow
                      (s/play! self (a/update-from-alpha-func
                                     dot1
                                     (fn [mob alpha]
                                       (m/move-to mob [(- (* 4 alpha) 2) (* 2 (Math/sin (* Math/PI alpha))) 0]))
                                     :run_time 3))
                      (s/wait! self 1)))))

;; ============================================================================
;; Complete Demo Scene
;; ============================================================================

(defn full-demo
  "A comprehensive demo showcasing many features."
  []
  (s/create-scene "FullDemo"
                  (fn [self]
                    ;; Title
                    (let [title (-> (m/text "Manim Clojure Bindings" :font_size 60)
                                    (m/set-color @c/GOLD))]
                      (s/play! self (a/write title))
                      (s/wait! self 1)
                      (s/play! self (a/fade-out title :shift @c/UP)))

                    ;; Shapes
                    (let [shapes [(-> (m/circle :radius 1) (m/set-fill @c/RED :opacity 0.5))
                                  (-> (m/square :side_length 2) (m/set-fill @c/BLUE :opacity 0.5))
                                  (-> (m/triangle) (m/set-fill @c/GREEN :opacity 0.5))]]
                      (m/move-to (nth shapes 0) [-3 0 0])
                      (m/move-to (nth shapes 1) [0 0 0])
                      (m/move-to (nth shapes 2) [3 0 0])

                      (s/play! self (apply a/lagged-start
                                           (map a/grow-from-center shapes)
                                           [:lag_ratio 0.3]))
                      (s/wait! self 1)

                      ;; Transform circle to square
                      (s/play! self (a/transform (nth shapes 0) (m/copy (nth shapes 1))))
                      (s/wait! self 1)

                      ;; Fade out all
                      (s/play! self (apply a/animation-group (map a/fade-out shapes))))

                    ;; LaTeX
                    (let [eq (m/math-tex "\\oint_C \\vec{F} \\cdot d\\vec{r} = \\iint_S (\\nabla \\times \\vec{F}) \\cdot d\\vec{S}")]
                      (s/play! self (a/write eq :run_time 2))
                      (s/wait! self 2)
                      (s/play! self (a/fade-out eq)))

                    ;; Finale
                    (let [end-text (-> (m/text "Thank you!" :font_size 72)
                                       (m/set-color @c/YELLOW))]
                      (s/play! self (a/spin-in-from-nothing end-text))
                      (s/wait! self 2)))))

(defn run-full-demo []
  (core/init!)
  (let [scene ((full-demo))]
    (s/render! scene {:quality "medium_quality"})))

;; ============================================================================
;; REPL Helper
;; ============================================================================

(defn list-examples
  "List all available examples."
  []
  (println "Available examples:")
  (println "  (ex/run-circle-example)      - Basic circle animation")
  (println "  (ex/run-square-to-circle)    - Transform square to circle")
  (println "  (ex/run-latex-example)       - LaTeX equations")
  (println "  (ex/run-full-demo)           - Comprehensive demo")
  (println)
  (println "Or create and render manually:")
  (println "  (core/init!)")
  (println "  (let [scene ((ex/sphere-example))]")
  (println "    (s/render! scene {:quality \"low_quality\"}))"))

(comment
  ;; REPL usage:
  (require '[desargues.manim.examples :as ex])
  (ex/list-examples)

  ;; Run an example:
  (ex/run-circle-example)

  ;; Or manually:
  (require '[desargues.manim.core :as core])
  (require '[desargues.manim.scenes :as s])

  (core/init!)
  (let [scene ((ex/sphere-example))]
    (s/render! scene {:quality "low_quality"})))
