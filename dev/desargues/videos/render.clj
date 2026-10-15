(ns desargues.videos.render
  "Brachistochrone video rendering entry point (needs the Manim env).

   Usage:
     clojure -M -m desargues.videos.render              ; Full brachistochrone
     clojure -M -m desargues.videos.render intro        ; Just intro
     clojure -M -m desargues.videos.render step 7       ; Specific step
     clojure -M -m desargues.videos.render --low        ; Low quality, fast"
  (:require [libpython-clj2.python :as py]
            [libpython-clj2.python.class :as py-class]
            [desargues.manim.core :as manim]
            [emmy.env :as e])
  (:gen-class))

;; =============================================================================
;; Physics: Curve Definitions
;; =============================================================================

(defn straight-line
  "Straight line path from (x0, y0) to (x1, y1)."
  [x0 y0 x1 y1]
  (fn [t]
    [(+ x0 (* t (- x1 x0)))
     (+ y0 (* t (- y1 y0)))]))

(defn parabolic-path
  "Parabolic path with extra dip in the middle."
  [x0 y0 x1 y1 depth]
  (fn [t]
    [(+ x0 (* t (- x1 x0)))
     (+ y0
        (* t (- y1 y0))
        (* (- depth) (* t (- 1 t)) (Math/abs (- y1 y0))))]))

(defn cycloid-path
  "Cycloid path: the brachistochrone solution."
  [x0 y0 x1 y1]
  (let [theta-max 2.5
        r (/ (- x1 x0) (- theta-max (Math/sin theta-max)))]
    (fn [theta]
      (let [x-raw (+ x0 (* r (- theta (Math/sin theta))))
            y-raw (- y0 (* r (- 1 (Math/cos theta))))
            y-end (- y0 (* r (- 1 (Math/cos theta-max))))
            y-scale (/ (- y1 y0) (- y-end y0))
            y (+ y0 (* (- y-raw y0) y-scale))]
        [x-raw y]))))

;; =============================================================================
;; Physics Calculations
;; =============================================================================

(defn descent-time
  "Calculate descent time along a curve using energy conservation."
  [curve-fn t-start t-end n-points g]
  (let [dt (/ (- t-end t-start) n-points)
        points (mapv #(curve-fn (+ t-start (* % dt))) (range (inc n-points)))]
    (reduce +
            (map (fn [[x1 y1] [x2 y2]]
                   (let [dx (- x2 x1)
                         dy (- y2 y1)
                         ds (Math/sqrt (+ (* dx dx) (* dy dy)))
                         y-avg (/ (+ y1 y2) 2.0)
                         v (Math/sqrt (* 2 g (Math/abs y-avg)))]
                     (if (pos? v) (/ ds v) 0.0)))
                 points
                 (rest points)))))

(defn compare-paths [x0 y0 x1 y1]
  (let [g 9.8, n-points 100
        straight (straight-line x0 y0 x1 y1)
        parabola (parabolic-path x0 y0 x1 y1 2.0)
        cycloid (cycloid-path x0 y0 x1 y1)]
    {:straight {:time (descent-time straight 0.0 1.0 n-points g)}
     :parabola {:time (descent-time parabola 0.0 1.0 n-points g)}
     :cycloid {:time (descent-time cycloid 0.0 2.5 n-points g)}}))

;; =============================================================================
;; Pure Clojure Manim Helpers
;; =============================================================================

(defn ->np-array
  "Convert Clojure vector to numpy array."
  [[x y z]]
  (let [np (py/import-module "numpy")]
    (py/call-attr np "array" [x y z])))

(defn get-color
  "Get Manim color constant from keyword."
  [color-kw]
  (let [m (manim/manim)
        color-name (case color-kw
                     :blue "BLUE"
                     :red "RED"
                     :green "GREEN"
                     :yellow "YELLOW"
                     :orange "ORANGE"
                     :purple "PURPLE"
                     :pink "PINK"
                     :teal "TEAL"
                     :cyan "BLUE_C"
                     :white "WHITE"
                     :grey "GREY"
                     :gray "GREY"
                     "WHITE")]
    (py/get-attr m color-name)))

(defn create-text
  "Create Manim Text object."
  [text & {:keys [font-size color]
           :or {font-size 36}}]
  (let [m (manim/manim)
        Text (py/get-attr m "Text")
        obj (py/call-attr-kw Text "__call__" [text]
                             {"font_size" font-size})]
    (when color
      (py/call-attr obj "set_color" (get-color color)))
    obj))

(defn create-tex
  "Create Manim MathTex object."
  [latex & {:keys [font-size color]}]
  (let [m (manim/manim)
        MathTex (py/get-attr m "MathTex")
        obj (if font-size
              (py/call-attr-kw MathTex "__call__" [latex]
                               {"font_size" font-size})
              (MathTex latex))]
    (when color
      (py/call-attr obj "set_color" (get-color color)))
    obj))

(defn create-dot
  "Create Manim Dot."
  [point & {:keys [color radius]
            :or {radius 0.08}}]
  (let [m (manim/manim)
        Dot (py/get-attr m "Dot")]
    (py/call-attr-kw Dot "__call__" []
                     {"point" (->np-array point)
                      "radius" radius
                      "color" (when color (get-color color))})))

(defn create-line
  "Create Manim Line."
  [start end & {:keys [color stroke-width]
                :or {stroke-width 3}}]
  (let [m (manim/manim)
        Line (py/get-attr m "Line")]
    (py/call-attr-kw Line "__call__" []
                     {"start" (->np-array start)
                      "end" (->np-array end)
                      "stroke_width" stroke-width
                      "color" (when color (get-color color))})))

(defn create-parametric-curve
  "Create Manim ParametricFunction."
  [func t-range & {:keys [color stroke-width]
                   :or {stroke-width 3}}]
  (let [m (manim/manim)
        ParametricFunction (py/get-attr m "ParametricFunction")
        wrapped-fn (fn [t]
                     (let [[x y] (func t)]
                       (->np-array [x y 0])))]
    (py/call-attr-kw ParametricFunction "__call__" [wrapped-fn]
                     {"t_range" (py/->py-list t-range)
                      "stroke_width" stroke-width
                      "color" (when color (get-color color))})))

(defn create-rectangle
  "Create Manim SurroundingRectangle."
  [mobject & {:keys [color buff stroke-width]
              :or {buff 0.2 stroke-width 2}}]
  (let [m (manim/manim)
        SurroundingRectangle (py/get-attr m "SurroundingRectangle")]
    (py/call-attr-kw SurroundingRectangle "__call__" [mobject]
                     {"buff" buff
                      "stroke_width" stroke-width
                      "color" (when color (get-color color))})))

(defn create-vgroup
  "Create Manim VGroup."
  [& mobjects]
  (let [m (manim/manim)
        VGroup (py/get-attr m "VGroup")
        group (VGroup)]
    (doseq [mob mobjects]
      (py/call-attr group "add" mob))
    group))

(defn shift
  "Shift mobject by offset."
  [mobject offset]
  (py/call-attr mobject "shift" (->np-array offset))
  mobject)

(defn next-to
  "Position mobject next to another."
  [mobject target direction & {:keys [buff] :or {buff 0.25}}]
  (let [m (manim/manim)
        dir-vec (case direction
                  :up (py/get-attr m "UP")
                  :down (py/get-attr m "DOWN")
                  :left (py/get-attr m "LEFT")
                  :right (py/get-attr m "RIGHT")
                  (py/get-attr m "DOWN"))]
    (py/call-attr-kw mobject "next_to" [target dir-vec] {"buff" buff})
    mobject))

(defn to-edge
  "Move mobject to screen edge."
  [mobject edge & {:keys [buff] :or {buff 0.5}}]
  (let [m (manim/manim)
        edge-vec (case edge
                   :up (py/get-attr m "UP")
                   :down (py/get-attr m "DOWN")
                   :left (py/get-attr m "LEFT")
                   :right (py/get-attr m "RIGHT")
                   (py/get-attr m "UP"))]
    (py/call-attr-kw mobject "to_edge" [edge-vec] {"buff" buff})
    mobject))

(defn to-corner
  "Move mobject to screen corner."
  [mobject corner & {:keys [buff] :or {buff 0.5}}]
  (let [m (manim/manim)
        corner-vec (case corner
                     :ul (py/call-attr (py/get-attr m "UP") "__add__" (py/get-attr m "LEFT"))
                     :ur (py/call-attr (py/get-attr m "UP") "__add__" (py/get-attr m "RIGHT"))
                     :dl (py/call-attr (py/get-attr m "DOWN") "__add__" (py/get-attr m "LEFT"))
                     :dr (py/call-attr (py/get-attr m "DOWN") "__add__" (py/get-attr m "RIGHT"))
                     (py/get-attr m "UP"))]
    (py/call-attr-kw mobject "to_corner" [corner-vec] {"buff" buff})
    mobject))

(defn arrange
  "Arrange mobjects in VGroup."
  [vgroup direction & {:keys [buff] :or {buff 0.3}}]
  (let [m (manim/manim)
        dir-vec (case direction
                  :down (py/get-attr m "DOWN")
                  :up (py/get-attr m "UP")
                  :right (py/get-attr m "RIGHT")
                  :left (py/get-attr m "LEFT")
                  (py/get-attr m "DOWN"))]
    (py/call-attr-kw vgroup "arrange" [dir-vec] {"buff" buff})
    vgroup))

;; Animation helpers
(defn anim-write [mobject]
  (let [m (manim/manim)
        Write (py/get-attr m "Write")]
    (Write mobject)))

(defn anim-create [mobject]
  (let [m (manim/manim)
        Create (py/get-attr m "Create")]
    (Create mobject)))

(defn anim-fade-in [mobject]
  (let [m (manim/manim)
        FadeIn (py/get-attr m "FadeIn")]
    (FadeIn mobject)))

(defn anim-fade-out [mobject]
  (let [m (manim/manim)
        FadeOut (py/get-attr m "FadeOut")]
    (FadeOut mobject)))

(defn play! [scene & anims]
  (apply py/call-attr scene "play" anims))

(defn wait! [scene duration]
  (py/call-attr scene "wait" duration))

;; =============================================================================
;; LaTeX Equations
;; =============================================================================

(def eq-problem "\\text{Minimize } T = \\int_{A}^{B} dt")
(def eq-dt "dt = \\frac{ds}{v}")
(def eq-dt-explanation "\\text{(time = distance / velocity)}")
(def eq-energy-start "\\frac{1}{2}mv^2 = mgy")
(def eq-energy-cancel "\\frac{1}{2}v^2 = gy")
(def eq-velocity "v = \\sqrt{2gy}")
(def eq-arc-diff "ds^2 = dx^2 + dy^2")
(def eq-arc-final "ds = \\sqrt{1 + y'^2} \\, dx")
(def eq-functional-clean "T = \\int_{x_0}^{x_1} \\frac{\\sqrt{1 + y'^2}}{\\sqrt{2gy}} \\, dx")
(def eq-lagrangian-form "T = \\int L(y, y') \\, dx")
(def eq-lagrangian-def "L(y, y') = \\frac{\\sqrt{1 + y'^2}}{\\sqrt{2gy}}")
(def eq-euler-lagrange "\\frac{\\partial L}{\\partial y} - \\frac{d}{dx}\\frac{\\partial L}{\\partial y'} = 0")
(def eq-beltrami-condition "\\frac{\\partial L}{\\partial x} = 0")
(def eq-beltrami-identity "L - y' \\frac{\\partial L}{\\partial y'} = C")
(def eq-beltrami-name "\\text{(Beltrami Identity)}")
(def eq-partial-L-result "\\frac{\\partial L}{\\partial y'} = \\frac{y'}{\\sqrt{y(1+y'^2)}}")
(def eq-beltrami-simplify3 "\\frac{1}{\\sqrt{y(1+y'^2)}} = C")
(def eq-solution-step1 "\\sqrt{y(1+y'^2)} = \\frac{1}{C}")
(def eq-solution-final "y(1+y'^2) = 2A")
(def eq-constant-def "\\text{where } 2A = \\frac{1}{C^2}")
(def eq-ode-sqrt "y' = \\frac{dy}{dx} = \\sqrt{\\frac{2A - y}{y}}")
(def eq-ode-separate "\\sqrt{\\frac{y}{2A-y}} \\, dy = dx")
(def eq-sub-y "\\text{Let } y = A(1 - \\cos\\phi)")
(def eq-trig-identity "\\text{Using } \\frac{1-\\cos\\phi}{1+\\cos\\phi} = \\tan^2\\frac{\\phi}{2}")
(def eq-integral-final "A(1 - \\cos\\phi) \\, d\\phi = dx")
(def eq-integrate-x "x = A(\\phi - \\sin\\phi) + x_0")
(def eq-cycloid-x "x = A(\\phi - \\sin\\phi)")
(def eq-cycloid-y "y = A(1 - \\cos\\phi)")
(def eq-cycloid-name "\\textbf{The Cycloid!}")

;; =============================================================================
;; Render Functions
;; =============================================================================

(defn render-full-derivation
  "Render the complete brachistochrone derivation video."
  []
  (manim/init!)
  (println "\n=== Brachistochrone Full Derivation ===")
  (println "Rendering complete video with all 12 derivation steps...")

  (let [construct-fn
        (fn [self]
          ;; === INTRO ===
          (let [title (-> (create-text "The Brachistochrone Problem" :font-size 48)
                          (to-edge :up))]
            (play! self (anim-write title))
            (wait! self 1))

          (let [subtitle (-> (create-tex "\\beta\\rho\\alpha\\chi\\iota\\sigma\\tau o\\varsigma \\; \\chi\\rho o\\nu o\\varsigma = \\text{shortest time}" :font-size 32)
                             (shift [0 2 0]))]
            (play! self (anim-fade-in subtitle))
            (wait! self 2)
            (play! self (anim-fade-out subtitle)))

          ;; Show points A and B
          (let [pt-a (create-dot [-4 2 0] :color :green :radius 0.12)
                pt-b (create-dot [4 -2 0] :color :red :radius 0.12)
                label-a (-> (create-tex "A" :color :green) (next-to pt-a :up))
                label-b (-> (create-tex "B" :color :red) (next-to pt-b :down))]
            (play! self (anim-create pt-a) (anim-create pt-b))
            (play! self (anim-write label-a) (anim-write label-b))
            (wait! self 1)

            ;; Draw curves
            (let [straight (create-line [-4 2 0] [4 -2 0] :color :blue)
                  parabola (create-parametric-curve
                            (fn [t] [(+ -4 (* t 8)) (+ 2 (* t -4) (* -2 t (- 1 t)))])
                            [0 1] :color :yellow)
                  cycloid (create-parametric-curve
                           (fn [theta]
                             (let [r 1.6
                                   x (+ -4 (* r (- theta (Math/sin theta))))
                                   y (- 2 (* r (- 1 (Math/cos theta))))]
                               [x y]))
                           [0 2.5] :color :green)]
              (play! self (anim-create straight))
              (wait! self 0.5)
              (play! self (anim-create parabola))
              (wait! self 0.5)
              (play! self (anim-create cycloid))
              (wait! self 1)

              ;; Clear for derivation
              (play! self (anim-fade-out straight) (anim-fade-out parabola)
                     (anim-fade-out cycloid) (anim-fade-out pt-a) (anim-fade-out pt-b)
                     (anim-fade-out label-a) (anim-fade-out label-b))))

          ;; === STEP 1: Problem Setup ===
          (let [step-title (-> (create-text "Step 1: The Minimization Problem" :font-size 36 :color :yellow)
                               (to-edge :up))
                eq1 (-> (create-tex eq-problem :font-size 42) (shift [0 1 0]))]
            (play! self (anim-write step-title))
            (play! self (anim-write eq1))
            (wait! self 2)
            (play! self (anim-fade-out step-title) (anim-fade-out eq1)))

          ;; === STEP 2: Time Element ===
          (let [step-title (-> (create-text "Step 2: Time Element" :font-size 36 :color :yellow) (to-edge :up))
                eq1 (-> (create-tex eq-dt :font-size 42) (shift [0 1 0]))
                eq2 (-> (create-tex eq-dt-explanation :font-size 28) (shift [0 0 0]))]
            (play! self (anim-write step-title))
            (play! self (anim-write eq1))
            (wait! self 1)
            (play! self (anim-fade-in eq2))
            (wait! self 2)
            (play! self (anim-fade-out step-title) (anim-fade-out eq1) (anim-fade-out eq2)))

          ;; === STEP 3: Energy Conservation ===
          (let [step-title (-> (create-text "Step 3: Energy Conservation" :font-size 36 :color :yellow) (to-edge :up))
                eq1 (-> (create-tex eq-energy-start :font-size 36) (shift [0 1.5 0]))
                eq2 (-> (create-tex eq-energy-cancel :font-size 36) (shift [0 0.5 0]))
                eq3 (-> (create-tex eq-velocity :font-size 48) (shift [0 -0.8 0]))
                box (create-rectangle eq3 :color :teal :buff 0.2)]
            (play! self (anim-write step-title))
            (play! self (anim-write eq1))
            (wait! self 1)
            (play! self (anim-write eq2))
            (wait! self 1)
            (play! self (anim-write eq3))
            (wait! self 1)
            (play! self (anim-create box))
            (wait! self 2)
            (play! self (anim-fade-out step-title) (anim-fade-out eq1) (anim-fade-out eq2) (anim-fade-out eq3) (anim-fade-out box)))

          ;; === STEP 4: Arc Length ===
          (let [step-title (-> (create-text "Step 4: Arc Length Element" :font-size 36 :color :yellow) (to-edge :up))
                eq1 (-> (create-tex eq-arc-diff :font-size 36) (shift [0 1.2 0]))
                eq2 (-> (create-tex eq-arc-final :font-size 48) (shift [0 -0.2 0]))
                box (create-rectangle eq2 :color :teal :buff 0.2)]
            (play! self (anim-write step-title))
            (play! self (anim-write eq1))
            (wait! self 1.5)
            (play! self (anim-write eq2))
            (wait! self 1)
            (play! self (anim-create box))
            (wait! self 2)
            (play! self (anim-fade-out step-title) (anim-fade-out eq1) (anim-fade-out eq2) (anim-fade-out box)))

          ;; === STEP 5: The Functional ===
          (let [step-title (-> (create-text "Step 5: The Functional" :font-size 36 :color :yellow) (to-edge :up))
                recall1 (-> (create-tex "v = \\sqrt{2gy}" :font-size 28) (shift [-4 1.5 0]))
                recall2 (-> (create-tex "ds = \\sqrt{1+y'^2}\\,dx" :font-size 28) (shift [4 1.5 0]))
                eq1 (-> (create-tex eq-functional-clean :font-size 42) (shift [0 -0.5 0]))
                box (create-rectangle eq1 :color :teal :buff 0.2)]
            (play! self (anim-write step-title))
            (play! self (anim-fade-in recall1) (anim-fade-in recall2))
            (wait! self 1)
            (play! self (anim-write eq1))
            (wait! self 1)
            (play! self (anim-create box))
            (wait! self 2)
            (play! self (anim-fade-out step-title) (anim-fade-out recall1) (anim-fade-out recall2) (anim-fade-out eq1) (anim-fade-out box)))

          ;; === STEP 6: Identify Lagrangian ===
          (let [step-title (-> (create-text "Step 6: The Lagrangian" :font-size 36 :color :yellow) (to-edge :up))
                eq1 (-> (create-tex eq-lagrangian-form :font-size 36) (shift [0 1 0]))
                eq2 (-> (create-tex eq-lagrangian-def :font-size 42) (shift [0 -0.2 0]))
                note (-> (create-text "Notice: L depends on y and y', but NOT on x" :font-size 26 :color :teal) (shift [0 -1.5 0]))
                box (create-rectangle eq2 :color :teal :buff 0.2)]
            (play! self (anim-write step-title))
            (play! self (anim-write eq1))
            (wait! self 1)
            (play! self (anim-write eq2))
            (wait! self 1)
            (play! self (anim-create box))
            (wait! self 1)
            (play! self (anim-write note))
            (wait! self 2)
            (play! self (anim-fade-out step-title) (anim-fade-out eq1) (anim-fade-out eq2) (anim-fade-out note) (anim-fade-out box)))

          ;; === STEP 7: Beltrami Identity (KEY!) ===
          (let [step-title (-> (create-text "Step 7: Beltrami Identity" :font-size 36 :color :yellow) (to-edge :up))
                eq-el (-> (create-tex eq-euler-lagrange :font-size 32) (shift [0 1.5 0]))
                condition (-> (create-tex eq-beltrami-condition :font-size 32) (shift [0 0.5 0]))
                eq-beltrami (-> (create-tex eq-beltrami-identity :font-size 48) (shift [0 -0.8 0]))
                beltrami-label (-> (create-tex eq-beltrami-name :font-size 28 :color :teal) (shift [0 -1.8 0]))
                box (create-rectangle eq-beltrami :color :teal :buff 0.25)]
            (play! self (anim-write step-title))
            (play! self (anim-write eq-el))
            (wait! self 1.5)
            (play! self (anim-write condition))
            (wait! self 1.5)
            (play! self (anim-write eq-beltrami))
            (wait! self 1)
            (play! self (anim-create box) (anim-write beltrami-label))
            (wait! self 3)
            (play! self (anim-fade-out step-title) (anim-fade-out eq-el) (anim-fade-out condition)
                   (anim-fade-out eq-beltrami) (anim-fade-out beltrami-label) (anim-fade-out box)))

          ;; === STEP 8: Apply Beltrami ===
          (let [step-title (-> (create-text "Step 8: Apply Beltrami Identity" :font-size 36 :color :yellow) (to-edge :up))
                recall-L (-> (create-tex "L = \\sqrt{\\frac{1+y'^2}{y}}" :font-size 28) (to-corner :ul :buff 0.5))
                eq1 (-> (create-tex eq-partial-L-result :font-size 32) (shift [0 1 0]))
                eq2 (-> (create-tex eq-beltrami-simplify3 :font-size 36) (shift [0 -0.5 0]))]
            (play! self (anim-write step-title))
            (play! self (anim-fade-in recall-L))
            (wait! self 1)
            (play! self (anim-write eq1))
            (wait! self 2)
            (play! self (anim-write eq2))
            (wait! self 2)
            (play! self (anim-fade-out step-title) (anim-fade-out recall-L) (anim-fade-out eq1) (anim-fade-out eq2)))

          ;; === STEP 9: Key Equation ===
          (let [step-title (-> (create-text "Step 9: The Key Equation" :font-size 36 :color :yellow) (to-edge :up))
                eq1 (-> (create-tex eq-solution-step1 :font-size 36) (shift [0 1 0]))
                eq2 (-> (create-tex eq-solution-final :font-size 48) (shift [0 -0.5 0]))
                eq3 (-> (create-tex eq-constant-def :font-size 28) (shift [0 -1.5 0]))
                box (create-rectangle eq2 :color :teal :buff 0.25)]
            (play! self (anim-write step-title))
            (play! self (anim-write eq1))
            (wait! self 1.5)
            (play! self (anim-write eq2))
            (wait! self 1)
            (play! self (anim-create box) (anim-fade-in eq3))
            (wait! self 2)
            (play! self (anim-fade-out step-title) (anim-fade-out eq1) (anim-fade-out eq2) (anim-fade-out eq3) (anim-fade-out box)))

          ;; === STEP 10: Solve the ODE ===
          (let [step-title (-> (create-text "Step 10: Solve the Differential Equation" :font-size 36 :color :yellow) (to-edge :up))
                eq1 (-> (create-tex eq-ode-sqrt :font-size 32) (shift [0 1.2 0]))
                eq2 (-> (create-tex eq-ode-separate :font-size 32) (shift [0 0.3 0]))
                sub-label (-> (create-text "Substitution:" :font-size 28) (shift [-4 -0.6 0]))
                eq3 (-> (create-tex eq-sub-y :font-size 32) (shift [0 -0.6 0]))]
            (play! self (anim-write step-title))
            (play! self (anim-write eq1))
            (wait! self 1.5)
            (play! self (anim-write eq2))
            (wait! self 1.5)
            (play! self (anim-fade-in sub-label))
            (play! self (anim-write eq3))
            (wait! self 2)
            (play! self (anim-fade-out step-title) (anim-fade-out eq1) (anim-fade-out eq2) (anim-fade-out sub-label) (anim-fade-out eq3)))

          ;; === STEP 11: Integration ===
          (let [step-title (-> (create-text "Step 11: Integration" :font-size 36 :color :yellow) (to-edge :up))
                eq1 (-> (create-tex eq-trig-identity :font-size 28) (shift [0 1.2 0]))
                eq2 (-> (create-tex eq-integral-final :font-size 36) (shift [0 0.2 0]))
                eq3 (-> (create-tex eq-integrate-x :font-size 42) (shift [0 -1 0]))]
            (play! self (anim-write step-title))
            (play! self (anim-write eq1))
            (wait! self 1.5)
            (play! self (anim-write eq2))
            (wait! self 1.5)
            (play! self (anim-write eq3))
            (wait! self 2)
            (play! self (anim-fade-out step-title) (anim-fade-out eq1) (anim-fade-out eq2) (anim-fade-out eq3)))

          ;; === STEP 12: THE CYCLOID! ===
          (let [step-title (-> (create-text "Step 12: The Solution" :font-size 36 :color :yellow) (to-edge :up))
                result-label (-> (create-text "The curve that minimizes descent time:" :font-size 28) (shift [0 1.5 0]))
                eq-x (-> (create-tex eq-cycloid-x :font-size 48) (shift [0 0.5 0]))
                eq-y (-> (create-tex eq-cycloid-y :font-size 48) (shift [0 -0.5 0]))
                result-name (-> (create-tex eq-cycloid-name :font-size 56 :color :green) (shift [0 -1.8 0]))
                box (create-rectangle (create-vgroup eq-x eq-y) :color :green :buff 0.3)]
            (play! self (anim-write step-title))
            (play! self (anim-fade-in result-label))
            (wait! self 1)
            (play! self (anim-write eq-x))
            (wait! self 1)
            (play! self (anim-write eq-y))
            (wait! self 1)
            (play! self (anim-create box))
            (wait! self 1)
            (play! self (anim-write result-name))
            (wait! self 3)
            (play! self (anim-fade-out step-title) (anim-fade-out result-label) (anim-fade-out eq-x)
                   (anim-fade-out eq-y) (anim-fade-out result-name) (anim-fade-out box)))

          ;; === RACE ANIMATION ===
          (let [race-title (-> (create-text "The Race!" :font-size 40) (to-edge :up))
                pt-a (create-dot [-4 2 0] :color :green :radius 0.12)
                pt-b (create-dot [4 -2 0] :color :red :radius 0.12)
                straight (create-line [-4 2 0] [4 -2 0] :color :blue)
                cycloid (create-parametric-curve
                         (fn [theta]
                           (let [r 1.6
                                 x (+ -4 (* r (- theta (Math/sin theta))))
                                 y (- 2 (* r (- 1 (Math/cos theta))))]
                             [x y]))
                         [0 2.5] :color :green)
                physics (compare-paths -4 2 4 -2)
                time-straight (get-in physics [:straight :time])
                time-cycloid (get-in physics [:cycloid :time])
                results (-> (create-vgroup
                             (create-text (format "Straight: %.3fs" time-straight) :font-size 28 :color :blue)
                             (create-text (format "Cycloid: %.3fs (FASTEST!)" time-cycloid) :font-size 28 :color :green))
                            (arrange :down :buff 0.3)
                            (to-corner :ur :buff 0.5))]
            (play! self (anim-write race-title))
            (play! self (anim-create pt-a) (anim-create pt-b))
            (play! self (anim-create straight) (anim-create cycloid))
            (wait! self 2)
            (play! self (anim-write results))
            (wait! self 3)
            (play! self (anim-fade-out race-title) (anim-fade-out pt-a) (anim-fade-out pt-b)
                   (anim-fade-out straight) (anim-fade-out cycloid) (anim-fade-out results)))

          ;; === FINAL MESSAGE ===
          (let [final-msg (-> (create-text "The Cycloid is the Brachistochrone!" :font-size 42 :color :green) (shift [0 0 0]))]
            (play! self (anim-write final-msg))
            (wait! self 3)))

        ;; Create scene class with properly wrapped construct function
        m (manim/manim)
        Scene (py/get-attr m "Scene")
        wrapped-construct (py-class/make-tuple-instance-fn construct-fn)
        scene-class (py/create-class "BrachistochroneDerivation" [Scene]
                                     {"construct" wrapped-construct})
        scene (scene-class)]

    (println "Rendering video...")
    (py/call-attr scene "render")
    (println "\n=== Done! Check media/videos/ for the output ===")))

(defn render-intro
  "Render just the introduction scene."
  []
  (manim/init!)
  (println "\n=== Brachistochrone Intro ===")

  (let [construct-fn
        (fn [self]
          (let [title (-> (create-text "The Brachistochrone Problem" :font-size 48) (to-edge :up))]
            (play! self (anim-write title))
            (wait! self 1))

          (let [subtitle (-> (create-tex "\\beta\\rho\\alpha\\chi\\iota\\sigma\\tau o\\varsigma \\; \\chi\\rho o\\nu o\\varsigma = \\text{shortest time}" :font-size 32) (shift [0 2 0]))]
            (play! self (anim-fade-in subtitle))
            (wait! self 2)
            (play! self (anim-fade-out subtitle)))

          (let [pt-a (create-dot [-4 2 0] :color :green :radius 0.12)
                pt-b (create-dot [4 -2 0] :color :red :radius 0.12)
                label-a (-> (create-tex "A" :color :green) (next-to pt-a :up))
                label-b (-> (create-tex "B" :color :red) (next-to pt-b :down))]
            (play! self (anim-create pt-a) (anim-create pt-b))
            (play! self (anim-write label-a) (anim-write label-b))
            (wait! self 1)

            (let [straight (create-line [-4 2 0] [4 -2 0] :color :blue)
                  parabola (create-parametric-curve (fn [t] [(+ -4 (* t 8)) (+ 2 (* t -4) (* -2 t (- 1 t)))]) [0 1] :color :yellow)
                  cycloid (create-parametric-curve
                           (fn [theta] (let [r 1.6] [(+ -4 (* r (- theta (Math/sin theta)))) (- 2 (* r (- 1 (Math/cos theta))))]))
                           [0 2.5] :color :green)]
              (play! self (anim-create straight))
              (wait! self 0.5)
              (play! self (anim-create parabola))
              (wait! self 0.5)
              (play! self (anim-create cycloid))
              (wait! self 2))))

        m (manim/manim)
        Scene (py/get-attr m "Scene")
        wrapped-construct (py-class/make-tuple-instance-fn construct-fn)
        scene-class (py/create-class "BrachistochroneIntro" [Scene]
                                     {"construct" wrapped-construct})
        scene (scene-class)]

    (py/call-attr scene "render")
    (println "Done!")))

(defn render-step
  "Render a specific derivation step (1-12)."
  [n]
  (manim/init!)
  (println (str "\n=== Rendering Step " n " ==="))

  (let [step-configs
        {1 {:title "Step 1: The Minimization Problem" :equations [[eq-problem 42 [0 1 0]]]}
         2 {:title "Step 2: Time Element" :equations [[eq-dt 42 [0 1 0]] [eq-dt-explanation 28 [0 0 0]]]}
         3 {:title "Step 3: Energy Conservation" :equations [[eq-energy-start 36 [0 1.5 0]] [eq-energy-cancel 36 [0 0.5 0]] [eq-velocity 48 [0 -0.8 0]]] :highlight-last true}
         4 {:title "Step 4: Arc Length Element" :equations [[eq-arc-diff 36 [0 1.2 0]] [eq-arc-final 48 [0 -0.2 0]]] :highlight-last true}
         5 {:title "Step 5: The Functional" :equations [[eq-functional-clean 42 [0 0 0]]] :highlight-last true}
         6 {:title "Step 6: The Lagrangian" :equations [[eq-lagrangian-form 36 [0 1 0]] [eq-lagrangian-def 42 [0 -0.2 0]]] :highlight-last true}
         7 {:title "Step 7: Beltrami Identity" :equations [[eq-euler-lagrange 32 [0 1.5 0]] [eq-beltrami-condition 32 [0 0.5 0]] [eq-beltrami-identity 48 [0 -0.8 0]]] :highlight-last true}
         8 {:title "Step 8: Apply Beltrami Identity" :equations [[eq-partial-L-result 32 [0 1 0]] [eq-beltrami-simplify3 36 [0 -0.5 0]]]}
         9 {:title "Step 9: The Key Equation" :equations [[eq-solution-step1 36 [0 1 0]] [eq-solution-final 48 [0 -0.5 0]]] :highlight-last true}
         10 {:title "Step 10: Solve the ODE" :equations [[eq-ode-sqrt 32 [0 1.2 0]] [eq-ode-separate 32 [0 0.3 0]] [eq-sub-y 32 [0 -0.6 0]]]}
         11 {:title "Step 11: Integration" :equations [[eq-trig-identity 28 [0 1.2 0]] [eq-integral-final 36 [0 0.2 0]] [eq-integrate-x 42 [0 -1 0]]]}
         12 {:title "Step 12: The Solution - THE CYCLOID!" :equations [[eq-cycloid-x 48 [0 0.5 0]] [eq-cycloid-y 48 [0 -0.5 0]] [eq-cycloid-name 56 [0 -1.8 0]]] :highlight-last true :final-color :green}}
        config (get step-configs n)]

    (when config
      (let [construct-fn
            (fn [self]
              (let [title (-> (create-text (:title config) :font-size 36 :color :yellow) (to-edge :up))]
                (play! self (anim-write title))
                (wait! self 0.5))

              (let [eq-mobjects
                    (doall
                     (for [[latex font-size pos] (:equations config)]
                       (let [color (when (and (:final-color config) (= latex (first (last (:equations config))))) (:final-color config))
                             eq (-> (create-tex latex :font-size font-size :color color) (shift pos))]
                         (play! self (anim-write eq))
                         (wait! self 1.5)
                         eq)))]
                (when (:highlight-last config)
                  (let [last-eq (last eq-mobjects)
                        box (create-rectangle last-eq :color :teal :buff 0.2)]
                    (play! self (anim-create box))))
                (wait! self 2)))

            m (manim/manim)
            Scene (py/get-attr m "Scene")
            wrapped-construct (py-class/make-tuple-instance-fn construct-fn)
            scene-class (py/create-class (str "Step" n) [Scene]
                                         {"construct" wrapped-construct})
            scene (scene-class)]
        (py/call-attr scene "render")
        (println "Done!")))))

;; =============================================================================
;; Main Entry Point
;; =============================================================================

(defn -main
  "Render brachistochrone videos.

   Usage:
     clojure -M -m desargues.videos.render                  ; Full derivation (high quality)
     clojure -M -m desargues.videos.render intro            ; Just intro
     clojure -M -m desargues.videos.render step 7           ; Specific step (1-12)
     clojure -M -m desargues.videos.render --low            ; Full derivation (low quality, fast)
     clojure -M -m desargues.videos.render intro --low      ; Intro (low quality)
     clojure -M -m desargues.videos.render all-steps        ; All steps as separate files (parallelizable)"
  [& args]
  (println "=== Brachistochrone Video Renderer ===\n")

  ;; Parse quality flag
  (let [low-quality? (some #{"--low" "-l"} args)
        args (remove #{"--low" "-l"} args)
        cmd (first args)]

    ;; Set quality before any rendering
    (manim/init!)
    (when low-quality?
      (println "Using LOW QUALITY for fast rendering...")
      (let [config (py/get-attr (manim/manim) "config")]
        (py/set-attr! config "quality" "low_quality")))

    (case cmd
      nil (render-full-derivation)
      "intro" (render-intro)
      "step" (let [n (Integer/parseInt (second args))]
               (render-step n))
      "all-steps" (do
                    (println "Rendering all 12 steps as separate files...")
                    (println "TIP: Run multiple instances in parallel for speed!")
                    (doseq [n (range 1 13)]
                      (println (str "\n--- Step " n " of 12 ---"))
                      (render-step n)))
      (println (str "Unknown command: " cmd "\nUsage: clojure -M -m desargues.videos.render [intro|step N|all-steps] [--low]"))))

  (println "\n=== Done! ==="))
