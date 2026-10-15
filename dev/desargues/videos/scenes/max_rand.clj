(ns desargues.videos.scenes.max-rand
  "Recreation of max_rand.py scenes from _2024/puzzles.
   Demonstrates random value visualization with TrackingDots.

   Key concepts demonstrated:
   - Custom animation classes (Randomize, TrackingDots)
   - ValueTracker reactive patterns
   - Multiple interval displays with updaters
   - TransformMatchingStrings for probability equations
   - CDF visualization on unit square
   - 3D extension for max of three"
  (:require [desargues.videos.timeline :as tl]
            [desargues.videos.math-objects :as mo]
            [desargues.videos.typography :as typo]
            [desargues.videos.characters :as char]))

;; =============================================================================
;; Custom Animation Classes
;; =============================================================================

(defprotocol IRandomize
  "Protocol for randomize animation that cycles through random values."
  (interpolate [this alpha])
  (new-step? [this alpha]))

(deftype RandomizeAnimation
         [value-tracker frequency rand-func final-value
          ^:unsynchronized-mutable last-alpha ^:unsynchronized-mutable running-tally]
  IRandomize
  (new-step? [_this alpha]
    (let [d-alpha (- alpha last-alpha)]
      (set! last-alpha alpha)
      (set! running-tally (+ running-tally (* frequency d-alpha)))
      (when (> running-tally 1)
        (set! running-tally (mod running-tally 1))
        true)))

  (interpolate [this alpha]
    (when (new-step? this alpha)
      (let [value (if (< alpha 1)
                    (rand-func)
                    final-value)]
        (reset! value-tracker value)))))

(defn randomize
  "Create a Randomize animation for a value tracker.

   Parameters:
   - value-tracker: Atom holding the current value
   - frequency: How often to update per second (default 8)
   - rand-func: Function to generate random values (default random)
   - final-value: Value to settle on at end (default from rand-func)
   - run-time: Animation duration"
  [value-tracker & {:keys [frequency rand-func final-value run-time]
                    :or {frequency 8
                         rand-func rand
                         run-time 1}}]
  (let [final (or final-value (rand-func))]
    {:type :randomize
     :value-tracker value-tracker
     :frequency frequency
     :rand-func rand-func
     :final-value final
     :run-time run-time}))

(defprotocol ITrackingDots
  "Protocol for tracking dots that accumulate and fade."
  (add-point [this point])
  (fade-points [this fade-factor]))

(deftype TrackingDotsAnimation
         [point-func fade-factor radius color
          ^:unsynchronized-mutable points ^:unsynchronized-mutable opacities]
  ITrackingDots
  (add-point [_this point]
    (set! points (conj points point))
    (set! opacities (conj opacities 1.0)))

  (fade-points [_this factor]
    (set! opacities (mapv #(* % factor) opacities))))

(defn tracking-dots
  "Create a TrackingDots animation that shows trajectory.

   Parameters:
   - point-func: Function returning current point
   - fade-factor: Opacity multiplier per frame (default 0.95)
   - radius: Dot radius (default 0.25)
   - color: Dot color
   - run-time: Animation duration"
  [point-func & {:keys [fade-factor radius color run-time]
                 :or {fade-factor 0.95
                      radius 0.25
                      color :yellow
                      run-time 1}}]
  {:type :tracking-dots
   :point-func point-func
   :fade-factor fade-factor
   :radius radius
   :color color
   :run-time run-time})

;; =============================================================================
;; Random Variable Label Group
;; =============================================================================

(defrecord RandomVarLabel
           [axis label-name color tracker tip label direction])

(defn get-random-var-label-group
  "Create a group with tracker, tip arrow, and label for a random variable.

   Parameters:
   - axis: The axis (number line) to attach to
   - label-name: LaTeX string for the label
   - color: Color for tip and label
   - initial-value: Starting value (random if nil)
   - font-size: Label font size (default 36)
   - direction: Direction for tip placement"
  [axis label-name & {:keys [color initial-value font-size direction]
                      :or {color :grey
                           font-size 36}}]
  (let [x-range (mo/get-range axis)
        init-val (or initial-value
                     (+ (first x-range)
                        (* (rand) (- (second x-range) (first x-range)))))
        tracker (atom init-val)
        tip {:type :arrow-tip
             :angle 90
             :height 0.15
             :fill color
             :updater (fn [m] (mo/move-to m (mo/n2p axis @tracker) direction))}
        label {:type :tex
               :content label-name
               :font-size font-size
               :color color
               :backstroke {:color :black :width 5}
               :updater (fn [m] (mo/next-to m tip (- direction) :buff 0.1))}]
    (->RandomVarLabel axis label-name color tracker tip label direction)))

;; =============================================================================
;; Scene: MaxProcess
;; =============================================================================

(defn max-process-scene
  "Visualize max(rand(), rand()) process with three intervals.

   Shows:
   - Two input random variables x1 and x2
   - The max of the two
   - TrackingDots showing distribution of each"
  []
  (let [;; Create three unit intervals
        intervals (mo/vgroup
                   (for [_ (range 3)]
                     (-> (mo/unit-interval)
                         (mo/set-width 3)
                         (mo/add-numbers (range 0 1.1 0.2)
                                         :font-size 16 :buff 0.1 :direction :up))))
        _ (-> intervals
              (mo/arrange :down :buff 2.5)
              (mo/shift [-2 0 0]))
        _ (mo/shift (nth intervals 1) [0 0.5 0])

        ;; Create random variable groups
        x1-group (get-random-var-label-group (nth intervals 0) "" :color :blue)
        x2-group (get-random-var-label-group (nth intervals 1) "" :color :yellow)
        max-group (get-random-var-label-group (nth intervals 2) "" :color :green)

        x1-tracker (:tracker x1-group)
        x2-tracker (:tracker x2-group)
        max-tracker (:tracker max-group)

        ;; Max tracker follows the maximum
        _ (add-watch x1-tracker :max-update
                     (fn [_ _ _ _] (reset! max-tracker (max @x1-tracker @x2-tracker))))
        _ (add-watch x2-tracker :max-update
                     (fn [_ _ _ _] (reset! max-tracker (max @x1-tracker @x2-tracker))))

        ;; Labels with changing numbers
        tex-t2c {"x_1" :blue "x_2" :yellow}
        labels (mo/vgroup
                [(typo/tex "x_1 = \\text{rand}() \\rightarrow 0.00" :t2c tex-t2c)
                 (typo/tex "x_2 = \\text{rand}() \\rightarrow 0.00" :t2c tex-t2c)
                 (typo/tex "\\max(x_1, x_2) \\rightarrow 0.00" :t2c tex-t2c)])

        ;; Position labels next to intervals
        _ (doseq [[label group interval] (map vector
                                              (mo/children labels)
                                              [x1-group x2-group max-group]
                                              (mo/children intervals))]
            (mo/next-to label interval :right :buff 0.5))

        ;; Surrounding rectangle for top two intervals
        top-rect (-> (mo/surrounding-rectangle (mo/vgroup [(nth intervals 0) (nth intervals 1)])
                                               :buff 0.25)
                     (mo/stretch 1.1 1)
                     (mo/set-stroke :white 2)
                     (mo/set-fill :grey-e 1))

        ;; Arrow and label
        arrow (mo/vector-arrow [0 -1.5 0] :thickness 5)
        _ (mo/next-to arrow top-rect :down)
        arrow-label (mo/text "max" :font-size 60)
        _ (mo/next-to arrow-label arrow :right)

        ;; Dashed line connecting max to whichever input is larger
        get-line (fn []
                   (let [x1 @x1-tracker
                         x2 @x2-tracker
                         tip (if (> x1 x2) (:tip x1-group) (:tip x2-group))]
                     (mo/dashed-line (mo/get-top (:tip max-group))
                                     (mo/get-top tip)
                                     :stroke {:color :grey :width 2 :opacity 0.5})))
        line (mo/always-redraw get-line)

        ;; Timeline
        timeline
        (tl/timeline
         [:t 0
          (tl/add intervals)
          (tl/add [x1-group x2-group max-group])
          (tl/add labels)
          (tl/add top-rect)
          (tl/add [arrow arrow-label])
          (tl/add line)]

         [:t 0.5
          (tl/|| (randomize x1-tracker :frequency 4 :run-time 30)
                 (randomize x2-tracker :frequency 4 :run-time 30)
                 (tracking-dots #(mo/get-top (:tip x1-group)) :color :blue :run-time 30)
                 (tracking-dots #(mo/get-top (:tip x2-group)) :color :yellow :run-time 30)
                 (tracking-dots #(mo/get-top (:tip max-group)) :color :green :run-time 30))])]

    {:intervals intervals
     :groups [x1-group x2-group max-group]
     :labels labels
     :timeline timeline}))

;; =============================================================================
;; Scene: SqrtProcess
;; =============================================================================

(defn sqrt-process-scene
  "Visualize sqrt(rand()) process with two intervals.

   Shows that sqrt transforms the uniform distribution."
  []
  (let [;; Create two unit intervals
        intervals (mo/vgroup
                   (for [_ (range 2)]
                     (-> (mo/unit-interval)
                         (mo/set-width 3)
                         (mo/add-numbers (range 0 1.1 0.2)
                                         :font-size 16 :buff 0.1 :direction :up))))
        _ (-> intervals
              (mo/arrange :down :buff 3.5)
              (mo/shift [-2 0 0]))

        ;; Create random variable groups
        x-group (get-random-var-label-group (nth intervals 0) "" :color :blue)
        sqrt-group (get-random-var-label-group (nth intervals 1) "" :color :teal)

        x-tracker (:tracker x-group)
        sqrt-tracker (:tracker sqrt-group)

        ;; sqrt tracker follows sqrt of x
        _ (add-watch x-tracker :sqrt-update
                     (fn [_ _ _ v] (reset! sqrt-tracker (Math/sqrt v))))

        ;; Labels
        sqrt-t2c {"x" :blue}
        labels (mo/vgroup
                [(typo/tex "x = \\text{rand}() \\rightarrow 0.00" :t2c sqrt-t2c)
                 (typo/tex "\\sqrt{x} \\rightarrow 0.00" :t2c sqrt-t2c)])

        ;; Position labels
        _ (doseq [[label group interval] (map vector
                                              (mo/children labels)
                                              [x-group sqrt-group]
                                              (mo/children intervals))]
            (mo/next-to label interval :right :buff 0.5))

        ;; Arrow and label
        arrow (mo/arrow (mo/get-center (nth intervals 0))
                        (mo/get-center (nth intervals 1))
                        :buff 0.5 :thickness 5)
        label (mo/text "sqrt" :font-size 60)
        _ (mo/next-to label arrow :right)

        ;; Timeline
        timeline
        (tl/timeline
         [:t 0
          (tl/add intervals)
          (tl/add [x-group sqrt-group])
          (tl/add labels)
          (tl/add [arrow label])]

         [:t 0.5
          (tl/|| (randomize x-tracker :frequency 4 :run-time 30)
                 (tracking-dots #(mo/get-top (:tip x-group)) :color :blue :run-time 30)
                 (tracking-dots #(mo/get-top (:tip sqrt-group)) :color :teal :run-time 30))])]

    {:intervals intervals
     :groups [x-group sqrt-group]
     :timeline timeline}))

;; =============================================================================
;; Scene: SquareAndSquareRoot
;; =============================================================================

(defn square-and-square-root-scene
  "Show the relationship between squaring and square root.

   Demonstrates TransformMatchingStrings for equation morphing."
  []
  (let [lines [(typo/tex "\\left(\\frac{1}{2}\\right)^2 = \\frac{1}{4}")
               (typo/tex "\\sqrt{\\frac{1}{4}} = \\frac{1}{2}")]
        _ (doseq [line lines] (mo/scale line 1.5))

        timeline
        (tl/timeline
         [:t 0 (tl/fade-in (first lines))]
         [:t 1 (tl/wait 1)]
         [:t 2 (typo/transform-matching-strings
                (first lines) (second lines)
                :matched-pairs [[(typo/get-part (first lines) "2" 1)
                                 (typo/get-part (second lines) "\\sqrt" 0)]]
                :path-arc (/ Math/PI 2))])]

    {:lines lines
     :timeline timeline}))

;; =============================================================================
;; Scene: GawkAtEquivalence
;; =============================================================================

(defn gawk-at-equivalence-scene
  "Pi creature reacting to the surprising equivalence.

   max(rand(), rand()) <-> sqrt(rand())"
  []
  (let [expr (mo/vgroup
              [(mo/text "max(rand(), rand())")
               (typo/tex "\\updownarrow" :font-size 90)
               (mo/text "sqrt(rand())")])
        _ (-> expr
              (mo/arrange :down)
              (mo/to-edge :up))

        randy (char/randolph :position :down)

        timeline
        (tl/timeline
         [:t 0 (tl/add expr)]
         [:t 0.5 (char/change randy :confused expr)]
         [:t 2 (char/blink randy)]
         [:t 3 (tl/wait 1)]
         [:t 4 (char/change randy :maybe expr)]
         [:t 5 (char/blink randy)])]

    {:expr expr
     :randy randy
     :timeline timeline}))

;; =============================================================================
;; Scene: VisualizeMaxOfPairCDF
;; =============================================================================

(defn visualize-max-of-pair-cdf-scene
  "Visualize the CDF of max(x1, x2) on a unit square.

   Key demonstration:
   - Sampling points in unit square
   - Region where max(x1, x2) <= R forms a square of side R
   - Therefore P(max(x1, x2) <= R) = R^2
   - Extends to 3D for max of three"
  []
  (let [;; Setup axes
        axes (mo/axes [0 1 0.1] [0 1 0.1] :width 6 :height 6)
        _ (mo/add-coordinate-labels axes
                                    :x-values [0 0.5 1.0]
                                    :y-values [0.5 1.0]
                                    :num-decimal-places 1
                                    :buff 0.15
                                    :font-size 16)

        ;; Trackers for x1 and x2
        cdf-t2c {"x_1" :blue "x_2" :yellow "x_3" :red}

        x1-group (get-random-var-label-group (mo/x-axis axes) "x_1"
                                             :color :blue :initial-value 0.7)
        x2-group (get-random-var-label-group (mo/y-axis axes) "x_2"
                                             :color :yellow :initial-value 0.3)

        x1-tracker (:tracker x1-group)
        x2-tracker (:tracker x2-group)

        ;; Point function for the (x1, x2) coordinate
        get-xy-point (fn [] (mo/c2p axes [@x1-tracker @x2-tracker]))

        ;; Lines from tips to point
        v-line (mo/line :stroke {:color :white :width 1 :opacity 0.5})
        _ (mo/f-always v-line :put-start-and-end-on
                       #(mo/get-top (:tip x1-group)) get-xy-point)
        h-line (mo/line :stroke {:color :white :width 1 :opacity 0.5})
        _ (mo/f-always h-line :put-start-and-end-on
                       #(mo/get-right (:tip x2-group)) get-xy-point)

        ;; Dot at (x1, x2)
        xy-dot (mo/dot :radius 0.05)
        _ (mo/f-always xy-dot :move-to get-xy-point)

        ;; Coordinate label
        coord-label (typo/tex "(x_1, x_2)" :t2c cdf-t2c :font-size 36)
        _ (mo/always coord-label :next-to xy-dot :ur :small-buff)

        ;; Big square representing sample space
        big-square (-> (mo/square)
                       (mo/set-fill :grey-e 1)
                       (mo/set-stroke :grey-d 1)
                       (mo/replace (mo/line (mo/c2p axes [0 0])
                                            (mo/c2p axes [1 1]))))

        ;; Max lines showing where max(x1, x2) = 0.7
        max-lines (mo/vgroup
                   [(mo/line (mo/c2p axes [0.7 0.7]) (mo/c2p axes [0.7 0])
                             :stroke {:color :green :width 3})
                    (mo/line (mo/c2p axes [0.7 0.7]) (mo/c2p axes [0 0.7])
                             :stroke {:color :green :width 3})])

        ;; Inner square for P(max <= R)
        inner-square (-> (mo/square)
                         (mo/set-fill :green 0.35)
                         (mo/set-stroke :green 0)
                         (mo/replace max-lines))

        ;; Equations
        max-expr (typo/tex "\\max(x_1, x_2) = 0.7" :t2c cdf-t2c)
        _ (mo/to-corner max-expr :ur)

        prob-eq (typo/tex "P(\\max(x_1, x_2) = 0.7)" :t2c cdf-t2c)
        prob-ineq (typo/tex "P(\\max(x_1, x_2) \\le 0.7)" :t2c cdf-t2c)
        gen-prob-ineq (typo/tex "P(\\max(x_1, x_2) \\le R)" :t2c cdf-t2c)
        cdf-expr (typo/tex "P(\\max(x_1, x_2) \\le R) = R^2" :t2c cdf-t2c)

        ;; Position equations
        _ (doseq [eq [prob-eq prob-ineq gen-prob-ineq cdf-expr]]
            (-> eq (mo/to-corner :ur) (mo/shift [0 0.5 0])))

        not-helpful-text (-> (mo/text "Not helpful")
                             (mo/set-color :red)
                             (mo/next-to prob-eq :down))

        ;; Comparison with sqrt case
        sqrt-lines [(typo/tex "P(\\sqrt{x_1} \\le R)" :t2c cdf-t2c)
                    (typo/tex "P(x_1 \\le R^2)" :t2c cdf-t2c)
                    (typo/tex "= R^2" :t2c cdf-t2c)]

        ;; 3D extension
        axes3d (mo/three-d-axes [0 1 0.1] [0 1 0.1] [0 1 0.1])

        x3-group (get-random-var-label-group (mo/z-axis axes3d) "x_3"
                                             :color :red :initial-value 0.2)
        x3-tracker (:tracker x3-group)

        get-xyz-point (fn [] (mo/c2p axes3d [@x1-tracker @x2-tracker @x3-tracker]))

        new-cdf-expr (typo/tex "P(\\max(x_1, x_2, x_3) \\le R) = R^3" :t2c cdf-t2c)

        ;; 3D cube
        width (mo/get-width inner-square)
        cube (mo/vgroup
              [(mo/shift (mo/copy inner-square) [0 0 width])
               (mo/rotate (mo/copy inner-square) (/ Math/PI 2) :left :about-edge :up)
               (mo/rotate (mo/copy inner-square) (/ Math/PI 2) :up :about-edge :right)
               (mo/rotate (mo/copy inner-square) (/ Math/PI 2) :down :about-edge :left)
               (mo/rotate (mo/copy inner-square) (/ Math/PI 2) :right :about-edge :down)])

        _ (-> cube
              (mo/set-stroke :green 3)
              (mo/set-fill :green 0.2))

        ;; Timeline with multiple phases
        timeline
        (tl/timeline
         ;; Phase 1: Show x-axis and x1
         [:t 0 (tl/add (mo/x-axis axes))]
         [:t 0.5 (tl/|| (tl/fade-in (:tip x1-group))
                        (tl/fade-in (:label x1-group)))]
         [:t 1.5 (tl/wait 1)]

         ;; Phase 2: Randomize x1
         [:t 2.5 (tl/|| (randomize x1-tracker :run-time 8)
                        (tracking-dots #(mo/get-top (:tip x1-group)) :run-time 10 :color :blue))]

         ;; Phase 3: Show y-axis and x2
         [:t 13 (tl/|| (tl/fade-in (mo/y-axis axes))
                       (tl/write (:tip x2-group))
                       (tl/write (:label x2-group)))]
         [:t 14 (tl/wait 1)]

         ;; Phase 4: Randomize x2
         [:t 15 (tl/|| (randomize x2-tracker :run-time 8 :final-value 0.48)
                       (tracking-dots #(mo/get-right (:tip x2-group)) :run-time 10 :color :yellow))]

         ;; Phase 5: Show the pair inside the square
         [:t 26 (tl/|| (tl/show-creation v-line)
                       (tl/show-creation h-line)
                       (tl/fade-in xy-dot)
                       (tl/fade-in coord-label))]
         [:t 27.5 (tl/wait 1)]

         ;; Phase 6: Randomize in square
         [:t 29 (tl/add big-square)]
         [:t 29.5 (tl/|| (tl/fade-in big-square :run-time 2)
                         (randomize x1-tracker :run-time 6 :frequency 8 :final-value 0.42)
                         (randomize x2-tracker :run-time 6 :frequency 8 :final-value 0.69)
                         (tracking-dots get-xy-point :run-time 9 :fade-factor 0.97 :color :green))]

         ;; Phase 7: Show max expression and CDF
         [:t 40 (tl/fade-in max-expr :up)]
         [:t 41 (tl/|| (tl/animate x1-tracker :set-value 0.7)
                       (tl/animate x2-tracker :set-value 0.7))]
         [:t 42.5 (tl/wait 0.5)]

         ;; Phase 8: Show max lines
         [:t 43 (tl/>> (tl/|| (tl/animate x2-tracker :set-value 0 :run-time 3)
                              (tl/show-creation (first (mo/children max-lines))))
                       (tl/animate x2-tracker :set-value 0.7 :run-time 3)
                       (tl/wait 1))]
         [:t 50 (tl/>> (tl/|| (tl/animate x1-tracker :set-value 0 :run-time 3)
                              (tl/show-creation (second (mo/children max-lines))))
                       (tl/animate x1-tracker :set-value 0.7 :run-time 3)
                       (tl/wait 1))]

         ;; Phase 9: Transform to probability equations
         [:t 58 (typo/transform-matching-strings max-expr prob-eq)]
         [:t 59 (tl/wait 1)]

         [:t 60 (typo/transform-matching-strings
                 prob-eq prob-ineq :key-map {"=" "\\le"})]

         [:t 62 (tl/fade-in inner-square)]

         [:t 64 (tl/|| (randomize x1-tracker :run-time 4 :frequency 8 :final-value 0.38)
                       (randomize x2-tracker :run-time 4 :frequency 8 :final-value 0.42)
                       (tracking-dots get-xy-point :run-time 6 :fade-factor 0.97 :color :green))]

         ;; Phase 10: Show CDF formula
         [:t 71 (typo/transform-matching-strings
                 prob-ineq gen-prob-ineq :key-map {"0.7" "R"})]
         [:t 72 (typo/transform-matching-strings gen-prob-ineq cdf-expr)]
         [:t 73 (tl/animate (mo/vgroup [inner-square max-lines])
                            :scale 0.5 :about-edge :dl
                            :rate-func tl/there-and-back :run-time 3)]

         ;; Phase 11: Compare to sqrt case
         [:t 77 (tl/|| (tl/animate cdf-expr :shift [5 1 0])
                       (tl/animate (tl/frame) :shift [4.5 0 0] :scale 1.2))]
         [:t 79 (tl/write (first sqrt-lines))]
         [:t 80.5 (tl/wait 0.5)]
         [:t 81 (tl/|| (typo/transform-matching-strings
                        (mo/copy (first sqrt-lines)) (second sqrt-lines))
                       (tl/write (mo/tex "=") :rotate [90 :degrees]))]
         [:t 82.5 (tl/wait 0.5)]
         [:t 83 (tl/write (nth sqrt-lines 2))]

         ;; Phase 12: 3D extension
         [:t 86 (tl/|| (tl/write (mo/z-axis axes3d))
                       (tl/animate (tl/frame) :reorient [26 74 0 [-0.42 0.83 1.66] 10.11] :run-time 3))]

         [:t 90 (tl/|| (tl/fade-in (:tip x3-group))
                       (tl/fade-in (:label x3-group))
                       (tl/restore cube)
                       (tl/fade-out h-line)
                       (tl/fade-out v-line)
                       (tl/fade-out coord-label))]

         [:t 94 (tl/|| (randomize x1-tracker :run-time 16)
                       (randomize x2-tracker :run-time 16)
                       (randomize x3-tracker :run-time 16)
                       (tracking-dots get-xyz-point :run-time 20 :color :green :radius 0.15))])]

    {:axes axes
     :axes3d axes3d
     :x1-group x1-group
     :x2-group x2-group
     :x3-group x3-group
     :not-helpful-text not-helpful-text
     :new-cdf-expr new-cdf-expr
     :timeline timeline}))

;; =============================================================================
;; Scene: MaxOfThreeTex
;; =============================================================================

(defn max-of-three-tex-scene
  "Show the max of three equivalence with text."
  []
  (let [expr (typo/tex-text "max(rand(), rand(), rand()) $\\leftrightarrow$ rand()$^{1 / 3}$")
        _ (typo/set-submobject-colors-by-gradient
           (typo/get-part expr "rand()")
           [:blue :yellow :red :blue])

        timeline
        (tl/timeline
         [:t 0 (tl/write expr)]
         [:t 1.5 (tl/wait 1)])]

    {:expr expr
     :timeline timeline}))

;; =============================================================================
;; Scene: Arrows
;; =============================================================================

(defn arrows-scene
  "Simple scene with three arrows pointing down."
  []
  (let [arrows (-> (mo/vector-arrow :down :thickness 5)
                   (mo/replicate 3)
                   (mo/arrange :right :buff 1.0)
                   (mo/set-fill :yellow))

        timeline
        (tl/timeline
         [:t 0 (tl/lagged-start-map tl/grow-arrow arrows)]
         [:t 1.5 (tl/wait 1)])]

    {:arrows arrows
     :timeline timeline}))

;; =============================================================================
;; Utility Functions for Rendering
;; =============================================================================

(defn render-max-process
  "Render the MaxProcess scene."
  []
  (let [scene (max-process-scene)]
    (tl/render-timeline (:timeline scene))))

(defn render-sqrt-process
  "Render the SqrtProcess scene."
  []
  (let [scene (sqrt-process-scene)]
    (tl/render-timeline (:timeline scene))))

(defn render-visualize-cdf
  "Render the CDF visualization scene."
  []
  (let [scene (visualize-max-of-pair-cdf-scene)]
    (tl/render-timeline (:timeline scene))))

(comment
  ;; Example usage:
  (render-max-process)
  (render-sqrt-process)
  (render-visualize-cdf)

  ;; Create individual scenes
  (def scene (max-process-scene))
  (def cdf-scene (visualize-max-of-pair-cdf-scene))

  ;; Access components
  (:x1-group cdf-scene)
  (:timeline cdf-scene))
