(ns desargues.videos.scenes.spring-mass
  "Spring-mass system scene recreation from 3b1b's Laplace Transform series.

   This demonstrates:
   - Spring-mass physics simulation
   - Live-drawn graphs
   - Velocity and force vectors
   - Equation derivation with TransformMatchingTex
   - Number line tracking"
  (:require [desargues.videos.physics :as phys]
            [desargues.videos.math-objects :as mo]
            [desargues.videos.timeline :as tl]
            [desargues.videos.typography :as typ]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def default-spring-config
  {:x0 2.0 ; initial displacement
   :v0 0.0 ; initial velocity
   :k 3.0 ; spring constant
   :mu 0.1 ; damping coefficient
   :equilibrium-length 7.0
   :equilibrium-position [-2 0 0] ; ORIGIN offset
   :direction [1 0 0] ; RIGHT
   :spring-stroke-color :grey-b
   :spring-stroke-width 2
   :spring-radius 0.25
   :n-spring-curls 8
   :mass-width 1.0
   :mass-color :blue-e
   :mass-label "m"})

(defn get-coef-colors
  "Generate coefficient colors from TEAL to RED."
  [n-coefs]
  (let [colors [:teal :gold :red]]
    (take n-coefs colors)))

;; =============================================================================
;; Spring-Mass Mobject
;; =============================================================================

(defrecord SpringMassSystem [config state spring mass
                             is-running? velocity])

(defn create-spring-curve
  "Create the visual spring curve."
  [config]
  (let [{:keys [n-spring-curls spring-radius spring-stroke-color
                spring-stroke-width]} config]
    {:type :parametric-curve
     :func (fn [t]
             [t
              (* (- spring-radius) (Math/sin (* 2 Math/PI t)))
              (* spring-radius (Math/cos (* 2 Math/PI t)))])
     :t-range [0 n-spring-curls 0.01]
     :stroke-color spring-stroke-color
     :stroke-width spring-stroke-width}))

(defn create-mass-square
  "Create the visual mass square."
  [config]
  (let [{:keys [mass-width mass-color mass-label]} config]
    (mo/square :side mass-width
               :fill-color mass-color
               :fill-opacity 1
               :stroke-color :white
               :stroke-width 1
               :label (typ/tex mass-label))))

(defn create-spring-mass-system
  "Create a spring-mass system with visual components."
  [& {:keys [config] :or {config default-spring-config}}]
  (let [{:keys [x0 v0 k mu equilibrium-position direction]} config
        ;; Create physics state
        physics-state (phys/make-spring-mass
                       :k k
                       :mass 1.0 ; normalized
                       :damping mu
                       :x x0
                       :v v0
                       :equilibrium-position equilibrium-position)
        ;; Create visual components
        spring (create-spring-curve config)
        mass (create-mass-square config)]
    (->SpringMassSystem config physics-state spring mass
                        (atom true) (atom v0))))

(defn get-x
  "Get current displacement from equilibrium."
  [system]
  (:x (phys/get-state (:state system))))

(defn set-x
  "Set displacement from equilibrium."
  [system x]
  (let [new-state (phys/set-state (:state system)
                                  {:x x
                                   :v @(:velocity system)})]
    (assoc system :state new-state)))

(defn get-velocity
  "Get current velocity."
  [system]
  @(:velocity system))

(defn set-velocity
  "Set velocity."
  [system v]
  (reset! (:velocity system) v)
  system)

(defn get-force
  "Calculate the force on the mass."
  [system]
  (let [{:keys [k mu]} (:config system)
        x (get-x system)
        v (get-velocity system)]
    (- (- (* k x)) (* mu v))))

(defn pause
  "Pause the simulation."
  [system]
  (reset! (:is-running? system) false)
  system)

(defn unpause
  "Resume the simulation."
  [system]
  (reset! (:is-running? system) true)
  system)

(defn time-step
  "Advance the system by dt using sub-stepping."
  [system dt & {:keys [dt-size] :or {dt-size 0.01}}]
  (when (and @(:is-running? system) (> dt 0))
    (let [{:keys [k mu]} (:config system)
          sub-steps (max (int (/ dt dt-size)) 1)
          true-dt (/ dt sub-steps)]
      (loop [n sub-steps
             x (get-x system)
             v (get-velocity system)]
        (if (zero? n)
          (do
            (set-x system x)
            (set-velocity system v))
          (let [dx (* v true-dt)
                dv (* (- (- (* k x)) (* mu v)) true-dt)]
            (recur (dec n) (+ x dx) (+ v dv)))))))
  system)

;; =============================================================================
;; Velocity and Force Vectors
;; =============================================================================

(defn create-velocity-vector
  "Create a velocity vector that updates with the system."
  [system & {:keys [scale-factor color v-offset]
             :or {scale-factor 0.5 color :green v-offset -0.25}}]
  {:type :updating-vector
   :system system
   :update-fn (fn [sys]
                (let [v (get-velocity sys)
                      mass-pos (get-x sys)]
                  {:start [mass-pos v-offset 0]
                   :end [(+ mass-pos (* scale-factor v)) v-offset 0]}))
   :color color})

(defn create-force-vector
  "Create a force vector that updates with the system."
  [system & {:keys [scale-factor color v-offset]
             :or {scale-factor 0.5 color :red v-offset -0.25}}]
  {:type :updating-vector
   :system system
   :update-fn (fn [sys]
                (let [f (get-force sys)
                      mass-pos (get-x sys)]
                  {:start [mass-pos v-offset 0]
                   :end [(+ mass-pos (* scale-factor f)) v-offset 0]}))
   :color color})

;; =============================================================================
;; Number Line Tracking
;; =============================================================================

(defn create-tracking-setup
  "Create the number line tracking setup for the spring-mass system."
  [system & {:keys [number-line-config]
             :or {number-line-config {:x-range [-4 4 1]}}}]
  (let [eq-pos (get-in system [:config :equilibrium-position])
        number-line (mo/number-line :x-range (:x-range number-line-config)
                                    :position [(first eq-pos)
                                               (- (second eq-pos) 2)
                                               0])
        dashed-line {:type :dashed-line
                     :update-fn (fn [sys]
                                  {:start [(get-x sys) 0 0]
                                   :end [(get-x sys) -2 0]})
                     :stroke-color :grey
                     :stroke-width 2}
        arrow-tip {:type :arrow-tip
                   :update-fn (fn [sys]
                                {:position [(get-x sys) -2 0]})
                   :direction :down
                   :color :teal}
        x-label (typ/tex "x = 0.00" :font-size 24)]
    {:number-line number-line
     :dashed-line dashed-line
     :arrow-tip arrow-tip
     :x-label x-label}))

;; =============================================================================
;; Solution Graph (TracedPath)
;; =============================================================================

(defn create-position-graph
  "Create a traced path showing x(t)."
  [system axes & {:keys [stroke-color stroke-width]
                  :or {stroke-color :blue stroke-width 3}}]
  {:type :traced-path
   :system system
   :axes axes
   :get-point (fn [time sys]
                [(* time 1.0) ; x = time
                 (get-x sys)]) ; y = position
   :stroke-color stroke-color
   :stroke-width stroke-width
   :time (atom 0)
   :points (atom [])})

(defn update-position-graph
  "Update the traced path with a new point."
  [graph dt]
  (let [{:keys [system get-point time points]} graph]
    (swap! time + dt)
    (swap! points conj (get-point @time system))
    graph))

;; =============================================================================
;; Scene: BasicSpringScene
;; =============================================================================

(defn basic-spring-scene
  "Recreate the BasicSpringScene from shm.py.

   This scene:
   1. Shows a spring-mass system oscillating
   2. Adds a number line with position tracking
   3. Shows velocity and acceleration vectors
   4. Demonstrates Hooke's law
   5. Shows the solution graph"
  []
  (let [;; Create spring-mass system
        spring (create-spring-mass-system
                :config (assoc default-spring-config
                               :x0 2
                               :mu 0.1
                               :k 3
                               :equilibrium-position [-2 0 0]
                               :equilibrium-length 5))

        ;; Create tracking elements
        tracking (create-tracking-setup spring)

        ;; Create vectors
        colors (get-coef-colors 3)
        v-vect (create-velocity-vector spring :color (second colors) :scale-factor 0.25)
        a-vect (create-force-vector spring :color (nth colors 2) :scale-factor 0.25)

        ;; Create axes for solution graph
        axes (mo/axes :x-range [0 20 1]
                      :y-range [-2 2 1]
                      :width 12
                      :height 3
                      :axis-config {:stroke-color :grey})

        ;; Create position graph
        position-graph (create-position-graph spring axes)

        ;; Timeline
        timeline
        (tl/timeline
         ;; Part 1: Add spring and let it oscillate
         (tl/at 0
                (tl/add-to-scene spring))

         ;; Part 2: Add tracking elements
         (tl/at 2
                (tl/|| (tl/v-fade-in (:number-line tracking))
                       (tl/v-fade-in (:dashed-line tracking))
                       (tl/v-fade-in (:arrow-tip tracking))
                       (tl/v-fade-in (:x-label tracking))))

         ;; Part 3: Show velocity vector
         (tl/at 9
                (tl/v-fade-in v-vect))

         ;; Part 4: Show acceleration/force vector
         (tl/at 14
                (tl/v-fade-in a-vect))

         ;; Part 5: Pause and demonstrate force law
         (tl/at 22
                (pause spring))

         ;; Manually set different positions to show F = -kx
         (tl/at 23
                (tl/animate (set-x spring 2)))
         (tl/at 25
                (tl/animate (set-x spring 3)))
         (tl/at 27
                (tl/animate (set-x spring 4)))

         ;; Part 6: Resume and show solution graph
         (tl/at 35
                (unpause spring)
                (tl/|| (tl/frame-reorient [0 0 0] [2.88 1.88 0] 12.48)
                       (tl/fade-in axes)
                       (tl/v-fade-out a-vect)
                       (tl/animate (set-x spring 2))))

         ;; Part 7: Draw position graph
         (tl/at 37
                (tl/add-to-scene position-graph)))]

    {:spring spring
     :tracking tracking
     :v-vect v-vect
     :a-vect a-vect
     :axes axes
     :position-graph position-graph
     :timeline timeline}))

;; =============================================================================
;; Scene: SolveDampedSpringEquation
;; =============================================================================

(defn solve-damped-spring-equation-scene
  "Recreate the SolveDampedSpringEquation scene.

   This scene derives the equation of motion for a damped spring."
  []
  (let [;; Derivative labels
        colors (get-coef-colors 3)
        [x-color v-color a-color] colors

        position-func (typ/tex "x(t)" :t2c (typ/t2c {"x(t)" x-color}))
        velocity-func (typ/tex "x'(t)" :t2c (typ/t2c {"x'(t)" v-color}))
        accel-func (typ/tex "x''(t)" :t2c (typ/t2c {"x''(t)" a-color}))

        position-label (typ/tex-text "Position" :color x-color)
        velocity-label (typ/tex-text "Velocity" :color v-color)
        accel-label (typ/tex-text "Acceleration" :color a-color)

        ;; Main equation: m x''(t) = -k x(t) - μ x'(t)
        t2c-map (typ/t2c {"x(t)" x-color
                          "x'(t)" v-color
                          "x''(t)" a-color})

        equation1 (typ/tex "m x''(t) = -k x(t) - \\mu x'(t)"
                           :t2c t2c-map)

        ;; Braces and labels
        ma-brace (mo/brace (typ/tex-part equation1 "m x''(t)") :direction :down)
        kx-brace (mo/brace (typ/tex-part equation1 "-k x(t)") :direction :down)
        mu-v-brace (mo/brace (typ/tex-part equation1 "- \\mu x'(t)") :direction :down)

        force-label (typ/tex "\\textbf{F}")
        spring-force-label (typ/tex-text "Spring force")
        damping-label (typ/tex-text "Damping")

        timeline
        (tl/timeline
         ;; Part 1: Show x(t) and label
         (tl/at 0
                (tl/write position-func)
                (tl/write position-label))

         ;; Part 2: Show derivative to velocity
         (tl/at 2
                (let [arrow (mo/curved-arrow position-func velocity-func
                                             :path-arc (* 150 (/ Math/PI 180)))
                      ddt-label (typ/tex "\\frac{d}{dt}" :font-size 30)]
                  (tl/|| (tl/grow-from-point arrow position-func :path-arc 30)
                         (typ/transform-matching-tex position-func velocity-func :path-arc 30)
                         (typ/fade-transform position-label velocity-label))))

         ;; Part 3: Show derivative to acceleration
         (tl/at 5
                (let [arrow (mo/curved-arrow velocity-func accel-func
                                             :path-arc (* 150 (/ Math/PI 180)))
                      ddt-label (typ/tex "\\frac{d}{dt}" :font-size 30)]
                  (tl/|| (tl/grow-from-point arrow velocity-func :path-arc 30)
                         (typ/transform-matching-tex velocity-func accel-func :path-arc 30)
                         (typ/fade-transform velocity-label accel-label))))

         ;; Part 4: Show F = ma equation
         (tl/at 9
                (let [ma-part (typ/tex-part equation1 "m x''(t)")]
                  (typ/transform-from-copy accel-func ma-part :path-arc -45)))

         (tl/at 10
                (tl/|| (tl/grow-from-center ma-brace)
                       (tl/write (typ/tex-part equation1 "m"))))

         ;; Part 5: Show spring force
         (tl/at 12
                (tl/|| (tl/write (typ/tex-part equation1 "= -k"))
                       (typ/fade-transform-pieces ma-brace kx-brace)
                       (typ/transform-from-copy position-func (typ/tex-part equation1 "x(t)"))))

         ;; Part 6: Show damping
         (tl/at 15
                (tl/|| (tl/write (typ/tex-part equation1 "- \\mu"))
                       (typ/fade-transform-pieces kx-brace mu-v-brace)
                       (typ/transform-from-copy velocity-func (typ/tex-part equation1 "x'(t)")))))]

    {:position-func position-func
     :velocity-func velocity-func
     :accel-func accel-func
     :equation1 equation1
     :timeline timeline}))

;; =============================================================================
;; Scene: DampingForceDemo
;; =============================================================================

(defn damping-force-demo-scene
  "Demonstrate pure damping (no spring force).

   Shows a mass sliding with only friction/damping."
  []
  (let [spring (create-spring-mass-system
                :config (assoc default-spring-config
                               :x0 -4
                               :v0 2
                               :k 0 ; No spring force
                               :mu 0.3 ; Only damping
                               :equilibrium-position [0 0 0]
                               :equilibrium-length 6))

        v-color (second (get-coef-colors 3))

        velocity-vector (create-velocity-vector spring :color v-color :scale-factor 0.8)
        velocity-label (typ/tex "\\vec{\\textbf{v}}" :font-size 24 :color v-color)

        damping-vector (create-velocity-vector spring :scale-factor -0.5 :color :red :v-offset -0.5)
        damping-label (typ/tex "-\\mu v" :font-size 24 :color :red)

        timeline
        (tl/timeline
         (tl/at 0
                (tl/add-to-scene spring)
                (tl/add-to-scene velocity-vector)
                (tl/add-to-scene velocity-label)
                (tl/add-to-scene damping-vector)
                (tl/add-to-scene damping-label))

         ;; Hide the spring visually
         (tl/at 0.1
                (tl/set-opacity (:spring spring) 0)))]

    {:spring spring
     :velocity-vector velocity-vector
     :damping-vector damping-vector
     :timeline timeline}))

;; =============================================================================
;; Utility: Generate Multiple Systems for Comparison
;; =============================================================================

(defn create-comparison-systems
  "Create multiple spring-mass systems with different parameters for comparison."
  [& {:keys [k-values mu-values colors]
      :or {k-values [1 3 5]
           mu-values [0.1 0.1 0.1]
           colors [:blue :green :red]}}]
  (mapv (fn [k mu color]
          (create-spring-mass-system
           :config (assoc default-spring-config
                          :k k
                          :mu mu
                          :mass-color color)))
        k-values
        mu-values
        colors))

;; =============================================================================
;; Export Scene for Rendering
;; =============================================================================

(defn render-scene
  "Render a spring-mass scene to video."
  [scene-fn & {:keys [quality preview?]
               :or {quality :high preview? false}}]
  (let [scene (scene-fn)]
    {:scene scene
     :quality quality
     :preview? preview?}))

;; =============================================================================
;; Usage Examples
;; =============================================================================

(comment
  ;; Render basic spring scene
  (render-scene basic-spring-scene)

  ;; Render equation derivation
  (render-scene solve-damped-spring-equation-scene)

  ;; Render damping demo
  (render-scene damping-force-demo-scene)

  ;; Create a custom spring-mass simulation
  (let [system (create-spring-mass-system
                :config {:x0 3
                         :v0 0
                         :k 5
                         :mu 0.2
                         :equilibrium-position [0 0 0]
                         :equilibrium-length 5
                         :mass-width 0.8
                         :mass-color :teal
                         :mass-label "M"})]
    ;; Simulate for 10 seconds
    (loop [t 0]
      (when (< t 10)
        (time-step system 0.016)
        (println (format "t=%.2f x=%.3f v=%.3f"
                         t (get-x system) (get-velocity system)))
        (recur (+ t 0.016)))))

  ;; Compare systems with different spring constants
  (let [systems (create-comparison-systems
                 :k-values [1 4 9]
                 :colors [:blue :green :red])]
    (doseq [[i sys] (map-indexed vector systems)]
      (println (format "System %d: k=%.1f" i (get-in sys [:config :k]))))))
