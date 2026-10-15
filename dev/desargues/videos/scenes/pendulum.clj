(ns desargues.videos.scenes.pendulum
  "Pendulum scene recreation from 3b1b's differential equations series.

   This demonstrates the Clojure DSL for recreating Manim videos,
   showcasing:
   - Physics simulation with the pendulum DSL
   - Mathematical objects (axes, graphs)
   - Typography (equations with t2c)
   - Animation timeline composition
   - Pi creature integration"
  (:require [desargues.videos.physics :as phys]
            [desargues.videos.math-objects :as mo]
            [desargues.videos.timeline :as tl]
            [desargues.videos.typography :as typ]
            [desargues.videos.characters :as char]))

;; =============================================================================
;; Configuration Constants
;; =============================================================================

(def pendulum-config
  "Default pendulum configuration matching 3b1b's IntroducePendulum scene."
  {:length 3
   :gravity 20
   :weight-diameter 0.35
   :initial-theta 0.3 ; radians (~17 degrees)
   :damping 0.1
   :top-point [4 2 0]
   :rod-style {:stroke-width 3
               :stroke-color :grey-b
               :sheen-direction :up
               :sheen-factor 1}
   :weight-style {:stroke-width 0
                  :fill-opacity 1
                  :fill-color :grey-brown
                  :sheen-direction :ul
                  :sheen-factor 0.5}})

(def theta-vs-t-axes-config
  "Configuration for theta(t) graph axes."
  {:x-min 0
   :x-max 12
   :y-min (- (/ Math/PI 4))
   :y-max (/ Math/PI 4)
   :y-axis-config {:tick-frequency (/ Math/PI 16)
                   :unit-size 2}
   :axis-config {:stroke-width 2}})

(def lg-formula-config
  "Common configuration for L and g in formulas."
  (typ/t2c {"L" :blue "{g}" :yellow}))

;; =============================================================================
;; Pendulum Mobject (Visual Representation)
;; =============================================================================

(defrecord PendulumMobject [config pendulum-state
                            rod weight dashed-line angle-arc theta-label
                            velocity-vector])

(defn create-pendulum-mobject
  "Create a visual pendulum mobject from configuration."
  [config]
  (let [{:keys [length top-point weight-diameter initial-theta
                rod-style weight-style]} config
        ;; Create physics pendulum
        pendulum-state (phys/make-pendulum
                        :length length
                        :gravity (:gravity config 9.8)
                        :damping (:damping config 0.1)
                        :theta initial-theta
                        :omega 0
                        :top-point top-point)
        ;; Create visual elements
        rod (mo/line (first top-point) (second top-point)
                     (+ (first top-point) (* length (Math/sin initial-theta)))
                     (- (second top-point) (* length (Math/cos initial-theta)))
                     :style rod-style)
        weight (mo/circle :radius (/ weight-diameter 2)
                          :style weight-style)
        dashed-line (mo/dashed-line (first top-point) (second top-point)
                                    (first top-point) (- (second top-point) length)
                                    :num-dashes 25)
        angle-arc (mo/arc :center top-point
                          :radius 1
                          :start-angle (- (/ Math/PI 2))
                          :angle initial-theta)
        theta-label (typ/tex "\\theta" :font-size 36)]
    (->PendulumMobject config pendulum-state
                       rod weight dashed-line angle-arc theta-label nil)))

(defn get-theta
  "Get current angle of pendulum."
  [pendulum-mob]
  (:theta (phys/get-state (:pendulum-state pendulum-mob))))

(defn set-theta
  "Set angle of pendulum."
  [pendulum-mob theta]
  (let [new-state (phys/set-state (:pendulum-state pendulum-mob)
                                  {:theta theta
                                   :omega (:omega (phys/get-state (:pendulum-state pendulum-mob)))})]
    (assoc pendulum-mob :pendulum-state new-state)))

(defn start-swinging
  "Add updater to make pendulum swing according to physics."
  [pendulum-mob]
  (tl/add-updater pendulum-mob
                  (fn [mob dt]
                    (let [state (phys/get-state (:pendulum-state mob))
                          {:keys [theta omega]} state
                          config (:config mob)
                          new-state (phys/step-euler (:pendulum-state mob) dt 100)]
                      (assoc mob :pendulum-state new-state)))))

;; =============================================================================
;; Gravity Vector
;; =============================================================================

(defn create-gravity-vector
  "Create a gravity vector attached to the pendulum weight."
  [pendulum-mob & {:keys [length-multiple color]
                   :or {length-multiple (/ 1 9.8) color :yellow}}]
  (let [gravity (get-in pendulum-mob [:config :gravity])]
    (mo/vector :direction :down
               :length (* length-multiple gravity)
               :color color)))

;; =============================================================================
;; Theta vs Time Axes
;; =============================================================================

(defn create-theta-vs-t-axes
  "Create axes for plotting theta(t)."
  [& {:keys [config] :or {config theta-vs-t-axes-config}}]
  (let [{:keys [x-min x-max y-min y-max]} config
        axes (mo/axes :x-range [x-min x-max]
                      :y-range [y-min y-max]
                      :config config)]
    ;; Add labels
    (-> axes
        (mo/add-label :x (typ/tex "t"))
        (mo/add-label :y (typ/tex "\\theta(t)")))))

(defn get-y-axis-coordinates
  "Generate y-axis coordinate labels for pi fractions."
  [y-min y-max]
  (let [texs ["\\pi / 4" "\\pi / 2" "3 \\pi / 4" "\\pi"]
        values (map #(* % (/ Math/PI 4)) (range 1 5))]
    (for [[tex value] (map vector texs values)
          :let [neg-tex (str "-" tex)
                neg-value (- value)]
          [t v] [[tex value] [neg-tex neg-value]]
          :when (and (<= v y-max) (>= v y-min))]
      {:tex t :value v})))

;; =============================================================================
;; Live-Drawn Graph
;; =============================================================================

(defn create-live-drawn-graph
  "Create a graph that draws itself as the pendulum swings.

   This replicates the get_live_drawn_graph method from ThetaVsTAxes."
  [axes pendulum-mob & {:keys [t-max t-step style]
                        :or {t-step (/ 1.0 60)
                             style {:stroke-color :green :stroke-width 3}}}]
  (let [t-max (or t-max (:x-max (:config axes)))]
    {:type :live-drawn-graph
     :axes axes
     :pendulum pendulum-mob
     :t-max t-max
     :t-step t-step
     :style style
     :all-coords (atom [[0 (get-theta pendulum-mob)]])
     :time (atom 0)
     :time-of-last-addition (atom 0)}))

(defn update-live-graph
  "Updater function for live-drawn graph."
  [graph dt]
  (let [{:keys [axes pendulum t-max t-step all-coords time time-of-last-addition]} graph]
    (swap! time + dt)
    (when (<= @time t-max)
      (let [new-coords [@time (get-theta pendulum)]]
        (when (>= (- @time @time-of-last-addition) t-step)
          (swap! all-coords conj new-coords)
          (reset! time-of-last-addition @time))))
    graph))

;; =============================================================================
;; Period Formula
;; =============================================================================

(defn get-period-formula
  "Create the period formula: T = 2π√(L/g)"
  []
  (typ/tex "T = 2\\pi \\sqrt{\\frac{L}{g}}"
           :t2c lg-formula-config))

(defn calculate-period
  "Calculate the theoretical period of a pendulum."
  [pendulum-mob]
  (let [{:keys [length gravity]} (:config pendulum-mob)]
    (* 2 Math/PI (Math/sqrt (/ length gravity)))))

;; =============================================================================
;; Scene: IntroducePendulum
;; =============================================================================

(defn introduce-pendulum-scene
  "Recreate the IntroducePendulum scene.

   This scene:
   1. Shows a swinging pendulum
   2. Labels the theta angle
   3. Adds a theta(t) graph
   4. Shows the period formula
   5. Demonstrates length and gravity effects"
  []
  (let [;; Create pendulum
        pendulum (create-pendulum-mobject pendulum-config)

        ;; Create Pi creatures
        randy (char/randolph :position :dl)
        morty (char/mortimer :position :dr)

        ;; Create axes and graph
        axes (create-theta-vs-t-axes)

        ;; Period formula
        period (calculate-period pendulum)
        amplitude (:initial-theta pendulum-config)
        period-formula (get-period-formula)

        ;; Build the scene timeline
        timeline
        (tl/timeline
         ;; Part 1: Add pendulum and start swinging
         (tl/at 0
                (tl/add-to-scene pendulum)
                (tl/add-to-scene randy)
                (tl/add-to-scene morty))

         (tl/at 0.5
                (start-swinging pendulum))

         ;; Part 2: Label pendulum theta
         (tl/at 2
                (char/change randy :pondering pendulum))

         (tl/at 3
                (char/change morty :pondering pendulum))

         ;; Part 3: Show surrounding rectangle around theta label
         (tl/at 6
                (tl/show-creation-then-fade-out
                 (mo/surrounding-rectangle (:theta-label pendulum))))

         ;; Part 4: Add graph axes
         (tl/at 8
                (tl/|| (tl/restore :camera-frame)
                       (tl/draw-border-then-fill axes)))

         ;; Part 5: Start drawing the graph
         (tl/at 11
                (let [graph (create-live-drawn-graph axes pendulum)]
                  (tl/add-to-scene graph)))

         ;; Part 6: Add "Simple harmonic motion" label
         (tl/at 13
                (tl/fade-in (typ/tex-text "Simple harmonic motion"
                                          :font-size 72)
                            :direction :down))

         ;; Part 7: Show cos formula
         (tl/at 15
                (tl/write (typ/tex "= \\theta_0 \\cos(\\sqrt{g / L} t)"
                                   :t2c lg-formula-config)))

         ;; Part 8: Show period brace and formula
         (tl/at 20
                (let [brace (mo/brace-horizontal period :direction :up)]
                  (tl/|| (tl/grow-from-center brace)
                         (tl/fade-in-from-down period-formula))))

         ;; Part 9: Show length visualization
         (tl/at 25
                (let [L-part (typ/tex-part period-formula "L")]
                  (tl/show-creation-then-destruction-around L-part)))

         ;; Part 10: Show gravity vector
         (tl/at 28
                (let [g-vect (create-gravity-vector pendulum)]
                  (tl/grow-arrow g-vect))))]

    {:pendulum pendulum
     :randy randy
     :morty morty
     :axes axes
     :period-formula period-formula
     :timeline timeline}))

;; =============================================================================
;; Scene: MultiplePendulumsOverlayed
;; =============================================================================

(defn multiple-pendulums-overlayed-scene
  "Recreate the MultiplePendulumsOverlayed scene.

   Shows multiple pendulums with different initial angles,
   overlayed on the same pivot point."
  []
  (let [initial-thetas [(* 150 (/ Math/PI 180))
                        (* 90 (/ Math/PI 180))
                        (* 60 (/ Math/PI 180))
                        (* 30 (/ Math/PI 180))
                        (* 10 (/ Math/PI 180))]
        weight-colors [:pink :red :green :blue :grey]

        pendulums (mapv (fn [theta color]
                          (create-pendulum-mobject
                           (assoc pendulum-config
                                  :initial-theta theta
                                  :top-point [0 0 0]
                                  :weight-style (assoc (:weight-style pendulum-config)
                                                       :fill-color color
                                                       :fill-opacity 0.5))))
                        initial-thetas
                        weight-colors)

        ;; Create Pi creature
        randy (char/randolph :position :dl)

        ;; Create axes
        axes (create-theta-vs-t-axes :config (assoc theta-vs-t-axes-config
                                                    :x-max 20
                                                    :y-axis-config {:unit-size 0.5}))

        ;; Create graphs for each pendulum
        graphs (mapv (fn [p color]
                       (create-live-drawn-graph axes p
                                                :style {:stroke-color color
                                                        :stroke-width 1}))
                     pendulums
                     weight-colors)

        timeline
        (tl/timeline
         ;; Add all pendulums
         (tl/at 0
                (apply tl/add-to-scene pendulums))

         ;; Add axes and graphs
         (tl/at 0
                (tl/add-to-scene axes)
                (apply tl/add-to-scene graphs))

         ;; Start all pendulums swinging
         (tl/at 0.5
                (doseq [p pendulums]
                  (start-swinging p)))

         ;; Randy reactions
         (tl/at 1
                (char/change randy :sassy))

         (tl/at 4
                (char/blink randy))

         (tl/at 10
                (char/change randy :angry)))]

    {:pendulums pendulums
     :randy randy
     :axes axes
     :graphs graphs
     :timeline timeline}))

;; =============================================================================
;; Scene: LowAnglePendulum
;; =============================================================================

(defn low-angle-pendulum-scene
  "Recreate the LowAnglePendulum scene.

   Shows a pendulum at low angle with the small-angle approximation
   prediction overlayed."
  []
  (let [config (assoc pendulum-config
                      :initial-theta (* 20 (/ Math/PI 180))
                      :length 2.0
                      :damping 0
                      :top-point [0 0 0])

        pendulum (create-pendulum-mobject config)

        axes (create-theta-vs-t-axes
              :config (assoc theta-vs-t-axes-config
                             :x-max 25
                             :y-axis-config {:unit-size 0.75}))

        ;; Prediction curve: θ₀ cos(√(g/L) t)
        L (:length config)
        g (:gravity config)
        theta0 (:initial-theta config)
        omega-natural (Math/sqrt (/ g L))

        prediction-fn (fn [t] (* theta0 (Math/cos (* omega-natural t))))

        prediction-curve (mo/dashed-graph axes prediction-fn
                                          :x-range [0 25]
                                          :num-dashes 300
                                          :style {:stroke-color :white
                                                  :stroke-width 1})

        prediction-formula (typ/tex "\\theta_0 \\cos(\\sqrt{g / L} \\cdot t)"
                                    :t2c lg-formula-config
                                    :font-size 54)

        theta0-label (typ/integer (* theta0 (/ 180 Math/PI))
                                  :unit "^\\circ"
                                  :font-size 54)

        graph (create-live-drawn-graph axes pendulum)

        timeline
        (tl/timeline
         ;; Add elements
         (tl/at 0
                (tl/add-to-scene axes)
                (tl/add-to-scene prediction-curve)
                (tl/add-to-scene pendulum))

         ;; Show prediction
         (tl/at 1
                (tl/|| (tl/show-creation prediction-curve :run-time 2)
                       (tl/fade-in-from-down prediction-formula)
                       (tl/fade-in-from-down theta0-label)))

         ;; Highlight the angle label
         (tl/at 4
                (tl/show-creation-then-fade-around theta0-label)
                (tl/show-creation-then-fade-around (:theta-label pendulum)))

         ;; Start swinging and draw graph
         (tl/at 6
                (start-swinging pendulum)
                (tl/add-to-scene graph)))]

    {:pendulum pendulum
     :axes axes
     :prediction-curve prediction-curve
     :prediction-formula prediction-formula
     :graph graph
     :timeline timeline}))

;; =============================================================================
;; Scene: AnalyzePendulumForce
;; =============================================================================

(defn analyze-pendulum-force-scene
  "Recreate the AnalyzePendulumForce scene.

   This scene analyzes the forces on a pendulum and derives
   the equation of motion."
  []
  (let [config (assoc pendulum-config
                      :length 5
                      :top-point [0 3.5 0]
                      :initial-theta (* 60 (/ Math/PI 180)))

        pendulum (create-pendulum-mobject config)

        ;; Arc length equation: x = Lθ
        x-eq (typ/tex "x = L\\theta"
                      :t2c (typ/t2c {"x" :green "\\theta" :blue "L" :white}))

        ;; Gravity vector
        g-vect (create-gravity-vector pendulum :length-multiple 0.25)

        ;; Component labels
        g-sin-label (typ/tex "-g\\sin(\\theta)"
                             :t2c (typ/t2c {"\\theta" :blue})
                             :font-size 36)
        g-cos-label (typ/tex "-g\\cos(\\theta)"
                             :t2c (typ/t2c {"\\theta" :blue})
                             :font-size 36)

        ;; Main equation parts
        acceleration-eq (typ/tex "a = -g\\sin(\\theta)"
                                 :t2c (typ/t2c {"\\theta" :blue "a" :yellow}))

        final-equation (typ/tex "\\ddot{\\theta} = -\\frac{g}{L}\\sin(\\theta)"
                                :t2c (typ/t2c {"\\theta" :blue "L" :white "g" :yellow}))

        ;; Air resistance term
        air-resistance-term (typ/tex "-\\mu \\dot{\\theta}"
                                     :t2c (typ/t2c {"\\theta" :blue "\\mu" :red}))

        timeline
        (tl/timeline
         ;; Add pendulum
         (tl/at 0
                (tl/add-to-scene pendulum))

         ;; Show arc length equation
         (tl/at 1
                (tl/write x-eq))

         ;; Add gravity vector
         (tl/at 5
                (tl/grow-arrow g-vect))

         ;; Show constraint (swing back and forth)
         (tl/at 7
                (tl/animate-value (:pendulum-state pendulum) :theta
                                  :from (* 60 (/ Math/PI 180))
                                  :to (* -60 (/ Math/PI 180))
                                  :run-time 2))

         (tl/at 9
                (tl/animate-value (:pendulum-state pendulum) :theta
                                  :from (* -60 (/ Math/PI 180))
                                  :to (* 60 (/ Math/PI 180))
                                  :run-time 2))

         ;; Show component labels
         (tl/at 12
                (tl/fade-in g-sin-label)
                (tl/fade-in g-cos-label))

         ;; Show acceleration equation
         (tl/at 16
                (tl/write acceleration-eq))

         ;; Transform to final equation
         (tl/at 20
                (typ/transform-matching-tex acceleration-eq final-equation))

         ;; Add air resistance
         (tl/at 25
                (let [brace (mo/brace air-resistance-term :direction :down)
                      label (typ/tex-text "Air resistance")]
                  (tl/|| (tl/fade-in-from-down air-resistance-term)
                         (tl/grow-from-center brace)
                         (tl/write label)))))]

    {:pendulum pendulum
     :x-eq x-eq
     :g-vect g-vect
     :acceleration-eq acceleration-eq
     :final-equation final-equation
     :timeline timeline}))

;; =============================================================================
;; Export Scene for Rendering
;; =============================================================================

(defn render-scene
  "Render a pendulum scene to video.

   This would interface with the actual Manim renderer."
  [scene-fn & {:keys [quality preview?]
               :or {quality :high preview? false}}]
  (let [scene (scene-fn)]
    ;; In the actual implementation, this would:
    ;; 1. Convert the Clojure scene to Python Manim code
    ;; 2. Call the renderer
    ;; For now, just return the scene data
    {:scene scene
     :quality quality
     :preview? preview?}))

;; =============================================================================
;; Usage Examples
;; =============================================================================

(comment
  ;; Render the main pendulum introduction scene
  (render-scene introduce-pendulum-scene)

  ;; Render multiple pendulums
  (render-scene multiple-pendulums-overlayed-scene)

  ;; Render low-angle approximation scene
  (render-scene low-angle-pendulum-scene)

  ;; Render force analysis scene
  (render-scene analyze-pendulum-force-scene)

  ;; Example of composing a custom pendulum scene
  (let [my-pendulum (create-pendulum-mobject
                     {:length 4
                      :gravity 10
                      :initial-theta 0.5
                      :damping 0.2
                      :top-point [0 2 0]})]
    (tl/timeline
     (tl/at 0 (tl/add-to-scene my-pendulum))
     (tl/at 1 (start-swinging my-pendulum))
     (tl/at 10 (tl/fade-out my-pendulum)))))
