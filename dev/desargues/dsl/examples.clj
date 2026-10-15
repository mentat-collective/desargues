(ns desargues.dsl.examples
  "Example animations demonstrating the Clojure DSL.

   These examples show how to rewrite 3b1b-style animations
   using our high-level Clojure abstractions.

   ## Running Examples

   ```clojure
   (require '[desargues.dsl.examples :as ex])
   (ex/run-max-rand-demo)
   (ex/run-harmonic-oscillator)
   (ex/run-laplace-transform)
   ```"
  (:require [desargues.dsl.core :as dsl]
            [desargues.dsl.math :as math]
            [desargues.dsl.renderer :as r]))

;; =============================================================================
;; Example 1: Max of Random Numbers (inspired by max_rand.py)
;; =============================================================================

(defn max-rand-scene
  "Demonstrate max(rand(), rand()) ~ sqrt(rand()).

   This is a Clojure rewrite of the 3b1b max_rand.py visualization.
   It shows how random variable transformations work."
  []
  ;; Create reactive values for our random variables
  (let [x1 (dsl/reactive 0.5)
        x2 (dsl/reactive 0.3)
        max-val (dsl/derive-reactive [x1 x2] max)]

    ;; Build the scene
    {:type :scene
     :name "MaxRandScene"
     :objects
     {:interval1 (dsl/axes {:x [0 1]} :labels true)
      :interval2 (dsl/axes {:x [0 1]} :labels true)
      :interval3 (dsl/axes {:x [0 1]} :labels true)
      :x1-label (dsl/tex "x_1 = \\text{rand}()" :colors {"x_1" :blue})
      :x2-label (dsl/tex "x_2 = \\text{rand}()" :colors {"x_2" :yellow})
      :max-label (dsl/tex "\\max(x_1, x_2)" :colors {"x_1" :blue "x_2" :yellow})}

     :reactive-state
     {:x1 x1
      :x2 x2
      :max max-val}

     :timeline
     (-> (dsl/timeline)
         (dsl/at 0 {:action :setup-intervals})
         (dsl/at 1 {:action :randomize :targets [:x1 :x2] :duration 30})
         (dsl/at 32 {:action :highlight-equivalence}))}))

;; =============================================================================
;; Example 2: Harmonic Oscillator (inspired by driven_harmonic_oscillator.py)
;; =============================================================================

(defn harmonic-oscillator-scene
  "Demonstrate driven harmonic oscillator physics.

   This shows how our physics DSL can express complex dynamics
   in a declarative way."
  []
  ;; Physics parameters
  (let [k 20.0 ; spring constant
        m 1.0 ; mass
        damping 0.1 ; damping coefficient
        omega-r (Math/sqrt (/ k m)) ; resonant frequency

        ;; Create particle with spring force
        particle (dsl/particle {:position [1 0 0]
                                :velocity [0 0 0]
                                :mass m})

        ;; Spring force pulls toward origin
        spring (dsl/spring-force [0 0 0] k :damping damping)]

    ;; Apply spring force
    (dsl/apply-force! particle spring)

    {:type :scene
     :name "HarmonicOscillatorScene"
     :physics {:particle particle
               :forces [spring]}

     :equations
     {:hooke (math/equation '(F t) '(- (* k (x t)))
                            :colors {'F :yellow 'k :white 'x :red})
      :solution (math/expr '(* x_0 (cos (* omega_r t)))
                           :colors {'x_0 :red 'omega_r :pink 't :blue})}

     :timeline
     (-> (dsl/timeline)
         (dsl/at 0 {:action :show-spring-system})
         (dsl/at 2 {:action :displace-particle :to [1 0 0]})
         (dsl/at 3 {:action :release :duration 10})
         (dsl/at 13 {:action :show-equation :eq :hooke})
         (dsl/at 15 {:action :show-solution :eq :solution}))}))

;; =============================================================================
;; Example 3: Laplace Transform (inspired by main_equations.py)
;; =============================================================================

(defn laplace-transform-scene
  "Demonstrate the Laplace transform of cos(t).

   This shows equation manipulation and transformation animations."
  []
  (let [;; Color scheme
        t2c {'t :blue 's :yellow 'omega :pink}

        ;; Mathematical expressions
        cos-t (math/expr '(cos t) :colors t2c)
        integral (math/expr '(integral (* (cos t) (exp (- (* s t)))) 0 infinity)
                            :colors t2c)
        euler-form (math/expr '(* 1/2 (+ (exp (* i t)) (exp (- (* i t)))))
                              :colors (assoc t2c 'i :white))
        pole-form (math/expr '(+ (* 1/2 (/ 1 (- s i)))
                                 (* 1/2 (/ 1 (+ s i))))
                             :colors t2c)
        simplified (math/expr '(/ s (+ (* s s) 1))
                              :colors t2c)]

    {:type :scene
     :name "LaplaceTransformScene"
     :expressions
     {:cos-t cos-t
      :integral integral
      :euler-form euler-form
      :pole-form pole-form
      :simplified simplified}

     :derivation
     (math/derivation
      (math/transform-step cos-t euler-form
                           :description "Express cos as exponentials")
      (math/transform-step euler-form pole-form
                           :description "Apply Laplace transform")
      (math/transform-step pole-form simplified
                           :description "Simplify"))

     :timeline
     (-> (dsl/timeline)
         (dsl/at 0 {:action :show-cos})
         (dsl/at 2 {:action :show-laplace-definition})
         (dsl/at 5 {:action :split-exponentials})
         (dsl/at 8 {:action :show-poles :at '(i -i)})
         (dsl/at 12 {:action :simplify-result}))}))

;; =============================================================================
;; Example 4: Function and Derivative (simple demo)
;; =============================================================================

(defn derivative-demo-scene
  "Show a function and its derivative side by side.

   This is a simple example showing Emmy integration."
  []
  (let [;; Define our function symbolically
        f (fn [x] (Math/sin x))

        ;; Create expressions
        f-expr (math/expr '(sin x) :colors {'x :blue})
        df-expr (math/derivative f-expr 'x)]

    {:type :scene
     :name "DerivativeDemo"
     :expressions
     {:f f-expr
      :df df-expr}

     :graphs
     {:f-graph (dsl/graph f {:x-range [(- Math/PI) Math/PI]
                             :color :blue})
      :df-graph (dsl/graph Math/cos {:x-range [(- Math/PI) Math/PI]
                                     :color :red})}

     :timeline
     (-> (dsl/timeline)
         ;; Show function expression
         (dsl/at 0 (dsl/animation :write :f-tex))
         (dsl/wait 1)
         ;; Show axes and graph
         (dsl/then (dsl/animation :create :axes))
         (dsl/then (dsl/animation :create :f-graph))
         (dsl/wait 2)
         ;; Transform to derivative
         (dsl/then (dsl/animation :transform :f-tex :to :df-tex))
         (dsl/then (dsl/animation :transform :f-graph :to :df-graph))
         (dsl/wait 2))}))

;; =============================================================================
;; Example 5: Physics with Springs (like the 3b1b oscillator)
;; =============================================================================

(defn spring-physics-demo
  "Demonstrate spring physics with visual feedback.

   Shows how Clojure's functional approach makes physics simulations
   more composable and testable."
  []
  (let [;; Create multiple particles
        particles (mapv #(dsl/particle {:position [(* % 2) 0 0]
                                        :mass 1.0})
                        (range -2 3))

        ;; Connect each to origin with springs
        springs (mapv #(dsl/spring-force [0 0 0] 5.0 :damping 0.5)
                      particles)

        ;; Connect adjacent particles
        inter-springs
        (for [i (range (dec (count particles)))]
          (let [p1 (nth particles i)
                p2 (nth particles (inc i))]
            ;; Spring between adjacent particles
            (fn [_]
              (let [pos1 (dsl/get-position p1)
                    pos2 (dsl/get-position p2)
                    diff (mapv - pos2 pos1)
                    dist (Math/sqrt (reduce + (map #(* % %) diff)))
                    rest-len 2.0
                    k 3.0]
                (mapv #(* k (- dist rest-len) (/ % dist)) diff)))))]

    {:type :scene
     :name "SpringPhysicsDemo"
     :physics
     {:particles particles
      :springs springs
      :inter-springs inter-springs}

     :visuals
     {:dots (mapv #(dsl/shape :dot {:color :blue :radius 0.15}) particles)
      :spring-lines (for [i (range (dec (count particles)))]
                      (dsl/shape :line {:color :grey}))}

     :timeline
     (-> (dsl/timeline)
         (dsl/at 0 {:action :setup-particles})
         (dsl/at 1 {:action :perturb :particle 0 :direction [0 1 0]})
         (dsl/at 2 {:action :simulate :duration 20}))}))

;; =============================================================================
;; Example 6: Equation Transformation (like 3b1b equation scenes)
;; =============================================================================

(defn equation-transformation-demo
  "Demonstrate smooth equation transformations.

   This shows the power of TransformMatchingTex-style animations
   expressed in our DSL."
  []
  (let [t2c {'x :blue 'y :red}

        ;; Chain of equation transformations
        eq1 (math/expr '(= (+ (* a (** x 2)) (* b x) c) 0) :colors t2c)
        eq2 (math/expr '(= (** x 2) (- (/ (* b x) a) (/ c a))) :colors t2c)
        eq3 (math/expr '(= x (/ (- (- b) (sqrt (- (** b 2) (* 4 a c))))
                                (* 2 a)))
                       :colors t2c)]

    {:type :scene
     :name "EquationTransformDemo"
     :expressions [eq1 eq2 eq3]

     :timeline
     (-> (dsl/timeline)
         (dsl/at 0 (dsl/animation :write eq1))
         (dsl/wait 2)
         (dsl/then (dsl/animation :transform eq1 :to eq2))
         (dsl/wait 2)
         (dsl/then (dsl/animation :transform eq2 :to eq3))
         (dsl/wait 3))}))

;; =============================================================================
;; Runner Functions
;; =============================================================================

(defn run-example!
  "Run a scene definition through the renderer."
  [scene-def & {:keys [quality] :or {quality "medium_quality"}}]
  (r/init!)
  (println "Rendering scene:" (:name scene-def))
  ;; The actual rendering would happen here
  ;; For now, just return the scene data
  scene-def)

(defn run-max-rand-demo
  "Run the max(rand(), rand()) demonstration."
  []
  (run-example! (max-rand-scene)))

(defn run-harmonic-oscillator
  "Run the harmonic oscillator demonstration."
  []
  (run-example! (harmonic-oscillator-scene)))

(defn run-laplace-transform
  "Run the Laplace transform demonstration."
  []
  (run-example! (laplace-transform-scene)))

(defn run-derivative-demo
  "Run the derivative demonstration."
  []
  (run-example! (derivative-demo-scene)))

(defn run-spring-physics
  "Run the spring physics demonstration."
  []
  (run-example! (spring-physics-demo)))

(defn run-equation-transform
  "Run the equation transformation demonstration."
  []
  (run-example! (equation-transformation-demo)))

;; =============================================================================
;; Comparison: Python vs Clojure
;; =============================================================================

(comment
  "
  ## Python (3b1b style):

  ```python
  class MaxProcess(InteractiveScene):
      def construct(self):
          x1_tracker = ValueTracker(0.5)
          x2_tracker = ValueTracker(0.3)

          def get_max():
              return max(x1_tracker.get_value(), x2_tracker.get_value())

          max_tracker = ValueTracker(get_max())
          max_tracker.add_updater(lambda m: m.set_value(get_max()))

          self.play(
              Randomize(x1_tracker, run_time=30),
              Randomize(x2_tracker, run_time=30),
          )
  ```

  ## Clojure (our DSL):

  ```clojure
  (let [x1 (reactive 0.5)
        x2 (reactive 0.3)
        max-val (derive-reactive [x1 x2] max)]
    ;; max-val automatically updates when x1 or x2 change
    ;; No explicit updater needed - this is functional reactive programming

    (-> (timeline)
        (at 0 (randomize-animation x1 :duration 30))
        (at 0 (randomize-animation x2 :duration 30))
        (render!)))
  ```

  Key advantages of the Clojure approach:
  1. Derived values are automatically reactive (no manual updaters)
  2. Timeline is data, can be inspected/transformed
  3. Physics is composable via protocols
  4. Expressions are symbolic, can be manipulated
  5. Everything is immutable except explicit state
  ")

;; =============================================================================
;; DSL Showcase - The \"DNA\" of Animations
;; =============================================================================

(defn showcase-dsl-power
  "Demonstrate Clojure's unique strengths for animation DSLs.

   LISP is like DNA - self-describing, adaptable, evolvable."
  []
  {:metadata
   {:title "DSL Power Showcase"
    :description "Demonstrating Clojure's meta-programming capabilities"}

   ;; 1. Code as Data - Animations ARE data structures
   :animation-data
   {:type :sequence
    :children [{:type :fade-in :target :circle :duration 1}
               {:type :wait :duration 0.5}
               {:type :transform :from :circle :to :square}]}

   ;; 2. Macros - Extend the language
   :custom-syntax
   '(defscene my-scene []
      (with-objects [circle (circle {:color :blue})
                     square (square {:color :red})]
        (play! (create circle))
        (transform! circle square)))

   ;; 3. Protocols - Polymorphic extension
   :extensibility
   {:shapes #{:circle :square :line :arrow :dot :polygon}
    :animations #{:create :fade-in :fade-out :transform :rotate :scale}
    :physics #{:particle :spring :gravity :collision}}

   ;; 4. Functional Composition - Build complex from simple
   :composition
   (let [fade-in (fn [obj] {:type :fade-in :target obj})
         rotate (fn [obj angle] {:type :rotate :target obj :angle angle})
         combined (fn [obj angle]
                    (dsl/parallel (fade-in obj) (rotate obj angle)))]
     {:simple-animations ['fade-in 'rotate]
      :composed 'combined})

   ;; 5. Symbolic Math - Emmy integration
   :symbolic-power
   {:expression '(sin (* 2 x))
    :derivative '(* 2 (cos (* 2 x)))
    :evaluation '(fn [x-val] (* 2 (Math/cos (* 2 x-val))))}})
