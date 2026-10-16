# Desargues Video DSL

A Clojure Domain-Specific Language for creating mathematical animations, inspired by 3Blue1Brown's Manim videos.

## Overview

This DSL provides high-level abstractions for creating mathematical animations in Clojure, leveraging:
- **Protocol-based design** following DDD/SOLID principles
- **Emmy** for symbolic mathematics
- **Manim Community** for rendering (via libpython-clj)
- **Clojure's LISP power** for composability and extensibility

## Architecture

```
desargues.videos/
├── timeline.clj      # Animation composition and timing
├── math_objects.clj  # Mathematical objects (axes, graphs)
├── typography.clj    # LaTeX rendering and equation morphing
├── characters.clj    # Pi creature character system
└── scenes/
    ├── pendulum.clj  # Pendulum demonstration scenes
    ├── spring_mass.clj # Spring-mass SHM scenes
    └── max_rand.clj  # Random variable CDF visualization
```

## Quick Start

```clojure
(require '[desargues.videos.timeline :as tl]
         '[desargues.videos.math-objects :as mo]
         '[desargues.videos.typography :as typo])

;; Create a simple scene
(let [eq (typo/tex "e^{i\\pi} + 1 = 0")
      axes (mo/axes [-3 3] [-2 2])
      
      timeline
      (tl/timeline
       [:t 0 (tl/fade-in eq)]
       [:t 1 (tl/write axes)]
       [:t 2 (tl/wait 1)])]
  
  (tl/render-timeline timeline))
```

## Core Modules

### 1. Timeline DSL (`timeline.clj`)

Declarative animation composition with cursor-based timing.

#### Animation Combinators

```clojure
;; Sequential animations (one after another)
(tl/>> anim1 anim2 anim3)

;; Parallel animations (all at once)
(tl/|| anim1 anim2 anim3)

;; Lagged start (staggered, overlapping)
(tl/|> 0.1 anim1 anim2 anim3)  ; 0.1 lag ratio
```

#### Timeline Syntax

```clojure
(tl/timeline
 [:t 0 (tl/fade-in obj)]           ; At t=0
 [:t 1 (tl/play anim :run-time 2)] ; At t=1
 [:cursor (tl/wait 0.5)]           ; After previous
 [:cursor (tl/>> a b c)])          ; Sequential after cursor
```

#### Rate Functions

```clojure
tl/smooth            ; Default smooth interpolation
tl/linear            ; Linear interpolation
tl/there-and-back    ; Go there, come back
tl/rush-into         ; Speed up at start
tl/rush-from         ; Slow down at end
(tl/squish-rate-func tl/smooth 0 0.5) ; Squish into [0, 0.5]
```

### 2. Mathematical Objects DSL (`math_objects.clj`)

Create coordinate systems, graphs, and geometric objects.

#### Coordinate Systems

```clojure
;; 2D axes
(mo/axes [x-min x-max x-step] [y-min y-max y-step]
         :width 6 :height 4)

;; Number plane with grid
(mo/number-plane [-8 8] [-4 4])

;; 3D axes
(mo/three-d-axes [0 1 0.1] [0 1 0.1] [0 1 0.1])
```

#### Coordinate Conversion

```clojure
;; coords -> point (c2p)
(mo/c2p axes [2 3])        ; [x, y] -> screen position

;; point -> coords (p2c)
(mo/p2c axes [1.5 2.0 0])  ; screen position -> [x, y]

;; number to point on axis
(mo/n2p axis 3.5)          ; value -> screen position
```

#### Graphs and Curves

```clojure
;; Function graph
(mo/get-graph axes (fn [x] (* x x)) :color :blue)

;; Parametric curve
(mo/parametric-curve
 (fn [t] [(Math/cos t) (Math/sin t)])
 [0 (* 2 Math/PI)])

;; Phase space trajectory
(mo/get-phase-trajectory axes state-fn dt total-time)
```

### 3. Physics (moved to `desargues.physics`)

The physics is its own bounded context now, `desargues.physics.*` in
src/desargues/physics/ (see its README.md); it was `desargues.videos.physics`
and `desargues.videos.lagrangian`, and its tests live in test/desargues/physics/.
The sketch below predates the move; the current names are `make-pendulum`,
`make-spring-mass`, `evolve` and `rk4`, all on the `desargues.physics` facade.

Simulate physical systems with ODE integration.

#### Protocols

```clojure
(defprotocol PhysicalSystem
  (get-state [this])
  (set-state [this state])
  (derivatives [this state])
  (state-labels [this]))

(defprotocol Integrator
  (step [this system dt]))
```

#### Pendulum

```clojure
(def pendulum
  (create-pendulum
   :length 3.0
   :gravity 10.0
   :damping 0.1
   :initial-theta (/ Math/PI 6)
   :initial-omega 0))

;; Simulate
(step-system pendulum rk4-integrator 0.01)
```

#### Spring-Mass

```clojure
(def spring
  (create-spring-mass
   :mass 1.0
   :k 10.0
   :damping 0.2
   :initial-x 2.0
   :initial-v 0))
```

#### Integrators

```clojure
euler-integrator   ; First-order Euler method
rk4-integrator     ; Fourth-order Runge-Kutta
```

### 4. Typography DSL (`typography.clj`)

LaTeX rendering with color mapping and morphing.

#### Basic Typography

```clojure
;; LaTeX math
(typo/tex "\\frac{d}{dx}\\sin(x) = \\cos(x)")

;; Mixed text and math
(typo/tex-text "The derivative of $\\sin(x)$ is $\\cos(x)$")

;; Plain text
(mo/text "Hello, World!" :font-size 48)
```

#### Text-to-Color Mapping (t2c)

```clojure
;; Map substrings to colors
(typo/tex "f(x) = x^2" 
          :t2c {"x" :blue "f" :yellow})

;; Predefined schemes
typo/t2c-time-freq   ; {t: blue, s: yellow}
typo/t2c-derivatives ; {f: blue, f': red, f'': green}
```

#### Equation Morphing

```clojure
;; Transform matching parts
(typo/transform-matching-tex source target
                             :key-map {"=" "\\le"}
                             :path-arc (/ Math/PI 2))

;; Replace changing numbers
(typo/replace-changing-number tex "0.00" tracker)
```

### 5. Characters DSL (`characters.clj`)

Pi creature animations for educational videos.

#### Creating Characters

```clojure
;; Named characters
(char/randolph :position :dl)  ; Blue, down-left
(char/mortimer :position :dr)  ; Grey, down-right

;; Custom pi creature
(char/pi-creature :color :teal :position :center)
```

#### Character Modes

```clojure
;; Change expression
(char/change randy :confused target-object)
(char/change randy :happy)
(char/change randy :thinking)

;; Available modes
:plain :happy :sad :confused :thinking :speaking
:surprised :erm :hooray :pondering :angry :sassy
```

#### Animations

```clojure
(char/blink randy)           ; Eye blink
(char/look-at randy target)  ; Look at object
(char/says randy "Hello!")   ; Speech bubble
(char/thinks randy "Hmm...")  ; Thought bubble
```

#### Teacher-Students Scene

```clojure
(def ts-scene
  (char/teacher-students-scene
   :teacher-color :grey
   :student-colors [:blue :yellow :teal]))

(char/teacher-says ts-scene "Let me explain...")
(char/student-says ts-scene 0 "I understand!")
```

## Scene Examples

### Pendulum Scene

```clojure
(require '[desargues.videos.scenes.pendulum :as pend])

;; Create scene with all components
(def scene (pend/introduce-pendulum-scene))

;; Access components
(:pendulum scene)
(:timeline scene)

;; Render
(pend/render-introduce-pendulum)
```

### Spring-Mass Scene

```clojure
(require '[desargues.videos.scenes.spring-mass :as sm])

;; Create spring-mass demonstration
(def scene (sm/basic-spring-scene))

;; Render with tracking graphs
(sm/render-basic-spring)
```

### Random Variable CDF

```clojure
(require '[desargues.videos.scenes.max-rand :as mr])

;; Visualize max(rand(), rand()) distribution
(def scene (mr/visualize-max-of-pair-cdf-scene))

;; Render with 3D extension
(mr/render-visualize-cdf)
```

## Design Patterns

### 1. Always-Redraw Pattern

For objects that update based on other values:

```clojure
(mo/always-redraw
 (fn []
   (mo/line start-point (get-current-end))))
```

### 2. ValueTracker Pattern

Atoms with watchers for reactive updates:

```clojure
(def tracker (atom 0.5))

(add-watch tracker :update-dependent
           (fn [_ _ _ new-val]
             (update-dependent-object new-val)))

;; Animate the tracker
(tl/animate tracker :set-value 1.0 :run-time 2)
```

### 3. f-always Pattern

Continuous method binding:

```clojure
(mo/f-always mobject :move-to reference-point-fn)
;; Equivalent to: mobject.add_updater(lambda m: m.move_to(ref()))
```

### 4. Protocol-Based Extensibility

Extend existing types with new capabilities:

```clojure
(extend-type MyCustomObject
  mo/Mobject
  (get-center [this] (:center this))
  (set-color [this c] (assoc this :color c)))
```

## Common Patterns from 3B1B Videos

### 1. Equation Development

```clojure
(tl/timeline
 [:t 0 (tl/write eq1)]
 [:t 2 (typo/transform-matching-tex eq1 eq2)]
 [:t 4 (tl/indicate (typo/get-part eq2 "key-term"))])
```

### 2. Graph Animation

```clojure
(let [tracker (mo/value-tracker 0)
      graph (mo/always-redraw
             #(mo/get-graph axes f :x-range [0 @tracker]))]
  (tl/play (tl/animate tracker :set-value 5 :run-time 3)))
```

### 3. Phase Space Visualization

```clojure
(let [trajectory (mo/get-phase-trajectory phase-axes state-fn dt total-time)
      tracing-tail (mo/tracing-tail trajectory)]
  (tl/play (mo/move-along-path dot trajectory :run-time 10)))
```

### 4. Side-by-Side Comparison

```clojure
(let [left-group (mo/vgroup [obj1 obj2])
      right-group (mo/vgroup [obj3 obj4])]
  (mo/arrange-in-grid [left-group right-group] :cols 2 :buff 1))
```

## Integration with Emmy

```clojure
(require '[emmy.core :as e])

;; Symbolic differentiation
(defn f [x] (e/sin (e/* x x)))
(def df (e/D f))

;; Convert to LaTeX
(def f-latex (e/->TeX (f 'x)))
(def df-latex (e/->TeX (df 'x)))

;; Animate the derivative
(let [f-eq (typo/tex f-latex)
      df-eq (typo/tex df-latex)]
  (tl/timeline
   [:t 0 (tl/write f-eq)]
   [:t 2 (typo/transform-matching-tex f-eq df-eq)]))
```

## Best Practices

1. **Use declarative timelines** - Describe what happens, not how
2. **Compose small animations** - Build complex from simple
3. **Leverage protocols** - Extend without modifying
4. **Keep scenes pure** - Functions return data structures
5. **Separate concerns** - Physics, visuals, and timing in different namespaces

## Future Directions

- [ ] Full Manim backend integration
- [ ] GPU-accelerated rendering
- [ ] Live preview mode
- [ ] Scene version control
- [ ] Collaborative editing
