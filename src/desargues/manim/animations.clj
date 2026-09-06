(ns desargues.manim.animations
  "Manim Animation bindings - all animation types.
   
   Categories:
   - Creation/Destruction (Create, FadeIn, FadeOut, etc.)
   - Transformations (Transform, ReplacementTransform, etc.)
   - Movement (MoveToTarget, MoveAlongPath, etc.)
   - Indication (Indicate, Circumscribe, Flash, etc.)
   - Growing (GrowFromCenter, GrowArrow, etc.)
   - Text animations (Write, AddTextLetterByLetter, etc.)
   - Composition (AnimationGroup, LaggedStart, Succession)"
  (:require [desargues.manim.core :as core]
            [libpython-clj2.python :as py]))

;; ============================================================================
;; Helper Macros
;; ============================================================================

(defmacro ^:private defanim
  "Define an animation constructor."
  [fn-name class-name docstring & [extra-args]]
  `(defn ~fn-name
     ~docstring
     [mobject# & {:as kwargs#}]
     (let [cls# (core/get-class ~class-name)]
       (py/call-attr-kw cls# "__call__" [mobject#] (or kwargs# {})))))

;; ============================================================================
;; Creation Animations
;; ============================================================================

(defn create
  "Create animation - draws the mobject.
   
   Options:
   - :lag_ratio - stagger timing for submobjects
   - :run_time - animation duration"
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "Create")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn uncreate
  "Uncreate animation - reverse of Create."
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "Uncreate")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn draw-border-then-fill
  "Draw the border of a mobject, then fill it."
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "DrawBorderThenFill")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn write
  "Write animation - for text/LaTeX, draws strokes then fills."
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "Write")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn unwrite
  "Unwrite animation - reverse of Write."
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "Unwrite")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn show-partial
  "Show partial mobject (used internally by Create)."
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "ShowPartial")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn show-increasing-subsets
  "Show submobjects one by one."
  [group & {:as kwargs}]
  (let [cls (core/get-class "ShowIncreasingSubsets")]
    (py/call-attr-kw cls "__call__" [group] (or kwargs {}))))

(defn show-submobjects-one-by-one
  "Show each submobject, hiding previous ones."
  [group & {:as kwargs}]
  (let [cls (core/get-class "ShowSubmobjectsOneByOne")]
    (py/call-attr-kw cls "__call__" [group] (or kwargs {}))))

;; ============================================================================
;; Fade Animations
;; ============================================================================

(defn fade-in
  "Fade in animation.
   
   Options:
   - :shift - direction to fade from
   - :scale - scale factor during fade
   - :run_time - duration"
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "FadeIn")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn fade-out
  "Fade out animation.
   
   Options:
   - :shift - direction to fade to
   - :scale - scale factor during fade"
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "FadeOut")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn fade-to-color
  "Fade mobject to a new color."
  [mobject color & {:as kwargs}]
  (let [cls (core/get-class "FadeToColor")]
    (py/call-attr-kw cls "__call__" [mobject color] (or kwargs {}))))

(defn fade-transform
  "Transform one mobject to another with fade."
  [mobject target & {:as kwargs}]
  (let [cls (core/get-class "FadeTransform")]
    (py/call-attr-kw cls "__call__" [mobject target] (or kwargs {}))))

(defn fade-transform-pieces
  "Transform mobject pieces to target with fade."
  [mobject target & {:as kwargs}]
  (let [cls (core/get-class "FadeTransformPieces")]
    (py/call-attr-kw cls "__call__" [mobject target] (or kwargs {}))))

;; ============================================================================
;; Transform Animations
;; ============================================================================

(defn transform
  "Transform one mobject into another.
   
   The source mobject becomes the target.
   
   Options:
   - :path_arc - arc angle for the transformation path
   - :path_func - custom path function
   - :run_time - duration"
  [mobject target & {:as kwargs}]
  (let [cls (core/get-class "Transform")]
    (py/call-attr-kw cls "__call__" [mobject target] (or kwargs {}))))

(defn move-endpoints
  "Animate a Line's endpoints to start/end (line.animate.put_start_and_end_on).

   Options: :run_time :rate_func"
  [line start end & {:as kwargs}]
  (let [builder (py/call-attr-kw (py/get-attr line "animate") "__call__" [] (or kwargs {}))]
    (py/call-attr builder "put_start_and_end_on"
                  (core/->py-list start) (core/->py-list end))))

(defn replacement-transform
  "Transform, replacing source with target in scene.
   
   Unlike Transform, the target becomes the scene mobject."
  [source target & {:as kwargs}]
  (let [cls (core/get-class "ReplacementTransform")]
    (py/call-attr-kw cls "__call__" [source target] (or kwargs {}))))

(defn transform-from-copy
  "Transform a copy of source into target."
  [source target & {:as kwargs}]
  (let [cls (core/get-class "TransformFromCopy")]
    (py/call-attr-kw cls "__call__" [source target] (or kwargs {}))))

(defn clockwise-transform
  "Transform with clockwise rotation."
  [mobject target & {:as kwargs}]
  (let [cls (core/get-class "ClockwiseTransform")]
    (py/call-attr-kw cls "__call__" [mobject target] (or kwargs {}))))

(defn counterclockwise-transform
  "Transform with counterclockwise rotation."
  [mobject target & {:as kwargs}]
  (let [cls (core/get-class "CounterclockwiseTransform")]
    (py/call-attr-kw cls "__call__" [mobject target] (or kwargs {}))))

(defn transform-matching-shapes
  "Transform matching submobjects between source and target.
   
   Useful for transforming similar structures."
  [source target & {:as kwargs}]
  (let [cls (core/get-class "TransformMatchingShapes")]
    (py/call-attr-kw cls "__call__" [source target] (or kwargs {}))))

(defn transform-matching-tex
  "Transform matching LaTeX parts between equations.
   
   Options:
   - :key_map - map old parts to new parts"
  [source target & {:as kwargs}]
  (let [cls (core/get-class "TransformMatchingTex")]
    (py/call-attr-kw cls "__call__" [source target] (or kwargs {}))))

(defn morph-shape
  "Alias for TransformMatchingShapes."
  [source target & {:as kwargs}]
  (apply transform-matching-shapes source target (mapcat identity kwargs)))

;; ============================================================================
;; Movement Animations
;; ============================================================================

(defn move-to-target
  "Move mobject to its .target copy.
   
   Set the target first with (set-target! mobject) or mobject.generate_target()"
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "MoveToTarget")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn move-along-path
  "Move mobject along a path.
   
   path - VMobject defining the path"
  [mobject path & {:as kwargs}]
  (let [cls (core/get-class "MoveAlongPath")]
    (py/call-attr-kw cls "__call__" [mobject path] (or kwargs {}))))

(defn rotate-anim
  "Rotate animation.
   
   Options:
   - :angle - rotation angle (default TAU/2)
   - :axis - rotation axis
   - :about_point - center of rotation"
  [mobject & {:keys [angle] :or {angle Math/PI} :as kwargs}]
  (let [cls (core/get-class "Rotate")]
    (py/call-attr-kw cls "__call__" [mobject] (assoc kwargs :angle angle))))

(defn rotating
  "Continuously rotate a mobject.
   
   Options:
   - :radians - total rotation
   - :axis - rotation axis"
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "Rotating")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn maintain-position-relative-to
  "Keep mobject at same relative position to another."
  [mobject target & {:as kwargs}]
  (let [cls (core/get-class "MaintainPositionRelativeTo")]
    (py/call-attr-kw cls "__call__" [mobject target] (or kwargs {}))))

;; ============================================================================
;; Scaling Animations
;; ============================================================================

(defn scale-in-place
  "Scale mobject in place."
  [mobject scale-factor & {:as kwargs}]
  (let [cls (core/get-class "ScaleInPlace")]
    (py/call-attr-kw cls "__call__" [mobject scale-factor] (or kwargs {}))))

(defn shrink-to-center
  "Shrink mobject to its center point."
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "ShrinkToCenter")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn grow-from-center
  "Grow mobject from its center."
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "GrowFromCenter")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn grow-from-point
  "Grow mobject from a specific point."
  [mobject point & {:as kwargs}]
  (let [cls (core/get-class "GrowFromPoint")]
    (py/call-attr-kw cls "__call__" [mobject (core/->py-list point)] (or kwargs {}))))

(defn grow-from-edge
  "Grow mobject from an edge."
  [mobject edge & {:as kwargs}]
  (let [cls (core/get-class "GrowFromEdge")]
    (py/call-attr-kw cls "__call__" [mobject edge] (or kwargs {}))))

(defn grow-arrow
  "Grow animation specifically for arrows."
  [arrow & {:as kwargs}]
  (let [cls (core/get-class "GrowArrow")]
    (py/call-attr-kw cls "__call__" [arrow] (or kwargs {}))))

(defn spin-in-from-nothing
  "Spin mobject into existence."
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "SpinInFromNothing")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn spiral-in
  "Spiral mobject into position."
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "SpiralIn")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

;; ============================================================================
;; Indication Animations
;; ============================================================================

(defn indicate
  "Indicate a mobject (flash and scale).
   
   Options:
   - :color - indication color
   - :scale_factor - how much to scale"
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "Indicate")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn circumscribe
  "Draw a shape around a mobject.
   
   Options:
   - :shape - Circle, Rectangle, etc.
   - :color"
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "Circumscribe")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn flash
  "Flash effect at a point.
   
   point - [x y z] location
   
   Options:
   - :color
   - :line_length
   - :num_lines"
  [point & {:as kwargs}]
  (let [cls (core/get-class "Flash")]
    (py/call-attr-kw cls "__call__" [(core/->py-list point)] (or kwargs {}))))

(defn focus-on
  "Focus camera/attention on a mobject or point."
  [mobject-or-point & {:as kwargs}]
  (let [cls (core/get-class "FocusOn")
        arg (if (sequential? mobject-or-point)
              (core/->py-list mobject-or-point)
              mobject-or-point)]
    (py/call-attr-kw cls "__call__" [arg] (or kwargs {}))))

(defn wiggle
  "Wiggle a mobject."
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "Wiggle")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn show-passing-flash
  "Show a flash passing along a path."
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "ShowPassingFlash")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn apply-wave
  "Apply a wave effect to a mobject."
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "ApplyWave")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

(defn broadcast
  "Broadcast animation - expands rings from a point."
  [point & {:as kwargs}]
  (let [cls (core/get-class "Broadcast")]
    (py/call-attr-kw cls "__call__" [(core/->py-list point)] (or kwargs {}))))

;; ============================================================================
;; Text Animations
;; ============================================================================

(defn add-text-letter-by-letter
  "Add text one letter at a time."
  [text-mobject & {:as kwargs}]
  (let [cls (core/get-class "AddTextLetterByLetter")]
    (py/call-attr-kw cls "__call__" [text-mobject] (or kwargs {}))))

(defn remove-text-letter-by-letter
  "Remove text one letter at a time."
  [text-mobject & {:as kwargs}]
  (let [cls (core/get-class "RemoveTextLetterByLetter")]
    (py/call-attr-kw cls "__call__" [text-mobject] (or kwargs {}))))

(defn add-text-word-by-word
  "Add text one word at a time."
  [text-mobject & {:as kwargs}]
  (let [cls (core/get-class "AddTextWordByWord")]
    (py/call-attr-kw cls "__call__" [text-mobject] (or kwargs {}))))

(defn type-with-cursor
  "Type text with a cursor animation."
  [text-mobject & {:as kwargs}]
  (let [cls (core/get-class "TypeWithCursor")]
    (py/call-attr-kw cls "__call__" [text-mobject] (or kwargs {}))))

(defn untype-with-cursor
  "Remove typed text with cursor."
  [text-mobject & {:as kwargs}]
  (let [cls (core/get-class "UntypeWithCursor")]
    (py/call-attr-kw cls "__call__" [text-mobject] (or kwargs {}))))

;; ============================================================================
;; Number Animations
;; ============================================================================

(defn change-decimal-to-value
  "Animate a DecimalNumber to a new value."
  [decimal-number value & {:as kwargs}]
  (let [cls (core/get-class "ChangeDecimalToValue")]
    (py/call-attr-kw cls "__call__" [decimal-number value] (or kwargs {}))))

(defn changing-decimal
  "Continuously update a DecimalNumber during animation.
   
   decimal-number - the DecimalNumber mobject
   number-update-func - function returning the new value"
  [decimal-number number-update-func & {:as kwargs}]
  (let [cls (core/get-class "ChangingDecimal")]
    (py/call-attr-kw cls "__call__" [decimal-number number-update-func] (or kwargs {}))))

;; ============================================================================
;; Function/Homotopy Animations
;; ============================================================================

(defn apply-function
  "Apply a function to all points of a mobject."
  [mobject function & {:as kwargs}]
  (let [cls (core/get-class "ApplyFunction")]
    (py/call-attr-kw cls "__call__" [function mobject] (or kwargs {}))))

(defn apply-pointwise-function
  "Apply a function pointwise to mobject points."
  [mobject function & {:as kwargs}]
  (let [cls (core/get-class "ApplyPointwiseFunction")]
    (py/call-attr-kw cls "__call__" [function mobject] (or kwargs {}))))

(defn apply-complex-function
  "Apply a complex function to mobject points."
  [mobject function & {:as kwargs}]
  (let [cls (core/get-class "ApplyComplexFunction")]
    (py/call-attr-kw cls "__call__" [mobject function] (or kwargs {}))))

(defn apply-matrix
  "Apply a matrix transformation."
  [mobject matrix & {:as kwargs}]
  (let [cls (core/get-class "ApplyMatrix")]
    (py/call-attr-kw cls "__call__" [mobject (core/->py-list matrix)] (or kwargs {}))))

(defn homotopy
  "Apply a homotopy (continuous deformation).
   
   homotopy-func - function (x, y, z, t) -> (x', y', z')"
  [mobject homotopy-func & {:as kwargs}]
  (let [cls (core/get-class "Homotopy")]
    (py/call-attr-kw cls "__call__" [mobject homotopy-func] (or kwargs {}))))

(defn complex-homotopy
  "Apply a complex homotopy."
  [mobject homotopy-func & {:as kwargs}]
  (let [cls (core/get-class "ComplexHomotopy")]
    (py/call-attr-kw cls "__call__" [mobject homotopy-func] (or kwargs {}))))

(defn phase-flow
  "Animate flow along a vector field."
  [function mobject & {:as kwargs}]
  (let [cls (core/get-class "PhaseFlow")]
    (py/call-attr-kw cls "__call__" [function mobject] (or kwargs {}))))

;; ============================================================================
;; Updater-based Animations
;; ============================================================================

(defn update-from-func
  "Create animation from an updater function.
   
   updater - function taking (mobject) or (mobject, dt)"
  [mobject updater & {:as kwargs}]
  (let [cls (core/get-class "UpdateFromFunc")]
    (py/call-attr-kw cls "__call__" [mobject updater] (or kwargs {}))))

(defn update-from-alpha-func
  "Create animation from a function of alpha (0-1 progress).
   
   updater - function taking (mobject, alpha)"
  [mobject updater & {:as kwargs}]
  (let [cls (core/get-class "UpdateFromAlphaFunc")]
    (py/call-attr-kw cls "__call__" [mobject updater] (or kwargs {}))))

(defn restore
  "Restore mobject to saved state."
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "Restore")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

;; ============================================================================
;; Composition Animations
;; ============================================================================

(defn animation-group
  "Group multiple animations to play together.
   
   Options:
   - :lag_ratio - stagger between animations (0 = together, 1 = sequential)"
  [& animations-and-kwargs]
  (let [[anims kwargs] (if (keyword? (first (take-last 2 animations-and-kwargs)))
                         [(butlast (butlast animations-and-kwargs))
                          (apply hash-map (take-last 2 animations-and-kwargs))]
                         [animations-and-kwargs {}])
        cls (core/get-class "AnimationGroup")]
    (py/call-attr-kw cls "__call__" (vec anims) kwargs)))

(defn lagged-start
  "Play animations with a staggered start.
   
   Options:
   - :lag_ratio - delay between each animation start (default 0.05)"
  [& animations-and-kwargs]
  (let [[anims kwargs] (if (keyword? (first (take-last 2 animations-and-kwargs)))
                         [(butlast (butlast animations-and-kwargs))
                          (apply hash-map (take-last 2 animations-and-kwargs))]
                         [animations-and-kwargs {}])
        cls (core/get-class "LaggedStart")]
    (py/call-attr-kw cls "__call__" (vec anims) kwargs)))

(defn lagged-start-map
  "Apply an animation constructor to multiple mobjects with stagger.
   
   anim-class - animation class (e.g., FadeIn)
   mobjects - sequence of mobjects
   
   Options:
   - :lag_ratio"
  [anim-class mobjects & {:as kwargs}]
  (let [cls (core/get-class "LaggedStartMap")]
    (py/call-attr-kw cls "__call__" [anim-class (core/->py-list mobjects)] (or kwargs {}))))

(defn succession
  "Play animations one after another."
  [& animations-and-kwargs]
  (let [[anims kwargs] (if (keyword? (first (take-last 2 animations-and-kwargs)))
                         [(butlast (butlast animations-and-kwargs))
                          (apply hash-map (take-last 2 animations-and-kwargs))]
                         [animations-and-kwargs {}])
        cls (core/get-class "Succession")]
    (py/call-attr-kw cls "__call__" (vec anims) kwargs)))

(defn wait-anim
  "Wait animation (pause scene).
   
   duration - wait time in seconds"
  [& {:keys [duration] :or {duration 1} :as kwargs}]
  (let [cls (core/get-class "Wait")]
    (py/call-attr-kw cls "__call__" [] (assoc kwargs :duration duration))))

;; ============================================================================
;; Other Animations
;; ============================================================================

(defn cyclic-replace
  "Cyclically swap positions of mobjects."
  [& mobjects]
  (let [cls (core/get-class "CyclicReplace")]
    (apply cls mobjects)))

(defn swap
  "Swap two mobjects."
  [mob1 mob2 & {:as kwargs}]
  (let [cls (core/get-class "Swap")]
    (py/call-attr-kw cls "__call__" [mob1 mob2] (or kwargs {}))))

(defn change-speed
  "Change the speed of an animation.
   
   Options:
   - :speedinfo - dict mapping time ranges to speed factors"
  [animation & {:as kwargs}]
  (let [cls (core/get-class "ChangeSpeed")]
    (py/call-attr-kw cls "__call__" [animation] (or kwargs {}))))

(defn blink
  "Blink animation for eyes/dots."
  [mobject & {:as kwargs}]
  (let [cls (core/get-class "Blink")]
    (py/call-attr-kw cls "__call__" [mobject] (or kwargs {}))))

;; ============================================================================
;; Animation Helpers
;; ============================================================================

(defn set-run-time
  "Set the run time of an animation. Returns the animation."
  [animation time]
  (py/set-attr! animation "run_time" time)
  animation)

(defn set-rate-func
  "Set the rate function of an animation.
   
   Common rate funcs: linear, smooth, rush_into, rush_from,
   slow_into, double_smooth, there_and_back, wiggle"
  [animation rate-func]
  (py/set-attr! animation "rate_func" rate-func)
  animation)

(defn get-rate-func
  "Get a rate function by name."
  [name]
  (core/get-constant name))

;; Common rate functions
(def linear (delay (core/get-constant "linear")))
(def smooth (delay (core/get-constant "smooth")))
(def rush-into (delay (core/get-constant "rush_into")))
(def rush-from (delay (core/get-constant "rush_from")))
(def slow-into (delay (core/get-constant "slow_into")))
(def double-smooth (delay (core/get-constant "double_smooth")))
(def there-and-back (delay (core/get-constant "there_and_back")))
(def there-and-back-with-pause (delay (core/get-constant "there_and_back_with_pause")))
(def running-start (delay (core/get-constant "running_start")))
(def wiggle-rate (delay (core/get-constant "wiggle")))
(def lingering (delay (core/get-constant "lingering")))
(def exponential-decay (delay (core/get-constant "exponential_decay")))

;; ============================================================================
;; Target-based Animation Helpers
;; ============================================================================

(defn generate-target!
  "Generate a target copy for a mobject.
   
   Use this before move-to-target animation."
  [mobject]
  (py/call-attr mobject "generate_target")
  mobject)

(defn save-state!
  "Save the current state of a mobject.
   
   Use with restore animation."
  [mobject]
  (py/call-attr mobject "save_state")
  mobject)

(defn get-target
  "Get the target of a mobject."
  [mobject]
  (py/get-attr mobject "target"))
