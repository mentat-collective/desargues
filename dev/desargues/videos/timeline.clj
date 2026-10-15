(ns desargues.videos.timeline
  "Enhanced Timeline DSL for animation composition.

   This namespace provides a declarative, composable approach to building
   animation timelines. Inspired by 3Blue1Brown's animation patterns:
   - LaggedStart for staggered animations
   - Rate functions (smooth, linear, there_and_back)
   - Parallel and sequential composition
   - Time spans for sub-animations

   ## Design Philosophy

   Animations are DATA, not functions. This enables:
   - Serialization (save/load scenes)
   - Optimization (merge adjacent transforms)
   - Debugging (inspect timeline structure)
   - Composition (timelines compose like monoids)

   ## Example Usage

   ```clojure
   ;; Build a timeline declaratively
   (-> (timeline)
       (at 0 (create circle))
       (then (write equation))
       (wait 2)
       (then (lagged-start [obj1 obj2 obj3] fade-in {:lag-ratio 0.1}))
       (then (transform circle square {:run-time 2})))
   ```"
  (:require [clojure.spec.alpha :as s]))

;; =============================================================================
;; Rate Functions - Easing curves
;; =============================================================================

(defn linear [t] t)

(defn smooth
  "Smooth easing: 3t² - 2t³ (ease in-out)"
  [t]
  (let [t (max 0 (min 1 t))]
    (- (* 3 t t) (* 2 t t t))))

(defn rush-into
  "Start fast, slow down at end"
  [t]
  (let [t (max 0 (min 1 t))]
    (* 2 t (- 1 (/ t 2)))))

(defn rush-from
  "Start slow, speed up"
  [t]
  (- 1 (rush-into (- 1 t))))

(defn there-and-back
  "Go there and back: 0->1->0"
  [t]
  (let [t (max 0 (min 1 t))]
    (if (< t 0.5)
      (* 2 t)
      (* 2 (- 1 t)))))

(defn there-and-back-with-pause
  "Go there, pause, come back"
  [t]
  (let [t (max 0 (min 1 t))]
    (cond
      (< t 0.33) (* 3 t)
      (< t 0.66) 1.0
      :else (* 3 (- 1 t)))))

(defn double-smooth
  "Smooth twice - extra smooth"
  [t]
  (smooth (smooth t)))

(defn squish-rate-func
  "Squish a rate function to run between alpha and alpha+stretch"
  [func alpha stretch]
  (fn [t]
    (let [t (max 0 (min 1 t))]
      (if (or (< t alpha) (> t (+ alpha stretch)))
        (if (< t alpha) 0.0 1.0)
        (func (/ (- t alpha) stretch))))))

(defn wiggle
  "Sine-based wiggle"
  [t]
  (let [t (max 0 (min 1 t))]
    (* (Math/sin (* Math/PI t))
       (Math/sin (* Math/PI 2 t)))))

;; =============================================================================
;; Animation Records
;; =============================================================================

(defrecord Animation
           [type ; :create :transform :fade-in :fade-out :write :show-creation etc
            target ; Object or sequence of objects
            options]) ; {:run-time 1.0 :rate-func smooth :lag-ratio 0 ...}

(defn animation
  "Create an animation record."
  [type target & {:as opts}]
  (->Animation type target (merge {:run-time 1.0 :rate-func smooth} opts)))

;; Animation constructors
(defn create [target & opts] (apply animation :create target opts))
(defn fade-in [target & opts] (apply animation :fade-in target opts))
(defn fade-out [target & opts] (apply animation :fade-out target opts))
(defn write-anim [target & opts] (apply animation :write target opts))

(defn write
  "Alias for write-anim - write text animation."
  [target & opts]
  (apply write-anim target opts))

(defn add
  "Add mobjects to the scene (instant, no animation)."
  [& mobjects]
  {:type :add :targets mobjects})

(defn add-to-scene
  "Add mobjects to the scene."
  [& mobjects]
  {:type :add-to-scene :targets mobjects})

(defn animate
  "Create an animation builder for a mobject."
  [mobject]
  {:type :animate-builder :target mobject :animations []})

(defn frame
  "Create a frame marker in the timeline."
  [& {:keys [label] :or {label nil}}]
  {:type :frame :label label})

(declare lagged-start)

(defn lagged-start-map
  "Apply an animation to each element in a collection with lag."
  [anim-fn targets & {:keys [lag-ratio run-time] :or {lag-ratio 0.5 run-time 1.0}}]
  (lagged-start (map anim-fn targets) :lag-ratio lag-ratio :run-time run-time))

(defn render-timeline
  "Render a timeline to video."
  [tl & {:keys [quality preview output-file]
         :or {quality :high preview false}}]
  {:type :render-command :timeline tl :quality quality :preview preview :output-file output-file})

(defn restore
  "Restore a mobject to its saved state."
  [target]
  {:type :restore :target target})

(declare sequential show-creation fade-out)

(defn draw-border-then-fill
  "Draw the border of a mobject then fill it in."
  [target & opts]
  (apply animation :draw-border-then-fill target opts))

(defn fade-in-from-down
  "Fade in a mobject from below."
  [target & opts]
  (apply animation :fade-in-from-down target opts))

(defn v-fade-in
  "Vertically fade in a mobject."
  [target & opts]
  (apply animation :v-fade-in target opts))

(defn v-fade-out
  "Vertically fade out a mobject."
  [target & opts]
  (apply animation :v-fade-out target opts))

(defn set-opacity
  "Set the opacity of a mobject (instant)."
  [target opacity]
  {:type :set-opacity :target target :opacity opacity})

(defn show-creation-then-fade-out
  "Show creation animation then fade out."
  [target & {:keys [run-time fade-time] :or {run-time 1.0 fade-time 1.0}}]
  (sequential (show-creation target :run-time run-time)
              (fade-out target :run-time fade-time)))

(defn show-creation-then-fade-around
  "Show creation then fade, leaving original visible."
  [target & opts]
  (apply animation :show-creation-then-fade-around target opts))

(defn show-creation-then-destruction-around
  "Show creation then destruction animation around a target."
  [target & opts]
  (apply animation :show-creation-then-destruction-around target opts))

(defn frame-reorient
  "Reorient the camera/frame."
  [& {:keys [theta phi gamma run-time] :or {run-time 1.0}}]
  {:type :frame-reorient :theta theta :phi phi :gamma gamma :run-time run-time})
(defn show-creation [target & opts] (apply animation :show-creation target opts))
(defn transform [from to & opts] (apply animation :transform [from to] opts))
(defn replacement-transform [from to & opts] (apply animation :replacement-transform [from to] opts))
(defn move-to [target pos & opts] (apply animation :move-to [target pos] opts))
(defn rotate-anim [target angle & opts] (apply animation :rotate [target angle] opts))
(defn scale-anim [target factor & opts] (apply animation :scale [target factor] opts))
(defn indicate [target & opts] (apply animation :indicate target opts))
(defn circumscribe [target & opts] (apply animation :circumscribe target opts))
(defn grow-arrow [target & opts] (apply animation :grow-arrow target opts))
(defn grow-from-center [target & opts] (apply animation :grow-from-center target opts))
(defn grow-from-point [target point & opts] (apply animation :grow-from-point [target point] opts))
(defn uncreate [target & opts] (apply animation :uncreate target opts))

;; =============================================================================
;; Animation Combinators - The Heart of Composition
;; =============================================================================

(defrecord AnimationGroup
           [type ; :sequential :parallel :lagged
            animations ; vector of animations
            options]) ; {:lag-ratio 0.5 ...}

(defn sequential
  "Play animations one after another.
   Total duration = sum of individual durations."
  [& animations]
  (->AnimationGroup :sequential (vec animations) {}))

(defn parallel
  "Play animations simultaneously.
   Total duration = max of individual durations."
  [& animations]
  (->AnimationGroup :parallel (vec animations) {}))

(defn lagged-start
  "Play animations with staggered start times.
   lag-ratio determines overlap (0 = sequential, 1 = parallel).

   Example: (lagged-start [a b c] {:lag-ratio 0.3})
   - Animation 'a' starts at t=0
   - Animation 'b' starts at t=0.3*duration_a
   - Animation 'c' starts at t=0.3*(duration_a + 0.3*duration_a)"
  [animations & {:keys [lag-ratio run-time remover]
                 :or {lag-ratio 0.5}}]
  (->AnimationGroup :lagged (vec animations) {:lag-ratio lag-ratio
                                              :run-time run-time
                                              :remover remover}))

(defn succession
  "Like sequential but with smooth transitions between animations."
  [& animations]
  (->AnimationGroup :succession (vec animations) {}))

;; Operators for DSL
(def >> "Sequential composition operator" sequential)
(def || "Parallel composition operator" parallel)
(defn |> [lag-ratio & anims] (lagged-start anims :lag-ratio lag-ratio))

;; =============================================================================
;; Timeline Record
;; =============================================================================

(defrecord TimelineEvent
           [time ; Start time in seconds
            animation ; Animation or AnimationGroup
            label]) ; Optional label for referencing

(defrecord Timeline
           [events ; Sorted vector of TimelineEvent
            cursor ; Current time position
            objects ; Map of id -> object definitions
            camera]) ; Camera state

(defn timeline
  "Create a new empty timeline."
  []
  (->Timeline [] 0.0 {} {:position [0 0 7] :target [0 0 0]}))

(defn at
  "Add animation at specific time."
  [tl time animation & {:keys [label]}]
  (let [event (->TimelineEvent time animation label)]
    (update tl :events
            (fn [events]
              (vec (sort-by :time (conj events event)))))))

(defn then
  "Add animation after current cursor position."
  [tl animation & {:keys [label]}]
  (let [cursor (:cursor tl)
        event (->TimelineEvent cursor animation label)
        duration (get-in animation [:options :run-time] 1.0)
        new-cursor (+ cursor duration)]
    (-> tl
        (update :events conj event)
        (assoc :cursor new-cursor))))

(defn wait
  "Advance cursor without animation."
  [tl seconds]
  (update tl :cursor + seconds))

(defn play
  "Alias for then - matches 3b1b's self.play() pattern"
  [tl animation & opts]
  (apply then tl animation opts))

(defn add-object
  "Register an object in the timeline's object registry."
  [tl id object]
  (assoc-in tl [:objects id] object))

(defn get-object
  "Get an object from the timeline's registry."
  [tl id]
  (get-in tl [:objects id]))

;; =============================================================================
;; Timeline Queries
;; =============================================================================

(defn duration
  "Total duration of timeline."
  [tl]
  (if (empty? (:events tl))
    0.0
    (let [last-event (last (:events tl))
          last-duration (get-in last-event [:animation :options :run-time] 1.0)]
      (+ (:time last-event) last-duration))))

(defn events-at
  "Get all events active at time t."
  [tl t]
  (filter (fn [event]
            (let [start (:time event)
                  dur (get-in event [:animation :options :run-time] 1.0)
                  end (+ start dur)]
              (and (<= start t) (< t end))))
          (:events tl)))

(defn animation-progress
  "Get progress (0-1) of an animation at time t."
  [event t]
  (let [start (:time event)
        dur (get-in event [:animation :options :run-time] 1.0)
        rate-func (get-in event [:animation :options :rate-func] smooth)
        raw-progress (/ (- t start) dur)]
    (rate-func (max 0 (min 1 raw-progress)))))

;; =============================================================================
;; High-level Scene Patterns
;; =============================================================================

(defn transform-matching-tex
  "Transform LaTeX with matching terms highlighted.
   Like 3b1b's TransformMatchingTex."
  [from to & {:keys [key-map matched-keys path-arc]
              :or {path-arc 0}}]
  (animation :transform-matching-tex [from to]
             :key-map key-map
             :matched-keys matched-keys
             :path-arc path-arc))

(defn focus-on
  "Focus camera on an object."
  [target & {:keys [run-time] :or {run-time 1.0}}]
  (animation :focus-on target :run-time run-time))

(defn surrounding-rectangle
  "Create and show a surrounding rectangle."
  [target & {:keys [color buff] :or {color :yellow buff 0.1}}]
  (animation :surrounding-rectangle target :color color :buff buff))

(defn blink
  "Blink animation for Pi creatures."
  [creature]
  (animation :blink creature :run-time 0.2))

;; =============================================================================
;; Updaters - Continuous Updates
;; =============================================================================

(defrecord Updater
           [target ; Object to update
            update-fn ; (fn [obj dt] -> obj')
            condition]) ; Optional (fn [obj] -> boolean) to stop updater

(defn add-updater
  "Add a continuous updater to an object."
  [target update-fn & {:keys [condition]}]
  (->Updater target update-fn condition))

(defn always-redraw
  "Create an object that redraws every frame.
   Like 3b1b's always_redraw(lambda: ...)."
  [constructor-fn]
  {:type :always-redraw
   :constructor constructor-fn})

(defn f-always
  "Fluent updater syntax: obj.f_always.method(args)
   Returns an updater that calls method on obj every frame."
  [target method-kw & args]
  (add-updater target
               (fn [obj _dt]
                 (apply (get obj method-kw) args))))

;; =============================================================================
;; Value Trackers - Animatable Parameters
;; =============================================================================

(defrecord ValueTracker
           [value ; Current value
            listeners]) ; Functions to call on value change

(defn value-tracker
  "Create a value tracker (like 3b1b's ValueTracker)."
  [initial-value]
  (->ValueTracker initial-value []))

(defn get-value [tracker] (:value tracker))

(defn set-value [tracker new-value]
  (let [new-tracker (assoc tracker :value new-value)]
    (doseq [listener (:listeners tracker)]
      (listener new-value))
    new-tracker))

(defn add-listener [tracker listener-fn]
  (update tracker :listeners conj listener-fn))

(defn animate-value
  "Create animation that changes a tracker's value."
  [tracker target-value & {:keys [run-time rate-func]
                           :or {run-time 1.0 rate-func smooth}}]
  (animation :animate-value [tracker target-value]
             :run-time run-time
             :rate-func rate-func))

;; =============================================================================
;; Complex Value Tracker (for complex plane animations)
;; =============================================================================

(defrecord ComplexValueTracker
           [real imag])

(defn complex-tracker [initial]
  (->ComplexValueTracker (or (:real initial) (first initial) 0)
                         (or (:imag initial) (second initial) 0)))

(defn get-complex [tracker]
  [(:real tracker) (:imag tracker)])

(defn set-complex [tracker [r i]]
  (assoc tracker :real r :imag i))

;; =============================================================================
;; Tracing Tails - Path Tracing
;; =============================================================================

(defrecord TracingTail
           [point-fn ; Function returning current point
            time-traced ; How long to keep trace
            stroke-color
            stroke-width
            fade-factor])

(defn tracing-tail
  "Create a tracing tail that follows a point.
   Like 3b1b's TracingTail."
  [point-fn & {:keys [time-traced stroke-color stroke-width fade-factor]
               :or {time-traced 2.0
                    stroke-color :yellow
                    stroke-width 2
                    fade-factor 0.95}}]
  (->TracingTail point-fn time-traced stroke-color stroke-width fade-factor))

;; =============================================================================
;; Tracking Dots - Point Accumulation
;; =============================================================================

(defrecord TrackingDots
           [point-fn
            fade-factor
            radius
            color])

(defn tracking-dots
  "Accumulate dots at a point over time.
   Like 3b1b's TrackingDots."
  [point-fn & {:keys [fade-factor radius color]
               :or {fade-factor 0.95 radius 0.25 color :yellow}}]
  (->TrackingDots point-fn fade-factor radius color))

;; =============================================================================
;; Randomize Animation
;; =============================================================================

(defn randomize
  "Create animation that randomizes a value tracker.
   Like 3b1b's Randomize animation."
  [value-tracker & {:keys [frequency rand-func final-value run-time]
                    :or {frequency 8
                         rand-func rand
                         run-time 5.0}}]
  (animation :randomize value-tracker
             :frequency frequency
             :rand-func rand-func
             :final-value (or final-value (rand-func))
             :run-time run-time))

;; =============================================================================
;; Scene DSL
;; =============================================================================

(defmacro defscene
  "Define a scene as a function that returns a timeline.

   Example:
   ```clojure
   (defscene my-scene []
     (let [circle (shape :circle {:radius 1})]
       (-> (timeline)
           (add-object :circle circle)
           (then (create circle))
           (wait 2)
           (then (fade-out circle)))))
   ```"
  [name args & body]
  `(defn ~name ~args
     (let [result# (do ~@body)]
       (if (instance? Timeline result#)
         result#
         (throw (ex-info "defscene must return a Timeline" {:result result#}))))))

;; =============================================================================
;; Specs
;; =============================================================================

(s/def ::animation-type
  #{:create :transform :fade-in :fade-out :write :show-creation
    :move-to :rotate :scale :indicate :circumscribe :grow-arrow
    :grow-from-center :grow-from-point :uncreate :replacement-transform
    :transform-matching-tex :focus-on :surrounding-rectangle :blink
    :animate-value :randomize})

(s/def ::run-time (s/and number? pos?))
(s/def ::rate-func fn?)
(s/def ::lag-ratio (s/and number? #(<= 0 % 1)))

(s/def ::animation-options
  (s/keys :opt-un [::run-time ::rate-func ::lag-ratio]))

(s/def ::animation
  (s/and
   #(instance? Animation %)
   (s/keys :req-un [::type ::target ::options])))

(s/def ::time (s/and number? #(>= % 0)))

(s/def ::timeline-event
  (s/and
   #(instance? TimelineEvent %)
   (s/keys :req-un [::time ::animation])))
