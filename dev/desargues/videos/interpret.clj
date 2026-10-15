(ns desargues.videos.interpret
  "Interpreter: render a declarative `desargues.videos.timeline` Timeline through
   the backend-neutral `desargues.scene` facade.

   The timeline DSL models animations as DATA (Animation / AnimationGroup /
   Timeline records) but shipped without a renderer — `render-timeline` only
   returns a data command. This namespace closes that gap for the CORE animation
   vocabulary: it maps each Animation record's :type to a scene-facade animation,
   maps AnimationGroup combinators (sequential / parallel / lagged / succession)
   to scene groups, and plays a Timeline's events in order on a scene stage. A
   timeline authored as data therefore renders under ANY scene backend (Manim
   today, a browser backend tomorrow).

   Scope + limitations:
   - Targets must be SCENE mobjects (built via `desargues.scene`), not the
     richer `videos.math-objects`/`typography`/`characters` declarative object
     model — interpreting that object model is a separate, larger effort.
   - Timing is sequential-play (each event plays after the previous completes),
     matching how `then`/`play`-built timelines read. Absolute-time OVERLAP of
     events is not honored (Manim is a sequential-play model, not a scheduler)."
  (:require [desargues.scene :as scene]
            [desargues.videos.timeline :as tl])
  (:import [desargues.videos.timeline Animation AnimationGroup]))

;; ============================================================================
;; Animation record -> scene-facade animation
;; ============================================================================

(defn- run-time-of [a]
  (get-in a [:options :run-time]))

(defn- ->scene-anim
  "Map a single timeline Animation record to a scene-facade animation over its
   (scene-mobject) target(s)."
  [a]
  (let [{:keys [type target]} a
        rt (run-time-of a)]
    (case type
      (:transform :replacement-transform :transform-matching-tex)
      (scene/morph (first target) (second target) :run-time rt)

      (:create :show-creation :draw-border-then-fill :grow-from-center
       :grow-from-point :grow-arrow)
      (scene/draw target :run-time rt)

      (:write :add-text-letter-by-letter)
      (scene/draw target :run-time rt)

      (:fade-in :fade-in-from-down :v-fade-in)
      (scene/appear target :run-time rt)

      (:fade-out :v-fade-out :uncreate)
      (scene/vanish target :run-time rt)

      (:indicate :circumscribe :focus-on :surrounding-rectangle :blink)
      (scene/emphasize target :run-time rt)

      ;; unknown animation types degrade to a plain reveal of the target
      (scene/appear (if (sequential? target) (first target) target) :run-time rt))))

(declare ->node)

(defn- ->group
  "Map a timeline AnimationGroup to a scene group animation."
  [g]
  (let [{:keys [type animations options]} g
        kids (mapv ->node animations)
        lag  (:lag-ratio options 0.5)]
    (case type
      :parallel   (scene/together kids :lag-ratio 0)
      :sequential (scene/stagger  kids :lag-ratio 1.0)
      :succession (scene/stagger  kids :lag-ratio 1.0)
      :lagged     (scene/stagger  kids :lag-ratio lag)
      (scene/together kids :lag-ratio 0))))

(defn- ->node
  "Map any timeline animation node (Animation, AnimationGroup) to a scene anim."
  [x]
  (cond
    (instance? AnimationGroup x) (->group x)
    (instance? Animation x)      (->scene-anim x)
    :else (throw (ex-info "interpret: unsupported timeline node"
                          {:node x :class (class x)}))))

;; ============================================================================
;; Timeline -> scene playback
;; ============================================================================

(defn play-timeline!
  "Play Timeline `timeline` on scene `stage` via the facade. Events are played
   in start-time order; each event's animation (or group) becomes one
   `scene/play!`. Explicit gaps between event start-times become `scene/hold!`s
   so `wait`-spaced timelines pace roughly as authored."
  [stage timeline]
  (let [events (sort-by :time (:events timeline))]
    (reduce (fn [clock event]
              (let [t (double (:time event))]
                (when (> t (+ clock 1.0e-6))
                  (scene/hold! stage (- t clock)))
                (scene/play! stage (->node (:animation event)))
                (max t clock)))
            0.0
            events)
    stage))

(defn render-timeline!
  "Convenience: render a timeline to <scene-name>.mp4 via the scene facade.
   `build` is (fn [stage] -> Timeline): it constructs the scene mobjects (via
   `desargues.scene`) and returns a Timeline that animates them."
  [scene-name build & {:as opts}]
  (scene/render! scene-name
                 (fn [stage] (play-timeline! stage (build stage)))
                 opts))
