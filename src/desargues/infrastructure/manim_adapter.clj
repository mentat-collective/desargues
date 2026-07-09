(ns desargues.infrastructure.manim-adapter
  "Infrastructure: Adapter for Manim (implements rendering protocols)"
  (:require [desargues.domain.protocols :as p]
            [desargues.domain.math-expression :as expr]
            [desargues.domain.services :as svc]
            [desargues.config :as config]
            [libpython-clj2.python :as py]
            [desargues.manim-quickstart :as mq]))

;; ============================================================================
;; Manim Animation Wrapper (defined first as it's used by ManimMobject)
;; ============================================================================

(defrecord ManimAnimation [py-obj type options]
  p/IAnimation
  (run-time [this]
    (get-in this [:options :run_time] 1.0))

  (with-run-time [this time]
    (assoc-in this [:options :run_time] time))

  (reverse-animation [this]
    ;; Not all animations support reversal
    this))

;; ============================================================================
;; Manim Mobject Adapter (Adapter Pattern)
;; ============================================================================

(defrecord ManimMobject [py-obj type metadata]
  p/IRenderable
  (to-mobject [this] (:py-obj this))

  p/IAnimatable
  (create-animation [this animation-type options]
    (let [manim (py/import-module "manim")
          anim-class (py/get-attr manim (name animation-type))]
      (->ManimAnimation (anim-class (:py-obj this)) animation-type options)))

  (animate-creation [this]
    (p/create-animation this :Create {}))

  (animate-transformation [this target]
    (p/create-animation this :Transform {:target (p/to-mobject target)})))

;; ============================================================================
;; Math Expression → Manim Mobject
;; ============================================================================

(extend-type desargues.domain.math_expression.MathExpression
  p/IRenderable
  (to-mobject [this]
    (let [manim (py/import-module "manim")
          MathTex (py/get-attr manim "MathTex")
          latex (svc/expression-to-latex this)
          mobject (MathTex (:content latex))]
      (->ManimMobject mobject :MathTex {:source this})))

  (render [this options]
    (let [mobject (p/to-mobject this)]
      ;; Can configure position, color, etc from options
      (when-let [pos (:position options)]
        (py/call-attr (:py-obj mobject) "shift" (py/->py-list pos)))
      (when-let [color (:color options)]
        (py/call-attr (:py-obj mobject) "set_color" color))
      mobject))

  p/IAnimatable
  (create-animation [this animation-type options]
    (p/create-animation (p/to-mobject this) animation-type options))

  (animate-creation [this]
    (p/animate-creation (p/to-mobject this)))

  (animate-transformation [this target]
    (p/animate-transformation (p/to-mobject this) target)))

;; ============================================================================
;; Scene Builder (Builder Pattern)
;; ============================================================================

;; Forward declaration
(declare render-scene-impl)

(defrecord ManimSceneBuilder [objects animations config]
  p/IScene
  (add-object [this obj]
    (update this :objects conj obj))

  (remove-object [this obj]
    (update this :objects (fn [objs] (remove #(= % obj) objs))))

  (play-animation [this animation]
    (update this :animations conj animation))

  (wait-duration [this duration]
    (update this :animations conj {:type :wait :duration duration}))

  (render-scene [this]
    ;; Build and render the scene
    (render-scene-impl this)))

(defn create-scene-builder
  "Factory: Create a new scene builder"
  ([]
   (create-scene-builder {}))
  ([config]
   (->ManimSceneBuilder [] [] config)))

;; ============================================================================
;; Scene Rendering Implementation
;; ============================================================================

(defn render-scene-impl
  "Render a scene using the builder"
  [builder]
  ;; Add project directory to path
  (config/add-project-to-syspath!)

  ;; Create Python scene dynamically
  (let [manim (py/import-module "manim")
        Scene (py/get-attr manim "Scene")

        ;; Create construct function
        construct-fn
        (fn [self]
          ;; Add all objects
          (doseq [obj (:objects builder)]
            (let [mobject (if (satisfies? p/IRenderable obj)
                            (p/to-mobject obj)
                            obj)]
              (py/call-attr self "add" (:py-obj mobject))))

          ;; Play all animations
          (doseq [anim (:animations builder)]
            (cond
              (= (:type anim) :wait)
              (py/call-attr self "wait" (:duration anim))

              (instance? ManimAnimation anim)
              (py/call-attr self "play" (:py-obj anim))

              :else
              (py/call-attr self "play" anim))))

        ;; Create scene class
        SceneClass (py/create-class "DynamicScene" [Scene]
                                    {"construct" construct-fn})

        ;; Instantiate and render
        scene (SceneClass)]

    (mq/render-scene! scene)))

;; ============================================================================
;; Fluent API Builder (Facade Pattern)
;; ============================================================================

(defn ->scene
  "Start building a scene (Fluent API entry point)"
  []
  (create-scene-builder))

(defn with-object
  "Add object to scene (Fluent API)"
  [builder obj]
  (p/add-object builder obj))

(defn with-animation
  "Add animation to scene (Fluent API)"
  [builder animation]
  (p/play-animation builder animation))

(defn with-wait
  "Add wait to scene (Fluent API)"
  [builder duration]
  (p/wait-duration builder duration))

(defn build-and-render
  "Build and render the scene (Fluent API terminal)"
  [builder]
  (p/render-scene builder))

;; ============================================================================
;; High-Level Animation Helpers
;; ============================================================================

(defn show-expression
  "Helper: Show a mathematical expression"
  [expr]
  (-> (->scene)
      (with-object expr)
      (with-animation (p/animate-creation expr))
      (with-wait 2)))

(defn transform-expressions
  "Helper: Transform one expression into another"
  [from-expr to-expr]
  (-> (->scene)
      (with-object from-expr)
      (with-animation (p/animate-creation from-expr))
      (with-wait 1)
      (with-animation (p/animate-transformation from-expr to-expr))
      (with-wait 2)))

;; A MathFunction renders as its symbolic form applied to the canonical
;; variable x: (f 'x) -> Emmy expression -> reuse the MathExpression path.
(extend-type desargues.domain.math_expression.MathFunction
  p/IRenderable
  (to-mobject [this]
    (p/to-mobject (expr/create-expression ((:f this) 'x))))
  (render [this options]
    (p/render (expr/create-expression ((:f this) 'x)) options))

  p/IAnimatable
  (create-animation [this animation-type options]
    (p/create-animation (expr/create-expression ((:f this) 'x)) animation-type options))
  (animate-creation [this]
    (p/animate-creation (expr/create-expression ((:f this) 'x))))
  (animate-transformation [this target]
    (p/animate-transformation (expr/create-expression ((:f this) 'x)) target)))
