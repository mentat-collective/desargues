(ns desargues.dsl.renderer
  "Renderer that bridges the DSL to Manim.

   This namespace handles the translation from our declarative DSL
   to actual Manim Python calls. It's the infrastructure layer
   that connects domain concepts to the rendering backend.

   ## Architecture

   The renderer follows the Adapter pattern:
   - DSL objects (domain) → Manim objects (infrastructure)
   - Animations (domain) → Manim animations (infrastructure)
   - Scenes (domain) → Manim scenes (infrastructure)

   ## Usage

   ```clojure
   (require '[desargues.dsl.core :as dsl])
   (require '[desargues.dsl.renderer :as r])

   (r/init!)

   (let [circle (dsl/shape :circle {:radius 1 :color :blue})
         timeline (-> (dsl/timeline)
                      (dsl/at 0 (dsl/animation :create circle))
                      (dsl/wait 1)
                      (dsl/at 2 (dsl/animation :fade-out circle)))]
     (r/render-timeline! timeline))
   ```"
  (:require [desargues.dsl.core :as dsl]
            [desargues.manim.core :as manim]
            [libpython-clj2.python :as py]))

;; =============================================================================
;; Initialization
;; =============================================================================

(defonce ^:private initialized? (atom false))

(defn init!
  "Initialize the renderer and Manim."
  []
  (when-not @initialized?
    (manim/init!)
    (reset! initialized? true))
  :initialized)

(defn ensure-initialized!
  "Ensure renderer is initialized, throwing if not."
  []
  (when-not @initialized?
    (throw (ex-info "Renderer not initialized. Call (init!) first." {}))))

;; =============================================================================
;; Color Mapping
;; =============================================================================

(def color-map
  "Map from keyword colors to Manim color constants."
  {:red "RED"
   :blue "BLUE"
   :green "GREEN"
   :yellow "YELLOW"
   :orange "ORANGE"
   :purple "PURPLE"
   :pink "PINK"
   :teal "TEAL"
   :cyan "BLUE_C"
   :gold "GOLD"
   :maroon "MAROON"
   :white "WHITE"
   :black "BLACK"
   :grey "GREY"
   :gray "GREY"})

(defn resolve-color
  "Convert a color keyword or RGB vector to Manim color."
  [color]
  (cond
    (keyword? color) (manim/get-constant (get color-map color "WHITE"))
    (vector? color) (let [[r g b] color]
                      (py/call-attr (manim/get-class "rgb_to_color")
                                    "__call__"
                                    (manim/->py-list [r g b])))
    (string? color) (manim/get-constant color)
    :else color))

;; =============================================================================
;; Shape Rendering
;; =============================================================================

(defmulti render-shape
  "Render a shape specification to a Manim mobject."
  (fn [shape-spec] (:shape-type shape-spec)))

(defmethod render-shape :circle
  [{:keys [opts]}]
  (let [{:keys [radius color fill-opacity stroke-width]
         :or {radius 1 color :white fill-opacity 0.5 stroke-width 2}} opts
        Circle (manim/get-class "Circle")]
    (py/call-attr-kw Circle "__call__" []
                     {"radius" radius
                      "color" (resolve-color color)
                      "fill_opacity" fill-opacity
                      "stroke_width" stroke-width})))

(defmethod render-shape :square
  [{:keys [opts]}]
  (let [{:keys [side color fill-opacity stroke-width]
         :or {side 2 color :white fill-opacity 0.5 stroke-width 2}} opts
        Square (manim/get-class "Square")]
    (py/call-attr-kw Square "__call__" []
                     {"side_length" side
                      "color" (resolve-color color)
                      "fill_opacity" fill-opacity
                      "stroke_width" stroke-width})))

(defmethod render-shape :rectangle
  [{:keys [opts]}]
  (let [{:keys [width height color fill-opacity stroke-width]
         :or {width 4 height 2 color :white fill-opacity 0.5 stroke-width 2}} opts
        Rectangle (manim/get-class "Rectangle")]
    (py/call-attr-kw Rectangle "__call__" []
                     {"width" width
                      "height" height
                      "color" (resolve-color color)
                      "fill_opacity" fill-opacity
                      "stroke_width" stroke-width})))

(defmethod render-shape :line
  [{:keys [opts]}]
  (let [{:keys [start end color stroke-width]
         :or {start [0 0 0] end [1 0 0] color :white stroke-width 2}} opts
        Line (manim/get-class "Line")]
    (py/call-attr-kw Line "__call__"
                     [(manim/->py-list start) (manim/->py-list end)]
                     {"color" (resolve-color color)
                      "stroke_width" stroke-width})))

(defmethod render-shape :arrow
  [{:keys [opts]}]
  (let [{:keys [start end color stroke-width]
         :or {start [0 0 0] end [1 0 0] color :white stroke-width 4}} opts
        Arrow (manim/get-class "Arrow")]
    (py/call-attr-kw Arrow "__call__"
                     [(manim/->py-list start) (manim/->py-list end)]
                     {"color" (resolve-color color)
                      "stroke_width" stroke-width})))

(defmethod render-shape :dot
  [{:keys [opts]}]
  (let [{:keys [position radius color]
         :or {position [0 0 0] radius 0.08 color :white}} opts
        Dot (manim/get-class "Dot")]
    (py/call-attr-kw Dot "__call__"
                     [(manim/->py-list position)]
                     {"radius" radius
                      "color" (resolve-color color)})))

(defmethod render-shape :polygon
  [{:keys [opts]}]
  (let [{:keys [vertices color fill-opacity stroke-width]
         :or {color :white fill-opacity 0.5 stroke-width 2}} opts
        Polygon (manim/get-class "Polygon")]
    (apply py/call-attr-kw Polygon "__call__"
           (mapv manim/->py-list vertices)
           {"color" (resolve-color color)
            "fill_opacity" fill-opacity
            "stroke_width" stroke-width})))

(defmethod render-shape :default
  [shape-spec]
  (throw (ex-info (str "Unknown shape type: " (:shape-type shape-spec))
                  {:shape shape-spec})))

;; =============================================================================
;; Text and LaTeX Rendering
;; =============================================================================

(defn render-tex
  "Render a LaTeX specification to a Manim MathTex."
  [{:keys [content colors font-size] :or {font-size 48}}]
  (let [MathTex (manim/get-class "MathTex")
        t2c (when (seq colors)
              (into {} (map (fn [[k v]] [k (resolve-color v)]) colors)))]
    (if t2c
      (py/call-attr-kw MathTex "__call__" [content]
                       {"tex_to_color_map" (manim/->py-dict t2c)
                        "font_size" font-size})
      (py/call-attr-kw MathTex "__call__" [content]
                       {"font_size" font-size}))))

(defn render-text
  "Render a text specification to a Manim Text."
  [{:keys [content color font-size] :or {font-size 48 color :white}}]
  (let [Text (manim/get-class "Text")]
    (py/call-attr-kw Text "__call__" [content]
                     {"color" (resolve-color color)
                      "font_size" font-size})))

;; =============================================================================
;; Axes and Graphs
;; =============================================================================

(defn render-axes
  "Render axes specification to Manim Axes."
  [{:keys [ranges labels grid tips]}]
  (let [{:keys [x y z]} ranges
        Axes (manim/get-class "Axes")
        axes (py/call-attr-kw Axes "__call__" []
                              {"x_range" (manim/->py-list (or x [-5 5 1]))
                               "y_range" (manim/->py-list (or y [-3 3 1]))
                               "tips" tips})]
    (when labels
      (py/call-attr axes "add_coordinates"))
    axes))

(defn render-graph
  "Render a function graph on axes."
  [{:keys [function opts]} axes]
  (let [{:keys [x-range color stroke-width]
         :or {x-range [-5 5] color :blue stroke-width 3}} opts
        graph (py/call-attr-kw axes "plot"
                               [function]
                               {"x_range" (manim/->py-list x-range)
                                "color" (resolve-color color)
                                "stroke_width" stroke-width})]
    graph))

(defn render-parametric
  "Render a parametric curve."
  [{:keys [function opts]}]
  (let [{:keys [t-range color stroke-width]
         :or {t-range [0 (* 2 Math/PI)] color :yellow stroke-width 3}} opts
        ParametricFunction (manim/get-class "ParametricFunction")]
    (py/call-attr-kw ParametricFunction "__call__"
                     [(fn [t] (manim/->py-list (function t)))]
                     {"t_range" (manim/->py-list t-range)
                      "color" (resolve-color color)
                      "stroke_width" stroke-width})))

;; =============================================================================
;; Animation Rendering
;; =============================================================================

(defmulti render-animation
  "Render an animation specification to a Manim animation."
  (fn [anim-spec mobject-map] (:type anim-spec)))

(defmethod render-animation :create
  [{:keys [target opts]} mobject-map]
  (let [mobject (get mobject-map target target)
        Create (manim/get-class "Create")]
    (py/call-attr-kw Create "__call__" [mobject] (or opts {}))))

(defmethod render-animation :fade-in
  [{:keys [target opts]} mobject-map]
  (let [mobject (get mobject-map target target)
        FadeIn (manim/get-class "FadeIn")]
    (py/call-attr-kw FadeIn "__call__" [mobject] (or opts {}))))

(defmethod render-animation :fade-out
  [{:keys [target opts]} mobject-map]
  (let [mobject (get mobject-map target target)
        FadeOut (manim/get-class "FadeOut")]
    (py/call-attr-kw FadeOut "__call__" [mobject] (or opts {}))))

(defmethod render-animation :transform
  [{:keys [target opts]} mobject-map]
  (let [{:keys [to]} opts
        from-mobject (get mobject-map target target)
        to-mobject (get mobject-map to to)
        Transform (manim/get-class "Transform")]
    (Transform from-mobject to-mobject)))

(defmethod render-animation :move
  [{:keys [target opts]} mobject-map]
  (let [{:keys [to]} opts
        mobject (get mobject-map target target)]
    (py/call-attr (py/call-attr mobject "animate") "move_to"
                  (manim/->py-list to))))

(defmethod render-animation :rotate
  [{:keys [target opts]} mobject-map]
  (let [{:keys [angle about]} opts
        mobject (get mobject-map target target)
        Rotate (manim/get-class "Rotate")]
    (py/call-attr-kw Rotate "__call__" [mobject]
                     {"angle" angle
                      "about_point" (when about (manim/->py-list about))})))

(defmethod render-animation :scale
  [{:keys [target opts]} mobject-map]
  (let [{:keys [factor]} opts
        mobject (get mobject-map target target)]
    (py/call-attr (py/call-attr mobject "animate") "scale" factor)))

(defmethod render-animation :write
  [{:keys [target opts]} mobject-map]
  (let [mobject (get mobject-map target target)
        Write (manim/get-class "Write")]
    (py/call-attr-kw Write "__call__" [mobject] (or opts {}))))

(defmethod render-animation :default
  [anim-spec _]
  (throw (ex-info (str "Unknown animation type: " (:type anim-spec))
                  {:animation anim-spec})))

;; =============================================================================
;; Scene Rendering
;; =============================================================================

(defn create-scene-class
  "Create a Manim Scene class from a construct function."
  [name construct-fn]
  (manim/create-scene-class name construct-fn))

(defn render-timeline!
  "Render a timeline to video.

   Takes a timeline and optional config, creates a Manim scene,
   and renders it."
  [timeline & {:keys [quality output-dir]
               :or {quality "medium_quality"}}]
  (ensure-initialized!)
  (let [events (dsl/get-events timeline)
        mobject-map (atom {})

        construct-fn
        (fn [scene]
          (doseq [{:keys [time event]} events]
            ;; Handle different event types
            (cond
              ;; Shape specification - create mobject
              (= :shape (:type event))
              (let [mobject (render-shape event)]
                (swap! mobject-map assoc event mobject))

              ;; TeX specification - create mobject
              (= :tex (:type event))
              (let [mobject (render-tex event)]
                (swap! mobject-map assoc event mobject))

              ;; Animation - play it
              (instance? desargues.dsl.core.Animation event)
              (let [anim (render-animation
                          (dsl/at-time event 0)
                          @mobject-map)]
                (manim/play! scene anim))

              ;; Wait
              (number? event)
              (manim/wait! scene event)

              :else
              (println "Unknown event type:" event))))

        scene-class (create-scene-class "TimelineScene" construct-fn)
        scene (scene-class)]
    (manim/render-scene! scene :quality quality)))

;; =============================================================================
;; Quick Scene Builder - For simple scenes
;; =============================================================================

(defmacro quick-scene
  "Create and render a quick scene.

   ```clojure
   (quick-scene
     (let [c (circle {:color :blue})]
       (play! (create c))
       (wait 1)
       (play! (fade-out c))))
   ```"
  [& body]
  `(do
     (ensure-initialized!)
     (let [~'scene-objects (atom {})
           ~'scene-animations (atom [])

           ;; Helper functions available in body
           ~'circle (fn [opts#]
                      (let [mob# (render-shape (dsl/shape :circle opts#))]
                        (swap! ~'scene-objects assoc (gensym) mob#)
                        mob#))
           ~'square (fn [opts#]
                      (let [mob# (render-shape (dsl/shape :square opts#))]
                        (swap! ~'scene-objects assoc (gensym) mob#)
                        mob#))
           ~'tex (fn [content# & opts#]
                   (let [mob# (render-tex (apply dsl/tex content# opts#))]
                     (swap! ~'scene-objects assoc (gensym) mob#)
                     mob#))
           ~'create (fn [mob#]
                      (py/call-attr (manim/get-class "Create") "__call__" mob#))
           ~'fade-in (fn [mob#]
                       (py/call-attr (manim/get-class "FadeIn") "__call__" mob#))
           ~'fade-out (fn [mob#]
                        (py/call-attr (manim/get-class "FadeOut") "__call__" mob#))
           ~'transform (fn [from# to#]
                         (py/call-attr (manim/get-class "Transform") "__call__" from# to#))]

       (let [construct-fn#
             (fn [scene#]
               (let [~'play! (fn [& anims#]
                               (apply manim/play! scene# anims#))
                     ~'wait (fn [& [secs#]]
                              (manim/wait! scene# (or secs# 1)))
                     ~'add! (fn [& mobs#]
                              (apply manim/add! scene# mobs#))]
                 ~@body))
             scene-class# (create-scene-class "QuickScene" construct-fn#)
             scene# (scene-class#)]
         (manim/render-scene! scene#)))))

;; =============================================================================
;; Reactive Value Integration
;; =============================================================================

(defn create-value-tracker
  "Create a Manim ValueTracker from a reactive value."
  [reactive-val]
  (let [ValueTracker (manim/get-class "ValueTracker")
        tracker (ValueTracker (dsl/get-value reactive-val))]
    ;; Sync reactive value changes to ValueTracker
    (dsl/add-watcher! reactive-val :manim-sync
                      (fn [_ new-val]
                        (py/call-attr tracker "set_value" new-val)))
    tracker))

(defn create-updater
  "Create a Manim updater from a Clojure function."
  [f]
  (fn [mobject dt]
    (f mobject dt)))

(defn add-updater!
  "Add an updater function to a mobject."
  [mobject updater-fn]
  (py/call-attr mobject "add_updater" updater-fn))

;; =============================================================================
;; Physics Rendering
;; =============================================================================

(defn create-physics-updater
  "Create an updater that steps physics simulation."
  [particle mobject & {:keys [scale] :or {scale 1.0}}]
  (fn [mob dt]
    (dsl/step! particle dt)
    (let [[x y z] (dsl/get-position particle)]
      (py/call-attr mob "move_to"
                    (manim/->py-list [(* scale x)
                                      (* scale y)
                                      (* scale z)])))))

(defn render-particle!
  "Render a particle as a dot with physics."
  [scene particle & {:keys [color radius]
                     :or {color :blue radius 0.1}}]
  (let [dot (render-shape (dsl/shape :dot {:color color :radius radius}))
        updater (create-physics-updater particle dot)]
    (add-updater! dot updater)
    (manim/add! scene dot)
    dot))

;; =============================================================================
;; Convenience Functions
;; =============================================================================

(defn show-expression!
  "Show a mathematical expression and animate it.

   ```clojure
   (show-expression! scene (expr '(sin x)) :at [0 2 0])
   ```"
  [scene expression & {:keys [at color]}]
  (let [mobject (render-tex (dsl/to-manim expression))]
    (when at
      (py/call-attr mobject "move_to" (manim/->py-list at)))
    (manim/add! scene mobject)
    (manim/play! scene (py/call-attr (manim/get-class "Write") "__call__" mobject))
    mobject))

(defn transform-expression!
  "Transform one expression into another.

   ```clojure
   (transform-expression! scene expr1 expr2)
   ```"
  [scene from-expr to-expr & {:keys [run-time] :or {run-time 1}}]
  (let [from-mob (render-tex (dsl/to-manim from-expr))
        to-mob (render-tex (dsl/to-manim to-expr))
        TransformMatchingTex (manim/get-class "TransformMatchingTex")]
    (manim/play! scene
                 (py/call-attr-kw TransformMatchingTex "__call__"
                                  [from-mob to-mob]
                                  {"run_time" run-time}))))
