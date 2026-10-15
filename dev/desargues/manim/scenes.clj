(ns desargues.manim.scenes
  "Manim Scene and Camera bindings.
   
   Scene Types:
   - Scene - basic scene
   - MovingCameraScene - scene with camera movement
   - ThreeDScene - 3D scene with rotatable camera
   - ZoomedScene - scene with zoom capability
   - VectorScene - scene for vector operations
   - LinearTransformationScene - for matrix transformations
   
   Cameras:
   - Camera - basic camera
   - MovingCamera - camera with movement capabilities
   - ThreeDCamera - 3D camera
   - MultiCamera - multiple viewports"
  (:require [desargues.manim.core :as core]
            [libpython-clj2.python :as py]))

;; ============================================================================
;; Scene Types
;; ============================================================================

(defn scene-class
  "Get the basic Scene class."
  []
  (core/get-class "Scene"))

(defn moving-camera-scene-class
  "Get the MovingCameraScene class.
   
   Allows camera movement with move_camera, set_camera_orientation, etc."
  []
  (core/get-class "MovingCameraScene"))

(defn three-d-scene-class
  "Get the ThreeDScene class.
   
   For 3D scenes with rotatable camera."
  []
  (core/get-class "ThreeDScene"))

(defn zoomed-scene-class
  "Get the ZoomedScene class.
   
   For scenes with zoom/magnification capability."
  []
  (core/get-class "ZoomedScene"))

(defn vector-scene-class
  "Get the VectorScene class.
   
   For scenes involving vectors and vector operations."
  []
  (core/get-class "VectorScene"))

(defn linear-transformation-scene-class
  "Get the LinearTransformationScene class.
   
   For visualizing matrix/linear transformations."
  []
  (core/get-class "LinearTransformationScene"))

;; ============================================================================
;; Scene Construction
;; ============================================================================

(defn create-scene
  "Create a Scene subclass with a construct function.
   
   name - scene class name
   construct-fn - function taking (self) for building the scene
   
   Options:
   - :base-class - base scene class (default Scene)"
  [name construct-fn & {:keys [base-class] :or {base-class nil}}]
  (let [base (or base-class (scene-class))]
    (py/create-class name [base] {"construct" construct-fn})))

(defn create-3d-scene
  "Create a ThreeDScene subclass."
  [name construct-fn]
  (create-scene name construct-fn :base-class (three-d-scene-class)))

(defn create-moving-camera-scene
  "Create a MovingCameraScene subclass."
  [name construct-fn]
  (create-scene name construct-fn :base-class (moving-camera-scene-class)))

(defn create-zoomed-scene
  "Create a ZoomedScene subclass."
  [name construct-fn]
  (create-scene name construct-fn :base-class (zoomed-scene-class)))

;; ============================================================================
;; Scene Methods
;; ============================================================================

(defn play!
  "Play one or more animations in a scene.
   
   Options (as trailing keyword args):
   - :run_time - animation duration
   - :rate_func - timing function"
  [scene & animations-and-kwargs]
  (let [[anims kwargs] (loop [items animations-and-kwargs
                              anims []
                              kwargs {}]
                         (if (empty? items)
                           [anims kwargs]
                           (if (keyword? (first items))
                             (recur (rest (rest items))
                                    anims
                                    (assoc kwargs (first items) (second items)))
                             (recur (rest items)
                                    (conj anims (first items))
                                    kwargs))))]
    (if (empty? kwargs)
      (apply py/call-attr scene "play" anims)
      (py/call-attr-kw scene "play" (vec anims) kwargs))))

(defn wait!
  "Wait (pause) in a scene.
   
   duration - wait time in seconds (default 1)"
  ([scene]
   (wait! scene 1))
  ([scene duration]
   (py/call-attr scene "wait" duration)))

(defn add!
  "Add mobjects to a scene (no animation)."
  [scene & mobjects]
  (apply py/call-attr scene "add" mobjects)
  scene)

(defn remove!
  "Remove mobjects from a scene."
  [scene & mobjects]
  (apply py/call-attr scene "remove" mobjects)
  scene)

(defn clear!
  "Clear all mobjects from a scene."
  [scene]
  (py/call-attr scene "clear")
  scene)

(defn bring-to-front!
  "Bring mobjects to the front of the scene."
  [scene & mobjects]
  (apply py/call-attr scene "bring_to_front" mobjects)
  scene)

(defn bring-to-back!
  "Send mobjects to the back of the scene."
  [scene & mobjects]
  (apply py/call-attr scene "bring_to_back" mobjects)
  scene)

(defn get-mobjects
  "Get all mobjects in the scene."
  [scene]
  (py/get-attr scene "mobjects"))

(defn get-top-level-mobjects
  "Get top-level mobjects (not submobjects)."
  [scene]
  (py/call-attr scene "get_top_level_mobjects"))

;; ============================================================================
;; Camera Operations
;; ============================================================================

(defn get-camera
  "Get the scene's camera."
  [scene]
  (py/get-attr scene "camera"))

(defn get-frame
  "Get the camera's frame (for MovingCameraScene)."
  [scene]
  (py/get-attr (get-camera scene) "frame"))

;; MovingCameraScene methods
(defn move-camera!
  "Move the camera in a MovingCameraScene.
   
   Options:
   - :phi - vertical rotation (radians)
   - :theta - horizontal rotation (radians)
   - :gamma - roll (radians)
   - :zoom - zoom factor
   - :frame_center - center point"
  [scene & {:as kwargs}]
  (py/call-attr-kw scene "move_camera" [] kwargs)
  scene)

(defn set-camera-orientation!
  "Set camera orientation (ThreeDScene).
   
   Options:
   - :phi - vertical angle
   - :theta - horizontal angle
   - :gamma - roll angle
   - :zoom - zoom factor"
  [scene & {:as kwargs}]
  (py/call-attr-kw scene "set_camera_orientation" [] kwargs)
  scene)

(defn begin-ambient-camera-rotation!
  "Start continuous camera rotation (ThreeDScene).
   
   Options:
   - :rate - rotation rate (radians per second)
   - :about - axis to rotate about"
  [scene & {:keys [rate about] :or {rate 0.02} :as kwargs}]
  (py/call-attr-kw scene "begin_ambient_camera_rotation" [] kwargs)
  scene)

(defn stop-ambient-camera-rotation!
  "Stop ambient camera rotation."
  [scene]
  (py/call-attr scene "stop_ambient_camera_rotation")
  scene)

;; ZoomedScene methods
(defn activate-zooming!
  "Activate the zoomed display in a ZoomedScene.
   
   Options:
   - :animate - whether to animate activation"
  [scene & {:keys [animate] :or {animate true}}]
  (py/call-attr-kw scene "activate_zooming" [] {:animate animate})
  scene)

(defn get-zoomed-camera
  "Get the zoomed camera mobject."
  [scene]
  (py/get-attr scene "zoomed_camera"))

(defn get-zoomed-display
  "Get the zoomed display mobject."
  [scene]
  (py/get-attr scene "zoomed_display"))

;; ============================================================================
;; Camera Classes
;; ============================================================================

(defn camera
  "Create a Camera.
   
   Options:
   - :background_color
   - :frame_width
   - :frame_height"
  [& {:as kwargs}]
  (let [cls (core/get-class "Camera")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn moving-camera
  "Create a MovingCamera."
  [& {:as kwargs}]
  (let [cls (core/get-class "MovingCamera")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

(defn three-d-camera
  "Create a ThreeDCamera.
   
   Options:
   - :phi - vertical angle
   - :theta - horizontal angle
   - :focal_distance - focal distance
   - :light_source_position"
  [& {:as kwargs}]
  (let [cls (core/get-class "ThreeDCamera")]
    (if kwargs
      (py/call-attr-kw cls "__call__" [] kwargs)
      (cls))))

;; ============================================================================
;; Rendering
;; ============================================================================

(defn render!
  "Render a scene instance.
   
   Options:
   - :quality - \"low_quality\", \"medium_quality\", \"high_quality\"
   - :preview - open preview after render
   - :output-file - custom output filename"
  ([scene]
   (render! scene {}))
  ([scene {:keys [quality preview output-file]
           :or {quality "high_quality"
                preview false}}]
   (let [kwargs (cond-> {:quality quality :preview preview}
                  output-file (assoc :output_file output-file))]
     (py/call-attr-kw scene "render" [] kwargs))
   (println "Rendering complete! Check media/videos/ directory.")))

(defn quick-render!
  "Render scene with low quality for fast preview."
  [scene]
  (render! scene {:quality "low_quality"}))

(defn preview!
  "Render scene and open preview."
  [scene]
  (render! scene {:quality "medium_quality" :preview true}))

;; ============================================================================
;; Config
;; ============================================================================

(defn set-background-color!
  "Set the scene's background color."
  [scene color]
  (py/set-attr! (get-camera scene) "background_color" color)
  scene)

(defn set-frame-width!
  "Set the camera's frame width."
  [scene width]
  (py/set-attr! (get-camera scene) "frame_width" width)
  scene)

(defn set-frame-height!
  "Set the camera's frame height."
  [scene height]
  (py/set-attr! (get-camera scene) "frame_height" height)
  scene)

;; ============================================================================
;; Convenience Macros
;; ============================================================================

(defmacro defscene
  "Define a named scene class.
   
   Example:
   (defscene MyScene
     (let [c (m/circle)]
       (play! self (a/create c))
       (wait! self 2)))"
  [name & body]
  `(def ~name
     (create-scene ~(str name)
                   (fn [~'self]
                     ~@body))))

(defmacro def3dscene
  "Define a named 3D scene class."
  [name & body]
  `(def ~name
     (create-3d-scene ~(str name)
                      (fn [~'self]
                        ~@body))))

(defmacro run-scene
  "Create and render a scene in one expression.
   
   Example:
   (run-scene
     (let [c (m/circle)]
       (play! self (a/create c))))"
  [& body]
  `(let [scene-class# (create-scene "TempScene"
                                    (fn [~'self]
                                      ~@body))
         scene# (scene-class#)]
     (render! scene#)))

(defmacro run-3d-scene
  "Create and render a 3D scene in one expression."
  [& body]
  `(let [scene-class# (create-3d-scene "Temp3DScene"
                                       (fn [~'self]
                                         ~@body))
         scene# (scene-class#)]
     (render! scene#)))

;; ============================================================================
;; Scene Builder (Fluent API)
;; ============================================================================

(defrecord SceneBuilder [objects animations waits config])

(defn scene-builder
  "Create a new scene builder for fluent construction."
  []
  (->SceneBuilder [] [] [] {}))

(defn with-object
  "Add an object to the scene builder."
  [builder obj]
  (update builder :objects conj obj))

(defn with-animation
  "Add an animation to the scene builder."
  [builder anim]
  (update builder :animations conj [:play anim]))

(defn with-wait
  "Add a wait to the scene builder."
  [builder duration]
  (update builder :animations conj [:wait duration]))

(defn with-config
  "Set config options on the builder."
  [builder key value]
  (assoc-in builder [:config key] value))

(defn build-scene
  "Build a scene class from a builder."
  [builder]
  (create-scene "BuiltScene"
                (fn [self]
                  ;; Add objects
                  (doseq [obj (:objects builder)]
                    (py/call-attr self "add" obj))
                  ;; Play animations and waits
                  (doseq [[type arg] (:animations builder)]
                    (case type
                      :play (py/call-attr self "play" arg)
                      :wait (py/call-attr self "wait" arg))))))

(defn build-and-render!
  "Build and render a scene from a builder."
  [builder]
  (let [scene-class (build-scene builder)
        scene (scene-class)]
    (render! scene (:config builder))))

;; ============================================================================
;; Interactive/REPL helpers
;; ============================================================================

(defn make-scene
  "Create a scene instance from a construct function.
   
   Useful for REPL experimentation."
  [construct-fn]
  (let [scene-class (create-scene "ReplScene" construct-fn)]
    (scene-class)))

(defn run!
  "Create and render a scene from a construct function.
   
   Example:
   (run! (fn [self]
           (let [c (m/circle)]
             (play! self (a/create c)))))"
  ([construct-fn]
   (run! construct-fn {}))
  ([construct-fn opts]
   (let [scene (make-scene construct-fn)]
     (render! scene opts))))
