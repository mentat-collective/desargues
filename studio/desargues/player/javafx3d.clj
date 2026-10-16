(ns desargues.player.javafx3d
  "BOUNDARY stratum: a JavaFX host that renders 3D frames natively.

   Composition over the 2D host (desargues.player.javafx): the window,
   sliders, clock and pointer are the 2D host's, unchanged; this host adds a
   depth-buffered SubScene with a PerspectiveCamera under the 2D canvas, which
   becomes a transparent overlay for the frame's 2D shapes. It implements
   desargues.player.protocols/ISpace on top of everything the 2D host does,
   so the player hands it 3D frames as they are (desargues.player.present).

   Geometry is built in camera coordinates (desargues.player.space/->camera),
   one JavaFX unit per pixel at the target, and the camera's field of view is
   chosen so a point projects exactly where space/project puts it. Pointer
   hit-testing therefore runs on the pure flattened frame (space/->2d), and
   orbiting is the player's pure desargues.player.orbit: the host only
   reports pointer events.

   One `node!` method per 3D kind (OCP). An orthographic camera is drawn
   flattened on the canvas (the projection is exact either way).
   Requires the :studio alias (OpenJFX).

     (require '[desargues.player :as player]
              '[desargues.player.javafx3d :as fx3]
              '[desargues.player.space-demos :as sd])
     (player/open-source! (fx3/host) sd/source-fn
                          {:title \"saddle\" :params sd/defaults :controls sd/controls})"
  (:require [desargues.player.frame :as frame]
            [desargues.player.javafx :as fx]
            [desargues.player.protocols :as p]
            [desargues.player.space :as space])
  (:import [javafx.application Platform]
           [javafx.geometry Point3D]
           [javafx.scene AmbientLight Group Node PerspectiveCamera PointLight
            SceneAntialiasing SubScene]
           [javafx.scene.canvas Canvas]
           [javafx.scene.layout Pane]
           [javafx.scene.paint Color PhongMaterial]
           [javafx.scene.shape Cylinder CullFace DrawMode MeshView Sphere TriangleMesh]
           [javafx.scene.transform Rotate Translate]))

;; ── Toolkit thread ────────────────────────────────────────────────────────

(defn- on-fx
  "Run `f` on the JavaFX thread and wait for it (rethrowing its error)."
  [f]
  (if (Platform/isFxApplicationThread)
    (f)
    (let [out (promise)]
      (Platform/runLater #(deliver out (try {:ok (f)} (catch Throwable t {:err t}))))
      (let [{:keys [ok err]} @out]
        (if err (throw err) ok)))))

;; ── Colours ───────────────────────────────────────────────────────────────

(defn- color
  "A JavaFX Color from a palette keyword or CSS string, at `alpha`."
  ^Color [k alpha]
  (let [c (Color/web (if (string? k) k (get frame/palette k (frame/palette :white))))]
    (Color. (.getRed c) (.getGreen c) (.getBlue c) (* (.getOpacity c) (double alpha)))))

(defn- material ^PhongMaterial [k alpha]
  (doto (PhongMaterial. (color k alpha))
    (.setSpecularColor (Color/gray 0.25))))

;; ── Camera space -> JavaFX space ──────────────────────────────────────────

(defn- fx-point
  "JavaFX coordinates of world point `p`: camera coordinates (x right, y up,
   depth forward) scaled to pixels at the target, with y flipped (JavaFX y
   points down) and z the depth."
  [b k p]
  (let [[x y z] (space/->camera b p)]
    [(* k x) (- (* k y)) (* k z)]))

(defn- cylinder
  "A cylinder of radius `r` from JavaFX point `a` to `c`, or nil when they meet."
  [a c r ^PhongMaterial m]
  (let [d   (mapv - c a)
        len (space/norm d)]
    (when (> len 1e-9)
      (let [mid  (mapv #(/ (+ %1 %2) 2.0) a c)
            axis (space/cross [0.0 1.0 0.0] d)
            ang  (Math/toDegrees (Math/acos (max -1.0 (min 1.0 (/ (nth d 1) len)))))
            cyl  (doto (Cylinder. (double r) (double len) 6) (.setMaterial m))]
        (.addAll (.getTransforms cyl)
                 [(Translate. (mid 0) (mid 1) (mid 2))
                  (Rotate. ang (if (< (space/norm axis) 1e-9)
                                 (Point3D. 1 0 0)
                                 (Point3D. (axis 0) (axis 1) (axis 2))))])
        cyl))))

(defn- polyline-nodes [pts r m]
  (keep (fn [[a c]] (cylinder a c r m)) (partition 2 1 pts)))

(defn- stroke-px
  "Manim stroke widths are pixels at 1080p (135 px per scene unit)."
  [k s]
  (max 1.0 (* (double (or (get-in s [:stroke :width]) 4)) (/ (double k) 135.0))))

;; ── 3D nodes: one method per kind (OCP) ───────────────────────────────────

(defmulti node!
  "JavaFX nodes for 3D shape `s` under basis `b`, `k` pixels per unit."
  (fn [_b _k s] (:node s)))

(defmethod node! :default [_ _ _] [])

(defn- triangle-mesh ^TriangleMesh [b k {:keys [vertices faces]}]
  (let [mesh (TriangleMesh.)
        pts  (float-array (mapcat #(fx-point b k %) vertices))
        tris (mapcat (fn [[a c d e]] (if e [[a c d] [a d e]] [[a c d]])) faces)]
    (.addAll (.getPoints mesh) pts)
    (.addAll (.getTexCoords mesh) (float-array [0 0]))
    (.addAll (.getFaces mesh) (int-array (mapcat (fn [[a c d]] [a 0 c 0 d 0]) tris)))
    mesh))

(defmethod node! :surface3 [b k s]
  (let [{:keys [faces] :as m} (space/mesh s)
        n     (long (Math/ceil (* (double (:draw s 1.0)) (count faces))))
        mesh  (triangle-mesh b k (assoc m :faces (take n faces)))
        alpha (* (double (:opacity s 1.0)) (double (get-in s [:fill :opacity] 1.0)))
        fill  (doto (MeshView. mesh)
                (.setCullFace CullFace/NONE)
                (.setMaterial (material (get-in s [:fill :color] :blue) alpha)))]
    (cond-> [fill]
      (get-in s [:opts :wire])
      (conj (doto (MeshView. mesh)
              (.setCullFace CullFace/NONE)
              (.setDrawMode DrawMode/LINE)
              (.setMaterial (material (get-in s [:opts :wire]) (:opacity s 1.0))))))))

(defmethod node! :curve3 [b k s]
  (let [pts (:points s)
        n   (long (Math/ceil (* (double (:draw s 1.0)) (count pts))))]
    (polyline-nodes (map #(fx-point b k %) (take n pts))
                    (/ (stroke-px k s) 2.0)
                    (material (get-in s [:stroke :color] :white) (:opacity s 1.0)))))

(defmethod node! :points3 [b k s]
  (let [r (* k (double (get-in s [:opts :radius] 0.08)))
        m (material (or (get-in s [:opts :color]) (get-in s [:stroke :color]) :white) (:opacity s 1.0))]
    (for [p (:points s)
          :let [[x y z] (fx-point b k p)]]
      (doto (Sphere. r 12)
        (.setMaterial m)
        (.setTranslateX x) (.setTranslateY y) (.setTranslateZ z)))))

(defmethod node! :axes3 [b k s]
  (let [[[x0 x1] [y0 y1] [z0 z1]] (get-in s [:opts :range] [[-3 3] [-3 3] [-3 3]])
        m (material (or (get-in s [:opts :color]) :grey) (:opacity s 1.0))]
    (mapcat (fn [[a c]] (polyline-nodes [(fx-point b k a) (fx-point b k c)] 1.0 m))
            [[[x0 0 0] [x1 0 0]] [[0 y0 0] [0 y1 0]] [[0 0 z0] [0 0 z1]]])))

;; ── The 2D overlay of a 3D frame ──────────────────────────────────────────

(defn overlay
  "The 2D frame painted over the 3D view: the frame's own 2D shapes plus the
   text a 3D kind contributes (axis labels), on a transparent background."
  [f]
  (let [labels (->> (:shapes (space/->2d (update f :shapes #(filter space/three-d-shape? %))))
                    (filter #(= :text (:node %))))]
    (assoc (frame/frame (:t f) (concat labels (remove space/three-d-shape? (:shapes f))))
           :background "transparent")))

;; ── The host ──────────────────────────────────────────────────────────────

(defn- install-subscene!
  "Put a depth-buffered SubScene under the 2D host's canvas, sized with it."
  [state ^Canvas canvas]
  (let [^Pane pane (.getParent canvas)
        root  (Group.)
        cam   (doto (PerspectiveCamera. true)
                (.setNearClip 1.0)
                (.setFarClip 1e6))
        sub   (doto (SubScene. root 10.0 10.0 true SceneAntialiasing/BALANCED)
                (.setCamera cam)
                (.setMouseTransparent true))]
    (.bind (.widthProperty sub) (.widthProperty pane))
    (.bind (.heightProperty sub) (.heightProperty pane))
    (.add (.getChildren pane) 0 sub)
    (swap! state assoc :sub sub :root root :camera cam)))

(defn- show-space!
  "Rebuild the 3D scene for frame `f` (perspective camera) on the FX thread."
  [state f]
  (let [{:keys [^SubScene sub ^Group root ^PerspectiveCamera camera]} @state
        w   (.getWidth sub) h (.getHeight sub)
        vp  (frame/viewport w h)
        k   (double (:scale vp))
        cam (space/camera f)
        b   (space/basis cam)
        d   (double (:distance cam))
        zm  (double (:zoom cam 1.0))
        ;; vertical fov so a point at depth z lands where space/project puts
        ;; it (k * zoom * distance / z pixels per unit)
        fov (Math/toDegrees (* 2.0 (Math/atan (/ h (* 2.0 k zm d)))))
        nodes (mapcat #(node! b k %)
                      (filter #(and (space/three-d-shape? %) (frame/visible? %)) (:shapes f)))]
    (.setFieldOfView camera fov)
    (.setVerticalFieldOfView camera true)
    (.setFill sub (if-let [bg (:background f)] (color bg 1.0) Color/BLACK))
    (.setAll (.getChildren root)
             ^java.util.Collection
             (concat [(AmbientLight. (Color/gray 0.45))
                      (doto (PointLight. (Color/gray 0.75)) (.setTranslateY (- (* k d 0.5))))]
                     nodes))
    (.setVisible sub true)))

(defrecord FxSpaceHost [inner state]
  p/ISurface
  (-present! [this f]
    (when-let [^SubScene sub (:sub @state)] (on-fx #(.setVisible sub false)))
    (p/-present! inner f)
    this)
  (-size [_] (p/-size inner))

  p/ISpace
  (-present-space! [this f]
    (if (= :orthographic (:projection (space/camera f)))
      (p/-present! this (space/->2d f))
      (do (on-fx #(show-space! state f))
          (p/-present! inner (overlay f))
          ;; the pointer hit-tests what is seen: the frame flattened
          (swap! (:state inner) assoc :frame (space/->2d f))))
    this)

  p/IWindow
  (-show! [this opts]
    (p/-show! inner opts)
    (on-fx #(install-subscene! state (:canvas @(:state inner))))
    this)
  (-close! [this] (p/-close! inner) this)
  (-snapshot! [_ path] (p/-snapshot! inner path))

  p/IControls
  (-control! [this spec on-change] (p/-control! inner spec on-change) this)
  (-set-control! [this id v] (p/-set-control! inner id v) this)

  p/IPointer
  (-on-pointer! [this f] (p/-on-pointer! inner f) this)

  p/IClock
  (-on-tick! [this f] (p/-on-tick! inner f) this)
  (-stop-ticks! [this] (p/-stop-ticks! inner) this))

(extend-type FxSpaceHost
  p/IKeyboard
  (-on-key! [this f] (p/-on-key! (:inner this) f) this))

(defn host
  "A fresh, unopened JavaFX host that renders 3D frames natively. Hand it to
   desargues.player/open-source! or open!."
  []
  (->FxSpaceHost (fx/host) (atom {})))
