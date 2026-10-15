(ns desargues.player.javafx
  "BOUNDARY stratum: a JavaFX host for the player (desargues.player).

   Implements every port of desargues.player.protocols over one JavaFX Stage:
   ISurface (a Canvas), IWindow (the Stage, PNG snapshots), IControls (Sliders
   in a side panel) and IClock (an AnimationTimer). It is the only namespace
   that names JavaFX; another toolkit is another namespace implementing the
   same ports, and the player cannot tell them apart (LSP).

   Painting is one `paint!` method per scene-graph node kind (OCP).
   Requires the :studio alias (OpenJFX on the classpath)."
  (:require [desargues.player.frame :as frame]
            [desargues.player.protocols :as p])
  (:import [java.awt.image BufferedImage]
           [java.io File]
           [javafx.animation AnimationTimer]
           [javafx.application Platform]
           [javafx.beans.value ChangeListener]
           [javafx.geometry Insets VPos]
           [javafx.scene Scene]
           [javafx.scene.canvas Canvas GraphicsContext]
           [javafx.scene.control Label Slider]
           [javafx.scene.image WritableImage]
           [javafx.scene.layout BorderPane VBox]
           [javafx.scene.paint Color]
           [javafx.scene.shape ArcType]
           [javafx.scene.text Font TextAlignment]
           [javafx.stage Stage]
           [javax.imageio ImageIO]
[javafx.scene.layout Pane]))

;; ── Toolkit thread ──────────────────────────────────────────────────────────

(defonce ^:private started? (atom false))

(defn- ensure-toolkit!
  "Start the JavaFX platform once per JVM and keep it alive across windows."
  []
  (when (compare-and-set! started? false true)
    (let [ready (promise)]
      (try (Platform/startup #(deliver ready true))
           (catch IllegalStateException _ (deliver ready true)))
      @ready
      (Platform/setImplicitExit false))))

(defn- on-fx
  "Run `f` on the JavaFX thread and return its value (rethrowing its error)."
  [f]
  (if (Platform/isFxApplicationThread)
    (f)
    (let [out (promise)]
      (Platform/runLater #(deliver out (try {:ok (f)} (catch Throwable t {:err t}))))
      (let [{:keys [ok err]} @out]
        (if err (throw err) ok)))))

;; ── Colors and units ────────────────────────────────────────────────────────

(defn- fx-color ^Color [k]
  (Color/web (get frame/palette k (frame/palette :white))))

(defn- stroke-color ^Color [s]
  (let [c (fx-color (or (get-in s [:stroke :color]) (get-in s [:opts :color]) :white))]
    (if-let [{:keys [to alpha]} (:recolor s)]
      (.interpolate c (fx-color to) (double alpha))
      c)))

(defn- fill-color
  "The fill of shape `s`, or nil when it has none."
  ^Color [s]
  (when-let [{:keys [color opacity]} (:fill s)]
    (let [c (fx-color color)]
      (Color. (.getRed c) (.getGreen c) (.getBlue c) (double (or opacity 1.0))))))

(defn- units->px [vp u] (* (double (:scale vp)) (double u)))

(defn- stroke-px
  "Manim stroke widths are pixels at 1080p (135 px per scene unit)."
  [vp s]
  (max 1.0 (* (double (or (get-in s [:stroke :width]) (get-in s [:opts :width]) 4))
              (/ (double (:scale vp)) 135.0))))

(defn- emphasis-scale [s] (+ 1.0 (* 0.25 (double (:emphasis s 0.0)))))

(defn- center [vp s] (frame/->px vp (:at s [0 0])))

;; ── Painting: one method per node kind (OCP) ────────────────────────────────

(defmulti paint!
  "Paint shape `s` on GraphicsContext `gc` under viewport `vp`."
  (fn [_gc _vp s] (:node s)))

(defmethod paint! :default [_ _ _] nil)

(defmethod paint! :dot [^GraphicsContext gc vp s]
  (let [[cx cy] (center vp s)
        r (* (units->px vp (get-in s [:opts :radius] 0.08)) (emphasis-scale s))]
    (.setFill gc (or (fill-color s) (stroke-color s)))
    (.fillOval gc (- cx r) (- cy r) (* 2 r) (* 2 r))))

(defmethod paint! :circle [^GraphicsContext gc vp s]
  (let [[cx cy] (center vp s)
        r (* (units->px vp (get-in s [:opts :radius] 1.0)) (emphasis-scale s))]
    (when-let [f (fill-color s)]
      (.setFill gc f)
      (.fillOval gc (- cx r) (- cy r) (* 2 r) (* 2 r)))
    (.setStroke gc (stroke-color s))
    (.setLineWidth gc (stroke-px vp s))
    (.strokeArc gc (- cx r) (- cy r) (* 2 r) (* 2 r)
                90.0 (* -360.0 (double (:draw s 1.0))) ArcType/OPEN)))

(defmethod paint! :line [^GraphicsContext gc vp s]
  (let [[x0 y0] (frame/->px vp (:from s))
        [x1 y1] (frame/->px vp (:to s))
        d (double (:draw s 1.0))]
    (.setStroke gc (stroke-color s))
    (.setLineWidth gc (stroke-px vp s))
    (.strokeLine gc x0 y0 (+ x0 (* d (- x1 x0))) (+ y0 (* d (- y1 y0))))))

(defmethod paint! :path [^GraphicsContext gc vp s]
  (let [pts (mapv #(frame/->px vp %) (:points s))
        n   (long (Math/ceil (* (double (:draw s 1.0)) (count pts))))
        pts (cond-> (subvec pts 0 (min n (count pts)))
              (get-in s [:opts :closed?]) (conj (first pts)))]
    (when (> (count pts) 1)
      (.setStroke gc (stroke-color s))
      (.setLineWidth gc (stroke-px vp s))
      (.strokePolyline gc (double-array (map first pts)) (double-array (map second pts))
                       (count pts)))))

(defn- paint-box! [^GraphicsContext gc vp s corner]
  (let [[cx cy] (center vp s)
        k (emphasis-scale s)
        w (* k (units->px vp (get-in s [:opts :width] 4.0)))
        h (* k (units->px vp (get-in s [:opts :height] 2.0)))
        a (* 2 (units->px vp corner))
        x (- cx (/ w 2)) y (- cy (/ h 2))]
    (when-let [f (fill-color s)]
      (.setFill gc f)
      (.fillRoundRect gc x y w h a a))
    (.setStroke gc (stroke-color s))
    (.setLineWidth gc (stroke-px vp s))
    (.strokeRoundRect gc x y w h a a)))

(defmethod paint! :rectangle [gc vp s] (paint-box! gc vp s 0.0))

(defmethod paint! :rounded-rectangle [gc vp s]
  (paint-box! gc vp s (get-in s [:opts :corner-radius] 0.25)))

(defn- paint-text! [^GraphicsContext gc vp s ^String text]
  (let [[cx cy] (center vp s)
        size (* (emphasis-scale s) (double (get-in s [:opts :font-size] 48))
                (/ (double (:scale vp)) 120.0))]
    (.setFont gc (Font. size))
    (.setTextAlign gc TextAlignment/CENTER)
    (.setTextBaseline gc VPos/CENTER)
    (.setFill gc (stroke-color s))
    (.fillText gc text cx cy)))

(defmethod paint! :text [gc vp s] (paint-text! gc vp s (str (:text s))))

(defmethod paint! :decimal [gc vp s]
  (paint-text! gc vp s (format (str "%." (get-in s [:opts :num-decimal-places] 2) "f")
                               (double (:value s 0)))))

(defn- paint-frame! [^Canvas canvas f]
  (let [gc (.getGraphicsContext2D canvas)
        w  (.getWidth canvas)
        h  (.getHeight canvas)
        vp (frame/viewport w h)]
    (.setGlobalAlpha gc 1.0)
    (.setFill gc Color/BLACK)
    (.fillRect gc 0 0 w h)
    (doseq [s (frame/visible-shapes f)]
      (.save gc)
      (.setGlobalAlpha gc (double (:opacity s)))
      (paint! gc vp s)
      (.restore gc))))

;; ── Snapshots ───────────────────────────────────────────────────────────────

(defn- write-png! [^WritableImage img path]
  (let [w  (int (.getWidth img))
        h  (int (.getHeight img))
        rd (.getPixelReader img)
        bi (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)]
    (dotimes [y h]
      (dotimes [x w]
        (.setRGB bi x y (.getArgb rd x y))))
    (ImageIO/write bi "png" (File. (str path)))
    (str path)))

;; ── The host ────────────────────────────────────────────────────────────────

(defn- slider-listener [state label ^Label lbl id on-change]
  (reify ChangeListener
    (changed [_ _ _ nv]
      (.setText lbl (format "%s  %.3f" label (double nv)))
      (when-not (:muted? @state)
        (on-change id (double nv))))))

(defrecord FxHost [state]
  p/ISurface
  (-present! [this f]
    (on-fx #(when-let [c (:canvas @state)] (paint-frame! c f)))
    this)
  (-size [_]
    (let [^Canvas c (:canvas @state)] [(.getWidth c) (.getHeight c)]))

  p/IWindow
  (-show! [this {:keys [title w h]}]
    (ensure-toolkit!)
    (on-fx
     (fn []
       (let [canvas   (Canvas. (double w) (double h))
             ;; The canvas follows its pane, so a tiling window manager or a
             ;; user resize re-lays the scene instead of cropping it.
             pane     (doto (Pane.)
                        (.setPrefSize (double w) (double h))
                        (.setMinSize 0.0 0.0))
             controls (doto (VBox. 6.0)
                        (.setPadding (Insets. 14.0))
                        (.setPrefWidth 260.0)
                        (.setMinWidth 220.0)
                        (.setStyle "-fx-background-color: #1b1b1f;"))
             root     (doto (BorderPane.)
                        (.setCenter pane)
                        (.setRight controls)
                        (.setStyle "-fx-background-color: #000000;"))
             stage    (doto (Stage.)
                        (.setTitle (str title))
                        (.setScene (Scene. root)))]
         (.add (.getChildren pane) canvas)
         (.bind (.widthProperty canvas) (.widthProperty pane))
         (.bind (.heightProperty canvas) (.heightProperty pane))
         (.show stage)
         (swap! state assoc :stage stage :canvas canvas :controls-box controls))))
    this)
  (-close! [this]
    (on-fx #(some-> ^Stage (:stage @state) .close))
    this)
  (-snapshot! [_ path]
    (on-fx #(write-png! (.snapshot (.getScene ^Stage (:stage @state)) nil) path)))

  p/IControls
  (-control! [this {:keys [id label min max value]} on-change]
    (on-fx
     (fn []
       (let [label (or label (name id))
             lbl   (doto (Label. (format "%s  %.3f" label (double value)))
                     (.setStyle "-fx-text-fill: #d8d8d8; -fx-font-size: 13px;"))
             sl    (Slider. (double min) (double max) (double value))]
         (.addListener (.valueProperty sl) (slider-listener state label lbl id on-change))
         (.addAll (.getChildren ^VBox (:controls-box @state)) [lbl sl])
         (swap! state assoc-in [:sliders id] sl))))
    this)
  (-set-control! [this id v]
    (on-fx
     (fn []
       (when-let [^Slider sl (get-in @state [:sliders id])]
         (swap! state assoc :muted? true)
         (try (.setValue sl (double v))
              (finally (swap! state assoc :muted? false))))))
    this)

  p/IClock
  (-on-tick! [this f]
    (on-fx
     (fn []
       (let [last  (atom nil)
             timer (proxy [AnimationTimer] []
                     (handle [now]
                       (let [dt (if-let [l @last] (/ (- now l) 1e9) 0.0)]
                         (reset! last now)
                         (try (f dt)
                              (catch Throwable t (swap! state assoc :tick-error t))))))]
         (.start timer)
         (swap! state assoc :timer timer))))
    this)
  (-stop-ticks! [this]
    (on-fx #(some-> ^AnimationTimer (:timer @state) .stop))
    this))

(defn host
  "A fresh, unopened JavaFX host. Hand it to desargues.player/open!."
  []
  (->FxHost (atom {})))
