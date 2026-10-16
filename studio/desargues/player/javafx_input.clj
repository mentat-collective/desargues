(ns desargues.player.javafx-input
  "BOUNDARY stratum: JavaFX input translated into the player's input events
   (desargues.player.protocols IPointer and IKeyboard), for the JavaFX host
   (desargues.player.javafx).

   Mouse presses, drags and releases over the canvas become :down / :move /
   :up in scene units, hit-tested on the frame last presented (already seen
   through the viewer's camera or viewport), with :button and :mods. A
   ScrollEvent becomes :wheel (pixels, the browser's sign: > 0 scrolls
   down), a ZoomEvent (trackpad pinch) :pinch, a double click :double, and
   a key pressed while the canvas has focus :key, named as the DOM names it
   (\"ArrowLeft\", \"+\"); a key the player used is consumed, so arrows do
   not move the focus. What any of it does is decided by the pure
   desargues.player.pointer/step. Requires the :studio alias (OpenJFX)."
  (:require [desargues.player.frame :as frame]
            [desargues.player.hit :as hit])
  (:import [javafx.event EventHandler EventType]
           [javafx.scene.canvas Canvas]
           [javafx.scene.input KeyCode KeyEvent MouseButton MouseEvent ScrollEvent ZoomEvent]))

(def hit-tolerance-px
  "How far (pixels) the pointer may miss a shape and still grab it."
  8.0)

(def ^:private mouse-event-type
  "JavaFX mouse event -> IPointer event :type."
  {MouseEvent/MOUSE_PRESSED  :down
   MouseEvent/MOUSE_DRAGGED  :move
   MouseEvent/MOUSE_MOVED    :move
   MouseEvent/MOUSE_RELEASED :up})

(def ^:private buttons
  "JavaFX MouseButton -> IPointer :button."
  {MouseButton/PRIMARY :primary MouseButton/MIDDLE :middle MouseButton/SECONDARY :secondary})

(def ^:private key-names
  "JavaFX key codes whose DOM name is not the text they type."
  {KeyCode/LEFT "ArrowLeft" KeyCode/RIGHT "ArrowRight" KeyCode/UP "ArrowUp"
   KeyCode/DOWN "ArrowDown" KeyCode/ADD "+" KeyCode/SUBTRACT "-"
   KeyCode/NUMPAD0 "0"})

(def scroll-px
  "Browser wheel pixels per JavaFX scroll unit (JavaFX reports 40 per notch,
   a browser about 100), so a notch zooms the same in both hosts."
  2.5)

(defn- mods [shift? alt? ctrl? meta?]
  (cond-> #{} shift? (conj :shift) alt? (conj :alt) ctrl? (conj :ctrl) meta? (conj :meta)))

(defn- scene-at
  "Scene point and hit under canvas pixel [x y], given the presented frame `f`."
  [^Canvas canvas f x y]
  (let [vp (frame/viewport (.getWidth canvas) (.getHeight canvas))
        px [x y]]
    {:at  (frame/px->scene vp px)
     :hit (when f (hit/hit-px f vp px hit-tolerance-px))}))

(defn pointer-event
  "The IPointer event for JavaFX mouse event `e` on `canvas`, given the last
   presented Frame `f`: pixels -> scene units, then the pure hit test."
  [^Canvas canvas f ^MouseEvent e]
  (merge {:type   (mouse-event-type (.getEventType e))
          :button (buttons (.getButton e) :primary)
          :mods   (mods (.isShiftDown e) (.isAltDown e) (.isControlDown e) (.isMetaDown e))}
         (scene-at canvas f (.getX e) (.getY e))))

(defn key-name
  "The DOM KeyboardEvent.key name of JavaFX key event `e`, or nil."
  [^KeyEvent e]
  (or (key-names (.getCode e))
      (let [t (.getText e)] (when (seq t) t))))

(defn- handler ^EventHandler [f]
  (reify EventHandler (handle [_ e] (f e))))

(defn wire!
  "Forward the canvas' mouse, scroll, zoom and key events to the host's
   :on-pointer and :on-key fns (in host atom `state`), when set."
  [state ^Canvas canvas]
  (let [send! (fn [ev]
                (when-let [f (:on-pointer @state)]
                  (try (f ev) (catch Throwable t (swap! state assoc :pointer-error t)))))
        mouse (handler (fn [^MouseEvent e]
                         (when (= MouseEvent/MOUSE_PRESSED (.getEventType e)) (.requestFocus canvas))
                         (send! (pointer-event canvas (:frame @state) e))))]
    (.setFocusTraversable canvas true)
    (doseq [t (keys mouse-event-type)]
      (.addEventHandler canvas ^EventType t mouse))
    (.addEventHandler canvas MouseEvent/MOUSE_CLICKED
                      (handler (fn [^MouseEvent e]
                                 (when (= 2 (.getClickCount e))
                                   (send! (assoc (scene-at canvas (:frame @state) (.getX e) (.getY e))
                                                 :type :double))))))
    (.addEventHandler canvas ScrollEvent/SCROLL
                      (handler (fn [^ScrollEvent e]
                                 (when-not (zero? (.getDeltaY e))
                                   (send! {:type :wheel
                                           :at   (:at (scene-at canvas nil (.getX e) (.getY e)))
                                           :dy   (* (- scroll-px) (.getDeltaY e))
                                           :mods (mods (.isShiftDown e) (.isAltDown e)
                                                       (.isControlDown e) (.isMetaDown e))})
                                   (.consume e)))))
    (.addEventHandler canvas ZoomEvent/ZOOM
                      (handler (fn [^ZoomEvent e]
                                 (send! {:type   :pinch
                                         :at     (:at (scene-at canvas nil (.getX e) (.getY e)))
                                         :factor (.getZoomFactor e)})
                                 (.consume e))))
    (.addEventHandler canvas KeyEvent/KEY_PRESSED
                      (handler (fn [^KeyEvent e]
                                 (when-let [f (:on-key @state)]
                                   (when-let [k (key-name e)]
                                     (when (try (f {:type :key :key k
                                                    :mods (mods (.isShiftDown e) (.isAltDown e)
                                                                (.isControlDown e) (.isMetaDown e))})
                                                (catch Throwable t (swap! state assoc :pointer-error t) nil))
                                       (.consume e)))))))))
