(ns desargues.browser.input
  "BOUNDARY stratum: raw browser input translated into the player's input
   events (desargues.player.protocols IPointer and IKeyboard), for the
   canvas host (desargues.browser.canvas).

   Pointer events over the stage canvas become :down / :move / :up in scene
   units, hit-tested on the frame last painted (already seen through the
   viewer's camera or viewport), with :button and :mods. A wheel becomes
   :wheel (dy in pixels); a wheel with ctrlKey is what browsers report for a
   trackpad pinch, and becomes :pinch. A double click becomes :double. Keys
   pressed anywhere on the page outside a widget become :key, and the key's
   default action (a page scroll) is stopped when the player used it. What
   any of it does is decided by the pure desargues.player.pointer/step."
  (:require [desargues.player.frame :as frame]
            [desargues.player.hit :as hit]))

(def ^:private buttons
  "DOM MouseEvent.button -> IPointer :button."
  {0 :primary 1 :middle 2 :secondary})

(def pinch-rate
  "Magnification per pixel of a ctrl+wheel (pinch) delta."
  0.01)

(defn- mods
  "The modifier set of DOM event `e`."
  [e]
  (cond-> #{}
    (.-shiftKey e) (conj :shift)
    (.-altKey e)   (conj :alt)
    (.-ctrlKey e)  (conj :ctrl)
    (.-metaKey e)  (conj :meta)))

(defn- scene-at
  "Scene point and the hit under DOM event `e` on `canvas`."
  [state canvas e]
  (let [{:keys [w h last-frame]} @state
        r  (.getBoundingClientRect canvas)
        px [(- (.-clientX e) (.-left r)) (- (.-clientY e) (.-top r))]
        vp (frame/viewport w h)]
    {:at (frame/px->scene vp px)
     :hit (when last-frame (hit/hit-px last-frame vp px 8))}))

(defn- wheel-px
  "Wheel event `e`'s vertical delta in pixels (lines and pages scaled)."
  [state e]
  (* (.-deltaY e) (case (.-deltaMode e) 1 16 2 (:h @state 540) 1)))

(defn wire-pointer!
  "Report pointer, wheel, pinch and double-click events over the host's
   canvas to `f`. Errors are kept in the host state as :tick-error."
  [state f]
  (when-let [canvas (:canvas @state)]
    (let [send! (fn [ev]
                  (try (f ev) (catch :default err (swap! state assoc :tick-error err))))
          press (fn [type e]
                  (send! (merge {:type type :button (buttons (.-button e) :primary) :mods (mods e)}
                                (scene-at state canvas e))))]
      (set! (.. canvas -style -touchAction) "none")
      (.addEventListener canvas "pointerdown"
                         (fn [e]
                           (.preventDefault e)
                           (.focus canvas)
                           (.setPointerCapture canvas (.-pointerId e))
                           (press :down e)))
      (.addEventListener canvas "pointermove"
                         (fn [e] (when (pos? (.-buttons e)) (press :move e))))
      (.addEventListener canvas "pointerup" (fn [e] (press :up e)))
      (.addEventListener canvas "pointercancel" (fn [e] (press :up e)))
      (.addEventListener canvas "contextmenu" (fn [e] (.preventDefault e)))
      (.addEventListener canvas "dblclick"
                         (fn [e] (send! (assoc (scene-at state canvas e) :type :double))))
      (.addEventListener canvas "wheel"
                         (fn [e]
                           (.preventDefault e)
                           (let [at (:at (scene-at state canvas e))
                                 dy (wheel-px state e)]
                             (send! (if (.-ctrlKey e)
                                      {:type :pinch :at at :factor (js/Math.exp (- (* pinch-rate dy)))}
                                      {:type :wheel :at at :dy dy :mods (mods e)}))))
                         #js {:passive false}))))

(defn- typing?
  "Is key event `e` aimed at a widget that takes text or choices?"
  [e]
  (let [t (.-target e)]
    (or (contains? #{"INPUT" "SELECT" "TEXTAREA" "BUTTON"} (some-> t .-tagName))
        (some-> t .-isContentEditable))))

(defn wire-keys!
  "Report key presses on the page (outside widgets, without ctrl or meta,
   which belong to the browser) to `f`; stop a key's default action when
   `f` returns true. The listener is kept in the host state for `unwire!`."
  [state f]
  (let [h (fn [e]
            (when-not (or (typing? e) (.-ctrlKey e) (.-metaKey e))
              (when (try (f {:type :key :key (.-key e) :mods (mods e)})
                         (catch :default err (swap! state assoc :tick-error err) nil))
                (.preventDefault e))))]
    (.addEventListener js/window "keydown" h)
    (swap! state assoc :key-listener h)))

(defn unwire!
  "Remove the page-wide key listener `wire-keys!` installed, if any."
  [state]
  (when-let [h (:key-listener @state)]
    (.removeEventListener js/window "keydown" h)
    (swap! state dissoc :key-listener)))
