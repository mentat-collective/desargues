(ns desargues.player.native-input
  "PURE stratum of the player: the native (cljrs) host's raw window input
   translated into the player's input events (desargues.player.protocols
   IPointer and IKeyboard), as desargues.browser.input does for a page and
   desargues.player.javafx-input for JavaFX.

   desargues-native forwards winit events as EDN in canvas pixels
   (native/src/window.rs, through `serve` as {:ev :input :event raw}):

     {:type :down|:up :px [x y] :button b :mods [..] :ms t}
     {:type :move  :px [x y] :mods [..]}
     {:type :wheel :px [x y] :lines n | :pixels n :mods [..]}  winit signs: > 0 up
     {:type :pinch :px [x y] :delta d}                         d > 0 spreading
     {:type :key   :key \"ArrowLeft\" :text \"..\" :mods [..]}

   `translate` turns one into zero or more player events: pixels go back
   through the stage viewport the host painted with (frame/viewport, the
   inverse of its scene->pixel map), presses and moves are hit-tested on the
   frame the JVM last presented (desargues.player.hit, the one hit test), a
   wheel becomes :wheel with dy in browser pixels (> 0 scrolling down), a
   pinch :pinch with a factor, two quick primary clicks a :double after the
   second release, and a key :key (ctrl and meta chords stay the
   desktop's, as in the browser). What the events do is decided by the pure
   desargues.player.pointer/step. Host-free .cljc."
  (:require [desargues.player.frame :as frame]
            [desargues.player.hit :as hit]))

(def hit-tolerance-px
  "How far (pixels) the pointer may miss a shape and still grab it (the
   JavaFX and browser hosts use 8)."
  8.0)

(def line-px
  "Browser wheel pixels per winit scroll line: one notch, as a browser
   (about 100 px) and the JavaFX host (40 x 2.5) report one."
  100.0)

(def double-click-ms
  "Longest gap between two primary presses that makes a double click."
  400.0)

(def double-click-px
  "Farthest two presses of a double click may lie apart, pixels."
  6.0)

(defn initial
  "The translator's state before any event: the buttons held and the last
   primary press (for double clicks)."
  []
  {:pressed #{} :last-press nil :double? false})

(defn- mods [raw] (set (:mods raw)))

(defn scene-at
  "Scene point and hit under canvas pixel `px` of a `w` x `h` stage that
   shows frame `f` (nil: no hit test)."
  [w h f px]
  (let [vp (frame/viewport w h)]
    {:at  (frame/px->scene vp px)
     :hit (when f (hit/hit-px f vp px hit-tolerance-px))}))

(defn wheel-dy
  "Raw wheel event `raw`'s vertical delta in browser pixels (> 0 down)."
  [{:keys [lines pixels]}]
  (cond lines  (- (* line-px (double lines)))
        pixels (- (double pixels))
        :else  0.0))

(defn- near? [[ax ay] [bx by]]
  (<= (Math/hypot (- (double ax) (double bx)) (- (double ay) (double by))) double-click-px))

(defn- press [st {:keys [px ms button]}]
  (let [{lp :px lms :ms} (:last-press st)
        dbl? (boolean (and (= :primary button) lp ms lms
                           (<= (- (double ms) (double lms)) double-click-ms)
                           (near? lp px)))]
    (-> st
        (update :pressed conj button)
        (assoc :double? dbl?
               :last-press (when (and (= :primary button) (not dbl?)) {:px px :ms ms})))))

(defn translate
  "One raw native event `raw` against translator state `st` (see `initial`)
   on a `w` x `h` stage showing frame `f`. Returns [st' events], events the
   IPointer / IKeyboard maps for the player, in order."
  [st w h f {:keys [type px] :as raw}]
  (let [held (first (:pressed st))
        ptr  (fn [t b] (merge {:type t :button (or b :primary) :mods (mods raw)}
                              (scene-at w h f px)))]
    (case type
      :down  (let [st' (press st raw)] [st' [(ptr :down (:button raw))]])
      :up    (let [st' (-> st (update :pressed disj (:button raw)) (assoc :double? false))]
               [st' (cond-> [(ptr :up (:button raw))]
                      (:double? st) (conj {:type :double :at (:at (scene-at w h nil px))}))])
      :move  [st (if (seq (:pressed st)) [(ptr :move held)] [])]
      :wheel (let [dy (wheel-dy raw)]
               [st (if (zero? dy) []
                       [{:type :wheel :at (:at (scene-at w h nil px)) :dy dy :mods (mods raw)}])])
      :pinch [st [{:type :pinch :at (:at (scene-at w h nil px))
                   :factor (max 0.05 (+ 1.0 (double (:delta raw 0.0))))}]]
      :key   (let [m (mods raw)]
               [st (if (or (m :ctrl) (m :meta) (nil? (:key raw))) []
                       [{:type :key :key (:key raw) :mods m}])])
      [st []])))
