(ns desargues.player.navigate
  "PURE stratum of the player: zooming, panning, orbiting by key and
   resetting the view, the same in every host.

   What the viewer looks through is a `look`, {:camera :view}: the orbited
   camera of a 3D frame (desargues.player.orbit) and the viewport of a 2D
   frame (desargues.player.view). nil in either is the source's own, so a
   reset is {:camera nil :view nil}. The player keeps the look apart from
   the params, so changing a slider never moves the viewer's zoom.

   Hosts only translate raw input into events (desargues.player.protocols):

     {:type :wheel  :at [x y] :dy px}      a wheel; dy > 0 scrolls down = out
     {:type :pinch  :at [x y] :factor f}   a trackpad pinch; f > 1 = in
     {:type :double :at [x y]}             a double click: reset the view
     {:type :key    :key \"ArrowLeft\" :mods #{:alt}}  a key (DOM key names)

   and `step` decides. A 3D frame zooms its camera toward the pointer, a 2D
   frame zooms its viewport about the pointer; both keep the point under the
   pointer fixed. Pans (a drag with shift, the middle or the right button,
   or a plain drag on empty 2D space; see desargues.player.pointer) move the
   picture with the pointer. `bindings` is the keyboard, as data.
   Host-free .cljc."
  (:require [clojure.string :as str]
            [desargues.player.orbit :as orbit]
            [desargues.player.space :as space]
            [desargues.player.view :as view]))

(def wheel-rate
  "Magnification per wheel pixel: a wheel `dy` magnifies exp(-dy * rate), so
   one 100 px notch is about 1.16 times, and opposite notches cancel."
  0.0015)

(def key-zoom
  "Magnification of one + key press (- divides by it)."
  1.25)

(def key-step
  "Scene units one arrow press pans (or turns, as pointer travel)."
  0.5)

(def bindings
  "Key (DOM KeyboardEvent.key; hosts translate their own) -> action:
   [:zoom m] magnify about the middle, [:pan [dx dy]] move the picture,
   [:turn [dx dy]] orbit as a pointer drag of that travel (3D only),
   [:reset] back to the source's own camera and viewport. With :alt held
   the arrows turn instead of pan."
  {"+" [:zoom key-zoom] "=" [:zoom key-zoom]
   "-" [:zoom (/ 1.0 key-zoom)] "_" [:zoom (/ 1.0 key-zoom)]
   "0" [:reset]
   "ArrowLeft"  [:pan [key-step 0.0]]     "ArrowRight" [:pan [(- key-step) 0.0]]
   "ArrowUp"    [:pan [0.0 (- key-step)]] "ArrowDown"  [:pan [0.0 key-step]]
   "a" [:turn [(- key-step) 0.0]] "d" [:turn [key-step 0.0]]
   "w" [:turn [0.0 key-step]]     "s" [:turn [0.0 (- key-step)]]})

(defn key-action
  "The action of key `k` with modifier set `mods`, or nil when unbound."
  [k mods]
  (let [k (if (= 1 (count (str k))) (str/lower-case (str k)) (str k))
        [op arg :as act] (bindings k)]
    (if (and (= :pan op) (contains? (set mods) :alt))
      [:turn (mapv - arg)]
      act)))

(defn look
  "The look {:camera :view} held in player state `st`."
  [st]
  (select-keys st [:camera :view]))

(defn- cam-of [lk f] (or (:camera lk) (space/camera f)))

(defn zoom
  "Look `lk` on frame `f` magnified `m` times about scene point `at`."
  [lk f m at]
  (if (space/three-d? f)
    (assoc lk :camera (orbit/zoom-at (cam-of lk f) m at))
    (assoc lk :view (view/zoom-at (:view lk) m at))))

(defn pan
  "Look `lk` on frame `f` with the picture moved by [dx dy] scene units."
  [lk f d]
  (if (space/three-d? f)
    (assoc lk :camera (orbit/pan (cam-of lk f) d))
    (assoc lk :view (view/pan (:view lk) d))))

(defn turn
  "Look `lk` on 3D frame `f` orbited as by a pointer travel `d`; nil for a
   2D frame, which has nothing to orbit."
  [lk f d]
  (when (space/three-d? f)
    (assoc lk :camera (orbit/turn (cam-of lk f) d))))

(defn reset
  "The source's own camera and viewport."
  [_lk]
  {:camera nil :view nil})

(defn describe
  "What look `lk` shows of frame `f`, for a REPL or a status probe:
   {:three-d? :camera :view :zoom}. :camera and :view are the viewer's
   overrides (nil = the source's own); :zoom is the magnification in effect
   (the camera's :zoom for 3D, the view's :scale for 2D)."
  [lk f]
  (let [three? (space/three-d? f)]
    {:three-d? three?
     :camera   (:camera lk)
     :view     (:view lk)
     :zoom     (if three?
                 (double (:zoom (cam-of lk f) 1.0))
                 (double (:scale (view/view (:view lk)))))}))

(defn act
  "Look `lk` on frame `f` after action `action` (see `bindings`) aimed at
   scene point `at`, or nil when the action does not apply."
  [lk f [op arg] at]
  (case op
    :zoom  (zoom lk f arg at)
    :pan   (pan lk f arg)
    :turn  (turn lk f arg)
    :reset (reset lk)
    nil))

(defn magnification
  "How much a :wheel or :pinch event magnifies (1.0 for anything else)."
  [{:keys [type dy factor]}]
  (case type
    :wheel (Math/exp (- (* wheel-rate (double (or dy 0.0)))))
    :pinch (double (or factor 1.0))
    1.0))

(def gesture-types
  "Event types `step` handles (the rest are pointer presses and moves)."
  #{:wheel :pinch :double :key})

(defn gesture?
  "An event for `step` rather than for a drag, an orbit or a pan."
  [event]
  (contains? gesture-types (:type event)))

(defn step
  "The look after gesture `event` (see the ns doc) on frame `f`, from look
   `lk`; nil when the event changes nothing (an unbound key, a turn key on a
   2D frame)."
  [lk f {:keys [type at key mods] :as event}]
  (let [at (or at [0.0 0.0])]
    (case type
      (:wheel :pinch) (zoom lk f (magnification event) at)
      :double         (reset lk)
      :key            (some-> (key-action key mods) (as-> a (act lk f a [0.0 0.0])))
      nil)))

;; ── Pan drags ───────────────────────────────────────────────────────────

(defn pan-gesture?
  "Does pointer press `event` ask for a pan whatever it lands on: the middle
   or the right button, or shift held?"
  [{:keys [button mods]}]
  (boolean (or (#{:middle :secondary} button) (contains? (set mods) :shift))))

(defn pan-pointer
  "One pointer `event` against pan value `p` ({:from :look}, nil when idle)
   on frame `f` with look `lk`. :down starts one, :move moves the picture by
   the travel since :down (from the look at :down, so nothing accumulates),
   :up ends it. Returns {:pan :look}, :look nil when unchanged."
  [p lk f {:keys [type at]}]
  (case type
    :down {:pan {:from at :look lk} :look nil}
    :move (if p
            {:pan p :look (pan (:look p) f (mapv - at (:from p)))}
            {:pan nil :look nil})
    :up   {:pan nil :look nil}
    {:pan p :look nil}))
