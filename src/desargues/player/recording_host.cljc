(ns desargues.player.recording-host
  "BOUNDARY stratum (toolkit-free): a host with no window that RECORDS what
   a real host would show. It implements ISurface, IWindow, IControls,
   IControlFeedback and IClock over one atom, so tests (and a CI job, or a
   cljs page without a DOM) drive the whole player: every widget a spec asks
   for is kept as data, `user!` plays a user acting on a widget (a slider
   drag, a click on a toggle, a choice in a select, typing, a press) through
   the same on-change a JavaFX or browser widget would call, and `tick!`
   advances the clock by hand.

   State: {:widgets {id spec} :order [id ...] :values {id shown}
           :errors {id msg} :frames n :frame last-frame :shown? bool}.
   Host-free .cljc."
  (:require [desargues.player.protocols :as p]))

(defn- assoc-widget [st {:keys [id] :as spec} on-change]
  (-> st
      (assoc-in [:widgets id] spec)
      (assoc-in [:on-change id] on-change)
      (assoc-in [:values id] (:value spec))
      (update :order (fnil conj []) id)))

(defrecord RecordingHost [state]
  p/ISurface
  (-present! [this f]
    (swap! state #(-> % (assoc :frame f) (update :frames (fnil inc 0))))
    this)
  (-size [_] (:size @state [1280 720]))

  p/IWindow
  (-show! [this opts] (swap! state assoc :shown? true :window opts) this)
  (-close! [this] (swap! state assoc :shown? false) this)
  (-snapshot! [_ _] nil)

  p/IControls
  (-control! [this spec on-change] (swap! state assoc-widget spec on-change) this)
  (-set-control! [this id v]
    (when (contains? (:widgets @state) id)
      (swap! state assoc-in [:values id] v))
    this)

  p/IControlFeedback
  (-control-error! [this id msg]
    (swap! state #(if msg (assoc-in % [:errors id] msg) (update % :errors dissoc id)))
    this)

  p/IClock
  (-on-tick! [this f] (swap! state assoc :tick f) this)
  (-stop-ticks! [this] (swap! state dissoc :tick) this))

(defn host
  "A fresh recording host."
  []
  (->RecordingHost (atom {})))

(defn widgets
  "The widget specs the player asked for, in order."
  [h]
  (let [{:keys [order widgets]} @(:state h)]
    (mapv widgets order)))

(defn shown
  "What widget `id` shows now (its last -control!/-set-control! value)."
  [h id]
  (get-in @(:state h) [:values id]))

(defn error
  "The refusal message shown beside widget `id`, or nil."
  [h id]
  (get-in @(:state h) [:errors id]))

(defn user!
  "Act on widget `id` as a user would: the widget shows `raw` (a text field
   keeps what was typed, a slider sits where it was dropped) and the
   player's on-change fires with it. A button press passes nil."
  [h id raw]
  (let [f (get-in @(:state h) [:on-change id])]
    (when-not f
      (throw (ex-info (str "no widget " (pr-str id)) {:id id})))
    (when-not (= :button (get-in @(:state h) [:widgets id :kind]))
      (swap! (:state h) assoc-in [:values id] raw))
    (f id raw)
    h))

(defn tick!
  "Advance the host clock by `dt` seconds (calls the player's tick fn)."
  [h dt]
  (when-let [f (:tick @(:state h))] (f dt))
  h)
