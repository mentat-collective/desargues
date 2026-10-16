(ns desargues.player.pointer
  "PURE stratum of the player: one input event against the player's state.

   Both players (desargues.player on the JVM, desargues.browser.player in a
   page) feed every IPointer and IKeyboard event through `step` and apply
   what it returns, so dragging, orbiting, zooming and panning behave the
   same in every host.

   A press on a shape named by a drag spec starts a drag
   (desargues.player.drag); on a 3D frame, a press on empty space starts an
   orbit (desargues.player.orbit); a shift, middle or right press, or a press
   on empty 2D space, starts a pan; wheels, pinches, keys and double clicks
   zoom, pan, turn and reset (desargues.player.navigate). The drag sees the
   frame through the orbited camera, the one the viewer is looking through,
   so a 3D point follows the pointer under any orbit, and a 2D drag sees
   pointer positions taken back through the viewport (desargues.player.view).
   Host-free .cljc."
  (:require [desargues.player.drag :as drag]
            [desargues.player.navigate :as navigate]
            [desargues.player.orbit :as orbit]
            [desargues.player.space :as space]
            [desargues.player.view :as view]))

(defn seen-frame
  "Frame `f` as the viewer sees it: through orbited camera `cam` when `f` is
   3D and `cam` is set, else `f` itself."
  [f cam]
  (if (and cam (space/three-d? f)) (space/with-camera f cam) f))

(defn wants-pointer?
  "Does a player with drag `specs` showing frame `f` need pointer input?
   Always: every frame can be zoomed and panned (a 3D one orbited, and drag
   specs dragged, too). Kept so hosts and players ask one question."
  [_specs _f]
  true)

(defn step
  "One input `event` against player state `st` ({:drag :orbit :pan :camera
   :view :params}), the drag `specs` and frame `f` as the source gives it.
   Returns {:drag :orbit :pan :params :look}: the new drag, orbit and pan
   values, the new params when a drag moved them (else nil), and the new look
   {:camera :view} when the viewer's camera or viewport changed (else nil;
   nil inside it is the source's own).

   Priority: a gesture (wheel, pinch, key, double click) goes to
   desargues.player.navigate; a pan in progress keeps the pointer; a press
   on a draggable shape drags (with the primary button, shift or not); then
   a press with the middle or right button or shift, or any press on empty
   2D space, pans; and a press on empty 3D space orbits. A 2D frame's
   pointer positions are taken back through the view first, so a drag
   writes source scene units whatever the zoom."
  [{:keys [drag orbit pan camera view params] :as st} specs f {:keys [type button] :as event}]
  (let [lk     (navigate/look st)
        three? (space/three-d? f)
        idle   {:drag drag :orbit orbit :pan pan :params nil :look nil}]
    (cond
      (navigate/gesture? event)
      (assoc idle :look (navigate/step lk f event))

      pan
      (let [o (navigate/pan-pointer pan lk f event)]
        (assoc idle :pan (:pan o) :look (:look o)))

      :else
      (let [ev  (if three? event (view/source-event view event))
            out (if (#{:middle :secondary} button)
                  {:drag nil :params nil}
                  (drag/pointer drag specs (seen-frame f camera) params ev))]
        (cond
          (:drag out)
          {:drag (:drag out) :orbit nil :pan nil :params (:params out) :look nil}

          (and (= :down type) (or (not three?) (navigate/pan-gesture? event)))
          (let [o (navigate/pan-pointer nil lk f event)]
            {:drag nil :orbit nil :pan (:pan o) :params nil :look nil})

          three?
          (let [o (orbit/pointer orbit (or camera (space/camera f)) event)]
            {:drag nil :orbit (:orbit o) :pan nil :params (:params out)
             :look (when-let [c (:camera o)] (assoc lk :camera c))})

          :else
          {:drag nil :orbit nil :pan nil :params (:params out) :look nil})))))

(defn settle
  "Player state `st` after `step` outcome `out`: the new drag, orbit and pan
   values, and the new look when it changed. Params are the player's to
   apply (they rebuild the source)."
  [st out]
  (cond-> (assoc st :drag (:drag out) :orbit (:orbit out) :pan (:pan out))
    (:look out) (merge (select-keys (:look out) [:camera :view]))))
