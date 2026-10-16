(ns desargues.player.pointer
  "PURE stratum of the player: one pointer event against the player's state.

   Both players (desargues.player on the JVM, desargues.browser.player in a
   page) feed every IPointer event through `step` and apply what it returns,
   so dragging and orbiting behave the same in every host.

   A press on a shape named by a drag spec starts a drag
   (desargues.player.drag); on a 3D frame, a press on empty space starts an
   orbit (desargues.player.orbit). The drag sees the frame through the
   orbited camera, the one the viewer is looking through, so a 3D point
   follows the pointer under any orbit. Host-free .cljc."
  (:require [desargues.player.drag :as drag]
            [desargues.player.orbit :as orbit]
            [desargues.player.space :as space]))

(defn seen-frame
  "Frame `f` as the viewer sees it: through orbited camera `cam` when `f` is
   3D and `cam` is set, else `f` itself."
  [f cam]
  (if (and cam (space/three-d? f)) (space/with-camera f cam) f))

(defn wants-pointer?
  "Does a player with drag `specs` showing frame `f` need pointer input?"
  [specs f]
  (boolean (or (some drag/drag-spec? specs) (space/three-d? f))))

(defn step
  "One pointer `event` ({:type :at :hit}) against player state `st`
   ({:drag :orbit :camera :params}), the drag `specs` and frame `f` as the
   source gives it. Returns {:drag :orbit :params :camera}: the new drag and
   orbit values, the new params when a drag moved them (else nil), and the
   new camera when an orbit turned it (else nil)."
  [{:keys [drag orbit camera params]} specs f event]
  (let [out (drag/pointer drag specs (seen-frame f camera) params event)]
    (if (or (:drag out) (not (space/three-d? f)))
      {:drag (:drag out) :orbit nil :params (:params out) :camera nil}
      (let [o (orbit/pointer orbit (or camera (space/camera f)) event)]
        {:drag nil :orbit (:orbit o) :params (:params out) :camera (:camera o)}))))
