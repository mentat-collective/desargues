(ns desargues.player.orbit
  "PURE stratum of the player: orbiting a 3D camera with the pointer.

   An orbit is a small value threaded through `pointer`, like a drag
   (desargues.player.drag): :down on empty space starts one, remembering the
   grab point and the camera at that moment; :move turns the camera by the
   pointer's travel (yaw from x, pitch from y); :up ends it. Pointer
   positions are scene units (IPointer events), so the turn rate does not
   depend on window size.

   `zoom` scales the camera distance (a wheel or a pinch). Host-free .cljc."
  (:require [desargues.player.space :as space]))

(def radians-per-unit
  "How far the camera turns per scene unit of pointer travel."
  0.35)

(def pitch-limit
  "Pitch is clamped short of the poles, where yaw is undefined."
  1.5)

(defn- clamp [lo hi x] (max lo (min hi (double x))))

(defn turn
  "Camera `cam` turned by pointer travel [dx dy] (scene units): dragging
   right turns the scene right (yaw decreases), dragging up tilts it toward
   the viewer from above (pitch increases)."
  [cam [dx dy]]
  (let [cam (merge space/default-camera cam)]
    (assoc cam
           :yaw   (- (double (:yaw cam)) (* radians-per-unit (double dx)))
           :pitch (clamp (- pitch-limit) pitch-limit
                         (- (double (:pitch cam)) (* radians-per-unit (double dy)))))))

(defn zoom
  "Camera `cam` with its distance scaled by `k` (> 1 moves away)."
  [cam k]
  (let [cam (merge space/default-camera cam)]
    (assoc cam :distance (clamp 0.5 1e4 (* (double (:distance cam)) (double k))))))

(defn pointer
  "One pointer `event` ({:type :at :hit}) against orbit value `orbit` (nil
   when idle) and camera `cam`. Returns {:orbit orbit' :camera cam'}; cam' is
   nil when unchanged. A :down only starts an orbit when it hits nothing, so
   draggable shapes keep their drag."
  [orbit cam {:keys [type at hit]}]
  (case type
    :down (if (nil? hit)
            {:orbit {:from at :camera (merge space/default-camera cam)} :camera nil}
            {:orbit nil :camera nil})
    :move (if-let [{:keys [from camera]} orbit]
            {:orbit orbit :camera (turn camera (mapv - at from))}
            {:orbit nil :camera nil})
    :up   {:orbit nil :camera nil}
    {:orbit orbit :camera nil}))
