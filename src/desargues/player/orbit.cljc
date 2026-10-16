(ns desargues.player.orbit
  "PURE stratum of the player: moving a 3D camera.

   An orbit is a small value threaded through `pointer`, like a drag
   (desargues.player.drag): :down on empty space starts one, remembering the
   grab point and the camera at that moment; :move turns the camera by the
   pointer's travel (yaw from x, pitch from y); :up ends it. Pointer
   positions are scene units (IPointer events), so the turn rate does not
   depend on window size.

   `zoom-at` magnifies toward a scene point (a wheel, a pinch, a key) and
   `pan` slides the target in the camera's right/up plane (a shift-drag, an
   arrow key); both keep the point under the pointer where it was. `zoom`
   only scales the distance. desargues.player.navigate routes the input.
   Host-free .cljc."
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

(def distance-limits
  "The nearest and farthest a camera may sit from its target."
  [0.5 1e4])

(def zoom-limits
  "The smallest and largest :zoom (target-plane magnification) of a camera."
  [0.05 50.0])

(defn- ortho? [cam] (= :orthographic (:projection cam)))

(defn- shift-target
  "Camera `cam` with its target moved by the scene-plane offset [dx dy]
   (camera right and up, in world units at the target)."
  [cam [dx dy]]
  (let [{:keys [right up]} (space/basis cam)]
    (assoc cam :target (space/v+ (mapv double (:target cam))
                                 (space/v+ (space/v* (double dx) right)
                                           (space/v* (double dy) up))))))

(defn pan
  "Camera `cam` with its target moved in the camera's right/up plane so the
   picture follows a pointer travel of [dx dy] scene units (dragging right
   moves the picture right). On the target plane a scene unit is 1 / :zoom
   world units, so the point grabbed stays under the pointer."
  [cam [dx dy]]
  (let [cam (merge space/default-camera cam)
        k   (double (:zoom cam 1.0))]
    (shift-target cam [(/ (- (double dx)) k) (/ (- (double dy)) k)])))

(defn zoom-at
  "Camera `cam` magnified `m` times (m > 1 zooms in) toward scene point `at`.
   The camera's :zoom (the target plane's magnification) is multiplied by m;
   a perspective camera also moves m times closer, as a real camera walking
   in would, so near things grow faster than far ones. The target slides
   toward the point of the target plane under `at`, so that point stays
   under the pointer. Clamped to `zoom-limits` and `distance-limits`, so `m`
   may be only partly applied."
  [cam m at]
  (let [cam   (merge space/default-camera cam)
        zm    (double (:zoom cam 1.0))
        d     (double (:distance cam))
        [zl zh] zoom-limits
        [dl dh] distance-limits
        m     (cond-> (clamp (/ zl zm) (/ zh zm) m)
                (not (ortho? cam)) (->> (clamp (/ d dh) (/ d dl))))
        cam'  (cond-> (assoc cam :zoom (* zm m))
                (not (ortho? cam)) (assoc :distance (/ d m)))
        tgt   (mapv double (:target cam))
        p     (space/ray-plane (space/unproject cam at)
                               {:point tgt :normal (:forward (space/basis cam))})]
    (if p
      (assoc cam' :target (space/v+ tgt (space/v* (- 1.0 (/ 1.0 m)) (space/v- p tgt))))
      cam')))

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
