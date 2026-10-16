(ns desargues.player.present
  "PROTOCOL stratum of the player: handing a Frame to a host.

   The one place that decides how a frame reaches a surface. A 2D frame goes
   to ISurface/-present! as is. A 3D frame (desargues.player.space) goes to
   ISpace/-present-space! when the host renders space natively, and is
   otherwise projected to 2D first (space/->2d), so a 2D host (headless PNG,
   browser canvas) shows 3D content without a line of 3D code (ISP, LSP).
   Host-free .cljc."
  (:require [desargues.player.protocols :as p]
            [desargues.player.space :as space]))

(defn native-space?
  "Does `host` render 3D frames itself?"
  [host]
  (satisfies? p/ISpace host))

(defn for-host
  "Frame `f` as `host` should receive it: unchanged for a 2D frame or a
   native-3D host, projected to 2D otherwise. `cam` (optional) overrides the
   frame's camera (an orbit)."
  ([host f] (for-host host f nil))
  ([host f cam]
   (let [f (if (and cam (space/three-d? f)) (space/with-camera f cam) f)]
     (if (or (native-space? host) (not (space/three-d? f)))
       f
       (space/->2d f)))))

(defn present!
  "Present frame `f` on `host`, through the port its content needs."
  ([host f] (present! host f nil))
  ([host f cam]
   (let [g (for-host host f cam)]
     (if (and (native-space? host) (space/three-d? g))
       (p/-present-space! host g)
       (p/-present! host g)))))
