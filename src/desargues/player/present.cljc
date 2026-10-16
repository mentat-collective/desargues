(ns desargues.player.present
  "PROTOCOL stratum of the player: handing a Frame to a host.

   The one place that decides how a frame reaches a surface. A 2D frame goes
   to ISurface/-present! as is. A 3D frame (desargues.player.space) goes to
   ISpace/-present-space! when the host renders space natively, and is
   otherwise projected to 2D first (space/->2d), so a 2D host (headless PNG,
   browser canvas) shows 3D content without a line of 3D code (ISP, LSP).
   Host-free .cljc."
  (:require [desargues.player.protocols :as p]
            [desargues.player.space :as space]
            [desargues.player.view :as view]))

(defn native-space?
  "Does `host` render 3D frames itself?"
  [host]
  (satisfies? p/ISpace host))

(defn seen
  "Frame `f` as the viewer sees it: a 3D frame through camera `cam` (when
   set), a 2D frame through viewport `v` (desargues.player.view, when set)."
  [f cam v]
  (if (space/three-d? f)
    (if cam (space/with-camera f cam) f)
    (view/seen-frame v f)))

(defn for-host
  "Frame `f` as `host` should receive it: seen through camera `cam` /
   viewport `v` (both optional, see `seen`), then unchanged for a 2D frame
   or a native-3D host, projected to 2D otherwise."
  ([host f] (for-host host f nil nil))
  ([host f cam] (for-host host f cam nil))
  ([host f cam v]
   (let [f (seen f cam v)]
     (if (or (native-space? host) (not (space/three-d? f)))
       f
       (space/->2d f)))))

(defn present!
  "Present frame `f` on `host`, through the port its content needs, seen
   through camera `cam` and viewport `v` when given."
  ([host f] (present! host f nil nil))
  ([host f cam] (present! host f cam nil))
  ([host f cam v]
   (let [g (for-host host f cam v)]
     (if (and (native-space? host) (space/three-d? g))
       (p/-present-space! host g)
       (p/-present! host g)))))
