(ns desargues.board.view
  "Pure layer of boards: the camera of a 3D construction, as data.

   A construction with a :view is three-dimensional. Its points live in world
   space (x, y, z; z up), the kernel computes their world coordinates, and
   the PAGE projects them (plato.board.view), so turning the camera never
   costs a kernel call:

     :view {:yaw 'yaw :pitch 'pitch :scale 1 :perspective 9}

   yaw turns the world about z, then pitch tilts it toward the viewer
   (positive looks down from above). Each is a number or the id of a param;
   a param the page should drive by dragging the board says so with
   :control :orbit and :axis :yaw or :pitch. Without :perspective the
   projection is orthographic, which keeps a point dragged along a segment
   exactly on it; with it, a camera at that distance.")

(defn board-view
  "The :board/view plato reads: a param id as a keyword, a number as is."
  [{:keys [yaw pitch scale perspective] :or {yaw 0.6 pitch 0.35 scale 1}}]
  (let [ref (fn [v] (if (symbol? v) (keyword (name v)) v))]
    (cond-> {:yaw (ref yaw) :pitch (ref pitch) :scale scale}
      perspective (assoc :perspective perspective))))
