(ns desargues.player.track-source
  "PURE stratum: a scene graph's time tracks as a stage source.

   The graph's :nodes say what each node is; its :tracks (desargues.tracks)
   say where it is at t. -frame-at samples every track and resolves each node
   through `apply-sample`, one method per node kind (OCP): the default puts
   :transform :translate at :at and merges :shape-params into the node, which
   is what a dot, circle, line or path needs. Untracked nodes are shown as
   they are. Tracks are exporter-neutral 3D; the stage paints 2D, so the
   source carries a `project` fn [x y z] -> [x y] (default: the x-z plane,
   z up, scaled and offset into Manim's frame).

   The player cannot tell this source from a recorded-graph one (LSP).
   Host-free .cljc."
  (:require [desargues.player.frame :as frame]
            [desargues.player.protocols :as p]
            [desargues.tracks.protocols :as tp]
            [desargues.tracks.sample :as sample]))

(defmulti apply-sample
  "Node `node` resolved by its track sample `s` ({:t :transform :shape-params})."
  (fn [node _s] (:node node)))

(defmethod apply-sample :default [node {:keys [transform shape-params]}]
  (cond-> (merge node shape-params)
    (:translate transform) (assoc :at (:translate transform))
    (:rotate transform)    (assoc :rotate (:rotate transform))
    (:scale transform)     (assoc :scale (:scale transform))))

(defn xz-projection
  "[x y z] -> [x y] onto the x-z plane (z up), times `scale`, plus `offset`."
  ([] (xz-projection 1.0 [0.0 0.0]))
  ([scale [ox oy]]
   (fn [[x _ z]] [(+ ox (* scale (double x))) (+ oy (* scale (double (or z 0.0))))])))

(defn- point? [v] (and (vector? v) (<= 2 (count v) 3) (every? number? v)))

(defn- project-point [project v]
  (if (= 3 (count v)) (project v) v))

(defn project-node
  "Node `n` with its point fields (:at :from :to :points) projected to 2D."
  [project n]
  (reduce (fn [n k]
            (let [v (get n k)]
              (cond
                (point? v) (assoc n k (project-point project v))
                (and (= k :points) (vector? v)) (assoc n k (mapv #(project-point project %) v))
                :else n)))
          n [:at :from :to :points]))

(defn- shown [n] (assoc n :opacity 1.0 :draw 1.0))

(defn draw-order
  "Node ids of `graph` in draw order: its :order when it carries one (ids it
   leaves out follow, sorted), else every node id sorted."
  [graph]
  (let [ids (sort-by str (keys (:nodes graph)))
        order (vec (filter (:nodes graph) (:order graph)))]
    (into order (remove (set order)) ids)))

(defrecord TrackSource [graph project overlay]
  p/ISource
  (-duration [_] (sample/duration (tp/-tracks graph)))
  (-frame-at [_ t]
    (let [tracks (tp/-tracks graph)
          shapes (for [id (draw-order graph)]
                   (let [node (get-in graph [:nodes id])
                         trk (get tracks id)
                         node (if trk (apply-sample node (sample/sample trk t)) node)]
                     (shown (project-node project node))))]
      (frame/frame t (concat shapes (map shown (overlay t)))))))

(defn track-source
  "The stage source playing the tracks of scene `graph` ({:nodes :tracks}).
   opts: :project ([x y z] -> [x y], default (xz-projection 2.0 [0 1])),
   :overlay (fn [t] -> extra 2D shapes drawn on top, e.g. a badge label)."
  ([graph] (track-source graph {}))
  ([graph {:keys [project overlay]
           :or {project (xz-projection 2.0 [0.0 1.0]) overlay (constantly [])}}]
   (->TrackSource graph project overlay)))
