(ns desargues.physics.trajectory
  "PURE stratum of the physics bounded context: Trajectory transforms.

   A Trajectory is a seq of {:time t :state state}. These turn one into the
   data a renderer plots (x(t) graphs, parametric curves, phase portraits)
   or thins it into animation keyframes. No solver, no rendering.

   Requires only desargues.physics.protocols. Host-free .cljc."
  (:require [desargues.physics.protocols :as p]))

(defn phase-portrait
  "Extract phase portrait data from trajectory.
   Returns sequence of [var1 var2] pairs for plotting."
  [system trajectory & {:keys [vars] :or {vars nil}}]
  (let [labels (p/state-labels system)
        [v1 v2] (or vars (take 2 labels))]
    (map (fn [{:keys [state]}]
           [(get state v1) (get state v2)])
         trajectory)))

(defn sample-trajectory
  "Sample trajectory at regular intervals, returning AT MOST n-samples points
   (the first point always included). Useful for discrete animation keyframes."
  [trajectory n-samples]
  (let [total (count trajectory)
        step (max 1 (long (Math/ceil (/ total (double (max 1 n-samples))))))]
    (take-nth step trajectory)))

(defn trajectory->graph-data
  "Convert trajectory to data suitable for plotting x(t) graph.
   Returns {:xs [times] :ys [values] :label label}"
  [trajectory var-key & {:keys [label] :or {label (name var-key)}}]
  {:xs (mapv :time trajectory)
   :ys (mapv #(get-in % [:state var-key]) trajectory)
   :label label})

(defn trajectory->parametric-data
  "Convert trajectory to parametric curve data.
   Returns sequence of [x y] points."
  [system trajectory position-fn]
  (mapv (fn [{:keys [state]}]
          (position-fn system state))
        trajectory))

(defn velocity-vector-at
  "Compute velocity vector at a given state.
   Useful for drawing velocity arrows on animations."
  [system state & {:keys [scale] :or {scale 0.5}}]
  (let [derivs (p/derivatives system state)
        [v1 v2] (take 2 (vals derivs))]
    [(* scale v1) (* scale v2)]))
