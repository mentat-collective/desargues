(ns desargues.tracks.projection
  "PURE stratum of the tracks context: physics Trajectories projected onto
   Tracks, and the energy-drift badge.

   A Trajectory (desargues.physics.trajectory) is a seq of {:time t :state s}.
   Projection takes a `keyer`, state -> {node-id keyframe-body}, where a body is
   {:transform ..} and/or {:shape-params ..}: one physics run can drive many
   nodes (a pendulum's bob AND its rod). The keyer is where the physics meets
   the picture, so it is the caller's (a teach path's), never this namespace's.

   The drift badge is max |E(t) - E(0)| / |E(0)| over the run: a number on the
   track's :meta, not a rendering. Requires desargues.tracks.core only (the
   trajectory is plain data). Host-free .cljc."
  (:require [desargues.tracks.core :as core]))

(defn trajectory->tracks
  "Tracks {node-id Track} from `trajectory` through `keyer` (state ->
   {node-id {:transform ..|:shape-params ..}}). opts: :every n keeps every n-th
   point (the last point is always kept, so the track ends where the run
   does), :meta merged into every track's meta."
  ([trajectory keyer] (trajectory->tracks trajectory keyer {}))
  ([trajectory keyer {:keys [every meta] :or {every 1}}]
   (let [pts (vec trajectory)
         kept (if (<= every 1)
                pts
                (let [thinned (vec (take-nth every pts))]
                  (if (= (peek thinned) (peek pts)) thinned (conj thinned (peek pts)))))
         by-node (reduce (fn [acc {:keys [time state]}]
                           (reduce-kv (fn [acc id body]
                                        (update acc id (fnil conj []) (assoc body :t (double time))))
                                      acc (keyer state)))
                         {} kept)]
     (into {} (map (fn [[id kfs]] [id (core/track kfs meta)])) by-node))))

(defn trajectory->track
  "One Track from `trajectory` through `body-fn` (state -> {:transform ..
   |:shape-params ..}). Same opts as trajectory->tracks."
  ([trajectory body-fn] (trajectory->track trajectory body-fn {}))
  ([trajectory body-fn opts]
   (get (trajectory->tracks trajectory (fn [s] {::one (body-fn s)}) opts) ::one)))

(defn energy-drift
  "max |E(t) - E(0)| / |E(0)| over `trajectory`, `energy-fn` state -> number.
   When E(0) is 0 the drift is absolute (max |E(t)|), stated in the result.
   Returns {:max-relative d :e0 E0 :samples n :basis :relative|:absolute}."
  [trajectory energy-fn]
  (let [es (mapv (comp double energy-fn :state) trajectory)
        e0 (first es)
        dev (reduce max 0.0 (map #(Math/abs (- (double %) e0)) es))
        absolute? (zero? e0)]
    {:max-relative (if absolute? dev (/ dev (Math/abs e0)))
     :e0 e0
     :samples (count es)
     :basis (if absolute? :absolute :relative)}))

(defn with-drift-badge
  "Every track of `tracks` carrying the energy-drift badge (see energy-drift)
   under :meta :energy-drift."
  [tracks drift]
  (update-vals tracks #(core/with-meta-entry % :energy-drift drift)))
