(ns desargues.tracks
  "FACADE of the tracks context: time tracks in the desargues scene graph.

     desargues.tracks.core        Core      Keyframe / Track / Tracks + malli
     desargues.tracks.protocols   Protocol  ITracks
     desargues.tracks.sample      Pure      sample at t (clamped, exact at keys)
     desargues.tracks.projection  Pure      Trajectory -> Tracks, drift badge
     desargues.tracks.lagrangian  App (JVM) Emmy teach path: pendulum, top

   A scene graph carries its tracks under :tracks, next to :nodes and :steps
   ({:scene .. :nodes {id node} :steps [..] :tracks {id Track}}); with-tracks
   attaches them and graph-tracks reads them back through ITracks.

   ```clojure
   (def g (-> (player/record \"Pendulum\" construct)
              (tracks/with-tracks (tracks/trajectory->tracks traj keyer))))
   (tracks/sample (get (tracks/graph-tracks g) bob-id) 2.5)
   ```"
  (:require [desargues.tracks.core :as core]
            [desargues.tracks.projection :as projection]
            [desargues.tracks.protocols :as p]
            [desargues.tracks.sample :as sample]))

(def track core/track)
(def valid-track? core/valid-track?)
(def valid-tracks? core/valid-tracks?)
(def explain-track core/explain-track)
(def badge core/badge)
(def Track core/Track)
(def Tracks core/Tracks)
(def Keyframe core/Keyframe)

(def sample sample/sample)
(def sample-tracks sample/sample-tracks)
(def duration sample/duration)

(def trajectory->tracks projection/trajectory->tracks)
(def trajectory->track projection/trajectory->track)
(def energy-drift projection/energy-drift)
(def with-drift-badge projection/with-drift-badge)

(defn graph-tracks
  "The Tracks of `holder` (a scene graph map, or anything implementing
   desargues.tracks.protocols/ITracks)."
  [holder]
  (p/-tracks holder))

(defn with-tracks
  "Scene graph `graph` with `tracks` merged under :tracks (validated: throws
   ex-info when they are not Tracks)."
  [graph tracks]
  (when-not (core/valid-tracks? tracks)
    (throw (ex-info "with-tracks: not a Tracks value" {:ids (keys tracks)})))
  (update graph :tracks (fnil merge {}) tracks))
