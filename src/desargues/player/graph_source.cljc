(ns desargues.player.graph-source
  "PURE stratum: a recorded desargues.scene graph as a stage source.

   The graph is the EDN the RecordingBackend yields for a construct, so the
   construct that renders an mp4 through Manim plays here unchanged. The
   schedule is computed once, when the source is built. Host-free .cljc."
  (:require [desargues.player.protocols :as p]
            [desargues.player.timeline :as timeline]))

(defrecord GraphSource [graph sched]
  p/ISource
  (-duration [_] (:duration sched))
  (-frame-at [_ t] (timeline/frame-at graph sched t)))

(defn graph-source
  "The stage source playing scene `graph`."
  [graph]
  (->GraphSource graph (timeline/schedule graph)))
