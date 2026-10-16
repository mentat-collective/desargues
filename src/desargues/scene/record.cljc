(ns desargues.scene.record
  "COMPOSITION stratum (host-free .cljc): record a construct into the EDN scene
   graph by running it through the desargues.scene facade bound to a fresh
   RecordingBackend. The one recorder every host shares: desargues.player on
   the JVM, desargues.browser.player in a page, golden tests anywhere.

     (record \"Tangent\" (fn [stage] (objects/add! stage spec) (s/hold! stage 1)))
     ;; => {:scene \"Tangent\" :nodes {...} :steps [...] :node-count n}"
  (:require [desargues.scene :as scene]
            [desargues.scene.data :as data]))

(defn record
  "The EDN scene graph of construct `construct` (fn [stage]), named
   `scene-name` (default \"Live\"). Pure apart from the fresh backend's own
   counter: the same construct records the same graph on every host."
  ([construct] (record "Live" construct))
  ([scene-name construct]
   (scene/with-backend (data/recording-backend)
     (scene/render! scene-name construct))))
