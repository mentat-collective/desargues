(ns desargues.browser.record
  "BOUNDARY (JVM, build time only): record the scene graphs the static demo
   page ships as data. `clojure -M:browser-record [out-dir]` writes
   <out-dir>/<name>.edn (default browser/public/scenes) for every entry of
   `scenes`; the page plays them with no JVM at view time.

   A scene here is any parametric construct (fn [params] (fn [stage] ...))
   over the desargues.scene facade, recorded through the RecordingBackend by
   desargues.player/record. Adding a shipped scene is one map entry."
  (:require [clojure.java.io :as io]
            [desargues.player :as player]
            [desargues.studio.demos :as demos]))

(def scenes
  "Shipped scene name -> {:construct (fn [params] construct) :params {}}."
  {"pendulum" {:construct demos/pendulum
               :params (into {} (map (juxt :id :value)) demos/pendulum-controls)}})

(defn write-scene!
  "Record scene `scene-name` and write its EDN graph under `out-dir`."
  [out-dir scene-name {:keys [construct params]}]
  (let [f (io/file out-dir (str scene-name ".edn"))]
    (io/make-parents f)
    (binding [*print-length* nil *print-level* nil]
      (spit f (pr-str (player/record scene-name (construct params)))))
    (str f)))

(defn -main [& [out-dir]]
  (let [out-dir (or out-dir "browser/public/scenes")]
    (doseq [[n spec] scenes]
      (println "wrote" (write-scene! out-dir n spec)))
    (shutdown-agents)))
