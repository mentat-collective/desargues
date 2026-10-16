(ns desargues.browser.parity
  "BOUNDARY (Node, build/test time only): record every studio object demo at
   its default params through the shared facade recorder on the ClojureScript
   host and write {demo-key graph} as EDN, so the JVM suite can assert both
   hosts record the same graph (desargues.scene.parity-test).

     clojure -M:browser-parity && node target/browser-parity/parity.js <out.edn>"
  (:require [desargues.scene.record :as recorder]
            [desargues.studio.objects-demos :as demos]))

(def fs (js/require "fs"))

(defn graphs
  "{demo-key scene-graph} for every registered demo at its default params."
  []
  (into (sorted-map)
        (map (fn [[k {:keys [construct]}]]
               [k (recorder/record (name k) (construct (demos/defaults k)))]))
        demos/demos))

(defn -main [& [out]]
  (let [out (or out "target/browser-parity/demos.cljs.edn")]
    (.writeFileSync fs out (binding [*print-length* nil *print-level* nil]
                             (pr-str (graphs))))
    (println "wrote" out)))

(set! *main-cli-fn* -main)
