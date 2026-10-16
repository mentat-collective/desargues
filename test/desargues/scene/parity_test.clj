(ns desargues.scene.parity-test
  "One recorder, two hosts: every studio object demo records the same EDN
   scene graph on the JVM and on the ClojureScript host (Node), through the
   same desargues.scene facade and RecordingBackend.

   The cljs side is the Node build of desargues.browser.parity
   (`clojure -M:browser-parity`); when it is not built, or node is missing,
   the cross-host comparison is skipped and only the JVM self-check runs."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.test :refer [deftest is testing]]
            [desargues.scene.compare :as cmp]
            [desargues.scene.record :as recorder]
            [desargues.studio.objects-demos :as demos]))

(def ^:private parity-js "target/browser-parity/parity.js")

(defn- jvm-graphs []
  (into (sorted-map)
        (map (fn [[k {:keys [construct]}]]
               [k (recorder/record (name k) (construct (demos/defaults k)))]))
        demos/demos))

(defn- cljs-graphs
  "The cljs-recorded graphs, or nil when the Node build or node is absent."
  []
  (when (.exists (io/file parity-js))
    (let [out (java.io.File/createTempFile "desargues-parity" ".edn")]
      (try
        (let [{:keys [exit err]} (sh/sh "node" parity-js (str out))]
          (if (zero? exit)
            (edn/read-string (slurp out))
            (do (println "parity: node failed:" err) nil)))
        (catch java.io.IOException e
          (println "parity: node unavailable:" (.getMessage e))
          nil)
        (finally (.delete out))))))

(deftest recording-is-deterministic-on-the-jvm
  (is (cmp/same-graph? (jvm-graphs) (jvm-graphs) {:tol 0.0})))

(deftest every-demo-records-the-same-graph-on-both-hosts
  (if-let [cljs (cljs-graphs)]
    (let [jvm (jvm-graphs)]
      (is (= (set (keys jvm)) (set (keys cljs))))
      (doseq [k (keys jvm)]
        (testing (name k)
          (is (pos? (count (:nodes (get jvm k)))))
          (is (= [] (cmp/diffs (get jvm k) (get cljs k)))))))
    (println "parity: skipped (build it with clojure -M:browser-parity)")))
