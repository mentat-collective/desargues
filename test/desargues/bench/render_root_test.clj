(ns desargues.bench.render-root-test
  "The render bench root stays slim: desargues.bench.render and its child
   JVMs must not pull in the pure-pipeline suites (desargues.bench, Emmy,
   physics, the geometry benches). Checked on the static require graph, so it
   needs no Manim env and loads nothing."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.tools.namespace.find :as find]
            [clojure.tools.namespace.parse :as parse]
            [desargues.bench.reference :as reference]
            [desargues.bench.render :as render]))

(defn- ns-decls
  "Namespace symbol -> its static deps, for every ns under `dirs`."
  [dirs]
  (into {}
        (for [d dirs
              decl (find/find-ns-decls-in-dir (io/file d))]
          [(parse/name-from-ns-decl decl) (parse/deps-from-ns-decl decl)])))

(defn- closure
  "Static require closure of `root` within `graph` (project namespaces only)."
  [graph root]
  (loop [todo [root] seen #{}]
    (if-let [[n & more] (seq todo)]
      (if (seen n)
        (recur more seen)
        (recur (into (vec more) (filter graph (graph n))) (conj seen n)))
      seen)))

(deftest render-root-skips-the-pure-suites
  (let [reach (closure (ns-decls ["src" "bench"]) 'desargues.bench.render)]
    (is (contains? reach 'desargues.bench.reference))
    (testing "none of the heavy bench and pipeline namespaces is statically required"
      (doseq [heavy '[desargues.bench desargues.bench.hotpaths desargues.bench.geometry
                      desargues.bench.chart desargues.physics desargues.pipeline.emmy]]
        (is (not (contains? reach heavy)) (str heavy " reachable from desargues.bench.render"))))))

(deftest reference-layout-is-a-grid-under-a-title
  (let [tree (reference/layout-tree 16)]
    (is (= 5 (count (:children tree))))
    (is (every? #(= 4 (count (:children %))) (rest (:children tree))))))

(deftest preload-goes-after-the-child-mode
  (is (= ["desargues.bench.render" "--child" "full:lean" "--preload" "desargues.bench"]
         (take-last 5 (render/child-cmd "full:lean" {:preload ['desargues.bench]})))))
