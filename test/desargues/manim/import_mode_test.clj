(ns desargues.manim.import-mode-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.config :as config]
            [desargues.manim.core :as core]
            [libpython-clj2.python :as py]))

(deftest import-plans-are-data
  (testing "eager is the plain import"
    (is (= {:env {} :defer []} (core/import-plan "eager"))))
  (testing "lean defers only modules a Cairo render never touches"
    (let [{:keys [env defer]} (core/import-plan "lean")]
      (is (= "1" (get env "OPENBLAS_NUM_THREADS")))
      (is (some #{"networkx"} defer))
      (is (not-any? #(re-find #"^(scipy|av|numpy)" %) defer))))
  (testing "an unknown mode fails loudly"
    (is (thrown? clojure.lang.ExceptionInfo (core/import-plan "fast")))))

(deftest import-mode-config-is-a-known-mode
  (let [mode (get-in (config/resolve-ManimConfig) [:ok :import-mode] "lean")]
    (is (map? (core/import-plan mode)))))

(deftest deferred-modules-resolve-on-use
  (if-not (config/manim-ready?)
    (println "SKIP deferred-modules-resolve-on-use: no Manim env")
    (let [manim (core/init!)
          sys   (py/import-module "sys")]
      (testing "a manim Graph still builds after init!"
        (let [graph ((py/get-attr manim "Graph") [1 2 3] [[1 2] [2 3]])]
          (is (= 3 (py/call-attr (py/get-attr graph "vertices") "__len__")))))
      (testing "the real networkx is in sys.modules once used"
        (is (py/call-attr (py/get-attr sys "modules") "__contains__" "networkx.algorithms"))))))
