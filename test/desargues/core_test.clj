(ns desargues.core-test
  (:require [clojure.test :refer :all]
            [desargues.core :refer :all]))

(deftest entry-point-loads
  (testing "the CLI entry point namespace loads and exposes -main"
    (is (fn? @(requiring-resolve 'desargues.core/-main)))))
