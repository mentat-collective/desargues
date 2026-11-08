(ns desargues.manim.preload-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.config :as config]
            [desargues.manim.core :as core]))

(deftest preload-paths-splits-the-config-string
  (testing "colon-separated paths become a vector, blanks dropped"
    (is (= ["/a/libskia.so" "/b/libx.so"]
           (core/preload-paths "/a/libskia.so::/b/libx.so:"))))
  (testing "empty and nil mean nothing to preload"
    (is (= [] (core/preload-paths "")))
    (is (= [] (core/preload-paths nil)))))

(deftest preload-default-never-invalidates-config
  (testing "the field is a string even when no candidate lib exists"
    (let [f (get-in (config/resolve-ManimConfig) [:ok :preload-libs] "")]
      (is (string? f))
      (is (every? #(.exists (java.io.File. ^String %)) (core/preload-paths f))))))
