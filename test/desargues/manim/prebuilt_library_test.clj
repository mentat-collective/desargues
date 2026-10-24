(ns desargues.manim.prebuilt-library-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.manim.core :as core]
            [desargues.pyaot :as pyaot]))

(deftest prebuilt-library-follows-the-class-finder
  (testing "no prebuilt class -> nil, libpython-clj generates bindings at runtime"
    (is (nil? (core/prebuilt-library (constantly nil)))))
  (testing "a found class becomes the :library-definition"
    (let [asked (atom nil)
          lib   (core/prebuilt-library (fn [n] (reset! asked n) String))]
      (is (= core/prebuilt-library-class @asked))
      (is (= {:library-class String} lib)))))

(deftest default-class-finder-misses-quietly
  (is (nil? (core/class-on-classpath "desargues.no.SuchClass")))
  (is (= String (core/class-on-classpath "java.lang.String"))))

(deftest pyaot-classpath-check
  (is (pyaot/on-classpath? "a:target/pyaot:b" "target/pyaot"))
  (is (not (pyaot/on-classpath? "a:b" "target/pyaot")))
  (is (not (pyaot/on-classpath? nil "target/pyaot"))))
