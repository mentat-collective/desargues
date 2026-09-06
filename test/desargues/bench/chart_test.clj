(ns desargues.bench.chart-test
  "Charts are pure: a report map in, SVG text out."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [desargues.bench.chart :as chart]))

(def report
  {:jvm "21" :clojure "1.12.3" :cpu "8 cores"
   :results [{:group :emmy :id "sin x" :mean-ms 0.02 :std-ms 0.005}
             {:group :layout :n 4 :mean-ms 0.03 :std-ms 0.01}
             {:group :layout :n 64 :mean-ms 0.4 :std-ms 0.1}
             {:group :recording :n 14 :mean-ms 0.01 :std-ms 0.002}
             {:group :solver :id "clojure rk4" :mean-ms 4.8 :std-ms 1.2}]})

(deftest hiccup-renders-to-svg-text
  (is (= "<g fill=\"#fff\" x=\"1\">hi &amp; bye</g>"
         (chart/svg->str [:g {:x 1 :fill "#fff"} "hi & bye"])))
  (testing "seqs splice and nil vanishes"
    (is (= "<g><a></a><b></b></g>" (chart/svg->str [:g (list [:a] nil [:b])])))))

(deftest human-units
  (is (= "20.0 µs" (chart/fmt-ms 0.02)))
  (is (= "4.80 ms" (chart/fmt-ms 4.8)))
  (is (= "1.50 s" (chart/fmt-ms 1500))))

(deftest one-chart-per-present-group
  (let [cs (chart/charts report)]
    (is (= #{"emmy-conveyor.svg" "solvers.svg" "scaling.svg"} (set (keys cs))))
    (is (nil? (get cs "manim-render.svg")) "no manim rows, no manim chart")
    (doseq [[f svg] cs]
      (let [s (chart/svg->str svg)]
        (is (str/starts-with? s "<svg") f)
        (is (str/includes? s "JDK 21") f)))))

(deftest bars-and-lines-carry-every-datum
  (let [bars (chart/svg->str (chart/bar-chart {:title "t" :subtitle "s" :bars [{:label "a" :value 1 :err 0.1} {:label "b" :value 2}]}))
        line (chart/svg->str (chart/line-chart {:title "t" :subtitle "s" :series [{:label "l" :points [[4 0.03] [64 0.4]]}]}))]
    (is (= 2 (count (re-seq #"<rect [^>]*rx=\"3\"" bars))))
    (is (str/includes? bars "1.00 ms"))
    (is (= 2 (count (re-seq #"<circle" line))))
    (is (str/includes? line ">64<"))))
