(ns desargues.doctor-test
  "The doctor's pure core: rows -> report, and the checks that need no env."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [desargues.doctor :as doctor]))

(defn- rows [& overrides]
  (merge {:jvm {:id :jvm :ok? true :detail "java 21"}
          :rec {:id :recording-backend :ok? true :detail "2 nodes"}
          :cfg {:id :manim-config :ok? false :detail "no conda env" :fix "conda activate manim"}
          :man {:id :manim :ok? false :detail "no python" :fix "fix manim-config first"}
          :tex {:id :latex :ok? true :detail "/usr/bin/latex"}
          :ff  {:id :ffmpeg :ok? true :detail "/usr/bin/ffmpeg"}}
         (apply hash-map overrides)))

(deftest pure-rows-decide-the-exit-code
  (testing "Manim rows missing -> still exit 0, with the advisory sentence"
    (let [{:keys [exit lines]} (doctor/report (vals (rows)))]
      (is (= 0 exit))
      (is (some #(str/includes? % "MISSING  manim-config") lines))
      (is (some #(str/includes? % "fix: conda activate manim") lines))
      (is (str/starts-with? (last lines) "The pure path works"))))
  (testing "a pure row missing -> exit 1"
    (let [{:keys [exit lines]} (doctor/report (vals (rows :rec {:id :recording-backend :ok? false :detail "boom" :fix "run the tests"})))]
      (is (= 1 exit))
      (is (= "The pure path is broken." (last lines)))))
  (testing "everything ok"
    (let [{:keys [exit lines]} (doctor/report (vals (rows :cfg {:id :manim-config :ok? true :detail "py"}
                                                       :man {:id :manim :ok? true :detail "v0.21"})))]
      (is (= 0 exit))
      (is (str/starts-with? (last lines) "Everything is in place")))))

(deftest env-free-checks-pass-here
  (is (:ok? (doctor/jvm-check)))
  (is (:ok? (doctor/recording-backend-check)))
  (is (= 6 (count (doctor/checks)))))
