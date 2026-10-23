(ns desargues.manim.updater-test
  (:require [clojure.test :refer [deftest is testing]]
            [libpython-clj2.python :as py]
            [desargues.config :as config]
            [desargues.manim.core :as core]
            [desargues.manim.mobjects :as mob]))

(def manim-env? (delay (config/manim-ready?)))

(deftest fn-arities-reads-declared-arities
  (is (= #{1} (core/fn-arities (fn [m] m))))
  (is (= #{2} (core/fn-arities (fn [m dt] [m dt]))))
  (is (= #{1 2} (core/fn-arities (fn ([m] m) ([m dt] [m dt]))))))

(deftest clojure-updaters-survive-manim-signature-inspection
  (if-not @manim-env?
    (println "[updater-test] no Manim env; skipping")
    (do
      (core/init!)
      (testing "a 2-arity Clojure fn is a time-based updater and receives dt"
        (let [seen (atom nil)
              dot  (mob/add-updater (py/call-attr (core/manim) "Dot")
                                    (fn [m dt] (reset! seen dt) m))]
          (is (true? (py/call-attr dot "has_time_based_updater")))
          (py/call-attr dot "update" 0.25)
          (is (= 0.25 @seen))))
      (testing "a 1-arity Clojure fn is a plain updater"
        (let [calls (atom 0)
              dot   (mob/add-updater (py/call-attr (core/manim) "Dot")
                                     (fn [m] (swap! calls inc) m))]
          (is (false? (py/call-attr dot "has_time_based_updater")))
          (py/call-attr dot "update" 0.25)
          (is (= 1 @calls)))))))
