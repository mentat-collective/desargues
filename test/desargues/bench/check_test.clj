(ns desargues.bench.check-test
  "The regression check is pure: two reports in, a verdict out."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.bench.check :as check]
            [desargues.bench.sample :as smp]))

(def base
  {:results [{:group :x :id "steady" :median-ms 1.0 :mad-ms 0.01}
             {:group :x :id "noisy"  :median-ms 1.0 :mad-ms 0.3}
             {:group :x :id "faster" :median-ms 1.0 :mad-ms 0.01}
             {:group :x :id "gone"   :median-ms 1.0 :mad-ms 0.01}]})

(def fresh
  {:results [{:group :x :id "steady" :median-ms 1.5 :mad-ms 0.01}
             {:group :x :id "noisy"  :median-ms 1.5 :mad-ms 0.3}
             {:group :x :id "faster" :median-ms 0.5 :mad-ms 0.01}
             {:group :x :id "new"    :median-ms 1.0 :mad-ms 0.01}]})

(deftest verdicts
  (let [v (check/compare-reports base fresh)
        status (into {} (map (juxt (comp second :key) :status)) (:rows v))]
    (is (= {"steady" :regression "noisy" :ok "faster" :improvement} status)
        "a slowdown inside 3x MAD is noise, not a regression")
    (is (false? (:ok? v)))
    (is (= [[:x "gone" nil nil]] (:missing v)))
    (is (= [[:x "new" nil nil]] (:new v)))
    (testing "a looser tolerance absorbs the slowdown"
      (is (:ok? (check/compare-reports base fresh {:tolerance 0.6}))))
    (testing "reports without medians fall back to mean and std"
      (is (:ok? (check/compare-reports {:results [{:group :y :mean-ms 1.0 :std-ms 0.1}]}
                                       {:results [{:group :y :mean-ms 1.05 :std-ms 0.1}]}))))))

(deftest summary-names-the-outcome
  (is (= "bench:check FAILED" (last (check/summary-lines (check/compare-reports base fresh)))))
  (is (= "bench:check OK" (last (check/summary-lines (check/compare-reports base base))))))

(deftest median-and-mad
  (is (= 2.0 (smp/median [3 1 2])))
  (is (= 2.5 (smp/median [1 2 3 4])))
  (let [s (smp/summarize [1.0 1.0 1.0 10.0])]
    (is (= 1.0 (:median-ms s)))
    (is (= 0.0 (:mad-ms s)) "one outlier does not move the MAD")))
