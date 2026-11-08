(ns desargues.bench.ab-test
  "The A/B harness's pure parts: the interleave schedule and the stats."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.bench.ab :as ab]))

(deftest schedule-interleaves
  (let [s (ab/schedule ["A" "B"] {:rounds 3 :warmup 1})]
    (is (= ["A" "B" "A" "B" "A" "B" "A" "B"] (mapv :variant s))
        "ABAB..., never AAA BBB")
    (is (= [true true false false false false false false] (mapv :warmup? s)))
    (is (= [0 0 1 1 2 2 3 3] (mapv :round s))))
  (testing "alternate? reverses odd rounds"
    (is (= ["A" "B" "B" "A" "A" "B"]
           (mapv :variant (ab/schedule ["A" "B"] {:rounds 3 :warmup 0 :alternate? true})))))
  (testing "three variants, no warmup"
    (is (= ["A" "B" "C" "A" "B" "C"]
           (mapv :variant (ab/schedule ["A" "B" "C"] {:rounds 2 :warmup 0})))))
  (testing "every variant gets exactly `rounds` measured slots"
    (let [s (remove :warmup? (ab/schedule ["A" "B" "C"] {:rounds 7 :warmup 2}))]
      (is (= {"A" 7 "B" 7 "C" 7} (frequencies (map :variant s)))))))

(deftest percentile-nearest-rank
  (is (= 9.0 (ab/percentile 90 (range 1 11))))
  (is (= 10.0 (ab/percentile 100 (range 1 11))))
  (is (= 1.0 (ab/percentile 10 (range 1 11))))
  (is (= 5.0 (ab/percentile 90 [5])))
  (is (= 4.0 (ab/percentile 90 [4 1 3 2])) "order of the input does not matter"))

(deftest stats-of-samples
  (let [s (ab/stats [3 1 2 10])]
    (is (= 2.5 (:median s)))
    (is (= 1.0 (:min s)))
    (is (= 10.0 (:p90 s)))
    (is (= 10.0 (:max s)))
    (is (= 4.0 (:mean s)))
    (is (= 4 (:n s)))))

(deftest summarize-ratios-to-first
  (let [samples (for [[v w c] [["A" 100 1.0] ["B" 50 0.5] ["A" 110 1.2] ["B" 60 0.4]
                               ["A" 90 0.8] ["B" 55 0.6]]]
                  {:variant v :wall-ms w :cpu-s c :load {:load1 2.0}})
        [a b] (ab/summarize samples ["A" "B"])]
    (is (= "A" (:variant a)))
    (is (= 100.0 (get-in a [:wall-ms :median])))
    (is (= {:wall 1.0 :cpu 1.0} (:ratio a)))
    (is (= 0.55 (get-in b [:ratio :wall])))
    (is (= 0.5 (get-in b [:ratio :cpu])))
    (is (= 2.0 (get-in b [:load1 :median])))))

(deftest loadavg-parse
  (is (= {:load1 20.7 :load5 13.96 :load15 19.47}
         (ab/parse-loadavg "20.70 13.96 19.47 16/5054 2729975\n")))
  (is (nil? (ab/parse-loadavg nil)))
  (is (nil? (ab/parse-loadavg "garbage x y"))))

(deftest run-arm-end-to-end
  (testing "a cheap arm runs, drops warmups from the summary, keeps every sample"
    (let [calls (atom [])
          v (fn [nm] {:name nm
                      :setup (fn [_] (swap! calls conj [:setup nm]) nm)
                      :run (fn [ctx] (swap! calls conj [:run ctx]) (Thread/sleep 1))
                      :teardown (fn [ctx] (swap! calls conj [:teardown ctx]))})
          r (ab/run-arm {:id "t" :variants [(v "A") (v "B")] :rounds 2 :warmup 1}
                        {:log (fn [& _])})]
      (is (= 6 (count (:samples r))))
      (is (= [2 2] (mapv (comp :n :wall-ms) (:variants r))))
      (is (= [[:setup "A"] [:run "A"] [:teardown "A"] [:setup "B"] [:run "B"] [:teardown "B"]]
             (take 6 @calls)))
      (is (every? (comp number? :cpu-s) (:samples r)))
      (is (= #{"A" "B"} (set (map :provider (ab/arm->rows r)))))))
  (testing "a child-process subject overrides the measurement"
    (let [r (ab/run-arm {:id "c" :variants [{:name "X" :run (fn [_] {::ab/measured {:wall-ms 7.0 :cpu-s 0.25}})}]
                         :rounds 1 :warmup 0}
                        {:log (fn [& _])})]
      (is (= 7.0 (get-in r [:variants 0 :wall-ms :median])))
      (is (= 0.25 (get-in r [:variants 0 :cpu-s :median]))))))
