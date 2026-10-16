(ns desargues.tracks.sample-test
  "The Track value object and its sampler: schema, clamping, exact endpoints
   and keys, monotone time, interpolation between keys (property tests)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.tracks :as tracks]
            [desargues.tracks.core :as core]
            [desargues.tracks.sample :as sample]))

(def ^:private gen-num (gen/double* {:infinite? false :NaN? false :min -100.0 :max 100.0}))

(def ^:private gen-track
  "A Track of 1..20 keys at distinct ascending times, each with a translate
   and a scalar shape param."
  (gen/let [n (gen/choose 1 20)
            ts (gen/vector-distinct (gen/double* {:infinite? false :NaN? false :min 0.0 :max 50.0})
                                    {:num-elements n})
            xs (gen/vector (gen/tuple gen-num gen-num gen-num) n)
            rs (gen/vector gen-num n)]
    (tracks/track (map (fn [t x r] {:t t :transform {:translate (vec x)} :shape-params {:r r}})
                       (sort ts) xs rs))))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(defspec endpoints-are-exact 100
  (prop/for-all [trk gen-track]
    (let [kfs (core/keyframes trk) a (first kfs) z (peek kfs)]
      (and (= (dissoc (sample/sample trk (:t a)) :t) (dissoc a :t))
           (= (dissoc (sample/sample trk (:t z)) :t) (dissoc z :t))))))

(defspec every-key-is-exact 100
  (prop/for-all [trk gen-track]
    (every? #(= (dissoc (sample/sample trk (:t %)) :t) (dissoc % :t)) (core/keyframes trk))))

(defspec clamped-outside-the-span 100
  (prop/for-all [trk gen-track d (gen/double* {:infinite? false :NaN? false :min 0.0 :max 10.0})]
    (let [kfs (core/keyframes trk)]
      (and (= (dissoc (sample/sample trk (- (:t (first kfs)) d)) :t) (dissoc (first kfs) :t))
           (= (dissoc (sample/sample trk (+ (:t (peek kfs)) d)) :t) (dissoc (peek kfs) :t))))))

(defspec sample-time-is-the-asked-time-and-monotone 100
  (prop/for-all [trk gen-track ts (gen/vector (gen/double* {:infinite? false :NaN? false :min -5.0 :max 60.0}) 2 10)]
    (let [ts (sort ts) out (map #(:t (sample/sample trk %)) ts)]
      (and (every? true? (map close? ts out))
           (apply <= out)))))

(defspec between-keys-stays-in-the-bracket 100
  (prop/for-all [trk gen-track alpha (gen/double* {:min 0.0 :max 1.0 :NaN? false})]
    (let [kfs (core/keyframes trk)]
      (or (< (count kfs) 2)
          (let [[a b] (take 2 kfs)
                t (+ (:t a) (* alpha (- (:t b) (:t a))))
                r (get-in (sample/sample trk t) [:shape-params :r])
                [lo hi] (sort [(get-in a [:shape-params :r]) (get-in b [:shape-params :r])])]
            (<= (- lo 1e-9) r (+ hi 1e-9)))))))

(deftest schema
  (testing "a keyframe must carry :transform or :shape-params"
    (is (not (core/valid-keyframe? {:t 0.0})))
    (is (core/valid-keyframe? {:t 0.0 :shape-params {:r 1.0}})))
  (testing "track sorts its keys and rejects duplicate times"
    (is (= [0.0 1.0] (map :t (core/keyframes (tracks/track [{:t 1.0 :shape-params {:r 1}}
                                                            {:t 0.0 :shape-params {:r 0}}])))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (tracks/track [{:t 1.0 :shape-params {:r 1}} {:t 1.0 :shape-params {:r 2}}]))))
  (testing "NaN is rejected"
    (is (not (core/valid-keyframe? {:t ##NaN :shape-params {:r 1}}))))
  (testing "tracks round-trip as EDN"
    (let [trk (tracks/track [{:t 0.0 :transform {:translate [0 0 0] :rotate [1 0 0 0]}}])]
      (is (= trk (read-string (pr-str trk)))))))

(deftest rotation-is-normalized-shorter-arc
  (let [trk (tracks/track [{:t 0.0 :transform {:rotate [1.0 0.0 0.0 0.0]}}
                           {:t 1.0 :transform {:rotate [0.0 0.0 0.0 1.0]}}])
        q (get-in (sample/sample trk 0.5) [:transform :rotate])]
    (is (close? 1.0 (Math/sqrt (reduce + (map * q q)))))
    (is (close? (Math/sqrt 0.5) (first q))))
  (testing "antipodal quaternions take the shorter arc"
    (is (every? #(< (Math/abs %) 1e-9)
                (map - [1.0 0.0 0.0 0.0]
                     (sample/nlerp [1.0 0.0 0.0 0.0] [-1.0 0.0 0.0 0.0] 0.5))))))

(deftest scene-graph-carries-tracks
  (let [trk (tracks/track [{:t 0.0 :shape-params {:r 1}} {:t 2.0 :shape-params {:r 3}}])
        g (tracks/with-tracks {:scene "S" :nodes {1 {:node :dot :id 1}} :steps []} {1 trk})]
    (is (= {1 trk} (tracks/graph-tracks g)))
    (is (= {} (tracks/graph-tracks {:nodes {}})))
    (is (= 2.0 (tracks/duration (tracks/graph-tracks g))))
    (is (close? 2.0 (get-in (tracks/sample-tracks (:tracks g) 1.0) [1 :shape-params :r])))
    (is (thrown? clojure.lang.ExceptionInfo (tracks/with-tracks {} {1 {:keyframes []}})))))
