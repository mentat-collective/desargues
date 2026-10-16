(ns desargues.player.navigate-test
  "3D camera navigation and the pure input routing: zoom toward the pointer
   keeps the point under it (perspective and orthographic), pans follow the
   pointer, the magnification is clamped, keys are bound as documented, and
   desargues.player.pointer/step gives drags priority over pans and orbits,
   seeing 2D pointer positions back through the viewport."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.player.navigate :as navigate]
            [desargues.player.orbit :as orbit]
            [desargues.player.pointer :as pointer]
            [desargues.player.space :as space]))

(defn- close?
  ([a b] (close? a b 1e-6))
  ([a b tol] (every? #(< (Math/abs (double %)) tol) (map - a b))))

(def ^:private at-gen
  (gen/tuple (gen/double* {:min -6.0 :max 6.0 :NaN? false :infinite? false})
             (gen/double* {:min -3.5 :max 3.5 :NaN? false :infinite? false})))

(def ^:private m-gen (gen/double* {:min 0.3 :max 4.0 :NaN? false :infinite? false}))

(def ^:private cam-gen
  (gen/fmap (fn [[yaw pitch d proj zoom]]
              {:target [0.5 -0.25 0.1] :yaw yaw :pitch pitch :distance d
               :projection proj :zoom zoom})
            (gen/tuple (gen/double* {:min -3.0 :max 3.0 :NaN? false})
                       (gen/double* {:min -1.2 :max 1.2 :NaN? false})
                       (gen/double* {:min 4.0 :max 30.0 :NaN? false})
                       (gen/elements [:perspective :orthographic])
                       (gen/double* {:min 0.5 :max 3.0 :NaN? false}))))

(defn- on-target-plane
  "The world point of camera `cam`'s target plane shown at scene point `at`."
  [cam at]
  (let [cam (merge space/default-camera cam)]
    (space/ray-plane (space/unproject cam at)
                     {:point (:target cam) :normal (:forward (space/basis cam))})))

(defspec camera-zoom-keeps-the-point-under-the-pointer 300
  (prop/for-all [cam cam-gen m m-gen at at-gen]
    (let [p    (on-target-plane cam at)
          cam' (orbit/zoom-at cam m at)]
      (close? at (:at (space/project cam' p)) 1e-6))))

(defspec camera-pan-keeps-the-grabbed-point-under-the-pointer 300
  (prop/for-all [cam cam-gen at at-gen d at-gen]
    (let [p    (on-target-plane cam at)
          cam' (orbit/pan cam d)]
      (close? (mapv + at d) (:at (space/project cam' p)) 1e-6))))

(deftest camera-zoom-magnifies-and-is-clamped
  (let [cam  (assoc space/default-camera :target [0.0 0.0 0.0])
        b    (space/basis cam)
        a    (space/v* 1.0 (:right b))
        span (fn [c] (Math/abs (double (first (:at (space/project c a))))))
        cam' (orbit/zoom-at cam 2.0 [0.0 0.0])]
    (is (close? [2.0] [(/ (span cam') (span cam))]) "the target plane shows twice as large")
    (is (close? [6.0] [(:distance cam')]) "a perspective camera also walks in")
    (is (close? [0.0 0.0 0.0] (:target cam')) "zooming about the middle keeps the target"))
  (let [c (orbit/zoom-at space/default-camera 1e9 [1.0 1.0])]
    (is (<= (:zoom c) (second orbit/zoom-limits)))
    (is (>= (:distance c) (first orbit/distance-limits))))
  (let [c (orbit/zoom-at (assoc space/default-camera :projection :orthographic) 1e-9 [0 0])]
    (is (= (first orbit/zoom-limits) (:zoom c)))
    (is (= 12.0 (:distance c)) "an orthographic camera does not move")))

(deftest keys-are-bound-as-documented
  (is (= [:zoom navigate/key-zoom] (navigate/key-action "+" #{})))
  (is (= [:zoom navigate/key-zoom] (navigate/key-action "=" #{:shift})))
  (is (= [:reset] (navigate/key-action "0" #{})))
  (is (= [:pan [navigate/key-step 0.0]] (navigate/key-action "ArrowLeft" #{})))
  (is (= [:turn [(- navigate/key-step) 0.0]] (navigate/key-action "ArrowLeft" #{:alt}))
      "alt turns the arrows into an orbit")
  (is (= (navigate/key-action "a" #{}) (navigate/key-action "A" #{:shift})))
  (is (nil? (navigate/key-action "q" #{}))))

;; ── Routing ─────────────────────────────────────────────────────────────

(defn- shown [s] (assoc s :opacity 1.0 :draw 1.0))

(def ^:private flat
  {:t 0 :shapes [(shown {:id 7 :node :dot :at [2.0 0.0] :opts {:name "A"}})]})

(def ^:private spatial
  {:t 0 :camera {:yaw -0.8 :pitch 0.6 :distance 11.0}
   :shapes [(shown {:id "pts" :node :points3 :points [[1.0 1.0 0.0]] :opts {:names ["A"]}})]})

(def ^:private specs [{:id :a :drag {:node "A"}}])

(defn- run
  "Thread `events` through pointer/step from state `st` on frame `f`."
  [st f events]
  (reduce (fn [st ev]
            (let [out (pointer/step st specs f ev)]
              (cond-> (pointer/settle st out)
                (:params out) (assoc :params (:params out)))))
          st events))

(deftest a-2d-drag-writes-source-units-through-a-zoomed-view
  (let [st (run {:params {:a [2.0 0.0]} :view {:center [0.0 0.0] :scale 2.0}} flat
                [{:type :down :at [4.0 0.0] :hit 7 :button :primary}
                 {:type :move :at [5.0 1.0] :hit nil}
                 {:type :up :at [5.0 1.0] :hit nil}])]
    (is (close? [2.5 0.5] (:a (:params st))) "seen travel [1 1] is source travel [0.5 0.5]")
    (is (= {:center [0.0 0.0] :scale 2.0} (:view st)) "a drag does not pan")))

(deftest drag-keeps-priority-and-other-presses-pan
  (testing "shift on a draggable still drags"
    (let [out (pointer/step {:params {:a [2.0 0.0]}} specs flat
                            {:type :down :at [2.0 0.0] :hit 7 :mods #{:shift}})]
      (is (some? (:drag out)))
      (is (nil? (:pan out)))))
  (testing "the right button pans, even over a draggable"
    (let [out (pointer/step {} specs flat {:type :down :at [2.0 0.0] :hit 7 :button :secondary})]
      (is (nil? (:drag out)))
      (is (some? (:pan out)))))
  (testing "a press on empty 2D space pans, and the picture follows"
    (let [st (run {} flat [{:type :down :at [0.0 0.0] :hit nil}
                           {:type :move :at [1.0 0.5] :hit nil}])]
      (is (close? [-1.0 -0.5] (get-in st [:view :center])))
      (is (nil? (:orbit st)))))
  (testing "on a 3D frame empty space orbits, shift pans"
    (is (some? (:orbit (pointer/step {} specs spatial {:type :down :at [5.0 5.0] :hit nil}))))
    (let [st (run {} spatial [{:type :down :at [5.0 5.0] :hit nil :mods #{:shift}}
                              {:type :move :at [6.0 5.0] :hit nil}])]
      (is (nil? (:orbit st)))
      (is (= -0.8 (get-in st [:camera :yaw])) "a pan does not turn")
      (is (not= [0.0 0.0 0.0] (get-in st [:camera :target])) "it slides the target"))))

(deftest gestures-zoom-turn-and-reset
  (testing "a wheel zooms a 2D view about the pointer"
    (let [lk (:look (pointer/step {} specs flat {:type :wheel :at [1.0 1.0] :dy -100}))]
      (is (< 1.0 (get-in lk [:view :scale])))
      (is (nil? (:camera lk)))))
  (testing "a pinch zooms a 3D camera"
    (is (close? [2.0] [(get-in (pointer/step {} specs spatial {:type :pinch :at [0 0] :factor 2.0})
                               [:look :camera :zoom])])))
  (testing "opposite wheel notches cancel"
    (let [st (run {} flat [{:type :wheel :at [1.0 2.0] :dy 120} {:type :wheel :at [1.0 2.0] :dy -120}])]
      (is (close? [1.0] [(get-in st [:view :scale])]))))
  (testing "turn keys orbit a 3D frame and do nothing to a 2D one"
    (is (some? (get-in (pointer/step {} specs spatial {:type :key :key "a"}) [:look :camera :yaw])))
    (is (nil? (:look (pointer/step {} specs flat {:type :key :key "a"})))))
  (testing "key 0 and a double click reset to the source's own"
    (let [st {:camera {:yaw 1.0} :view {:scale 3.0}}]
      (is (= {:camera nil :view nil} (:look (pointer/step st specs flat {:type :key :key "0"}))))
      (is (= {:camera nil :view nil} (:look (pointer/step st specs spatial {:type :double :at [0 0]})))))))
