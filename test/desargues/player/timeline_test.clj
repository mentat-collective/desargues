(ns desargues.player.timeline-test
  "The player's pure strata: scheduling, frames at t, and session transitions,
   over graphs recorded from real desargues.scene constructs."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.player :as player]
            [desargues.player.frame :as frame]
            [desargues.player.session :as session]
            [desargues.player.timeline :as tl]
            [desargues.scene :as s]
            [desargues.player.graph-source :as gs]
            [desargues.player.protocols :as p]))

(defn- glide-construct
  "A dot that appears (0.5 s), glides to `to` (2 s), then holds 1 s."
  [to]
  (fn [stage]
    (let [d (-> (s/dot :color :gold) (s/move-to [0 0]))]
      (s/play! stage (s/appear d :run-time 0.5))
      (s/play! stage (s/glide d to :run-time 2.0))
      (s/hold! stage 1))))

(defn- shape-at [g t id]
  (some #(when (= id (:id %)) %) (:shapes (tl/frame-at g (tl/schedule g) t))))

(deftest schedule-runs-steps-back-to-back
  (let [g  (player/record "Glide" (glide-construct [3 0]))
        sc (tl/schedule g)]
    (is (= 3.5 (:duration sc)))
    (is (= [[0.0 0.5] [0.5 2.5]] (mapv (juxt :start :end) (:anims sc))))))

(deftest groups-run-together-and-staggers-offset
  (let [g (player/record "Group"
                         (fn [stage]
                           (let [a (s/dot) b (s/dot)]
                             (s/play! stage (s/together [(s/appear a :run-time 1.0)
                                                         (s/appear b :run-time 1.0)]))
                             (s/play! stage (s/stagger [(s/vanish a :run-time 1.0)
                                                        (s/vanish b :run-time 1.0)]
                                                       :lag-ratio 0.5)))))
        starts (mapv :start (:anims (tl/schedule g)))]
    (is (= [0.0 0.0 1.0 1.5] starts))))

(deftest frame-at-resolves-endpoints-exactly
  (let [g (player/record "Glide" (glide-construct [3 0]))]
    (testing "hidden before it appears, visible after"
      (is (zero? (:opacity (shape-at g 0.0 1))))
      (is (= 1.0 (:opacity (shape-at g 0.5 1)))))
    (testing "the glide starts at the origin and ends exactly on target"
      (is (= [0.0 0.0] (:at (shape-at g 0.5 1))))
      (is (= [3.0 0.0] (:at (shape-at g 2.5 1))))
      (is (= [3.0 0.0] (:at (shape-at g 3.5 1)))))
    (testing "only visible shapes are painted"
      (is (empty? (frame/visible-shapes (tl/frame-at g (tl/schedule g) 0.0)))))))

(defspec glide-stays-between-its-endpoints 200
  (prop/for-all [x (gen/double* {:min -6.0 :max 6.0 :NaN? false})
                 t (gen/double* {:min 0.5 :max 2.5 :NaN? false})]
    (let [g  (player/record "Glide" (glide-construct [x 0]))
          px (first (:at (shape-at g t 1)))]
      (<= (min 0.0 x) px (max 0.0 x)))))

(deftest unknown-animation-kinds-are-ignored
  (is (= {1 {:opacity 0.5}}
         (tl/apply-anim {1 {:opacity 0.5}} {:anim :teleport :target 1} 1.0))))

(deftest session-transitions
  (let [src (fn [to] (gs/graph-source (player/record "Glide" (glide-construct to))))
        s   (session/session (src [3 0]))]
    (testing "a paused session does not move"
      (is (= 0.0 (:t (session/tick s 1.0)))))
    (testing "playing advances by speed * dt"
      (is (= 1.0 (:t (session/tick (session/play s) 1.0)))))
    (testing "looping wraps past the end"
      (is (< (Math/abs (- 0.5 (:t (session/tick (session/play s) 4.0)))) 1e-9)))
    (testing "without loop it stops on the last frame"
      (let [s' (session/tick (session/play (assoc s :loop? false)) 9.0)]
        (is (= 3.5 (:t s')))
        (is (false? (:playing? s')))))
    (testing "seek clamps to the duration"
      (is (= 3.5 (:t (session/seek s 99))))
      (is (= 0.0 (:t (session/seek s -1)))))
    (testing "reload keeps time and play state"
      (let [s' (session/reload (session/play (session/seek s 2.0)) (src [-3 0]))]
        (is (= 2.0 (:t s')))
        (is (:playing? s'))))))

(deftest any-source-plays-like-a-recorded-one
  (testing "LSP: a hand-written ISource drives the same session transitions"
    (let [still (reify p/ISource
                  (-duration [_] 2.0)
                  (-frame-at [_ t] (frame/frame t [{:id 1 :node :dot :at [t 0] :opacity 1.0}])))
          s     (session/tick (session/play (session/session still)) 1.5)]
      (is (= 1.5 (:t s)))
      (is (= [1.5 0] (:at (first (:shapes (session/current-frame s)))))))))
