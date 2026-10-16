(ns desargues.player.pointer-test
  "Pointer input, all pure: px <-> scene, hit-testing, and a drag sequence
   that re-records a construct through a headless host."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.player :as player]
            [desargues.player.drag :as drag]
            [desargues.player.frame :as frame]
            [desargues.player.hit :as hit]
            [desargues.player.protocols :as p]
            [desargues.scene :as s]))

(def ^:private coord (gen/double* {:min -50.0 :max 50.0 :NaN? false :infinite? false}))

(defn- close? [a b] (every? #(< (Math/abs (double %)) 1e-9) (map - a b)))

(defspec px-scene-round-trip 200
  (prop/for-all [w (gen/choose 100 3000) h (gen/choose 100 2000) x coord y coord]
    (let [vp (frame/viewport w h)]
      (and (close? [x y] (frame/px->scene vp (frame/->px vp [x y])))
           (close? [x y] (frame/->px vp (frame/px->scene vp [x y])))))))

(deftest px->scene-centre-and-y-up
  (let [vp (frame/viewport 1280 720)]
    (is (= [0.0 0.0] (frame/px->scene vp [640 360])))
    (is (pos? (second (frame/px->scene vp [640 0]))) "the top of the window is +y")))

(defn- shown [s] (assoc s :opacity 1.0 :draw 1.0))

(def ^:private fr
  (frame/frame 0 (map shown [{:id 1 :node :line :from [-2 0] :to [2 0]}
                             {:id 2 :node :circle :at [0 0] :opts {:radius 2.0}}
                             {:id 3 :node :dot :at [2 0]}
                             {:id 4 :node :rectangle :at [0 3] :opts {:width 2 :height 1}
                              :fill {:color :blue}}])))

(deftest hit-rules
  (testing "a dot outranks the line and circle it sits on"
    (is (= 3 (hit/hit fr [2.0 0.0] 0.1))))
  (testing "otherwise the topmost stroke within tolerance"
    (is (= 2 (hit/hit fr [-2.0 0.0] 0.1)) "circle drawn after the line")
    (is (= 1 (hit/hit fr [0.5 0.05] 0.1))))
  (testing "filled areas hit inside, unfilled only near the stroke"
    (is (= 4 (hit/hit fr [0.2 3.1] 0.01)))
    (is (nil? (hit/hit fr [0.5 1.0] 0.1)) "inside the unfilled circle, off every stroke"))
  (testing "hidden shapes are never hit"
    (is (nil? (hit/hit (update fr :shapes (partial mapv #(assoc % :opacity 0.0))) [2 0] 0.5)))))

(defspec hit-is-within-tolerance 200
  (prop/for-all [x (gen/double* {:min -5.0 :max 5.0 :NaN? false})
                 y (gen/double* {:min -5.0 :max 5.0 :NaN? false})
                 tol (gen/double* {:min 0.0 :max 1.0 :NaN? false})]
    (let [id (hit/hit fr [x y] tol)]
      (or (nil? id)
          (<= (hit/distance (some #(when (= id (:id %)) %) (:shapes fr)) [x y]) tol)))))

(defspec growing-tolerance-never-loses-a-hit 200
  (prop/for-all [x (gen/double* {:min -5.0 :max 5.0 :NaN? false})
                 y (gen/double* {:min -5.0 :max 5.0 :NaN? false})]
    (or (nil? (hit/hit fr [x y] 0.1)) (some? (hit/hit fr [x y] 0.5)))))

(deftest hit-px-uses-a-pixel-tolerance
  (let [vp (frame/viewport 1280 720)
        [px py] (frame/->px vp [2 0])]
    (is (= 3 (hit/hit-px fr vp [(+ px 5) py] 8)))
    (is (nil? (hit/hit-px fr vp [(+ px 60) (+ py 60)] 8)))))

;; ── Drag sequence through a headless host ───────────────────────────────

(defn- segment
  "A dot named A at params :a joined to a fixed dot B."
  [{:keys [a]}]
  (fn [stage]
    (let [pa (-> (s/dot :name "A") (s/move-to a))
          pb (-> (s/dot :name "B") (s/move-to [2 0]))
          ab (s/line a [2 0])]
      (s/play! stage (s/appear pa :run-time 0.1) (s/appear pb :run-time 0.1)
               (s/appear ab :run-time 0.1)))))

(defn- headless-host
  "A host with a surface, a window and a pointer, and no clock or widgets."
  []
  (let [handler (atom nil)]
    (reify
      p/ISurface (-present! [this _] this) (-size [_] [1280 720])
      p/IWindow (-show! [this _] this) (-close! [this] this) (-snapshot! [_ _] nil)
      p/IClock (-on-tick! [this _] this) (-stop-ticks! [this] this)
      p/IPointer (-on-pointer! [this f] (reset! handler f) this)
      clojure.lang.IDeref (deref [_] @handler))))

(deftest drag-sequence-updates-params
  (let [host     (headless-host)
        controls [{:id :a :drag {:node "A"}}]
        pl       (player/open! host segment {:params {:a [-2.0 0.0]} :controls controls
                                             :autoplay? false})
        _        (player/seek! pl 1.0)
        a-id     (:id (some #(when (= "A" (get-in % [:opts :name])) %)
                            (:shapes (player/current-frame pl))))
        send!    @host]
    (testing "the host's pointer was wired"
      (is (fn? send!)))
    (send! {:type :down :at [-2.05 0.0] :hit a-id})
    (send! {:type :move :at [-1.05 1.0] :hit nil})
    (testing "the grab offset is kept: the point moves by the pointer's delta"
      (is (close? [-1.0 1.0] (:a (player/params pl)))))
    (testing "the construct was re-recorded: A and the segment follow"
      (let [f (player/current-frame pl)]
        (is (close? [-1.0 1.0] (:at (some #(when (= a-id (:id %)) %) (:shapes f)))))
        (is (close? [-1.0 1.0] (:from (some #(when (= :line (:node %)) %) (:shapes f)))))))
    (send! {:type :up :at [-1.05 1.0] :hit nil})
    (send! {:type :move :at [3.0 3.0] :hit nil})
    (testing "after :up, moves change nothing"
      (is (close? [-1.0 1.0] (:a (player/params pl)))))
    (testing "a press off every draggable starts no drag"
      (send! {:type :down :at [0.0 0.0] :hit nil})
      (send! {:type :move :at [1.0 1.0] :hit nil})
      (is (close? [-1.0 1.0] (:a (player/params pl)))))))

(deftest drag-maps-position-onto-params
  (is (= {:x 1.5} (drag/->params {:id :x :drag {:node 1 :axis :x}} {} [1.5 9])))
  (is (= {:r 5.0} (drag/->params {:id :r :drag {:node 1 :->params
                                                 (fn [ps [x y]] (assoc ps :r (Math/hypot x y)))}}
                                 {} [3 4]))))
