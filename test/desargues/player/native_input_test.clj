(ns desargues.player.native-input-test
  "The native host's raw winit input translated into IPointer / IKeyboard
   events (desargues.player.native-input), and those events driving the
   player through desargues.player.pointer/step like any other host's."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.player.frame :as frame]
            [desargues.player.native-input :as ni]
            [desargues.player.navigate :as navigate]))

(def ^:private w 640)
(def ^:private h 360)

(def ^:private dot-frame
  (frame/frame 0 [{:id "A" :node :dot :at [1.0 1.0] :opacity 1.0}]))

(defn- px-of [p] (frame/->px (frame/viewport w h) p))

(defn- run [raws]
  (reduce (fn [[st out] raw]
            (let [[st' evs] (ni/translate st w h dot-frame raw)] [st' (into out evs)]))
          [(ni/initial) []] raws))

(defn- close? [[ax ay] [bx by]]
  (and (< (Math/abs (- ax bx)) 1e-9) (< (Math/abs (- ay by)) 1e-9)))

(deftest pixels-go-back-to-scene-units-and-hit-test
  (let [px (px-of [1.0 1.0])
        [_ [ev]] (run [{:type :down :px px :button :primary :mods [:shift] :ms 0.0}])]
    (is (= :down (:type ev)))
    (is (close? [1.0 1.0] (:at ev)))
    (is (= "A" (:hit ev)))
    (is (= :primary (:button ev)))
    (is (= #{:shift} (:mods ev))))
  (testing "the centre pixel is the scene origin, y up"
    (let [[_ [ev]] (run [{:type :down :px [320.0 100.0] :button :secondary :mods [] :ms 0.0}])]
      (is (close? [0.0 (/ 80.0 45.0)] (:at ev)))
      (is (nil? (:hit ev)))
      (is (= :secondary (:button ev))))))

(deftest moves-only-while-pressed
  (let [[st evs] (run [{:type :move :px [10 10] :mods []}
                       {:type :down :px [10 10] :button :middle :mods [] :ms 0.0}
                       {:type :move :px [20 10] :mods []}
                       {:type :up :px [20 10] :button :middle :mods [] :ms 50.0}
                       {:type :move :px [30 10] :mods []}])]
    (is (= [:down :move :up] (mapv :type evs)))
    (is (= :middle (:button (second evs))) "a move carries the held button")
    (is (empty? (:pressed st)))))

(deftest wheel-lines-and-pixels-in-browser-pixels
  (let [[_ [a b c]] (run [{:type :wheel :px [320 180] :lines -1.0 :mods []}
                          {:type :wheel :px [320 180] :pixels 30.0 :mods [:ctrl]}
                          {:type :wheel :px [320 180] :lines 0.0 :mods []}])]
    (is (= {:type :wheel :dy 100.0 :mods #{}} (dissoc a :at)) "a notch toward the user scrolls down")
    (is (close? [0.0 0.0] (:at a)))
    (is (= -30.0 (:dy b)))
    (is (nil? c) "a zero delta is no event")
    (is (> 1.0 (navigate/magnification a)) "scrolling down zooms out")))

(deftest pinch-is-a-factor
  (let [[_ [ev]] (run [{:type :pinch :px [320 180] :delta 0.1}])]
    (is (= :pinch (:type ev)))
    (is (close? [1.1 0.0] [(:factor ev) 0.0]))))

(deftest two-quick-clicks-are-a-double
  (let [down  (fn [ms x] {:type :down :px [x 100] :button :primary :mods [] :ms ms})
        up    (fn [ms x] {:type :up :px [x 100] :button :primary :mods [] :ms ms})
        types (fn [raws] (mapv :type (second (run raws))))
        dbls  (fn [raws] (count (filter #{:double} (types raws))))]
    (is (= [:down :up :down :up :double]
           (types [(down 0 100) (up 50 100) (down 200 101) (up 250 101)])))
    (testing "too slow, too far, or a third click is not (another) double"
      (is (zero? (dbls [(down 0 100) (up 50 100) (down 900 100) (up 950 100)])))
      (is (zero? (dbls [(down 0 100) (up 50 100) (down 100 140) (up 150 140)])))
      (is (= 1 (dbls [(down 0 100) (up 10 100) (down 20 100)
                      (up 30 100) (down 40 100) (up 50 100)]))))))

(deftest keys-keep-dom-names-and-leave-desktop-chords
  (let [[_ evs] (run [{:type :key :key "ArrowLeft" :mods [:alt]}
                      {:type :key :key "+" :text "+" :mods [:shift]}
                      {:type :key :key "c" :text "c" :mods [:ctrl]}])]
    (is (= [{:type :key :key "ArrowLeft" :mods #{:alt}}
            {:type :key :key "+" :mods #{:shift}}] evs))
    (is (= [:turn [-0.5 -0.0]] (navigate/key-action "ArrowLeft" #{:alt})))))
