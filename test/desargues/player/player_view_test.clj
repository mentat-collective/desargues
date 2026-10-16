(ns desargues.player.player-view-test
  "The view through the JVM player: zoom!, pan!, reset-view! and view on 2D
   and 3D sources, overrides surviving parameter changes, keys and wheels
   arriving through the host ports, and a headless render of a zoomed view
   probed pixel by pixel."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.player :as player]
            [desargues.player.frame :as frame]
            [desargues.player.headless :as headless]
            [desargues.player.protocols :as p]))

(defn- close? [a b] (every? #(< (Math/abs (double %)) 1e-6) (map - a b)))

(defrecord ProbeHost [surface pointer keys]
  p/ISurface
  (-present! [this f] (p/-present! surface f) this)
  (-size [_] (p/-size surface))
  p/IWindow
  (-show! [this _] this)
  (-close! [this] this)
  (-snapshot! [_ _] nil)
  p/IPointer
  (-on-pointer! [this f] (reset! pointer f) this)
  p/IKeyboard
  (-on-key! [this f] (reset! keys f) this)
  p/IClock
  (-on-tick! [this _] this)
  (-stop-ticks! [this] this))

(defn- host [] (->ProbeHost (headless/surface 1280 720) (atom nil) (atom nil)))

(defn- dot-source
  "A 2D source: a yellow dot at [x 0] and a circle of radius 1 about the origin."
  [{:keys [x]}]
  (reify p/ISource
    (-duration [_] 1.0)
    (-frame-at [_ t]
      (frame/frame t [{:id 1 :node :circle :at [0 0] :opts {:radius 1.0 :color :blue}
                       :opacity 1.0 :draw 1.0}
                      {:id 2 :node :dot :at [x 0] :opts {:radius 0.15 :color :yellow}
                       :opacity 1.0 :draw 1.0}]))))

(defn- space-source
  "A 3D source: one point at params :p."
  [{:keys [p]}]
  (reify p/ISource
    (-duration [_] 1.0)
    (-frame-at [_ t]
      {:t t :camera {:yaw -0.8 :pitch 0.6 :distance 11.0}
       :shapes [{:id "pts" :node :points3 :points [p] :opacity 1.0 :draw 1.0}]})))

(defn- pixel
  "[r g b] of the host's image at pixel [x y]."
  [h [x y]]
  (let [c (.getRGB (headless/image (:surface h)) (int x) (int y))]
    [(bit-and (bit-shift-right c 16) 255) (bit-and (bit-shift-right c 8) 255) (bit-and c 255)]))

(defn- yellow? [[r g b]] (and (> r 200) (> g 200) (< b 80)))

(deftest zoomed-2d-view-renders-and-survives-params
  (let [h  (host)
        pl (player/open-source! h dot-source {:params {:x 2.0} :autoplay? false})
        vp (frame/viewport 1280 720)]
    (testing "the source's own framing first"
      (is (= {:three-d? false :camera nil :view nil :zoom 1.0} (player/view pl)))
      (is (yellow? (pixel h (frame/->px vp [2.0 0.0])))))
    (player/zoom! pl 2.0)
    (testing "zoom 2 about the middle: the dot shows twice as far out"
      (is (= 2.0 (:zoom (player/view pl))))
      (is (yellow? (pixel h (frame/->px vp [4.0 0.0]))))
      (is (not (yellow? (pixel h (frame/->px vp [2.0 0.0]))))))
    (player/set-param! pl :x 1.0)
    (testing "a parameter change keeps the zoom"
      (is (= 2.0 (:zoom (player/view pl))))
      (is (yellow? (pixel h (frame/->px vp [2.0 0.0])))))
    (player/pan! pl [-2.0 0.0])
    (testing "a pan moves the picture by the travel"
      (is (yellow? (pixel h (frame/->px vp [0.0 0.0])))))
    (player/reset-view! pl)
    (testing "reset returns to the source's own framing"
      (is (nil? (:view (player/view pl))))
      (is (yellow? (pixel h (frame/->px vp [1.0 0.0])))))))

(deftest zoom-to-cursor-keeps-the-dot-under-the-wheel
  (let [h  (host)
        pl (player/open-source! h dot-source {:params {:x 2.0} :autoplay? false})
        vp (frame/viewport 1280 720)]
    (dotimes [_ 3] (@(:pointer h) {:type :wheel :at [2.0 0.0] :dy -200}))
    (is (< 1.5 (:zoom (player/view pl))))
    (is (yellow? (pixel h (frame/->px vp [2.0 0.0]))) "the dot stays under the pointer")
    (is (not (yellow? (pixel h (frame/->px vp [2.25 0.0])))) "a dot keeps its marker size")))

(deftest keys-arrive-through-the-keyboard-port
  (let [h  (host)
        pl (player/open-source! h dot-source {:params {:x 2.0} :autoplay? false})
        key! @(:keys h)]
    (is (true? (key! {:type :key :key "+" :mods #{}})) "a used key reports true")
    (is (close? [1.25] [(:zoom (player/view pl))]))
    (is (true? (key! {:type :key :key "ArrowLeft" :mods #{}})))
    (is (false? (key! {:type :key :key "w" :mods #{}})) "a turn key on a 2D frame is unused")
    (is (true? (key! {:type :key :key "0" :mods #{}})))
    (is (nil? (:view (player/view pl))))))

(deftest a-3d-camera-zooms-pans-and-survives-params
  (let [h  (host)
        pl (player/open-source! h space-source {:params {:p [1.0 1.0 0.0]} :autoplay? false})]
    (player/zoom! pl 2.0)
    (let [cam (:camera (player/view pl))]
      (is (:three-d? (player/view pl)))
      (is (close? [2.0 5.5] [(:zoom cam) (:distance cam)]))
      (is (= -0.8 (:yaw cam)) "the source's own angle is kept"))
    (player/pan! pl [1.0 0.0])
    (player/set-param! pl :p [0.0 0.0 1.0])
    (testing "the camera override survives a parameter change"
      (is (= 2.0 (:zoom (player/view pl))))
      (is (not= [0.0 0.0 0.0] (get-in (player/view pl) [:camera :target]))))
    (player/set-camera! pl {:yaw 0.3})
    (is (= 0.3 (get-in (player/view pl) [:camera :yaw])) "set-camera! still merges")
    (player/reset-view! pl)
    (is (= {:three-d? true :camera nil :view nil :zoom 1.0} (player/view pl)))))
