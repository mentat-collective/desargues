(ns desargues.player.space-test
  "The 3D vocabulary, all pure: camera basis, projection, frame validation,
   flattening to 2D, the orbit, and a 3D source played through a 2D host."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.player :as player]
            [desargues.player.frame :as frame]
            [desargues.player.headless :as headless]
            [desargues.player.orbit :as orbit]
            [desargues.player.present :as present]
            [desargues.player.protocols :as p]
            [desargues.player.space :as space]
            [desargues.player.space-demos :as demos]))

(defn- close?
  ([a b] (close? a b 1e-9))
  ([a b eps] (every? #(< (Math/abs (double %)) eps) (map - a b))))

(def ^:private angle (gen/double* {:min -3.0 :max 3.0 :NaN? false :infinite? false}))
(def ^:private pitch (gen/double* {:min -1.4 :max 1.4 :NaN? false :infinite? false}))
(def ^:private coord (gen/double* {:min -5.0 :max 5.0 :NaN? false :infinite? false}))

;; ── Camera and projection ─────────────────────────────────────────────────

(defspec basis-is-orthonormal 200
  (prop/for-all [yaw angle pitch pitch d (gen/double* {:min 1.0 :max 50.0 :NaN? false})]
    (let [{:keys [right up forward]} (space/basis {:yaw yaw :pitch pitch :distance d})]
      (and (close? [1 1 1] (map space/norm [right up forward]) 1e-9)
           (< (Math/abs (space/dot3 right up)) 1e-9)
           (< (Math/abs (space/dot3 right forward)) 1e-9)
           (< (Math/abs (space/dot3 up forward)) 1e-9)
           (>= (nth up 2) -1e-9)))))

(defspec target-projects-to-the-origin 200
  (prop/for-all [yaw angle pitch pitch x coord y coord z coord]
    (let [cam {:yaw yaw :pitch pitch :target [x y z] :distance 10.0}
          {:keys [at depth scale]} (space/project cam [x y z])]
      (and (close? [0 0] at 1e-9) (close? [10.0] [depth] 1e-9) (close? [1.0] [scale] 1e-12)))))

(defspec target-plane-matches-orthographic 200
  ;; On the plane through the target facing the eye, perspective and
  ;; orthographic agree: the 3D view keeps scene scale where it looks.
  (prop/for-all [yaw angle pitch pitch a coord b coord]
    (let [cam {:yaw yaw :pitch pitch :distance 12.0}
          {:keys [right up]} (space/basis cam)
          q (space/v+ (space/v* a right) (space/v* b up))]
      (close? (:at (space/project cam q))
              (:at (space/project (assoc cam :projection :orthographic) q))
              1e-9))))

(deftest perspective-shrinks-with-depth
  (let [cam {:yaw 0.0 :pitch 0.0 :distance 10.0}
        ;; the eye sits on +x looking toward -x; +y is to the right
        near (space/project cam [2.0 1.0 0.0])
        far  (space/project cam [-2.0 1.0 0.0])]
    (is (close? (:eye (space/basis cam)) [10.0 0.0 0.0]))
    (is (> (first (:at near)) 1.0) "nearer than the target: magnified")
    (is (< 0.0 (first (:at far)) 1.0) "farther: shrunk")
    (is (< (:depth near) (:depth far)))
    (is (close? [0.0 1.0] (:at (space/project cam [0.0 0.0 1.0]))) "z is up")))

;; ── Validation ────────────────────────────────────────────────────────────

(deftest frame-validation
  (testing "the demo frame is well formed"
    (is (space/valid? (p/-frame-at (demos/source-fn {}) 2.0))))
  (testing "each kind reports its own problems"
    (is (= [:surface3/ragged-grid]
           (space/shape-problems {:node :surface3 :grid [[[0 0 0] [1 0 0]] [[0 1 0]]]})))
    (is (= [:surface3/face-index]
           (space/shape-problems {:node :surface3 :vertices [[0 0 0] [1 0 0] [0 1 0]]
                                  :faces [[0 1 3]]})))
    (is (= [:curve3/too-short] (space/shape-problems {:node :curve3 :points [[0 0 0]]})))
    (is (= [:points3/points] (space/shape-problems {:node :points3 :points [[0 0]]})))
    (is (= [:axes3/range] (space/shape-problems {:node :axes3 :opts {:range [[0 1]]}})))
    (is (= [:surface3/point]
           (space/shape-problems {:node :surface3 :grid [[[0 0 ##NaN] [1 0 0]] [[0 1 0] [1 1 0]]]}))))
  (testing "camera problems and shape ids are reported"
    (is (= [{:problem :camera/distance} {:problem :camera/projection} {:shape 7 :problem :curve3/points}]
           (space/problems {:camera {:distance 0 :projection :fisheye}
                            :shapes [{:id 7 :node :curve3 :points :nope}]}))))
  (testing "2D shapes are not the vocabulary's business"
    (is (space/valid? (frame/frame 0 [{:id 1 :node :dot :at [0 0]}])))))

;; ── Flattening ────────────────────────────────────────────────────────────

(defn- shown [s] (assoc s :opacity 1.0 :draw 1.0))

(deftest flattening-to-2d
  (let [quad (shown {:id "q" :node :surface3 :grid [[[0 0 0] [0 1 0]] [[1 0 0] [1 1 0]]]
                     :fill {:color :blue :opacity 0.5}})
        f    {:t 0 :camera {:yaw 0.3 :pitch 0.6}
              :shapes [quad
                       (shown {:id "c" :node :curve3 :points [[0 0 1] [0 0 2] [0 0 3]]})
                       (shown {:id "p" :node :points3 :points [[1 1 1]] :opts {:names ["A"]}})
                       (shown {:id "label" :node :text :text "hi" :at [0 0]})]}
        g    (space/->2d f)
        kinds (map :node (:shapes g))]
    (testing "only frame kinds remain, the camera is gone"
      (is (every? frame/shape-kinds kinds))
      (is (not (space/three-d? g)))
      (is (nil? (:camera g))))
    (testing "a quad is one closed path, a 3-point curve two segments, a point a dot"
      (is (= {:path 1 :line 2 :dot 1 :text 1} (frequencies kinds))))
    (testing "primitives keep their shape's id, so hit-testing names the 3D shape"
      (is (= #{"q" "c" "p" "label"} (set (map :id (:shapes g))))))
    (testing "2D shapes are an overlay, after the 3D content"
      (is (= :text (:node (last (:shapes g))))))
    (testing "a point keeps its name for drag specs"
      (is (= "A" (get-in (some #(when (= :dot (:node %)) %) (:shapes g)) [:opts :name])))))
  (testing "a 2D frame passes through untouched"
    (let [f (frame/frame 1 [(shown {:id 1 :node :dot :at [0 0]})])]
      (is (identical? f (space/->2d f)))))
  (testing "hidden 3D shapes produce nothing; :draw draws a curve on"
    (let [c {:id "c" :node :curve3 :points (vec (for [i (range 11)] [i 0 0]))}
          n #(count (:shapes (space/->2d {:camera {} :shapes [(assoc c :opacity 1.0 :draw %)]})))]
      (is (zero? (count (:shapes (space/->2d {:camera {} :shapes [(assoc c :opacity 0.0)]})))))
      (is (< (n 0.5) (n 1.0)))
      (is (= 10 (n 1.0)))))
  (testing "painter's order: the far face is painted first"
    (let [face (fn [id x] (shown {:id id :node :surface3 :grid [[[x -1 -1] [x 1 -1]] [[x -1 1] [x 1 1]]]}))
          g    (space/->2d {:camera {:yaw 0.0 :pitch 0.0 :distance 10.0}
                            :shapes [(face "near" 2.0) (face "far" -2.0)]})]
      (is (= ["far" "near"] (map :id (:shapes g))))))
  (testing "geometry behind the eye is culled"
    (let [g (space/->2d {:camera {:yaw 0.0 :pitch 0.0 :distance 10.0}
                         :shapes [(shown {:id "p" :node :points3 :points [[20 0 0] [0 0 0]]})]})]
      (is (= 1 (count (:shapes g)))))))

(deftest grid-mesh
  (let [{:keys [vertices faces]} (space/grid->mesh (space/parametric-grid (fn [u v] [u v 0]) [0 1] [0 1] 3 2))]
    (is (= 12 (count vertices)))
    (is (= 6 (count faces)))
    (is (every? #(= 4 (count %)) faces))))

;; ── Orbit ─────────────────────────────────────────────────────────────────

(deftest orbit-by-pointer
  (let [cam {:yaw 0.0 :pitch 0.3}
        o1  (orbit/pointer nil cam {:type :down :at [0 0] :hit nil})
        o2  (orbit/pointer (:orbit o1) cam {:type :move :at [1.0 -2.0] :hit nil})
        o3  (orbit/pointer (:orbit o2) (:camera o2) {:type :up :at [1.0 -2.0]})]
    (is (some? (:orbit o1)) "a press on empty space starts an orbit")
    (is (nil? (:camera o1)))
    (is (close? [(- orbit/radians-per-unit) (+ 0.3 (* 2 orbit/radians-per-unit))]
                [(:yaw (:camera o2)) (:pitch (:camera o2))]))
    (is (nil? (:orbit o3)))
    (is (nil? (:orbit (orbit/pointer nil cam {:type :down :at [0 0] :hit "A"})))
        "a press on a shape is left to dragging"))
  (is (= orbit/pitch-limit (:pitch (orbit/turn {:pitch 0.0} [0 -100]))) "pitch is clamped")
  (is (close? [24.0] [(:distance (orbit/zoom {:distance 12.0} 2.0))])))

;; ── Hosts ─────────────────────────────────────────────────────────────────

(defrecord Recorder [shown]
  p/ISurface
  (-present! [this f] (reset! shown [:2d f]) this)
  (-size [_] [100 100]))

(defrecord SpaceRecorder [shown]
  p/ISurface
  (-present! [this f] (reset! shown [:2d f]) this)
  (-size [_] [100 100])
  p/ISpace
  (-present-space! [this f] (reset! shown [:3d f]) this))

(deftest present-picks-the-port
  (let [f3 (p/-frame-at (demos/source-fn {}) 2.0)
        f2 (frame/frame 0 [])
        r  (->Recorder (atom nil))
        s  (->SpaceRecorder (atom nil))]
    (present/present! r f3)
    (is (= :2d (first @(:shown r))))
    (is (not (space/three-d? (second @(:shown r)))) "a 2D host gets the projection")
    (present/present! s f3 {:yaw 1.0})
    (is (= :3d (first @(:shown s))) "a 3D host gets space")
    (is (= 1.0 (get-in (second @(:shown s)) [:camera :yaw])) "seen through the orbited camera")
    (present/present! s f2)
    (is (= [:2d f2] @(:shown s)) "a 2D frame still goes through ISurface")))

(defrecord PointerHost [shown pointer]
  p/ISurface
  (-present! [this f] (reset! shown f) this)
  (-size [_] [1280 720])
  p/IWindow
  (-show! [this _] this)
  (-close! [this] this)
  (-snapshot! [_ _] nil)
  p/IPointer
  (-on-pointer! [this f] (reset! pointer f) this)
  p/IClock
  (-on-tick! [this _] this)
  (-stop-ticks! [this] this))

(deftest player-orbits-a-3d-source
  (let [host (->PointerHost (atom nil) (atom nil))
        pl   (player/open-source! host demos/source-fn {:params demos/defaults :autoplay? false})
        _    (player/seek! pl 4.0)
        before (:shapes @(:shown host))
        send! @(:pointer host)]
    (is (fn? send!) "a 3D source wires the pointer even without drag specs")
    (send! {:type :down :at [3.0 -3.0] :hit nil})
    (send! {:type :move :at [4.0 -3.0] :hit nil})
    (send! {:type :up :at [4.0 -3.0] :hit nil})
    (is (not= before (:shapes @(:shown host))) "the picture turned")
    (is (close? [(- -0.9 orbit/radians-per-unit)] [(:yaw (:camera @(:state pl)))]))
    (player/set-camera! pl {:pitch 1.0})
    (is (= 1.0 (:pitch (:camera @(:state pl)))))
    (player/set-param! pl :height -0.3)
    (is (= 1.0 (:pitch (:camera @(:state pl)))) "re-recording keeps the orbit")))

(deftest headless-renders-3d
  (let [img (headless/render-image (demos/source-fn {}) 4.0 {:w 320 :h 180})
        lit (count (for [x (range 0 320 4) y (range 0 180 4)
                         :let [c (.getRGB img x y)]
                         :when (> (+ (bit-and (bit-shift-right c 16) 255)
                                     (bit-and (bit-shift-right c 8) 255)
                                     (bit-and c 255)) 60)]
                     1))]
    (is (< 300 lit) "the surface covers a good part of the picture")
    (is (= (headless/image-diff img (headless/render-image (demos/source-fn {}) 4.0 {:w 320 :h 180}) 0)
           {:same-size? true :pixels (* 320 180) :mismatched 0 :fraction 0.0 :max-delta 0})
        "deterministic, so golden images work")))

(deftest space-demo-golden
  ;; Regenerate after an intended change:
  ;;   (headless/golden-check img golden {:update? true})
  (let [img    (headless/render-image (demos/source-fn {}) 4.0 {:w 640 :h 360})
        golden (or (clojure.java.io/resource "desargues/player/golden/space-demo-t4.png")
                   "test/resources/desargues/player/golden/space-demo-t4.png")
        r      (headless/golden-check img golden {:actual-out "target/space-demo-t4-actual.png"})]
    (is (:pass? r) (pr-str r))))
