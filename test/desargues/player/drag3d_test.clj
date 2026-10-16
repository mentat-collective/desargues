(ns desargues.player.drag3d-test
  "Dragging a point in space, all pure: unprojection inverts projection under
   both projections, a pointer ray meets a plane, a 3D drag spec moves a
   :points3 point in world space, and the player drags it through the
   orbited camera."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.player :as player]
            [desargues.player.drag :as drag]
            [desargues.player.pointer :as pointer]
            [desargues.player.protocols :as p]
            [desargues.player.space :as space]))

(defn- close?
  ([a b] (close? a b 1e-9))
  ([a b eps] (every? #(< (Math/abs (double %)) eps) (map - a b))))

(def ^:private angle (gen/double* {:min -3.0 :max 3.0 :NaN? false :infinite? false}))
(def ^:private pitch (gen/double* {:min -1.4 :max 1.4 :NaN? false :infinite? false}))
(def ^:private coord (gen/double* {:min -5.0 :max 5.0 :NaN? false :infinite? false}))
(def ^:private projection (gen/elements [:perspective :orthographic]))

(defn- shown [s] (assoc s :opacity 1.0 :draw 1.0))

;; ── Unprojection ────────────────────────────────────────────────────────────

(defspec unproject-inverts-project 300
  ;; A world point lies on the ray of the scene point it projects to, and the
  ;; points of that ray in front of the eye project back to it.
  (prop/for-all [yaw angle pitch pitch proj projection x coord y coord z coord
                 t (gen/double* {:min 0.5 :max 30.0 :NaN? false})]
    (let [cam {:yaw yaw :pitch pitch :distance 12.0 :projection proj}
          p   [x y z]
          {:keys [origin dir]} (space/unproject cam (:at (space/project cam p)))
          rel (space/v- p origin)
          q   (space/v+ origin (space/v* t dir))]
      (and (< (space/norm (space/cross rel dir)) 1e-6)
           (or (= :orthographic proj) (pos? (space/dot3 rel dir)))
           (close? [1.0] [(space/norm dir)] 1e-12)
           (or (<= (:depth (space/project cam q)) space/near)
               (close? (:at (space/project cam p)) (:at (space/project cam q)) 1e-6))))))

(defspec unproject-then-project-round-trips 300
  (prop/for-all [yaw angle pitch pitch proj projection sx coord sy coord]
    (let [cam {:yaw yaw :pitch pitch :distance 12.0 :projection proj}
          {:keys [origin dir]} (space/unproject cam [sx sy])]
      (close? [sx sy] (:at (space/project cam (space/v+ origin (space/v* 7.0 dir)))) 1e-6))))

(deftest ray-meets-plane
  (let [ground {:point [0 0 0] :normal [0 0 1]}]
    (is (close? [1.0 2.0 0.0] (space/ray-plane {:origin [1 2 5] :dir [0 0 -1]} ground)))
    (is (nil? (space/ray-plane {:origin [1 2 5] :dir [1 0 0]} ground)) "parallel")
    (is (nil? (space/ray-plane {:origin [1 2 5] :dir [0 0 1]} ground)) "behind the origin")
    (testing "the pointer ray meets the plane where the plane's point projects"
      (let [cam {:yaw 0.7 :pitch 0.5 :distance 10.0}
            w   [1.5 -0.5 0.0]]
        (is (close? w (space/ray-plane (space/unproject cam (:at (space/project cam w))) ground)))))))

;; ── The drag constraint ─────────────────────────────────────────────────────

(defn- point-frame
  "A 3D frame with points A at `p` and B at [0 0 2], seen through `cam`."
  [cam p]
  {:t 0 :camera cam
   :shapes [(shown {:id "pts" :node :points3 :points [p [0 0 2]] :opts {:names ["A" "B"]}})]})

(deftest drag-moves-a-point-in-a-plane
  (let [cam    {:yaw -0.8 :pitch 0.6 :distance 11.0}
        ground {:point [0 0 0] :normal [0 0 1]}
        spec   {:id :p :drag {:node "A" :plane ground}}
        f      (point-frame cam [1.0 1.0 0.0])
        at     (fn [w] (:at (space/project cam w)))
        grab   [0.03 -0.02]
        down   (drag/pointer nil [spec] f {:p [1.0 1.0 0.0]}
                             {:type :down :at (mapv + (at [1.0 1.0 0.0]) grab) :hit "pts"})
        move   (drag/pointer (:drag down) [spec] f {:p [1.0 1.0 0.0]}
                             {:type :move :at (mapv + (at [2.0 -1.0 0.0]) grab) :hit nil})
        on-ground #(space/ray-plane (space/unproject cam (mapv + (at %) grab)) ground)
        want   (space/v+ (on-ground [2.0 -1.0 0.0])
                         (space/v- [1.0 1.0 0.0] (on-ground [1.0 1.0 0.0])))]
    (is (some? (:drag down)) "a press on the named point starts a drag")
    (is (= 3 (count (get-in down [:drag :offset]))) "the grab offset is kept in world space")
    (is (close? want (:p (:params move)))
        "A moves with the plane point under the pointer, keeping the world grab offset")
    (is (close? [0.0] [(nth (:p (:params move)) 2)]) "and stays on the plane")
    (is (< (space/norm (space/v- want [2.0 -1.0 0.0])) 0.05) "close to where it was aimed")
    (testing ":->params gets the world 3-vector"
      (let [s2  (assoc-in spec [:drag :->params] (fn [ps [x y z]] (assoc ps :h (+ x y z))))
            d   (drag/pointer nil [s2] f {} {:type :down :at (at [1.0 1.0 0.0]) :hit "pts"})
            out (drag/pointer (:drag d) [s2] f {} {:type :move :at (at [0.5 0.25 0.0]) :hit nil})]
        (is (close? [0.75] [(:h (:params out))]))))
    (testing ":plane may depend on the params"
      (let [s3  (assoc-in spec [:drag :plane] (fn [ps] {:point [0 0 (:lift ps)] :normal [0 0 1]}))
            d   (drag/pointer nil [s3] f {:lift 0.0} {:type :down :at (at [1.0 1.0 0.0]) :hit "pts"})
            out (drag/pointer (:drag d) [s3] f {:lift 0.0} {:type :move :at (at [0.0 0.0 0.0]) :hit nil})]
        (is (close? [0.0 0.0 0.0] (:p (:params out))))))
    (testing ":axis :z writes one coordinate"
      (is (= {:h 3.0} (drag/->params {:id :h :drag {:node "A" :axis :z}} {} [1 2 3]))))
    (testing "a pointer whose ray misses the plane leaves the params alone"
      (let [edge-on {:yaw 0.0 :pitch 0.0 :distance 10.0 :projection :orthographic}
            f2      (point-frame edge-on [1.0 1.0 0.0])
            d       (drag/pointer nil [spec] f2 {} {:type :down :at [1.0 0.0] :hit "pts"})]
        (is (nil? (:params (drag/pointer (:drag d) [spec] f2 {} {:type :move :at [2.0 0.0]})))))))
  (testing "of several named points in one :points3, the one nearest the pointer is grabbed"
    (let [cam   {:yaw -0.8 :pitch 0.6 :distance 11.0}
          f     (point-frame cam [1.0 1.0 0.0])
          specs [{:id :a :drag {:node "A"}} {:id :b :drag {:node "B"}}]
          d     (drag/pointer nil specs f {} {:type :down :at (:at (space/project cam [0 0 2])) :hit "pts"})]
      (is (= :b (get-in d [:drag :spec :id])))))
  (testing "without :plane the point moves in the plane facing the camera"
    (let [cam  {:yaw 0.3 :pitch 0.2 :distance 9.0}
          spec {:id :p :drag {:node "A"}}
          f    (point-frame cam [0.5 0.5 0.5])
          a    (:at (space/project cam [0.5 0.5 0.5]))
          d    (drag/pointer nil [spec] f {} {:type :down :at a :hit "pts"})
          out  (drag/pointer (:drag d) [spec] f {} {:type :move :at (mapv + a [0.4 0.1]) :hit nil})
          p    (:p (:params out))]
      (is (close? (mapv + a [0.4 0.1]) (:at (space/project cam p))) "under the pointer")
      (is (close? [0.0] [(space/dot3 (:forward (space/basis cam)) (space/v- p [0.5 0.5 0.5]))])
          "at the same depth"))))

;; ── Through the player ──────────────────────────────────────────────────────

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

(defn- point-source
  "A 3D source showing point A at params :p."
  [{:keys [p]}]
  (reify p/ISource
    (-duration [_] 1.0)
    (-frame-at [_ t]
      {:t t :camera {:yaw -0.8 :pitch 0.6 :distance 11.0}
       :shapes [(shown {:id "pts" :node :points3 :points [p] :opts {:names ["A"]}})]})))

(deftest player-drags-a-point-in-space-after-an-orbit
  (let [ground {:point [0 0 0] :normal [0 0 1]}
        host   (->PointerHost (atom nil) (atom nil))
        pl     (player/open-source! host point-source
                                    {:params {:p [1.0 1.0 0.0]} :autoplay? false
                                     :controls [{:id :p :drag {:node "A" :plane ground}}]})
        send!  @(:pointer host)]
    (send! {:type :down :at [3.0 -3.0] :hit nil})
    (send! {:type :move :at [4.0 -3.5] :hit nil})
    (send! {:type :up :at [4.0 -3.5] :hit nil})
    (let [cam (merge space/default-camera (:camera @(:state pl)))
          at  (fn [w] (:at (space/project cam w)))]
      (is (not= -0.8 (:yaw cam)) "the orbit turned the camera")
      (send! {:type :down :at (at [1.0 1.0 0.0]) :hit "pts"})
      (send! {:type :move :at (at [-1.0 0.5 0.0]) :hit nil})
      (is (close? [-1.0 0.5 0.0] (:p (player/params pl)))
          "the point follows the pointer through the orbited camera, on the plane")
      (is (= (:yaw cam) (:yaw (:camera @(:state pl)))) "a drag does not orbit")
      (send! {:type :up :at (at [-1.0 0.5 0.0]) :hit nil})
      (is (nil? (:drag @(:state pl)))))))

(deftest pointer-step-routes-drag-and-orbit
  (let [f3    (point-source {:p [1.0 1.0 0.0]})
        f     (p/-frame-at f3 0.0)
        specs [{:id :p :drag {:node "A"}}]]
    (is (pointer/wants-pointer? [] f) "a 3D frame wants the pointer for orbiting")
    (is (not (pointer/wants-pointer? [] {:t 0 :shapes []})) "a 2D frame without drag specs does not")
    (is (some? (:orbit (pointer/step {} specs f {:type :down :at [5.0 5.0] :hit nil})))
        "empty space on a 3D frame starts an orbit")
    (is (nil? (:orbit (pointer/step {} specs {:t 0 :shapes []} {:type :down :at [0 0] :hit nil})))
        "a 2D frame never orbits")
    (let [out (pointer/step {} specs f {:type :down :at (:at (space/project (space/camera f) [1 1 0]))
                                        :hit "pts"})]
      (is (some? (:drag out)))
      (is (nil? (:orbit out)) "a press on a draggable point drags, it does not orbit"))))
