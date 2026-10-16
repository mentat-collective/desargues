(ns desargues.player.calculus-test
  "The partial-derivatives demo: Emmy's symbolic partials agree with finite
   differences, the tangent plane contains P with normal (-f_x, -f_y, 1),
   slices lie on the surface, dragging P or Q on its plane moves (x0, y0),
   the buttons step and reset, the readout follows the params, every frame
   over the parameter space and the tour is valid, and one frame matches a
   golden image."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.player :as player]
            [desargues.player.calculus-demos :as cd]
            [desargues.player.calculus-scene :as scene]
            [desargues.player.calculus-surfaces :as surfaces]
            [desargues.player.calculus-symbolic :as sym]
            [desargues.player.drag :as drag]
            [desargues.player.headless :as hl]
            [desargues.player.protocols :as p]
            [desargues.player.space :as space]))

(def ^:private coord (gen/double* {:min -2.0 :max 2.0 :NaN? false :infinite? false}))

(defn- close? [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(defn- fd
  "Central finite difference of `f` along x (axis 0) or y (axis 1)."
  [f axis x y]
  (let [h 1e-5]
    (if (zero? axis)
      (/ (- (f (+ x h) y) (f (- x h) y)) (* 2 h))
      (/ (- (f x (+ y h)) (f x (- y h))) (* 2 h)))))

;; ── Symbolic partials ─────────────────────────────────────────────────────

(defspec catalog-partials-match-finite-differences 60
  (prop/for-all [k (gen/elements surfaces/order) x coord y coord]
    (let [{:keys [f fx fy]} (surfaces/surface k)]
      (and (close? (fx x y) (fd f 0 x y) 1e-5)
           (close? (fy x y) (fd f 1 x y) 1e-5)))))

(deftest emmy-derives-at-runtime-too
  (let [s (sym/surface '(* (sin x) (exp y)))]
    (is (= '(* (cos x) (exp y)) (get-in s [:expr :fx])))
    (is (str/includes? (get-in s [:infix :fy]) "exp"))
    (is (close? ((:fy s) 0.3 0.2) (* (Math/sin 0.3) (Math/exp 0.2)) 1e-12))
    (is (identical? (sym/partials '(* (sin x) (exp y))) (sym/partials '(* (sin x) (exp y))))
        "one derivation per surface (memoized)"))
  (testing "the compile-time catalog and the runtime derivation agree"
    (doseq [k surfaces/order
            :let [c (surfaces/surface k) s (sym/surface (:body c))]]
      (is (= (:infix c) (:infix s)) (str k))
      (is (close? ((:fx c) 0.7 -0.4) ((:fx s) 0.7 -0.4) 1e-12) (str k))))
  (testing "a monkey saddle: f_y = -xy"
    (is (= "- x y" (get-in (surfaces/surface :monkey) [:infix :fy])))))

(deftest mixing-is-linear
  (let [a (surfaces/surface :paraboloid) b (surfaces/surface :waves)
        m (surfaces/mix a b 0.25)]
    (is (close? ((:fx m) 0.5 1.0) (+ (* 0.75 ((:fx a) 0.5 1.0)) (* 0.25 ((:fx b) 0.5 1.0))) 1e-12))
    (is (close? ((:fx m) 0.5 1.0) (fd (:f m) 0 0.5 1.0) 1e-6))))

;; ── Geometry ──────────────────────────────────────────────────────────────

(defspec tangent-plane-contains-P-with-the-right-normal 60
  (prop/for-all [k (gen/elements surfaces/order) x coord y coord
                 s (gen/double* {:min -1 :max 1 :NaN? false}) t (gen/double* {:min -1 :max 1 :NaN? false})]
    (let [surf  (surfaces/surface k)
          P     (scene/point-on surf [x y])
          [u v] (scene/tangent-span surf [x y])
          n     (scene/normal surf [x y])
          q     (space/v+ P (space/v+ (space/v* s u) (space/v* t v)))
          [gx gy] (scene/gradient surf [x y])
          want  (space/unit [(- gx) (- gy) 1.0])]
      (and (close? 0.0 (space/dot3 n (space/v- q P)) 1e-9)
           (every? true? (map #(close? %1 %2 1e-12) n want))
           (pos? (nth n 2))
           (close? (nth q 2) (+ (nth P 2) (* s gx) (* t gy)) 1e-9)))))

(deftest slices-lie-on-the-surface-and-touch-their-tangents
  (doseq [k surfaces/order
          :let [{:keys [f] :as surf} (surfaces/surface k)]]
    (testing (str k)
      (is (every? (fn [[x y z]] (and (close? y 0.7 1e-12) (close? z (f x y) 1e-12)))
                  (scene/x-slice surf 0.7 40)))
      (is (every? (fn [[x y z]] (and (close? x -0.3 1e-12) (close? z (f x y) 1e-12)))
                  (scene/y-slice surf -0.3 40)))
      (let [[a b] (scene/tangent-line surf [-0.3 0.7] :x 1.0)
            [ax _ az] a [bx _ bz] b]
        (is (close? (/ (- bz az) (- bx ax)) ((:fx surf) -0.3 0.7) 1e-9) "x tangent slope is f_x"))
      (let [[a b] (scene/tangent-line surf [-0.3 0.7] :y 1.0)]
        (is (close? (/ (- (nth b 2) (nth a 2)) (- (nth b 1) (nth a 1))) ((:fy surf) -0.3 0.7) 1e-9)
            "y tangent slope is f_y")))))

(deftest gradient-ascent-climbs
  (let [surf (surfaces/surface :bump)
        path (scene/ascent-path surf [1.2 -0.8] {})]
    (is (> (count path) 5))
    (is (apply < (map (fn [[x y]] ((:f surf) x y)) path)) "f increases at every step")
    (is (< (apply + (map #(* % %) (peek path))) 0.05) "and reaches the summit at the origin")))

;; ── Controls, drag, readout ──────────────────────────────────────────────

(deftest buttons-step-and-reset
  (let [ps  (merge cd/defaults {:f :bump :x0 1.0 :y0 0.5})
        up  (cd/ascend ps)
        f   (:f (surfaces/surface :bump))]
    (is (> (f (:x0 up) (:y0 up)) (f 1.0 0.5)) "a step of gradient ascent goes uphill")
    (is (= :still (:path up)) "and holds the point still so the step shows")
    (is (= [1.0 0.5 0.0] ((juxt :x0 :y0 :morph) (merge up cd/reset-params))))))

(deftest dragging-on-the-plane-moves-the-point
  (let [ps   (merge cd/defaults {:tour false :path :still})
        f0   (cd/frame-at ps 0.0)
        cam  (space/camera f0)
        floor (cd/floor-z ps)
        at   (fn [w] (:at (space/project cam w)))
        q0   [1.0 0.5 floor]]
    (testing "Q on the floor"
      (let [down (drag/pointer nil cd/controls f0 ps {:type :down :at (at q0) :hit "point"})
            move (drag/pointer (:drag down) cd/controls f0 ps {:type :move :at (at [-0.5 1.25 floor])})]
        (is (= :drag-q (get-in down [:drag :spec :id])) "the press grabs Q, the nearer handle")
        (is (close? -0.5 (:x0 (:params move)) 1e-6))
        (is (close? 1.25 (:y0 (:params move)) 1e-6))))
    (testing "P on the surface: its handle moves in a horizontal plane"
      (let [P    (scene/point-on (cd/surface-at ps 0.0) [1.0 0.5])
            down (drag/pointer nil cd/controls f0 ps {:type :down :at (at P) :hit "point"})
            move (drag/pointer (:drag down) cd/controls f0 ps
                               {:type :move :at (at (space/v+ P [-0.4 0.3 0.0]))})]
        (is (= :drag-p (get-in down [:drag :spec :id])))
        (is (close? 0.6 (:x0 (:params move)) 1e-6))
        (is (close? 0.8 (:y0 (:params move)) 1e-6))))
    (testing "the point stays in the domain"
      (let [down (drag/pointer nil cd/controls f0 ps {:type :down :at (at q0) :hit "point"})
            move (drag/pointer (:drag down) cd/controls f0 ps {:type :move :at (at [9.0 0.5 floor])})]
        (is (= 2.0 (:x0 (:params move))))))))

(defrecord NullHost []
  p/ISurface (-present! [this _] this) (-size [_] [1280 720])
  p/IWindow (-show! [this _] this) (-close! [this] this) (-snapshot! [_ _] nil)
  p/IClock (-on-tick! [this _] this) (-stop-ticks! [this] this))

(deftest the-readout-follows-the-widgets
  (let [pl  (player/open-source! (->NullHost) cd/source-fn
                                 {:controls cd/controls :autoplay? false :params {:tour false}})
        txt #(cd/readout-text (player/params pl) 0.0)
        r0  (txt)]
    (is (str/includes? r0 "df/dx") r0)
    (is (str/includes? r0 "= 0.67") "paraboloid at (1, 0.5): f_x = 2/3")
    (player/control-input! pl :f :saddle)
    (is (not= r0 (txt)) "choosing another surface rewrites the readout")
    (is (str/includes? (txt) "1/3 x² - 1/3 y²") (txt))
    (player/control-input! pl :x0 -1.5)
    (is (str/includes? (txt) "at (-1.50, 0.50)"))
    (player/press! pl :ascend)
    (is (not= -1.5 (:x0 (player/params pl))) "the button moved the point")
    (player/press! pl :reset)
    (is (= 1.0 (:x0 (player/params pl))))
    (player/control-input! pl :morph-to :waves)
    (player/control-input! pl :morph 0.5)
    (is (str/includes? (txt) "(1-m)") "a morph shows both surfaces")))

(deftest fmt-is-host-neutral
  (is (= ["0.00" "1.50" "-0.25" "12.06" "0.05" "0.00"] (map cd/fmt [0 1.5 -0.25 12.057 0.049 -0.001]))))

;; ── Every frame is valid ──────────────────────────────────────────────────

(def ^:private gen-params
  (gen/hash-map :f (gen/elements surfaces/order) :morph-to (gen/elements surfaces/order)
                :x0 coord :y0 coord
                :morph (gen/double* {:min 0 :max 1 :NaN? false})
                :path (gen/elements [:still :circle :ascent])
                :tangent gen/boolean :xslice gen/boolean :yslice gen/boolean
                :gradient gen/boolean :levels gen/boolean :wire gen/boolean
                :auto-morph gen/boolean :fly gen/boolean :tour gen/boolean))

(defspec frames-are-valid-across-params-and-time 40
  (prop/for-all [ps gen-params t (gen/double* {:min 0 :max cd/duration :NaN? false})]
    (let [f (p/-frame-at (cd/source-fn ps) t)]
      (and (space/valid? f)
           (seq (:shapes (space/->2d f)))))))

(deftest the-tour-reveals-in-order
  (let [ids (fn [t] (set (map :id (filter #(pos? (:opacity % 0)) (:shapes (cd/frame-at cd/defaults t))))))]
    (is (not (contains? (ids 1.0) "x-tangent")) "early on: no tangent lines yet")
    (is (contains? (ids 4.5) "x-slice-plane") "the x slicing plane sweeps")
    (is (contains? (ids 7.0) "x-tangent"))
    (is (contains? (ids 12.0) "tangent-plane"))
    (is (not= (:camera (cd/frame-at cd/defaults 3.0)) (:camera (cd/frame-at cd/defaults 9.0)))
        "the camera flies")
    (is (not= (-> (cd/frame-at cd/defaults 12.0) :shapes (->> (filter #(= "point" (:id %)))) first :points)
              (-> (cd/frame-at cd/defaults 16.0) :shapes (->> (filter #(= "point" (:id %)))) first :points))
        "the point travels and the tangent plane follows it")))

;; ── Golden image ──────────────────────────────────────────────────────────

(def ^:private golden-path "test/resources/desargues/player/golden/calculus-t14.png")

(deftest calculus-golden-image
  (let [src (cd/source-fn {:f :waves :x0 0.8 :y0 -0.6})
        img (hl/render-image src 14.0 {:w 640 :h 360})
        res (hl/golden-check img golden-path
                             {:update? (some? (System/getenv "DESARGUES_UPDATE_GOLDEN"))
                              :actual-out "target/golden-actual/calculus-t14.png"})]
    (is (:pass? res) (str "calculus demo at t=14 differs from " golden-path ": " res))))
