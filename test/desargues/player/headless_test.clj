(ns desargues.player.headless-test
  "The headless host (ISurface only): every node kind paints, pixel spot
   checks pin the shared units and colour rules, frame sequences, and a
   golden image of the pendulum demo at t = 3.

   The pendulum construct lives in studio/ (desargues.studio.demos), which
   is not on the :test classpath. It depends only on src/ (scene, physics),
   so it is loaded from its file rather than adding studio/ (and JavaFX) to
   the test alias.

   Refresh the golden after an intended visual change:
     (golden-check img golden-path {:update? true})"
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.player :as player]
            [desargues.player.frame :as frame]
            [desargues.player.graph-source :as gs]
            [desargues.player.headless :as hl]
            [desargues.player.protocols :as p]
            [desargues.player.svg-path :as svg-path])
  (:import [java.awt.image BufferedImage]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private W 640)
(def ^:private H 360)
(def ^:private vp (frame/viewport W H))

(defn- argb [^BufferedImage img [x y]]
  (let [v (.getRGB img (int (Math/round (double x))) (int (Math/round (double y))))]
    [(bit-and (bit-shift-right v 24) 255) (bit-and (bit-shift-right v 16) 255)
     (bit-and (bit-shift-right v 8) 255) (bit-and v 255)]))

(defn- rgb [img xy] (subvec (argb img xy) 1))

(defn- close? [a b tol] (every? #(<= (Math/abs (long %)) tol) (map - a b)))

(defn- hex->rgb [k] (let [c (hl/css-color k)] [(.getRed c) (.getGreen c) (.getBlue c)]))

(defn- shown [shape] (merge {:opacity 1.0 :draw 1.0} shape))

(defn- paint [& shapes]
  (hl/frame->image (frame/frame 0.0 (map shown shapes)) W H))

(defn- px [xy] (frame/->px vp xy))

(defn- tmp-dir []
  (str (Files/createTempDirectory "headless-test" (make-array FileAttribute 0))))

;; ── Every node kind ────────────────────────────────────────────────────────

(def ^:private one-of-each
  [{:id 1 :node :text :text "Hello" :at [0 3] :opts {:color :white}}
   {:id 2 :node :tex :tex "e^{i\\pi}+1=0" :at [-4 2] :opts {:color :gold}}
   {:id 3 :node :circle :at [-4 -2] :opts {:radius 1.0 :color :blue}
    :fill {:color :blue :opacity 0.5}}
   {:id 4 :node :dot :at [0 0] :opts {:color :gold}}
   {:id 5 :node :rectangle :at [4 2] :opts {:width 2 :height 1 :color :green}}
   {:id 6 :node :rounded-rectangle :at [4 -2] :opts {:width 2 :height 1 :color :red}
    :fill {:color "#8FC7E8" :opacity 1.0}}
   {:id 7 :node :decimal :value 3.14159 :at [0 -3] :opts {:num-decimal-places 3}}
   {:id 8 :node :line :from [-6 -3.5] :to [6 -3.5] :opts {:color :grey} :dash [0.2 0.1]}
   {:id 9 :node :path :points [[-1 1] [0 2] [1 1]] :opts {:color :teal :closed? true}
    :fill {:color :teal :opacity 0.3}}
   {:id 10 :node :svg-path :d "M 1 -1 C 2 0 2 -2 3 -1 A 0.5 0.5 0 0 1 2 -1.5 Z"
    :stroke {:color "#FFAA00" :width 2} :fill {:color "rgb(40, 80, 120)"} :dash "0.05,0.03"}])

(deftest every-node-kind-paints
  (testing "the fixture covers every kind a Frame can carry"
    (is (= frame/shape-kinds (set (map :node one-of-each)))))
  (doseq [s one-of-each]
    (testing (str (:node s) " paints alone, leaving pixels off the background")
      (let [img (paint s)]
        (is (some (fn [[x y]] (not= [0 0 0] (rgb img [x y])))
                  (for [x (range 0 W 2) y (range 0 H 2)] [x y]))))))
  (testing "all together through the ISurface port"
    (let [surf (hl/surface W H)]
      (is (identical? surf (p/-present! surf (frame/frame 0.0 (map shown one-of-each)))))
      (is (= [W H] (p/-size surf))))))

(deftest unknown-kinds-and-hidden-shapes-paint-nothing
  (let [img (paint {:id 1 :node :hologram :at [0 0]})]
    (is (= [0 0 0] (rgb img (px [0 0])))))
  (let [img (hl/frame->image (frame/frame 0.0 [{:id 1 :node :dot :at [0 0] :opacity 0.0}]) W H)]
    (is (= [0 0 0] (rgb img (px [0 0]))))))

;; ── Pixel spot checks ──────────────────────────────────────────────────────

(deftest a-dot-centre-has-its-colour
  (testing "palette keyword"
    (is (= (hex->rgb :gold) (rgb (paint {:id 1 :node :dot :at [1 1] :opts {:color :gold}})
                                 (px [1 1])))))
  (testing "CSS colour string"
    (is (= [143 199 232] (rgb (paint {:id 1 :node :dot :at [-2 0.5] :opts {:color "#8FC7E8"}})
                              (px [-2 0.5]))))))

(defspec dots-land-where-the-viewport-says 30
  (prop/for-all [x (gen/double* {:min -6.5 :max 6.5 :NaN? false})
                 y (gen/double* {:min -3.6 :max 3.6 :NaN? false})]
    (= (hex->rgb :red)
       (rgb (paint {:id 1 :node :dot :at [x y] :opts {:color :red :radius 0.15}}) (px [x y])))))

(deftest opacity-blends-over-the-background
  (let [img (hl/frame->image
             (frame/frame 0.0 [{:id 1 :node :dot :at [0 0] :opacity 0.5 :opts {:color :white}}])
             W H)]
    (is (close? [128 128 128] (rgb img (px [0 0])) 2))))

(deftest frame-background
  (is (= [0 0 0] (rgb (paint) [5 5])) "black by default")
  (is (= (hex->rgb :blue)
         (rgb (hl/frame->image {:t 0 :shapes [] :background :blue} W H) [5 5])))
  (is (= [16 32 48]
         (rgb (hl/frame->image {:t 0 :shapes [] :background "#102030"} W H) [5 5]))))

(deftest stroke-color-nil-means-no-outline
  (let [s   {:id 1 :node :rectangle :at [0 0] :opts {:width 4 :height 2}
             :fill {:color :green :opacity 1.0}}
        lft (px [-2 0])]
    (testing "an outline is white by default"
      (is (= [255 255 255] (rgb (paint (assoc s :stroke {:width 8})) lft))))
    (testing ":stroke {:color nil} leaves only the fill (or the background) there"
      (let [c (rgb (paint (assoc s :stroke {:color nil :width 8})) lft)]
        (is (not= [255 255 255] c))
        (is (or (close? c (hex->rgb :green) 70) (close? c [0 0 0] 70)))))))

(deftest svg-path-is-in-scene-units-y-up
  (let [img (paint {:id 1 :node :svg-path :d "M 1 1 L 3 1 L 3 3 L 1 3 Z"
                    :stroke {:color nil} :fill {:color :gold :opacity 1.0}})]
    (is (= (hex->rgb :gold) (rgb img (px [2 2]))) "filled where y is up")
    (is (= [0 0 0] (rgb img (px [2 -2]))) "nothing at the y-down mirror")
    (is (= [0 0 0] (rgb img (px [0.5 2]))) "and nothing outside the square")))

(deftest svg-path-arcs-and-relative-commands
  (testing "a relative circle of two arcs fills its centre"
    (let [img (paint {:id 1 :node :svg-path :d "M -1 0 a 1 1 0 0 0 2 0 a 1 1 0 0 0 -2 0 z"
                      :stroke {:color nil} :fill {:color :red}})]
      (is (= (hex->rgb :red) (rgb img (px [0 0]))))
      (is (= (hex->rgb :red) (rgb img (px [0 0.9]))))
      (is (= [0 0 0] (rgb img (px [0.8 0.8]))) "outside the unit circle"))))

(deftest svg-path-segments
  (is (= [[:move 0.0 0.0] [:line 1.0 0.0] [:line 1.0 2.0] [:line 3.0 2.0] [:close]]
         (svg-path/segments "M0 0 h1 v2 L3 2 Z")))
  (is (= [[:move 1.0 1.0] [:line 2.0 2.0] [:line 3.0 3.0]]
         (svg-path/segments "m1 1 1 1 1 1")) "repeated moveto is lineto")
  (is (= [:cubic 4.0 4.0 4.0 4.0 5.0 5.0]
         (nth (svg-path/segments "M0 0 C 1 1 2 2 3 3 S 4 4 5 5") 2)) "S reflects")
  (let [arc (svg-path/segments "M 1 0 A 1 1 0 0 1 -1 0")
        [_ & cubics] arc]
    (is (= 2 (count cubics)) "a half turn is two quarter cubics")
    (is (close? [-1 0] (map #(Math/round (double %)) (take-last 2 (last cubics))) 0))))

(deftest text-anchor-and-baseline
  (let [s     {:id 1 :node :text :text "MMMMMM" :at [0 0] :opts {:font-size 48}}
        lit?  (fn [img xy] (not= [0 0 0] (rgb img xy)))
        cols  (fn [img] (filter (fn [x] (some #(lit? img [x %]) (range H))) (range W)))
        rows  (fn [img] (filter (fn [y] (some #(lit? img [% y]) (range W))) (range H)))
        [cx cy] (px [0 0])]
    (testing "anchor start draws right of :at, end left of it, middle across it"
      (is (>= (apply min (cols (paint (assoc-in s [:opts :anchor] "start")))) (- cx 2)))
      (is (<= (apply max (cols (paint (assoc-in s [:opts :anchor] "end")))) (+ cx 2)))
      (let [cs (cols (paint s))] (is (< (apply min cs) cx (apply max cs)))))
    (testing "SVG text (:opts :size) sits on its baseline, plain text is centred"
      (is (<= (apply max (rows (paint (assoc-in s [:opts :size] 1)))) (+ cy 2)))
      (let [rs (rows (paint s))] (is (< (apply min rs) cy (apply max rs)))))))

(deftest dashes-leave-gaps
  (let [line {:id 1 :node :line :from [-6 0] :to [6 0] :opts {:color :white :width 8}}
        lit  (fn [img] (count (filter #(= [255 255 255] (rgb img [% (second (px [0 0]))]))
                                      (range W))))
        solid (lit (paint line))]
    (is (< (lit (paint (assoc line :dash [0.3 0.3]))) (* 0.7 solid)))
    (is (< (lit (paint (assoc line :dash "0.3,0.3"))) (* 0.7 solid)))))

(deftest draw-progress-trims-lines
  (let [line {:id 1 :node :line :from [-4 0] :to [4 0] :opts {:color :white :width 12}}
        img  (paint (assoc line :draw 0.5))]
    (is (= [255 255 255] (rgb img (px [-2 0]))))
    (is (= [0 0 0] (rgb img (px [2 0]))))))

(deftest tex-typesets-when-jlatexmath-is-present
  (let [img (paint {:id 1 :node :tex :tex "\\frac{a}{b}" :at [0 0] :opts {:color :white}})]
    (is (some #(not= [0 0 0] (rgb img %))
              (for [x (range (- (first (px [0 0])) 40) (+ (first (px [0 0])) 40))
                    y (range (- (second (px [0 0])) 40) (+ (second (px [0 0])) 40))]
                [x y])))))

(deftest css-colors
  (is (= [255 0 0] (hex->rgb "rgb(255, 0, 0)")))
  (is (= [17 34 51] (hex->rgb "#123")))
  (is (= [255 165 0] (hex->rgb "orange")))
  (is (= [255 255 255] (hex->rgb :no-such-colour)) "unknown is white, as in JavaFX"))

;; ── Rendering a source ─────────────────────────────────────────────────────

(def ^:private still
  (reify p/ISource
    (-duration [_] 1.0)
    (-frame-at [_ t] (frame/frame t [{:id 1 :node :dot :at [(* 4 t) 0] :opacity 1.0
                                      :opts {:color :gold}}]))))

(deftest render-png-writes-the-frame-at-t
  (let [path (str (tmp-dir) "/still.png")]
    (is (= path (hl/render-png still 0.5 path {:w W :h H})))
    (let [img (hl/read-png path)]
      (is (= [W H] [(.getWidth img) (.getHeight img)]))
      (is (= (hex->rgb :gold) (rgb img (px [2 0])))))))

(deftest render-sequence-samples-the-whole-duration
  (let [dir   (tmp-dir)
        paths (hl/render-sequence still 4 dir {:w 160 :h 90})]
    (is (= 5 (count paths)) "t = 0, 0.25, 0.5, 0.75, 1")
    (is (every? #(.exists (io/file %)) paths))
    (is (= "frame-00004.png" (.getName (io/file (last paths)))))))

(deftest image-diff-and-golden-check
  (let [a (paint {:id 1 :node :dot :at [0 0]})
        b (paint {:id 1 :node :dot :at [3 0]})
        c (paint {:id 1 :node :circle :at [0 0] :opts {:radius 3} :fill {:color :red}})]
    (is (zero? (:mismatched (hl/image-diff a a 0))))
    (is (pos? (:mismatched (hl/image-diff a b 0))))
    (is (false? (:same-size? (hl/image-diff a (hl/frame->image (frame/frame 0 []) 10 10) 0))))
    (let [g (str (tmp-dir) "/g.png")]
      (is (:missing? (hl/golden-check a g {})))
      (is (:updated? (hl/golden-check a g {:update? true})))
      (is (:pass? (hl/golden-check a g {})))
      (testing "a small change stays under the default 1% budget, a big one fails"
        (is (:pass? (hl/golden-check b g {})))
        (is (not (:pass? (hl/golden-check b g {:max-fraction 0.0}))))
        (is (not (:pass? (hl/golden-check c g {}))))))))

;; ── Golden image: the pendulum demo at t = 3 ───────────────────────────────

(def ^:private golden-path "test/resources/desargues/player/golden/pendulum-t3.png")

(defn- pendulum-source []
  (load-file "studio/desargues/studio/demos.clj")
  (gs/graph-source (player/record "Damped pendulum"
                                  ((resolve 'desargues.studio.demos/pendulum) {}))))

(deftest pendulum-golden-image
  (let [img (hl/render-image (pendulum-source) 3.0 {:w W :h H})
        res (hl/golden-check img golden-path
                             {:actual-out "target/golden-actual/pendulum-t3.png"})]
    (is (:pass? res) (str "pendulum at t=3 differs from " golden-path ": " res
                          " (actual written to target/golden-actual/)"))))
