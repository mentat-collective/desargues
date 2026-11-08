(ns desargues.wallpaper-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.girih :as g]
            [desargues.wallpaper.loop :as lp]
            [desargues.wallpaper.render :as r]
            [desargues.wallpaper.post :as post]
            [desargues.wallpaper.curves :as cv]
            [desargues.wallpaper.space :as sp])
  (:import (java.awt.image BufferedImage)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

;; ── loop ──────────────────────────────────────────────────────────────────

(deftest phases-exclude-the-wrap
  (is (= [0.0 0.25 0.5 0.75] (lp/phases 4)))
  (is (= 360 (lp/frame-count 30 12)))
  (is (= 1 (lp/frame-count 30 0))))

(deftest motions-are-periodic
  (doseq [[label f] {"wave"   lp/wave
                     "osc"    #(lp/osc % 0.2 0.9)
                     "pulse"  lp/pulse
                     "window" #(lp/window % 0.1 0.6)
                     "circle" #(lp/circle % 2.0 0.3)
                     "shift"  #(lp/shift % 0.37)}]
    (testing label (is (lp/periodic? f)))))

(deftest window-is-a-unit-bump
  (is (zero? (lp/window 0.05 0.1 0.5)))
  (is (zero? (lp/window 0.7 0.1 0.5)))
  (is (< (Math/abs (- 1.0 (lp/window 0.3 0.1 0.5))) 1e-12))
  (is (< (Math/abs (- 1.0 (lp/pulse 0.5))) 1e-12)))

;; ── girih ─────────────────────────────────────────────────────────────────

(defn- round6 [x] (/ (Math/round (* 1e6 (double x))) 1e6))

(deftest khatam-star-is-eightfold
  (let [oct (-> (g/tiling :4.8.8) :cell first :vertices)
        {:keys [strands rosette]} (g/hankin oct (g/khatam-angle))]
    (is (= 8 (count strands)))
    (is (= 16 (count rosette)))
    (testing "every star point sits at one radius"
      (is (= 1 (count (distinct (map #(round6 (g/vlen (second %))) strands))))))
    (testing "every strand starts and ends on an edge midpoint at delta 0"
      (is (= 1 (count (distinct (map #(round6 (g/vlen (first %))) strands))))))))

(deftest two-point-variant-splits-each-midpoint
  (let [hex (-> (g/tiling :hex) :cell first :vertices)
        {:keys [strands]} (g/hankin hex (/ Math/PI 3.0) 0.2)
        [p _ q] (first strands)]
    (is (= 6 (count strands)))
    (is (> (g/vlen (g/v- p q)) 0.0))))

(deftest cover-reaches-every-corner
  (doseq [t (keys g/tilings)]
    (let [rect [-7.0 7.0 -4.0 4.0]
          cs (map :centre (g/cover t rect))
          near (fn [[x y]] (some (fn [[cx cy]] (< (Math/hypot (- cx x) (- cy y)) 2.5)) cs))]
      (testing (name t)
        (is (every? near [[-7.0 -4.0] [7.0 -4.0] [-7.0 4.0] [7.0 4.0] [0.0 0.0]]))))))

(deftest unknown-tiling-is-an-error
  (is (thrown? clojure.lang.ExceptionInfo (g/tiling :penrose))))

(deftest beads-march-one-spacing-per-phase
  (let [sq [[0 0] [1 0] [1 1] [0 1]]]
    (is (= 16 (count (g/beads sq 0.25 0.0 true))))
    (is (= (g/beads sq 0.25 0.0 true) (g/beads sq 0.25 1.0 true)))
    (testing "spacing is fitted so the beads close evenly: perimeter 4 at 0.3 is 13 beads 4/13 apart"
      (let [bs (g/beads sq 0.3 0.0 true)]
        (is (= 13 (count bs)))
        (is (= (round6 (/ 4.0 13)) (round6 (g/vlen (g/v- (second bs) (first bs))))))))
    (is (= [0.5 0.0] (mapv #(round6 %) (second (g/beads sq 0.5 0.0 true)))))))

(deftest space-filling-curves
  (testing "Hilbert order n visits 4^n cells with unit-length steps"
    (let [p (cv/curve :hilbert 4)]
      (is (= 256 (count p)))
      (is (= 1 (count (distinct (map (fn [a b] (round6 (g/vlen (g/v- b a)))) p (rest p))))))))
  (testing "the Moore curve of order n visits 4^(n+1) cells and closes"
    (let [p (cv/curve :moore 3)]
      (is (= (first p) (peek p)))
      (is (= 257 (count p)))))
  (testing "every preset fits [-1, 1] and its arc parameter runs 0 .. 1"
    (doseq [k (keys cv/systems)]
      (let [p (cv/curve k 3)
            s (cv/arc-params p)]
        (is (<= (apply max (map #(Math/abs (double %)) (flatten p))) 1.0000001) (name k))
        (is (= 0.0 (first s)))
        (is (< (Math/abs (- 1.0 (peek s))) 1e-9)))))
  (is (thrown? clojure.lang.ExceptionInfo (cv/curve :sierpinski-carpet 2))))

(deftest post-effects-run-and-scale
  (let [img (r/image 64 36)]
    (.setRGB img 32 18 0xFFFFFF)
    (post/apply-all! [{:type :bloom :radius 30 :strength 1.0}
                      {:type :chroma :shift 60}
                      {:type :dof}
                      {:type :vignette :strength 0.5}
                      {:type :fade :level 0.8}]
                     img)
    (testing "bloom spread the dot; chroma moved red right and blue left"
      (is (pos? (bit-and (.getRGB img 34 18) 0xFF0000)))
      (is (pos? (bit-and (.getRGB img 30 18) 0x0000FF))))
    (is (= 2 (post/px-at img 60)))
    (is (thrown? clojure.lang.ExceptionInfo (post/effect! {:type :sparkle} img)))))

;; ── render ────────────────────────────────────────────────────────────────

(def ^:private probe-spec
  {:id :probe :seconds 1 :fps 4 :size [64 36]
   :field (fn [u] (let [ph (* lp/tau (double u))]
                    (fn ^long [^double x ^double y]
                      (r/rgb (* 0.5 (+ 1.0 (Math/cos (+ x ph)))) 0.2 (* 0.5 (+ 1.0 (Math/sin (+ y ph))))))))
   :frame (fn [u] {:shapes [{:node :path :opacity 1.0 :opts {:closed? true}
                             :points (mapv #(g/v+ (lp/circle u 1.5) %) [[0 0] [1 0] [0 1]])
                             :fill {:color "#d9a441" :opacity 1.0} :stroke {:color nil}}]})
   :light (fn [u] (let [u (double u)]
                    (fn ^double [^double x ^double _y]
                      (+ 0.8 (* 0.4 (lp/wave (lp/frac (- (/ x 14.0) u))))))))})

(defn- pixels [^BufferedImage img]
  (vec (.getRGB img 0 0 (.getWidth img) (.getHeight img) nil 0 (.getWidth img))))

(deftest a-loop-wraps-without-a-seam
  (testing "u = 1 paints u = 0, up to one level per channel of float noise"
    (let [a (pixels (r/render-frame probe-spec 0.0))
          b (pixels (r/render-frame probe-spec 1.0))
          ch (fn [p s] (bit-and (bit-shift-right p s) 0xFF))
          worst (reduce max (for [[p q] (map vector a b) s [0 8 16]]
                              (Math/abs (- (ch p s) (ch q s)))))]
      (is (<= worst 1)))))

(deftest a-loop-moves
  (is (not= (pixels (r/render-frame probe-spec 0.0))
            (pixels (r/render-frame probe-spec 0.5)))))

(deftest supersampling-keeps-the-picture
  (let [a (r/render-frame (assoc probe-spec :frame nil :light nil) 0.2)
        b (r/render-frame (assoc probe-spec :frame nil :light nil :ss 2) 0.2)
        d (map (fn [p q] (Math/abs (- (bit-and p 0xFF) (bit-and q 0xFF)))) (pixels a) (pixels b))]
    (is (< (/ (reduce + d) (count d)) 4.0))))

(deftest field-raster-backends-are-open-by-key
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown backend"
                        (r/field-raster :no-such-engine (r/image 4 4) (fn ^long [^double _ ^double _] 0) {}))))

(deftest ffmpeg-reads-raw-rgb-on-stdin
  (let [args (r/ffmpeg-args :mp4 1920 1080 30 "out.mp4")]
    (is (= "ffmpeg" (first args)))
    (is (some #{"rgb24"} args))
    (is (some #{"1920x1080"} args))
    (is (= "out.mp4" (last args))))
  (is (some #{"libvpx-vp9"} (r/ffmpeg-args :webm 8 8 10 "o.webm"))))

(deftest encode-writes-every-frame
  (when (zero? (:exit (sh/sh "sh" "-c" "command -v ffmpeg")))
    (let [dir (str (Files/createTempDirectory "wallpaper" (make-array FileAttribute 0)))
          png (r/encode! :png probe-spec dir {})
          mp4 (r/encode! :mp4 probe-spec (str dir "/probe.mp4") {})]
      (is (= 4 (count (filter #(.endsWith (.getName ^java.io.File %) ".png") (.listFiles (io/file png))))))
      (is (pos? (.length (io/file mp4)))))))

(def ^:private cam
  (sp/camera {:eye [0 -6 0] :target [0 0 0] :up [0 0 1] :fov 40 :view [16.0 8.0]}))

(deftest projection-math
  (testing "the target sits at the window centre, at its distance"
    (let [[x y d] (sp/project cam [0 0 0])]
      (is (< (Math/abs x) 1e-12)) (is (< (Math/abs y) 1e-12)) (is (< (Math/abs (- d 6.0)) 1e-12))))
  (testing "fov is the vertical field of view: the edge ray lands on the window edge"
    (let [h (* 6.0 (Math/tan (Math/toRadians 20.0)))
          [_ y _] (sp/project cam [0 0 h])]
      (is (< (Math/abs (- y 4.0)) 1e-9))))
  (testing "x right, y up; farther is smaller"
    (let [[x1 _ _] (sp/project cam [1 0 0]) [x2 _ _] (sp/project cam [1 3 0])]
      (is (pos? x1)) (is (< 0 x2 x1))))
  (testing "points behind the near plane have no image"
    (is (nil? (sp/project cam [0 -7 0])))
    (is (nil? (sp/project cam [0 -5.99 0])))))

(deftest near-clipping-cuts-the-segment
  (let [runs (sp/clip-polyline cam [[0 -7 0] [0 0 0]])]
    (is (= 1 (count runs)))
    (is (< (Math/abs (- (:near cam) (get-in runs [0 0 2]))) 1e-9))
    (is (empty? (sp/clip-polyline cam [[0 -8 0] [0 -7 0]])))))

(deftest backface-and-depth-order
  (let [tri [[0 0 0] [1 0 0] [0 0 1]]]
    (is (some? (sp/face-shape cam tri {:cull? true})))
    (is (nil? (sp/face-shape cam (reverse tri) {:cull? true}))))
  (testing "shapes are painted far to near"
    (let [dots (sp/dot-shapes cam [[0 3 0] [0 -2 0] [0 0 0]])
          ds (map :depth (sp/sort-far-to-near dots))
          f (sp/frame dots)]
      (is (= ds (sort > ds)))
      (is (every? #(not (contains? % :depth)) (:shapes f)))
      (is (= [9 6 4] (mapv #(Math/round (* 1.0 (:depth %))) (sp/sort-far-to-near dots)))))))

(deftest depth-cue-is-linear-and-clamped
  (let [cue {:range [4.0 8.0] :alpha [1.0 0.2] :size [1.0 0.5] :fog [0.0 1.0]}]
    (is (= 1.0 (:alpha (sp/depth-cue cue 2.0))))
    (is (< (Math/abs (- 0.6 (:alpha (sp/depth-cue cue 6.0)))) 1e-12))
    (is (< (Math/abs (- 0.5 (:size (sp/depth-cue cue 99.0)))) 1e-12))
    (is (= {:t 0.0 :alpha 1.0 :size 1.0 :fog 0.0} (sp/depth-cue nil 5.0))))
  (testing "a cued far dot is dimmer and smaller than a near one"
    (let [cue {:range [4.0 8.0] :alpha [1.0 0.2] :size [1.0 0.5]}
          [far near'] (sp/sort-far-to-near (sp/dot-shapes cam [[0 2 0] [0 -2 0]] {:cue cue}))]
      (is (< (:opacity far) (:opacity near')))
      (is (< (get-in far [:opts :radius]) (get-in near' [:opts :radius]))))))

(deftest space-shapes-render
  (let [spec {:id :s :seconds 1 :fps 4 :size [128 72] :view [16.0 8.0]
              :frame (fn [u]
                       (sp/frame (concat (sp/dot-shapes cam [[0 0 0] [1 0 1]] {:radius 0.2})
                                         (sp/segment-shapes cam [[-2 0 0] [2 0 0]] {:width 6})
                                         (sp/path-shapes cam [[-2 0 1] [2 0 1]] {:width 6}))))}
        img (r/render-frame spec 0.0)]
    (is (pos? (bit-and 0xFFFFFF (.getRGB img 64 36))))))

(def ^:private blur-spec
  {:id :blur :seconds 1 :fps 8 :size [96 54] :view [16.0 8.0]
   :splat (fn [u] [[(* 4.0 (Math/cos (* lp/tau (double u)))) 0.0 0.25 1.0 0.6 0.3]])
   :blur {:samples 4 :shutter 1.0}})

(deftest motion-blur-preserves-the-seam
  (is (= (pixels (r/render-frame blur-spec 0.0)) (pixels (r/render-frame blur-spec 1.0))))
  (testing "the last frame blurs forward through the wrap, so it equals phase 7/8 + samples across 0"
    (let [last-u (/ 7.0 8.0)
          manual (r/render-frame (assoc blur-spec :blur {:samples 1}) 0.0)]
      (is (not= (pixels (r/render-frame blur-spec last-u)) (pixels manual))))))

(deftest motion-blur-spreads-light
  (let [sharp (r/render-frame (dissoc blur-spec :blur) 0.0)
        soft  (r/render-frame blur-spec 0.0)
        lit   (fn [img] (count (filter #(pos? (bit-and % 0xFF0000)) (pixels img))))]
    (is (> (lit soft) (lit sharp)))
    (testing "one sample with any shutter is the unblurred frame"
      (is (= (pixels sharp)
             (pixels (r/render-frame (assoc blur-spec :blur {:samples 1 :shutter 3.0}) 0.0)))))))

(deftest splat-adds-and-tone-maps
  (let [rows (repeat 50 [0.0 0.0 0.3 1.0 1.0 1.0])
        one  (r/render-frame {:size [64 36] :view [16.0 8.0] :splat (fn [_] [[0.0 0.0 0.3 1.0 1.0 1.0]])} 0.0)
        many (r/render-frame {:size [64 36] :view [16.0 8.0] :splat (fn [_] rows)} 0.0)
        g    (fn [img] (bit-and (.getRGB img 32 18) 0xFF))]
    (is (< 0 (g one) (g many) 256))
    (is (= 0 (bit-and (.getRGB one 0 0) 0xFFFFFF)))))
