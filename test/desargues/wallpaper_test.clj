(ns desargues.wallpaper-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.girih :as g]
            [desargues.wallpaper.loop :as lp]
            [desargues.wallpaper.render :as r])
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
