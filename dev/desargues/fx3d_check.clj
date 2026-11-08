(ns desargues.fx3d-check
  "Pixel check of the JavaFX 3D host (needs a display and the :studio alias):

     clojure -M:studio:run-fx3d-check   ; or
     clojure -M:studio -m desargues.fx3d-check out.png [lift]

   Opens the native 3D host on the saddle-and-spiral source, adds a 2D
   magenta square at the centre (over the surface), snapshots the window and
   counts magenta pixels (the 2D overlay must be ON TOP of the 3D view) and
   gold pixels (the :curve3 lying on the :surface3 must be visible). `lift`
   overrides javafx3d/line-lift, 0 reproducing the old behaviour."
  (:require [desargues.player.javafx3d :as fx3]
            [desargues.player.protocols :as p]
            [desargues.player.space-demos :as sd])
  (:import [java.io File]
           [javax.imageio ImageIO]))

(def probe
  {:id "probe" :node :rectangle :at [0 0] :opacity 1.0 :draw 1.0
   :fill {:color "#FF00FF" :opacity 1.0} :stroke {:color "#FF00FF"}
   :opts {:width 0.6 :height 0.6}})

(defn- channel [c shift] (bit-and (bit-shift-right (int c) shift) 255))

(defn counts
  "{:magenta n :gold n} pixel counts of the PNG at `path`."
  [path]
  (let [im (ImageIO/read (File. (str path)))
        px (for [y (range (.getHeight im)) x (range (.getWidth im))] (.getRGB im x y))]
    {:magenta (count (filter #(and (> (channel % 16) 200) (< (channel % 8) 60) (> (channel % 0) 200)) px))
     :gold    (count (filter #(and (> (channel % 16) 200) (> (channel % 8) 140) (< (channel % 0) 90)) px))}))

(defn- screen-capture!
  "Write what is ON SCREEN over the window's canvas to `path` (the JavaFX
   Robot reads the real framebuffer, which Scene.snapshot does not)."
  [h path]
  (let [canvas (:canvas @(:state (:inner h)))
        out    (promise)]
    (javafx.application.Platform/runLater
     (fn []
       (deliver out
                (try
                  (let [b   (.localToScreen ^javafx.scene.Node canvas
                                            (.getBoundsInLocal ^javafx.scene.Node canvas))
                        ^javafx.scene.image.Image img
                        (.getScreenCapture (javafx.scene.robot.Robot.) nil
                                           (javafx.geometry.Rectangle2D. (.getMinX b) (.getMinY b)
                                                                         (.getWidth b) (.getHeight b)))
                        w  (int (.getWidth img)) hh (int (.getHeight img))
                        rd (.getPixelReader img)
                        bi (java.awt.image.BufferedImage. w hh java.awt.image.BufferedImage/TYPE_INT_ARGB)]
                    (dotimes [y hh] (dotimes [x w] (.setRGB bi x y (.getArgb rd x y))))
                    bi)
                  (catch Throwable t t)))))
    (let [v @out]
      (if (instance? Throwable v) (throw v) (ImageIO/write ^java.awt.image.BufferedImage v "png" (File. (str path)))))))

(defn -main [& [out lift]]
  (let [out (or out "/tmp/fx3d-check.png")]
    (when lift
      (some-> (resolve 'desargues.player.javafx3d/line-lift)
              (alter-var-root (constantly (Double/parseDouble lift)))))
    (let [h (fx3/host)]
      (p/-show! h {:title "fx3d check" :w 960 :h 540})
      (Thread/sleep 1500)
      (p/-present-space! h (update (p/-frame-at (sd/source-fn sd/defaults) 3.9) :shapes conj probe))
      (Thread/sleep 1500)
      (p/-snapshot! h out)
      (println "snapshot" out (counts out))
      (try (screen-capture! h (str out ".screen.png"))
           (println "screen  " (str out ".screen.png") (counts (str out ".screen.png")))
           (catch Throwable t (println "screen capture unavailable:" (.getMessage t))))
      (System/exit 0))))
