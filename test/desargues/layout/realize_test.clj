(ns desargues.layout.realize-test
  "Python-guarded smoke test for the Manim layout backend
   (desargues.layout.realize). Realizes a small row/column tree, asserts the
   resulting VGroup fits the frame, and renders it at low quality so CI covers
   the backend end to end. Skips (loudly) when no Manim env is configured."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [libpython-clj2.python :as py]
            [desargues.config :as config]
            [desargues.layout.core :as l]
            [desargues.layout.realize :as realize]
            [desargues.scene :as scene]))

(def manim-env?
  "True when the resolved python can import manim (desargues.config/manim-ready?)."
  (delay (config/manim-ready?)))

(use-fixtures :once
  (fn [f]
    (if @manim-env?
      (scene/init!)
      (println "[realize-test] no Manim env (set CONDA_PREFIX/DESARGUES_CONDA_PREFIX); skipping"))
    (f)))

(def tree
  (l/column {:width :fill :height :fill :padding 0.5 :spacing 0.3 :align :center-x}
    (l/text "Realize smoke" {:font-size 36 :color :white})
    (l/row {:width :fill :height [:portion 1] :spacing 0.4 :align :center-y}
      (l/box {:width [:portion 1] :height :fill :bg :gray :bg-opacity 0.2
              :border [:teal 2] :padding 0.2 :align #{:center-x :center-y}}
        (l/math "x^{2} + y^{2}" {:font-size 40}))
      (l/box {:width [:portion 1] :height :fill :bg :gray :bg-opacity 0.2
              :border [:gold 2] :padding 0.2 :align #{:center-x :center-y}}
        (l/text "bounded" {:font-size 30})))))

(defn- py-double [obj attr]
  (double (py/->jvm (py/get-attr obj attr))))

(deftest realized-vgroup-fits-frame
  (if @manim-env?
    (let [[fw fh] (realize/read-frame)
          L       (realize/realize tree)
          vg      (:vgroup L)]
      (testing "the realized VGroup is bounded by the frame"
        (is (<= (py-double vg "width")  (+ fw 1e-6)))
        (is (<= (py-double vg "height") (+ fh 1e-6))))
      (testing "the placed tree carries a box for every node"
        (is (some? (get (:tree L) :desargues.layout/box)))))
    (is true "skipped: no Manim env")))

(deftest renders-at-low-quality
  (if @manim-env?
    (let [scene-name "RealizeSmoke"]
      (scene/configure! {:quality "low_quality"})
      (scene/render-layout! scene-name tree :hold 0.5)
      (let [mp4s (->> (file-seq (io/file "media" "videos"))
                      (filter #(= (str scene-name ".mp4") (.getName ^java.io.File %))))]
        (is (seq mp4s) "RealizeSmoke.mp4 was written under media/videos")))
    (is true "skipped: no Manim env")))
