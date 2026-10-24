(ns desargues.manim.patches.native-display-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [libpython-clj2.python :as py]
            [malli.core :as m]
            [desargues.config :as config]
            [desargues.manim.core :as core]
            [desargues.manim.patch-protocols :as pp]
            [desargues.manim.patches :as mp]
            [desargues.manim.patches.native-display :as nd]))

(def manim-env? (delay (config/manim-ready?)))

;; ---------------------------------------------------------------------------
;; Pure: no Manim env, no library
;; ---------------------------------------------------------------------------

(deftest constructing-needs-no-manim
  (let [p (nd/native-display "/nonexistent/libdesargues_path.so")]
    (is (= :native-display (pp/-patch-id p)))
    (is (= "/nonexistent/libdesargues_path.so" (:lib-path p))))
  (is (instance? desargues.manim.patches.native_display.NativeDisplay (nd/native-display))
      "the default lib path resolves without a Manim config"))

(deftest missing-library-installs-nothing
  (is (false? (mp/install! (nd/native-display "/nonexistent/libdesargues_path.so")))))

(deftest shim-is-on-the-classpath
  (is (some? (io/resource nd/shim-resource))))

(deftest path-builder-selection
  (is (= [:native-cairo-path :native-display]
         (map pp/-patch-id (mp/path-builder-patches "native-display"))))
  (is (= [:native-cairo-path :native-display]
         (map pp/-patch-id (mp/path-builder-patches " Native-Display ")))))

(deftest stats-schema
  (is (m/validate nd/Stats {:native 3 :fallback 0 :batches 1}))
  (is (not (m/validate nd/Stats {:native -1 :fallback 0 :batches 1}))))

;; ---------------------------------------------------------------------------
;; Against Manim (needs the Manim env and `bb native:path`)
;; ---------------------------------------------------------------------------

(def ^:private probe-src
  "import hashlib, numpy as np
from manim import *
from manim.camera.camera import Camera

def _family():
    s = Square(side_length=2.0, fill_opacity=0.5).set_color_by_gradient('#FF0000', '#00FF88', '#2244FF')
    c = Circle(radius=1.2, color=YELLOW, fill_opacity=0.3).set_stroke(BLUE, 8, background=True).shift(LEFT * 3)
    l = Line(LEFT, RIGHT * 2, stroke_width=12, cap_style=CapStyleType.ROUND).shift(UP * 2)
    p = Polygon([0, 0, 0], [1, 2, 0], [2, 0, 0], joint_type=LineJointType.BEVEL, stroke_width=10).shift(RIGHT * 3)
    t = Text('sheen', gradient=(RED, BLUE)).set_sheen(0.4, DR).shift(DOWN * 2)
    st = Star(color=GREEN, fill_opacity=1).set_sheen(-0.5, UL).shift(DOWN * 2 + LEFT * 3)
    m = MathTex(r'x^2+\\\\frac{1}{2}').shift(UP * 3 + LEFT * 2)
    bad = VMobject(fill_opacity=1)
    bad.set_points(np.array([[0, 0, 0], [np.nan, 1, 0], [1, 1, 0], [2, 2, 0]], dtype=float))
    dash = DashedLine(LEFT * 3, RIGHT * 3).shift(DOWN * 3)
    class Odd(VMobject):
        def get_stroke_width(self, background=False):
            return 30.0
    odd = Odd().set_points_as_corners([[-1, -1, 0], [1, 1, 0]])
    return [s, c, l, p, t, st, m, bad, dash, VMobject(), odd]

def desargues_display_probe(opacity):
    old = config.background_opacity
    config.background_opacity = opacity
    try:
        cam = Camera()
        cam.capture_mobjects(_family())
        return hashlib.sha256(cam.pixel_array.tobytes()).hexdigest()
    finally:
        config.background_opacity = old
")

(deftest native-display-matches-manim
  (let [p    (nd/native-display)
        path (mp/native-cairo-path)
        bgra mp/camera-bgra-pre]
    (cond
      (not @manim-env?) (println "skip: no Manim env")
      (not (mp/native-path-available? (:lib-path p))) (println "skip: native library not built (bb native:path)")
      :else
      (do
        (core/init!)
        (let [g     (:globals (py/run-simple-string probe-src))
              frame #(py/->jvm ((get g "desargues_display_probe") %))
              both  #(vector (frame 1.0) (frame 0.0))
              _     (run! mp/uninstall! [p path bgra])
              stock (both)]
          (try
            (testing "install cycle"
              (is (true? (mp/install! p)))
              (is (true? (mp/install! p)) "idempotent")
              (is (mp/installed? p)))
            (testing "same frame bytes as Manim: gradients, sheen, background stroke, caps, joins"
              (let [before (nd/stats)]
                (is (= stock (both)))
                (let [after (nd/stats)]
                  (is (< (:native before) (:native after)) "drawn natively")
                  (is (< (:fallback before) (:fallback after)) "the overriding class fell back"))))
            (testing "with the native path builder too"
              (mp/install! path)
              (is (= stock (both))))
            (testing "with camera-bgra-pre: the same as Manim under that patch"
              (mp/uninstall! p)
              (mp/install! bgra)
              (let [bgra-stock (both)]
                (mp/install! p)
                (is (= bgra-stock (both))))
              (mp/uninstall! bgra))
            (testing "uninstall restores"
              (is (true? (mp/uninstall! p)))
              (is (false? (mp/uninstall! p)))
              (is (not (mp/installed? p)))
              (mp/uninstall! path)
              (is (= stock (both))))
            (testing "install-path-builder! swaps builders"
              (is (= [:native-cairo-path :native-display]
                     (:installed (mp/install-path-builder! "native-display"))))
              (is (= [:native-cairo-path] (:installed (mp/install-path-builder! "native"))))
              (is (not (mp/installed? p)))
              (is (= [] (:installed (mp/install-path-builder! "manim"))))
              (is (not (mp/installed? path))))
            (finally (run! mp/uninstall! [p path bgra]))))))))
