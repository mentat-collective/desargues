(ns desargues.manim.patches-test
  (:require [clojure.test :refer [deftest is testing]]
            [libpython-clj2.python :as py]
            [desargues.config :as config]
            [desargues.manim.core :as core]
            [desargues.manim.patch-protocols :as pp]
            [desargues.manim.patches :as mp]))

(def manim-env? (delay (config/manim-ready?)))

;; ---------------------------------------------------------------------------
;; with-patches* over a stub port (pure)
;; ---------------------------------------------------------------------------

(defrecord StubPatch [id log state]
  pp/IManimPatch
  (-patch-id [_] id)
  (-install! [_] (swap! log conj [:install id]) (reset! state true))
  (-uninstall! [_] (swap! log conj [:uninstall id]) (let [was @state] (reset! state false) was))
  (-installed? [_] @state))

(defn- stub [id log installed?] (->StubPatch id log (atom installed?)))

(deftest with-patches-installs-then-restores-in-reverse
  (let [log (atom [])
        a   (stub :a log false)
        b   (stub :b log false)
        seen (mp/with-patches* [a b] (fn [] [(mp/installed? a) (mp/installed? b)]))]
    (is (= [true true] seen))
    (is (= [[:install :a] [:install :b] [:uninstall :b] [:uninstall :a]] @log))
    (is (not (mp/installed? a)))))

(deftest with-patches-leaves-preinstalled-patches-alone
  (let [log (atom [])
        a   (stub :a log true)]
    (mp/with-patches* [a] (constantly nil))
    (is (= [] @log))
    (is (mp/installed? a))))

(deftest with-patches-restores-on-throw
  (let [log (atom [])
        a   (stub :a log false)]
    (is (thrown? clojure.lang.ExceptionInfo
                 (mp/with-patches* [a] #(throw (ex-info "boom" {})))))
    (is (not (mp/installed? a)))))

;; ---------------------------------------------------------------------------
;; The Python shim (needs the Manim env)
;; ---------------------------------------------------------------------------

(def ^:private probe-src
  "from manim import Text, VGroup
import numpy as np
def desargues_copy_probe():
    t = VGroup(Text('ab'), Text('c'))
    c = t.copy()
    glyphs = [m for m in t.get_family() if hasattr(m, 'path_obj')]
    copies = [m for m in c.get_family() if hasattr(m, 'path_obj')]
    shared = all(a.path_obj is b.path_obj for a, b in zip(glyphs, copies))
    same_points = all(np.array_equal(a.points, b.points) for a, b in zip(glyphs, copies))
    distinct = all(a is not b and a.points is not b.points for a, b in zip(glyphs, copies))
    c.shift([1, 0, 0])
    independent = not np.array_equal(t.get_center(), c.get_center())
    return [len(glyphs) > 0, shared, same_points, distinct, independent]
")

(deftest svg-path-shared-copy-install-cycle
  (if-not @manim-env?
    (println "skip: no Manim env")
    (do
      (core/init!)
      (let [probe (get (:globals (py/run-simple-string probe-src)) "desargues_copy_probe")
            p     mp/svg-path-shared-copy]
        (mp/uninstall! p)
        (testing "unpatched: deepcopy clones the path"
          (is (= [true false true true true] (py/->jvm (probe)))))
        (testing "patched: path shared, points equal but owned, copies independent"
          (is (true? (mp/install! p)))
          (is (true? (mp/install! p)) "idempotent")
          (is (mp/installed? p))
          (is (= [true true true true true] (py/->jvm (probe)))))
        (testing "uninstall restores"
          (is (true? (mp/uninstall! p)))
          (is (false? (mp/uninstall! p)))
          (is (not (mp/installed? p)))
          (is (= [true false true true true] (py/->jvm (probe)))))
        (is (= :svg-path-shared-copy (pp/-patch-id p)))))))

(def ^:private glyph-probe-src
  "from manim import Text, MathTex, Tex, config
import numpy as np
def _pts(m):
    return m.get_all_points()
def desargues_glyph_probe():
    out = {}
    a = Text('glyph probe', font_size=30)
    b = Text('glyph probe', font_size=30)
    out['hit-equal'] = bool(np.array_equal(_pts(a), _pts(b)) and len(a) == len(b))
    out['distinct'] = a is not b and a.submobjects[0] is not b.submobjects[0]
    b.shift([1, 0, 0]); b.set_color('#FF0000'); b.submobjects[0].points[:] = 0
    c = Text('glyph probe', font_size=30)
    out['independent'] = bool(np.array_equal(_pts(a), _pts(c)) and a.get_color() == c.get_color())
    d = Text('glyph probe', font_size=31)
    out['kwargs-miss'] = not np.array_equal(_pts(a), _pts(d))
    m1 = MathTex('x^2'); m2 = MathTex('x^2'); t = Tex('x')
    out['mathtex-equal'] = bool(np.array_equal(_pts(m1), _pts(m2)))
    out['tex-type'] = type(t).__name__
    return out
def desargues_glyph_with_pixel_width(w, f):
    old = config.pixel_width
    config.pixel_width = w
    try:
        return f()
    finally:
        config.pixel_width = old
def desargues_glyph_build():
    Text('glyph probe', font_size=30)
    return True
")

(deftest glyph-cache-install-cycle
  (if-not @manim-env?
    (println "skip: no Manim env")
    (do
      (core/init!)
      (let [g     (:globals (py/run-simple-string glyph-probe-src))
            probe #(update-keys (py/->jvm ((get g "desargues_glyph_probe"))) keyword)
            p     (mp/glyph-cache 8)]
        (mp/uninstall! p)
        (mp/glyph-cache-clear!)
        (testing "patched: hits are equal, distinct and independent copies"
          (is (true? (mp/install! p)))
          (is (true? (mp/install! p)) "idempotent")
          (is (mp/installed? p))
          (is (= {:hit-equal true :distinct true :independent true :kwargs-miss true
                  :mathtex-equal true :tex-type "Tex"}
                 (probe)))
          (let [{:keys [hits misses size]} (mp/glyph-cache-stats)]
            (is (pos? hits))
            (is (pos? misses))
            (is (<= size 8) "LRU bound")))
        (testing "a config change misses the cache"
          (mp/glyph-cache-clear!)
          ((get g "desargues_glyph_build"))
          ((get g "desargues_glyph_with_pixel_width") 640 (get g "desargues_glyph_build"))
          (is (= {:hits 0 :misses 2} (select-keys (mp/glyph-cache-stats) [:hits :misses])))
          ((get g "desargues_glyph_build"))
          (is (= 1 (:hits (mp/glyph-cache-stats)))))
        (testing "LRU evicts past max-entries"
          (mp/glyph-cache-clear!)
          (py/run-simple-string "from manim import Text\nfor i in range(12): Text('lru %d' % i)")
          (is (= 8 (:size (mp/glyph-cache-stats)))))
        (testing "uninstall restores Manim; entries survive until cleared"
          (is (true? (mp/uninstall! p)))
          (is (false? (mp/uninstall! p)))
          (is (not (mp/installed? p)))
          (is (= 8 (:size (mp/glyph-cache-stats))))
          (mp/glyph-cache-clear!)
          (is (zero? (:size (mp/glyph-cache-stats))))
          (probe)
          (is (= {:hits 0 :misses 0} (select-keys (mp/glyph-cache-stats) [:hits :misses]))))
        (is (= :glyph-cache (pp/-patch-id p)))))))

(def ^:private bgra-probe-src
  "from manim import Circle, Square, Dot, config
from manim.camera.camera import Camera
import numpy as np
def desargues_bgra_probe(opacity):
    old = config.background_opacity
    config.background_opacity = opacity
    try:
        cam = Camera()
        s = Square(side_length=2.0, fill_opacity=0.5)
        s.set_color_by_gradient('#FF0000', '#00FF88', '#2244FF')
        ms = [Circle(radius=1.5, color='#FFD700', fill_opacity=0.4), s, Dot(color='#3399FF')]
        cam.capture_mobjects(ms)
        return cam.pixel_array.tobytes()
    finally:
        config.background_opacity = old
")

(defn- swap-rb
  "Swap bytes 0 and 2 of every 4-byte pixel in the byte vector `bs`."
  [bs]
  (into [] (mapcat (fn [[r g b a]] [b g r a])) (partition 4 bs)))

(deftest camera-bgra-pre-is-manims-frame-with-red-and-blue-swapped
  (if-not @manim-env?
    (println "skip: no Manim env")
    (do
      (core/init!)
      (let [probe (get (:globals (py/run-simple-string bgra-probe-src)) "desargues_bgra_probe")
            frame (fn [op] (vec (py/->jvm (probe op))))
            p     mp/camera-bgra-pre]
        (mp/uninstall! p)
        (let [rgba-opaque (frame 1.0) rgba-clear (frame 0.0)]
          (is (true? (mp/install! p)))
          (is (true? (mp/install! p)) "idempotent")
          (is (mp/installed? p))
          (let [bgra-opaque (frame 1.0) bgra-clear (frame 0.0)]
            (is (not= rgba-opaque bgra-opaque) "the patch changes the bytes")
            (is (= (swap-rb rgba-opaque) bgra-opaque) "opaque background: identical pixels")
            (is (= (swap-rb rgba-clear) bgra-clear)
                "transparent background: identical, already premultiplied"))
          (is (true? (mp/uninstall! p)))
          (is (false? (mp/uninstall! p)))
          (is (not (mp/installed? p)))
          (is (= rgba-opaque (frame 1.0)) "uninstall restores")
          (is (= :camera-bgra-pre (pp/-patch-id p))))))))
