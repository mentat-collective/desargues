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

;; ---------------------------------------------------------------------------
;; native-cairo-path: pure parts
;; ---------------------------------------------------------------------------

(deftest native-path-lib-resolution
  (is (= "/x/lib.so" (mp/native-path-lib "/x/lib.so" "/root")) "explicit path wins")
  (is (= "/root/native/zigpath/zig-out/lib/libdesargues_path.so" (mp/native-path-lib "" "/root")))
  (is (= "/root/native/zigpath/zig-out/lib/libdesargues_path.so" (mp/native-path-lib nil "/root")))
  (is (nil? (mp/native-path-lib "" nil))))

(deftest native-path-missing-library-installs-nothing
  (let [p (mp/native-cairo-path "/nonexistent/libdesargues_path.so")]
    (is (false? (mp/native-path-available? (:lib-path p))))
    (is (false? (mp/native-path-available? nil)))
    (is (false? (mp/install! p)) "no library: Manim keeps its own path building")
    (is (= :native-cairo-path (pp/-patch-id p)))))

(deftest path-builder-selection
  (is (= [] (mp/path-builder-patches "manim")))
  (is (= [] (mp/path-builder-patches nil)))
  (is (= [] (mp/path-builder-patches " Manim ")))
  (is (= [:native-cairo-path] (map pp/-patch-id (mp/path-builder-patches "native"))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown path builder"
                        (mp/path-builder-patches "skia"))))

;; ---------------------------------------------------------------------------
;; native-cairo-path against Manim (needs the Manim env and `bb native:path`)
;; ---------------------------------------------------------------------------

(def ^:private native-probe-src
  "import hashlib, cairo, numpy as np
from manim import Text, MathTex, VGroup, Rectangle, Circle, VMobject, Square
from manim.camera.camera import Camera

def desargues_native_path_probe():
    g = VGroup(Rectangle(), Circle(), Text('ab'), MathTex(r'x^2+\\frac{1}{2}'), Square().shift([1, 1, 0]))
    open_path = VMobject().set_points_as_corners([[0, 0, 0], [1, 0, 0], [1, 1, 0]])
    two = VMobject()
    two.set_points(np.concatenate([Square().points, Circle().shift([3, 0, 0]).points]))
    partial = VMobject()
    partial.set_points(np.concatenate([Circle().points, [[9, 9, 0], [9, 8, 0]]]))
    bad = VMobject()
    bad.set_points(np.array([[0, 0, 0], [np.nan, 1, 0], [1, 1, 0], [2, 2, 0]], dtype=float))
    fam = g.family_members_with_points() + [open_path, two, partial, bad, VMobject()]
    cam = Camera()
    surf = cairo.ImageSurface(cairo.FORMAT_ARGB32, 8, 8)
    out = []
    for vm in fam:
        c = cairo.Context(surf)
        cam.set_cairo_context_path(c, vm)
        out.append(repr(list(c.copy_path())))
    return hashlib.sha256('|'.join(out).encode()).hexdigest()

def desargues_native_frame_probe():
    from manim import FadeIn, Scene, config
    class P(Scene):
        def construct(self):
            self.add(VGroup(Text('desargues'), MathTex(r'\\alpha_1'), Circle()).arrange())
    s = P()
    s.render()
    return hashlib.sha256(s.renderer.get_frame().tobytes()).hexdigest()
")

(deftest native-cairo-path-matches-manim
  (let [p (mp/native-cairo-path)]
    (cond
      (not @manim-env?) (println "skip: no Manim env")
      (not (mp/native-path-available? (:lib-path p))) (println "skip: native path library not built (bb native:path)")
      :else
      (do
        (core/init!)
        (let [g      (:globals (py/run-simple-string native-probe-src))
              paths  #(py/->jvm ((get g "desargues_native_path_probe")))
              frame  #(py/->jvm ((get g "desargues_native_frame_probe")))
              _      (mp/uninstall! p)
              stock  [(paths) (frame)]]
          (try
            (testing "install cycle"
              (is (true? (mp/install! p)))
              (is (true? (mp/install! p)) "idempotent")
              (is (mp/installed? p)))
            (testing "same cairo path ops and the same frame bytes as Manim"
              (is (= stock [(paths) (frame)])))
            (testing "uninstall restores"
              (is (true? (mp/uninstall! p)))
              (is (false? (mp/uninstall! p)))
              (is (not (mp/installed? p)))
              (is (= stock [(paths) (frame)])))
            (testing "install-path-builder! follows the name"
              (is (= [:native-cairo-path] (:installed (mp/install-path-builder! "native"))))
              (is (= [] (:installed (mp/install-path-builder! "manim"))))
              (is (not (mp/installed? p))))
            (finally (mp/uninstall! p))))))))
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
