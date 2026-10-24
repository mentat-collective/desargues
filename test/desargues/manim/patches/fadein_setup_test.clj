(ns desargues.manim.patches.fadein-setup-test
  (:require [clojure.test :refer [deftest is testing]]
            [libpython-clj2.python :as py]
            [desargues.config :as config]
            [desargues.manim.core :as core]
            [desargues.manim.frames :as fr]
            [desargues.manim.patch-protocols :as pp]
            [desargues.manim.patches :as mp]
            [desargues.manim.patches.fadein-setup :as fs]
            [tech.v3.datatype :as dt]))

(def manim-env? (delay (config/manim-ready?)))

;; ---------------------------------------------------------------------------
;; Pure: the record needs no Python
;; ---------------------------------------------------------------------------

(deftest the-patch-record-needs-no-manim
  (is (= :fadein-setup (pp/-patch-id (fs/fadein-setup))))
  (is (= (fs/fadein-setup) (fs/->FadeInSetup)) "a value"))

;; ---------------------------------------------------------------------------
;; Against Manim (needs the Manim env)
;; ---------------------------------------------------------------------------

(def ^:private probe-src
  "import numpy as np
from manim import VGroup, Square, Circle, Text, Line
def desargues_fs_family():
    a = Square(fill_opacity=0.5)
    g = VGroup(a, VGroup(Circle(color='#FFD700'), Text('ab')), Line())
    g.alias = a
    return g
def desargues_fs_compare(src, cp):
    fs, fc = src.get_family(), cp.get_family()
    out = {'n': len(fs) == len(fc),
           'classes': [type(x) for x in fs] == [type(x) for x in fc],
           'fresh': all(x is not y for x, y in zip(fs, fc)),
           'alias': cp.alias is cp.submobjects[0]}
    same, own = True, True
    for x, y in zip(fs, fc):
        for k, v in x.__dict__.items():
            w = y.__dict__[k]
            if isinstance(v, np.ndarray):
                same &= np.array_equal(v, w)
                own &= v.size == 0 or not np.shares_memory(v, w)
    out['arrays-equal'] = bool(same)
    out['arrays-own'] = bool(own)
    before = fs[1].points.copy()
    fc[1].points += 1.0
    cp.fade(1)
    out['source-untouched'] = bool(np.array_equal(before, fs[1].points)) and src.get_fill_opacity() == fs[0].get_fill_opacity()
    return out
")

(deftest lean-copy-is-an-independent-copy-of-the-family
  (if-not @manim-env?
    (println "skip: no Manim env")
    (do
      (core/init!)
      (let [g   (:globals (py/run-simple-string probe-src))
            src ((get g "desargues_fs_family"))
            out (py/->jvm ((get g "desargues_fs_compare") src (fs/lean-copy src)))]
        (is (= {"n" true "classes" true "fresh" true "alias" true
                "arrays-equal" true "arrays-own" true "source-untouched" true}
               out))))))

(defn- digest
  "Hex SHA-256 of a byte array."
  [^bytes bs]
  (apply str (map #(format "%02x" %) (.digest (java.security.MessageDigest/getInstance "SHA-256") bs))))

(deftest fadein-frames-are-stock-frames
  (if-not @manim-env?
    (println "skip: no Manim env")
    (let [M  (core/init!)
          k  (fn [nm & kvs] (py/call-attr-kw M nm [] (apply hash-map kvs)))
          up (core/get-constant "UP")
          Sc (core/create-scene-class
              "FadeInSetupTestScene"
              (fn [self]
                (let [sq (k "Square" :fill_opacity 0.6 :color "#33AAFF")
                      g  (py/call-attr M "VGroup" sq (py/call-attr M "Text" "fade")
                                       (k "Circle" :radius 0.5 :color "#FFD700"))]
                  (core/play! self (py/call-attr M "FadeIn" g))
                  (core/play! self (py/call-attr-kw M "FadeIn" [(py/call-attr M "Dot")]
                                                    {:shift up :scale 1.5}))
                  (core/play! self (py/call-attr-kw M "FadeIn" [(py/call-attr M "Triangle")]
                                                    {:target_position sq :lag_ratio 0.3}))
                  (core/wait! self 0.2))))
          frames (fn [patches]
                   (let [out (atom [])]
                     (mp/with-patches* (into [mp/svg-path-shared-copy] patches)
                       #(fr/render-live! (fn [] (Sc))
                                         (fn [t _ _ _] (swap! out conj (digest (dt/->byte-array t))))))
                     @out))
          p      (fs/fadein-setup)
          stock  (frames [])
          before (:lean (fs/stats))
          lean   (frames [p])]
      (is (pos? (count stock)))
      (is (= stock lean) "every frame byte for byte")
      (is (= 3 (- (:lean (fs/stats)) before)) "the three FadeIns took the lean path")
      (is (not (mp/installed? p)) "with-patches* uninstalled it"))))

(deftest install-is-idempotent-and-uninstall-restores
  (if-not @manim-env?
    (println "skip: no Manim env")
    (do
      (core/init!)
      (let [p      (fs/fadein-setup)
            fadein (py/get-attr (py/import-module "manim") "FadeIn")
            own    #(set (py/->jvm (py/call-attr (py/import-module "builtins") "list"
                                                 (py/get-attr fadein "__dict__"))))
            stock  (own)]
        (mp/uninstall! p)
        (is (true? (mp/install! p)))
        (is (true? (mp/install! p)) "idempotent")
        (is (mp/installed? p))
        (is (contains? (own) "begin"))
        (is (true? (mp/uninstall! p)))
        (is (false? (mp/uninstall! p)))
        (is (not (mp/installed? p)))
        (is (= stock (own)) "FadeIn's own methods as before")))))
