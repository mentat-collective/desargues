(ns desargues.manim.patches.play-setup-test
  (:require [clojure.test :refer [deftest is testing]]
            [libpython-clj2.python :as py]
            [desargues.config :as config]
            [desargues.manim.core :as core]
            [desargues.manim.frames :as fr]
            [desargues.manim.patch-protocols :as pp]
            [desargues.manim.patches :as mp]
            [desargues.manim.patches.play-setup :as ps]
            [tech.v3.datatype :as dt]))

(def manim-env? (delay (config/manim-ready?)))

;; ---------------------------------------------------------------------------
;; Pure: the record needs no Python
;; ---------------------------------------------------------------------------

(deftest the-patch-record-needs-no-manim
  (is (= :play-setup (pp/-patch-id (ps/play-setup))))
  (is (= ps/all-parts (:parts (ps/play-setup))))
  (is (= (ps/play-setup [:family-walk]) (ps/->PlaySetup [:family-walk])) "a value")
  (is (= [] (:parts (ps/play-setup []))))
  (is (thrown? clojure.lang.ExceptionInfo (ps/play-setup [:nope])))
  (is (thrown? clojure.lang.ExceptionInfo (ps/play-setup [:family-walk :family-walk]))))

;; ---------------------------------------------------------------------------
;; Against Manim (needs the Manim env)
;; ---------------------------------------------------------------------------

(def ^:private probe-src
  "import numpy as np
from manim import VGroup, Square, Circle, Line, MathTex
def desargues_ps_tree():
    a = Square()
    inner = VGroup(Circle(), a)
    return VGroup(a, inner, Line(), VGroup(VGroup(Circle())))
def desargues_ps_families(m):
    return [id(x) for x in m.get_family()], [id(x) for x in m.family_members_with_points()]
def desargues_ps_tex():
    return MathTex('7')
def desargues_ps_compare(src, cp):
    fs, fc = src.get_family(), cp.get_family()
    same, own = True, True
    for x, y in zip(fs, fc):
        for k, v in x.__dict__.items():
            w = y.__dict__[k]
            if isinstance(v, np.ndarray):
                same &= np.array_equal(v, w)
                own &= v.size == 0 or not np.shares_memory(v, w)
    groups = list(cp.id_to_vgroup_dict.values())
    return {'n': len(fs) == len(fc),
            'classes': [type(x) for x in fs] == [type(x) for x in fc],
            'fresh': all(x is not y for x, y in zip(fs, fc)),
            'arrays-equal': bool(same), 'arrays-own': bool(own),
            'groups-copied': all(g is not h for g, h in zip(groups, src.id_to_vgroup_dict.values())),
            'groups-point-into-copy': all(any(s is f for f in fc) for g in groups for s in g.submobjects)}
")

(deftest family-walk-returns-the-stock-lists
  (if-not @manim-env?
    (println "skip: no Manim env")
    (do
      (core/init!)
      (let [g    (:globals (py/run-simple-string probe-src))
            tree ((get g "desargues_ps_tree"))
            fams #(py/->jvm ((get g "desargues_ps_families") tree))
            p    (ps/play-setup [:family-walk])
            _    (mp/uninstall! p)
            stock (fams)]
        (mp/with-patches* [p]
          #(is (= stock (fams)) "same members, same order, a repeated member kept last"))))))

(deftest lean-copy-copies-the-glyph-and-its-groups
  (if-not @manim-env?
    (println "skip: no Manim env")
    (do
      (core/init!)
      (let [g   (:globals (py/run-simple-string probe-src))
            src ((get g "desargues_ps_tex"))]
        (is (= {"n" true "classes" true "fresh" true "arrays-equal" true "arrays-own" true
                "groups-copied" true "groups-point-into-copy" true}
               (py/->jvm ((get g "desargues_ps_compare") src (ps/lean-copy src)))))))))

(defn- digest [^bytes bs]
  (apply str (map #(format "%02x" %) (.digest (java.security.MessageDigest/getInstance "SHA-256") bs))))

(deftest counter-frames-are-stock-frames
  (if-not @manim-env?
    (println "skip: no Manim env")
    (let [M  (core/init!)
          k  (fn [nm args kvs] (py/call-attr-kw M nm args kvs))
          Sc (core/create-scene-class
              "PlaySetupTestScene"
              (fn [self]
                (let [a (k "DecimalNumber" [0] {:font_size 24})
                      b (k "DecimalNumber" [3.5] {:font_size 36 :num_decimal_places 1})
                      c (k "Integer" [-2] {:unit "\\%" :show_ellipsis true})]
                  (py/call-attr b "shift" (core/get-constant "UP"))
                  (py/call-attr c "shift" (core/get-constant "DOWN"))
                  (core/play! self (py/call-attr M "AnimationGroup"
                                                 (py/call-attr M "ChangeDecimalToValue" a 12.75)
                                                 (py/call-attr M "ChangeDecimalToValue" b -40)
                                                 (py/call-attr M "ChangeDecimalToValue" c 9)))
                  (core/play! self (py/call-attr M "ChangeDecimalToValue" a 3))
                  (core/wait! self 0.2))))
          frames (fn [patches]
                   (let [out (atom [])]
                     (mp/with-patches* (into [mp/svg-path-shared-copy] patches)
                       #(fr/render-live! (fn [] (Sc))
                                         (fn [t _ _ _] (swap! out conj (digest (dt/->byte-array t))))))
                     @out))
          p      (ps/play-setup)
          ;; an earlier test may have left a profile (and this patch) installed
          _      (mp/uninstall! p)
          stock  (frames [])
          before (ps/stats)
          lean   (frames [p])
          lean2  (frames [p])
          after  (ps/stats)]
      (is (pos? (count stock)))
      (is (= stock lean lean2) "every frame byte for byte, cold and warm")
      (is (pos? (- (:lean after) (:lean before))) "glyphs took the lean path")
      (is (pos? (- (:stock after) (:stock before))) "the ellipsis (a call with kwargs) went to Manim")
      (is (not (mp/installed? p)) "with-patches* uninstalled it"))))

(deftest install-is-idempotent-and-uninstall-restores
  (if-not @manim-env?
    (println "skip: no Manim env")
    (do
      (core/init!)
      (let [p   (ps/play-setup)
            own (fn [cls nm] (py/get-item (py/get-attr cls "__dict__") nm))
            mob (py/get-attr (py/import-module "manim.mobject.mobject") "Mobject")
            dec (py/get-attr (py/import-module "manim.mobject.text.numbers") "DecimalNumber")
            stock-fam (own mob "get_family")
            stock-stm (own dec "_string_to_mob")]
        (mp/uninstall! p)
        (is (= [] (ps/installed-parts)))
        (is (true? (mp/install! p)))
        (is (true? (mp/install! p)) "idempotent")
        (is (mp/installed? p))
        (is (= ps/all-parts (ps/installed-parts)))
        (is (true? (mp/uninstall! p)))
        (is (false? (mp/uninstall! p)))
        (is (not (mp/installed? p)))
        (testing "Manim's own methods as before"
          (is (= stock-fam (own mob "get_family")))
          (is (= stock-stm (own dec "_string_to_mob"))))
        (testing "one part alone"
          (let [w (ps/play-setup [:family-walk])]
            (mp/with-patches* [w] #(is (= [:family-walk] (ps/installed-parts))))))))))
