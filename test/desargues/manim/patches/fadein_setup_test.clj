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
  (is (= (fs/fadein-setup) (fs/->FadeInSetup true)) "a value; the zero path is on by default")
  (is (= (fs/->FadeInSetup false) (fs/fadein-setup {:zero? false})))
  (is (thrown? clojure.lang.ExceptionInfo (fs/fadein-setup {:zero? :yes}))))

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
                  ;; plain, lag_ratio, shift+scale, target_position, then the
                  ;; plain one again over a scene that already holds it
                  (core/play! self (py/call-attr M "FadeIn" g))
                  (core/play! self (py/call-attr-kw M "FadeIn" [(py/call-attr M "VGroup" (py/call-attr M "Text" "lag")
                                                                              (k "Star" :fill_opacity 1.0))]
                                                    {:lag_ratio 0.5}))
                  (core/play! self (py/call-attr-kw M "FadeIn" [(py/call-attr M "Dot")]
                                                    {:shift up :scale 1.5}))
                  (core/play! self (py/call-attr-kw M "FadeIn" [(py/call-attr M "Triangle")]
                                                    {:target_position sq :lag_ratio 0.3}))
                  (core/play! self (py/call-attr M "FadeIn" g))
                  (core/wait! self 0.2))))
          frames (fn [patches]
                   (let [out (atom [])]
                     (mp/with-patches* (into [mp/svg-path-shared-copy] patches)
                       #(fr/render-live! (fn [] (Sc))
                                         (fn [t _ _ _] (swap! out conj (digest (dt/->byte-array t))))))
                     @out))
          stock  (frames [])
          s0     (fs/stats)
          lean   (frames [(fs/fadein-setup {:zero? false})])
          s1     (fs/stats)
          zero   (frames [(fs/fadein-setup)])
          s2     (fs/stats)]
      (is (pos? (count stock)))
      (is (= stock lean) "lean path: every frame byte for byte")
      (is (= stock zero) "zero path: every frame byte for byte")
      (is (= {:lean 5 :zero 0} (select-keys (merge-with - s1 s0) [:lean :zero]))
          "with :zero? false the five FadeIns took the lean path")
      (is (= {:lean 0 :zero 5} (select-keys (merge-with - s2 s1) [:lean :zero]))
          "plain, lag, shift+scale and target_position all take the zero path")
      (is (not (mp/installed? (fs/fadein-setup))) "with-patches* uninstalled it"))))

(def ^:private zero-probe-src
  "import numpy as np, hashlib
from manim import VGroup, Square, Circle, Text, FadeIn, UP
def _arrays(m):
    return {k: v for k, v in m.__dict__.items() if isinstance(v, np.ndarray)}
def _digest(mob):
    h = hashlib.sha256()
    for m in mob.get_family():
        for k, v in sorted(_arrays(m).items()):
            h.update(k.encode()); h.update(np.ascontiguousarray(v).tobytes())
    return h.hexdigest()
def desargues_fs_zero_probe(updater):
    g = VGroup(Square(fill_opacity=0.5), VGroup(Circle(color='#FFD700'), Text('ab')))
    if updater:
        g.add_updater(lambda m: m)
    before = [{k: v.copy() for k, v in _arrays(m).items()} for m in g.get_family()]
    a = FadeIn(g, lag_ratio=0.2)
    a.begin()
    fam, tgt, start = g.get_family(), a.target_copy.get_family(), a.starting_mobject.get_family()
    out = {'target-values': all(np.array_equal(v, b[k]) for m, b in zip(tgt, before) for k, v in _arrays(m).items()),
           'start-colours-own': all(not np.shares_memory(v, w) for m, s in zip(fam, start)
                                     for k, v in _arrays(m).items() if k != 'points' and v.size
                                     for w in [s.__dict__[k]])}
    steps = []
    for t in np.linspace(0, 1, 7):
        a.interpolate(t)
        steps.append(_digest(g))
    a.finish()
    steps.append(_digest(g))
    out['steps'] = steps
    # mutating the displayed mobject in place, on every member (with points
    # or not), reaches neither the target snapshot nor the start state
    tsnap = [{k: v.copy() for k, v in _arrays(m).items()} for m in tgt]
    ssnap = [{k: v.copy() for k, v in _arrays(m).items()} for m in start]
    for m in g.get_family():
        if len(m.points):
            m.points += 1.0
        m.fill_rgbas[:, 3] = 0.25
    g.set_stroke(opacity=0.3)
    g.set_background_stroke(opacity=0.2)
    out['target-untouched'] = all(np.array_equal(v, s[k]) for m, s in zip(tgt, tsnap) for k, v in _arrays(m).items())
    out['start-untouched'] = all(np.array_equal(v, s[k]) for m, s in zip(start, ssnap) for k, v in _arrays(m).items())
    return out
")

(deftest zero-path-sharing-is-safe-for-the-animation-and-after
  (if-not @manim-env?
    (println "skip: no Manim env")
    (do
      (core/init!)
      (let [g     (:globals (py/run-simple-string zero-probe-src))
            probe (get g "desargues_fs_zero_probe")
            run   (fn [patches updater?]
                    (let [s0  (fs/stats)
                          out (mp/with-patches* patches #(py/->jvm (probe updater?)))]
                      [out (select-keys (merge-with - (fs/stats) s0) [:lean :zero])]))
            [stock _]  (run [] false)
            [zero d]   (run [(fs/fadein-setup)] false)
            [stock2 _] (run [] true)
            [lean d2]  (run [(fs/fadein-setup)] true)
            ok {"target-values" true "start-colours-own" true "target-untouched" true "start-untouched" true}]
        (is (= ok (dissoc zero "steps")) "zero path: snapshot values, fresh start colours, independence after finish")
        (is (= (get stock "steps") (get zero "steps")) "every interpolated state and the finish are stock's")
        (is (= {:lean 0 :zero 1} d))
        (is (= ok (dissoc lean "steps")))
        (is (= (get stock2 "steps") (get lean "steps")))
        (is (= {:lean 1 :zero 0} d2) "a family with updaters is outside the zero guards")))))

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
