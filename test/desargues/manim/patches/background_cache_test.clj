(ns desargues.manim.patches.background-cache-test
  (:require [clojure.test :refer [deftest is testing]]
            [libpython-clj2.python :as py]
            [desargues.config :as config]
            [desargues.manim.core :as core]
            [desargues.manim.frames :as fr]
            [desargues.manim.patch-protocols :as pp]
            [desargues.manim.patches :as mp]
            [desargues.manim.patches.background-cache :as bc]
            [desargues.manim.patches.surface-reuse :as sr]
            [tech.v3.datatype :as dt]))

(def manim-env? (delay (config/manim-ready?)))

;; ---------------------------------------------------------------------------
;; Pure: the record needs no Python and no Manim config
;; ---------------------------------------------------------------------------

(deftest the-patch-record-needs-no-manim
  (is (= :background-cache (pp/-patch-id (bc/background-cache))))
  (is (= 8 (:max-entries (bc/background-cache))))
  (is (= 3 (:max-entries (bc/background-cache 3))))
  (is (thrown? clojure.lang.ExceptionInfo (bc/background-cache 0)))
  (is (thrown? clojure.lang.ExceptionInfo (bc/background-cache "8"))))

;; ---------------------------------------------------------------------------
;; Against Manim (needs the Manim env)
;; ---------------------------------------------------------------------------

(def ^:private probe-src
  "import hashlib
from manim import Circle, Square, config
def desargues_bc_probe(cls, color, opacity, h, w):
    saved = (config.background_color, config.background_opacity,
             config.pixel_height, config.pixel_width)
    config.background_color = color
    config.background_opacity = opacity
    config.pixel_height, config.pixel_width = h, w
    try:
        a, b = cls(), cls()
        ms = [Circle(radius=1.5, color='#FFD700', fill_opacity=0.4),
              Square(side_length=2.0, fill_opacity=0.5)]
        b.reset()
        b.capture_mobjects(ms)
        return [hashlib.sha256(a.background.tobytes()).hexdigest(),
                list(a.background.shape),
                hashlib.sha256(b.pixel_array.tobytes()).hexdigest(),
                a.background is b.background,
                bool(a.background.flags.writeable)]
    finally:
        (config.background_color, config.background_opacity,
         config.pixel_height, config.pixel_width) = saved

def desargues_bc_image_probe(cls, path):
    a, b = cls(background_image=path), cls(background_image=path)
    return a.background is b.background
")

(deftest cached-backgrounds-are-manims-backgrounds
  (if-not @manim-env?
    (println "skip: no Manim env")
    (do
      (core/init!)
      (let [g     (:globals (py/run-simple-string probe-src))
            probe (get g "desargues_bc_probe")
            cam   (py/get-attr (py/import-module "manim.camera.camera") "Camera")
            mcam  (py/get-attr (py/import-module "manim.camera.moving_camera") "MovingCamera")
            cases [[cam "#000000" 1.0 480 854] [cam "#1A334C" 1.0 480 854]
                   [cam "#112233" 0.5 480 854] [cam "#000000" 1.0 240 427]
                   [mcam "#203040" 1.0 480 854]]
            run   (fn [] (mapv (fn [[c col op h w]] (py/->jvm (probe c col op h w))) cases))
            p     (bc/background-cache 16) ; room for every key below, both colour layouts
            facts (fn [r] (mapv #(subvec % 0 3) r))]
        (mp/uninstall! p)
        (bc/clear!)
        (let [before (run)]
          (is (every? #(false? (nth % 3)) before) "Manim builds one background per Camera")
          (is (true? (mp/install! p)))
          (is (true? (mp/install! p)) "idempotent")
          (is (mp/installed? p))
          (let [after (run)]
            (testing "same background bytes, shape and frame per colour, opacity, size and class"
              (is (= (facts before) (facts after))))
            (testing "one shared, read-only array per key"
              (is (every? #(true? (nth % 3)) after))
              (is (every? #(false? (nth % 4)) after)))
            (is (= (count cases) (:misses (bc/stats))) "one build per key"))
          (testing "the colour swap of camera-bgra-pre is part of the key"
            (mp/with-patches* [mp/camera-bgra-pre]
              (fn []
                (let [x (run)]
                  (mp/uninstall! p)
                  (is (= (facts x) (facts (run))))
                  (mp/install! p)))))
          (testing "a background image is never cached"
            (let [png (str (java.io.File/createTempFile "desargues-bc" ".png"))]
              (py/call-attr (py/call-attr (py/import-module "PIL.Image") "new" "RGBA" [8 8] [10 20 30 255])
                            "save" png)
              (is (false? (py/->jvm ((get g "desargues_bc_image_probe") cam png))))))
          (is (true? (mp/uninstall! p)))
          (is (false? (mp/uninstall! p)))
          (is (not (mp/installed? p)))
          (is (= before (run)) "uninstall restores: kept arrays are not used")
          (testing "the store survives a reinstall (the live loop installs per render)"
            (let [n (:misses (bc/stats))]
              (mp/with-patches* [p] run)
              (is (= n (:misses (bc/stats))))))
          (is (true? (bc/clear!)))
          (is (zero? (:size (bc/stats))) "clear! empties the store"))))))

(defn- digest
  "Hex SHA-256 of a byte array."
  [^bytes bs]
  (apply str (map #(format "%02x" %) (.digest (java.security.MessageDigest/getInstance "SHA-256") bs))))

(deftest live-render-with-background-cache-is-byte-identical
  (if-not @manim-env?
    (println "skip: no Manim env")
    (let [M  (core/init!)
          Sc (core/create-scene-class
              "BackgroundCacheTestScene"
              (fn [self]
                (core/play! self (py/call-attr M "Create" (py/call-attr M "Circle")))
                (core/play! self (py/call-attr M "FadeIn" (py/call-attr M "Square")))
                (core/wait! self 0.2)))
          frames (fn [patches crossing]
                   (let [out (atom [])]
                     (mp/with-patches* patches
                       #(fr/render-live! (fn [] (Sc))
                                         (fn [t _ _ _] (swap! out conj (digest (dt/->byte-array t))))
                                         {:crossing crossing}))
                     @out))
          a  (frames [] :tensor)
          b  (frames [(bc/background-cache)] :tensor)
          c  (frames [mp/camera-bgra-pre sr/surface-reuse] :address)
          d  (frames [mp/camera-bgra-pre sr/surface-reuse (bc/background-cache)] :address)
          d2 (frames [mp/camera-bgra-pre sr/surface-reuse (bc/background-cache)] :address)]
      (is (pos? (count a)))
      (is (= a b) "every frame byte for byte")
      (is (= c d d2) "with surface-reuse and camera-bgra-pre, first and second edit")
      (is (not (mp/installed? (bc/background-cache)))))))
