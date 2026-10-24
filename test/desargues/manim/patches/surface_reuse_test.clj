(ns desargues.manim.patches.surface-reuse-test
  (:require [clojure.test :refer [deftest is testing]]
            [libpython-clj2.python :as py]
            [desargues.config :as config]
            [desargues.manim.core :as core]
            [desargues.manim.frames :as fr]
            [desargues.manim.patch-protocols :as pp]
            [desargues.manim.patches :as mp]
            [desargues.manim.patches.surface-reuse :as sr]
            [tech.v3.datatype :as dt]))

(def manim-env? (delay (config/manim-ready?)))

;; ---------------------------------------------------------------------------
;; Pure: the record needs no Python, the view cache needs no Manim
;; ---------------------------------------------------------------------------

(deftest the-patch-record-needs-no-manim
  (is (= :surface-reuse (pp/-patch-id sr/surface-reuse)))
  (is (= :surface-reuse (pp/-patch-id (sr/->SurfaceReuse)))))

(deftest address-view-wraps-a-buffer-once
  (let [mem   (tech.v3.datatype.native-buffer/malloc 24)
        addr  (.address ^tech.v3.datatype.native_buffer.NativeBuffer mem)
        cache (atom nil)
        a     (fr/address-view cache addr 2 3)
        b     (fr/address-view cache addr 2 3)
        c     (fr/address-view cache addr 3 2)]
    (is (= [2 3 4] (vec (dt/shape a))))
    (is (identical? a b) "same buffer, same shape: the cached view")
    (is (not (identical? a c)) "a new shape re-wraps")
    (is (= [3 2 4] (vec (dt/shape c))))
    (is (= addr (.address ^tech.v3.datatype.native_buffer.NativeBuffer (dt/as-native-buffer c))))))

;; ---------------------------------------------------------------------------
;; Against Manim (needs the Manim env)
;; ---------------------------------------------------------------------------

(def ^:private probe-src
  "import hashlib
from manim import Circle, Square, Dot, config
def desargues_sr_probe(cls, color, opacity):
    oc, oo = config.background_color, config.background_opacity
    config.background_color = color
    config.background_opacity = opacity
    try:
        cam = cls()
        s = Square(side_length=2.0, fill_opacity=0.5)
        s.set_color_by_gradient('#FF0000', '#00FF88', '#2244FF')
        ms = [Circle(radius=1.5, color='#FFD700', fill_opacity=0.4), s, Dot(color='#3399FF')]
        out = []
        buf = cam.pixel_array
        for k in range(3):
            cam.reset()
            cam.capture_mobjects(ms[:k + 1])
            out.append(hashlib.sha256(cam.pixel_array.tobytes()).hexdigest())
        out.append(cam.pixel_array is buf)
        return out
    finally:
        config.background_color, config.background_opacity = oc, oo
")

(deftest surface-reuse-frames-are-manims-frames
  (if-not @manim-env?
    (println "skip: no Manim env")
    (do
      (core/init!)
      (let [g     (:globals (py/run-simple-string probe-src))
            probe (get g "desargues_sr_probe")
            cam   (py/get-attr (py/import-module "manim.camera.camera") "Camera")
            mcam  (py/get-attr (py/import-module "manim.camera.moving_camera") "MovingCamera")
            cases [[cam "#000000" 1.0] [cam "#1A334C" 1.0] [cam "#112233" 0.5]
                   [cam "#FFFFFF" 0.0] [mcam "#203040" 1.0]]
            run   (fn [] (mapv (fn [[c col op]] (py/->jvm (probe c col op))) cases))
            p     sr/surface-reuse]
        (mp/uninstall! p)
        (let [before (run)]
          (is (true? (mp/install! p)))
          (is (true? (mp/install! p)) "idempotent")
          (is (mp/installed? p))
          (testing "opaque (paint) and translucent (copy) backgrounds, plain and moving cameras"
            (doseq [[c b a] (map vector cases before (run))]
              (is (= (butlast b) (butlast a)) (str "frames for " (rest c)))
              (is (true? (last a)) "the Camera keeps one buffer")))
          (testing "with the camera-bgra-pre patch too"
            (mp/with-patches* [mp/camera-bgra-pre]
              (fn []
                (let [x (run)]
                  (mp/uninstall! p)
                  (is (= (map butlast x) (map butlast (run))))
                  (mp/install! p)))))
          (is (true? (mp/uninstall! p)))
          (is (false? (mp/uninstall! p)))
          (is (not (mp/installed? p)))
          (is (= before (run)) "uninstall restores"))))))

(defn- digest
  "Hex SHA-256 of a byte array."
  [^bytes bs]
  (apply str (map #(format "%02x" %) (.digest (java.security.MessageDigest/getInstance "SHA-256") bs))))

(deftest live-render-with-surface-reuse-is-byte-identical
  (if-not @manim-env?
    (println "skip: no Manim env")
    (let [M  (core/init!)
          Sc (core/create-scene-class
              "SurfaceReuseTestScene"
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
          a (frames [] :tensor)
          b (frames [sr/surface-reuse] :address)]
      (is (pos? (count a)))
      (is (= (count a) (count b)))
      (is (= a b) "every frame byte for byte")
      (is (< (count (distinct a)) (count a)) "held frames repeat")
      (is (not (mp/installed? sr/surface-reuse))))))
