(ns desargues.manim.frames-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [libpython-clj2.python :as py]
            [desargues.config :as config]
            [desargues.manim.core :as core]
            [desargues.manim.frames :as fr]))

(def manim-env? (delay (config/manim-ready?)))

;; ---------------------------------------------------------------------------
;; Pure pacing
;; ---------------------------------------------------------------------------

(deftest pacing-basics
  (is (= 0.0 (fr/frame-time 0 15)))
  (is (= 1.0 (fr/frame-time 15 15)))
  (is (= 15 (fr/frame-count 1 15)))
  (is (= 8 (fr/frame-count 0.5 15)) "np.arange(0, 0.5, 1/15) has 8 points")
  (is (= 0 (fr/frame-count 0 60)))
  (is (= 3 (fr/frame-index 0.2 15)))
  (is (= 1000.0 (fr/frame-due-ms 0 15 15)))
  (is (= 0 (fr/frame-lag 0 50 3 15)) "early: not behind")
  (is (= 2 (fr/frame-lag 0 400 4 15)) "400 ms is frame 6, next is 4"))

(defspec frame-index-inverts-frame-time 200
  (prop/for-all [n   (gen/choose 0 100000)
                 fps (gen/elements [15 24 30 60 120])]
    (= n (fr/frame-index (fr/frame-time n fps) fps))))

(defspec frame-count-matches-arange 200
  (prop/for-all [ms  (gen/choose 1 20000)
                 fps (gen/elements [15 30 60])]
    (let [rt (/ ms 1000.0)
          step (/ 1.0 fps)]
      ;; numpy arange(0, rt, step) length = ceil(rt/step)
      (= (long (Math/ceil (/ rt step))) (fr/frame-count rt fps)))))

;; ---------------------------------------------------------------------------
;; Sinks (pure)
;; ---------------------------------------------------------------------------

(deftest collecting-sink-counts-and-keeps-last
  (let [s (fr/collecting-sink)]
    (fr/frame! s (byte-array [1 2 3 4]) 1 1 0)
    (fr/frame! s (byte-array [5 6 7 8]) 1 1 1)
    (let [c (fr/collected s)]
      (is (= 2 (:count c)))
      (is (= [0 1] (:indices c)))
      (is (= [5 6 7 8] (vec (:last c)))))))

(deftest fns-and-null-sink-are-sinks
  (let [seen (atom nil)]
    (fr/frame! (fn [_ w h n] (reset! seen [w h n])) nil 2 3 4)
    (is (= [2 3 4] @seen))
    (is (nil? (fr/frame! (fr/null-sink) nil 1 1 0)))))

;; ---------------------------------------------------------------------------
;; The hook against Manim
;; ---------------------------------------------------------------------------

(deftest every-frame-reaches-the-sink-with-no-movie
  (if-not @manim-env?
    (println "[frames-test] no Manim env; skipping")
    (let [M   (core/init!)
          cfg (py/get-attr M "config")
          before (into {} (map (fn [k] [k (py/->jvm (py/get-item cfg k))]))
                       (keys fr/live-config))
          Sc  (core/create-scene-class
               "FramesTestScene"
               (fn [self]
                 (core/play! self (py/call-attr M "Create" (py/call-attr M "Circle")))
                 (core/wait! self 0.5)))
          s   (fr/collecting-sink)
          n   (fr/render-live! #(Sc) s {:quality "low_quality"})
          c   (fr/collected s)]
      (testing "1 s Create (15 frames) + 0.5 s frozen wait (7 held frames) at 15 fps, 0-based, in order"
        (is (= (+ (fr/frame-count 1 15) (fr/held-frame-count 0.5 15)) n (:count c)))
        (is (= (vec (range n)) (:indices c))))
      (testing "480p RGBA"
        (is (= [854 480] [(:w c) (:h c)]))
        (is (= [480 854 4] (vec (tech.v3.datatype/shape (:last c))))))
      (testing "the hook is gone and the config restored afterwards"
        (is (false? (fr/installed?)))
        (is (false? (fr/uninstall!)))
        (is (= before (into {} (map (fn [k] [k (py/->jvm (py/get-item cfg k))]))
                            (keys fr/live-config))))))))

(deftest zero-copy-crossing-shares-the-numpy-buffer
  (if-not @manim-env?
    (println "[frames-test] no Manim env; skipping")
    (let [_     (core/init!)
          np    (py/import-module "numpy")
          frame (py/call-attr-kw np "zeros" [(py/->py-tuple [4 6 4])] {:dtype "uint8"})
          addr  (first (py/->jvm (py/get-item (py/get-attr frame "__array_interface__") "data")))
          seen  (atom {})
          sink  (fn [rgba w h n]
                  (swap! seen assoc n
                         [w h (.address (tech.v3.datatype/as-native-buffer rgba))]))]
      (fr/crossing-ns frame sink 1 {:copy? false})
      (is (= [6 4 addr] (get @seen 0)))
      (reset! seen {})
      (let [copy-sink (fn [rgba _ _ n] (swap! seen assoc n (tech.v3.datatype/as-native-buffer rgba)))]
        (fr/crossing-ns frame copy-sink 1 {:copy? true})
        (is (nil? (get @seen 0)) "a :copy? frame lives on the JVM heap")))))
