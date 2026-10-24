(ns desargues.player.raster-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.manim.frames :as fr]
            [desargues.manim.raster :as mr]
            [desargues.player.protocols :as p]
            [desargues.player.raster :as raster]
            [tech.v3.tensor :as dtt]))

;; ── stubs ────────────────────────────────────────────────────────────────

(defrecord RasterRecorder [shown layout]
  p/IRaster
  (-raster-layout [_] layout)
  (-present-raster! [this bs w h] (swap! shown conj [(vec bs) w h]) this))

(defn recorder [layout] (->RasterRecorder (atom []) layout))

(defrecord StepClock [t step]
  raster/IWallClock
  (-now-ns [_] (swap! t + step)))

(defrecord SurfaceOnly []
  p/ISurface
  (-present! [this _] this)
  (-size [_] [1 1]))

;; ── slot ─────────────────────────────────────────────────────────────────

(deftest newest-wins-and-drops-are-counted
  (let [s (raster/slot)]
    (is (nil? (raster/take! s)) "empty slot")
    (doseq [n (range 3)]
      (raster/publish! s (raster/acquire! s 1 1) 1 1 n 0))
    (let [r (raster/take! s)]
      (is (= 2 (:n r)) "the newest frame wins")
      (is (raster/raster? r))
      (raster/release! s r))
    (is (nil? (raster/take! s)) "a frame is taken once")
    (is (= {:published 3 :overwritten 2 :taken 1 :pending? false}
           (dissoc (raster/counts s) :allocated)))))

(deftest buffers-are-recycled
  (let [s (raster/slot)]
    (dotimes [n 100]
      (raster/publish! s (raster/acquire! s 4 2) 4 2 n 0)
      (when (even? n) (raster/release! s (raster/take! s))))
    (is (<= (:allocated (raster/counts s)) 3) "steady state allocates nothing new")
    (is (= 32 (alength ^bytes (raster/acquire! s 4 2))))
    (is (= 4 (alength ^bytes (raster/acquire! s 1 1))) "a size change gets a fresh buffer")))

(defspec take-never-goes-backwards 100
  (prop/for-all [ops (gen/vector (gen/elements [:pub :take]) 1 60)]
    (let [s (raster/slot)
          seen (reduce (fn [acc [i op]]
                         (if (= op :pub)
                           (do (raster/publish! s (raster/acquire! s 1 1) 1 1 i 0) acc)
                           (if-let [r (raster/take! s)]
                             (do (raster/release! s r) (conj acc (:n r)))
                             acc)))
                       [] (map-indexed vector ops))
          {:keys [published overwritten taken]} (raster/counts s)
          pending (if (:pending? (raster/counts s)) 1 0)]
      (and (= seen (sort seen)) (apply distinct? -1 seen)
           (= published (+ overwritten taken pending))))))

;; ── presenter ────────────────────────────────────────────────────────────

(deftest tick-uploads-the-newest-and-times-the-upload
  (let [s    (raster/slot)
        host (recorder :rgba)
        pr   (raster/presenter host s (->StepClock (atom 0) 1000))]
    (is (nil? (raster/tick! pr)) "nothing published, nothing shown")
    (raster/publish! s (byte-array [1 2 3 4]) 1 1 0 0)
    (raster/publish! s (byte-array [5 6 7 8]) 1 1 1 0)
    (is (= 1 (:n (raster/tick! pr))))
    (is (= [[[5 6 7 8] 1 1]] @(:shown host)))
    (let [rep (raster/report pr)]
      (is (= [1000] (:present-ns rep)) "upload time from the injected clock")
      (is (= 1 (:shown rep)))
      (is (= 2 (:ticks rep)))
      (is (= 1 (:overwritten rep))))))

(deftest iraster-is-opt-in
  (is (not (satisfies? p/IRaster (->SurfaceOnly))))
  (is (satisfies? p/IRaster (recorder :rgba))))

;; ── pixel conversion (pure) ──────────────────────────────────────────────

(deftest rgba-to-premultiplied-bgra
  (is (= 0xFFFF0000 (mr/rgba->bgra-pre 0xFF0000FF)) "opaque red")
  (is (= 0x80808080 (mr/rgba->bgra-pre 0x80FFFFFF)) "straight half white premultiplies")
  (is (= 0 (mr/rgba->bgra-pre 0)))
  (testing "Manim's words are already premultiplied: only red and blue swap"
    (is (= 0x6A5C5010 (mr/rgba-pre->bgra-pre 0x6A10505C)))
    (is (= [(unchecked-int 0xFF112233) (unchecked-int 0x6A5C5010)]
           (vec (mr/convert-words! (int-array [(unchecked-int 0xFF332211) (unchecked-int 0x6A10505C)])
                                   :bgra-pre)))
        "never premultiplied a second time"))
  (is (= [7] (vec (mr/convert-words! (int-array [7]) :rgba))) ":rgba is the identity"))

(defspec opaque-fast-path-matches-the-general-formula 200
  (prop/for-all [x (gen/choose 0 0xFFFFFF)]
    (let [w (bit-or 0xFF000000 x)]
      (= (unchecked-int (mr/rgba->bgra-pre w))
         (aget (mr/convert-words! (int-array [(unchecked-int w)]) :bgra-pre) 0)))))

;; ── the sink, fed a heap tensor (no Python) ─────────────────────────────

(deftest raster-sink-publishes-in-the-host-layout
  (let [s    (raster/slot)
        sink (mr/raster-sink s :bgra-pre {})
        t    (dtt/->tensor [[[255 0 0 255] [0 0 255 255]]] :datatype :uint8)]
    (fr/frame! sink t 2 1 0)
    (let [r (raster/take! s)]
      (is (= [0 0 -1 -1 -1 0 0 -1] (vec (:bytes r))) "red, blue as BGRA bytes")
      (is (= [2 1 0] [(:w r) (:h r) (:n r)])))))

(deftest raster-sink-copies-frames-already-in-the-host-layout
  (let [s    (raster/slot)
        sink (mr/raster-sink s :bgra-pre {:src-layout :bgra-pre})
        t    (dtt/->tensor [[[1 2 3 4] [5 6 7 8]]] :datatype :uint8)]
    (fr/frame! sink t 2 1 0)
    (is (= [1 2 3 4 5 6 7 8] (vec (:bytes (raster/take! s)))) "bytes as they came")))

(defspec native-copy-equals-converted-swizzle 100
  (prop/for-all [px (gen/vector (gen/tuple (gen/choose 0 255) (gen/choose 0 255)
                                           (gen/choose 0 255) (gen/choose 0 255)) 1 16)]
    ;; a premultiplied pixel: no channel above alpha
    (let [pre  (mapv (fn [[r g b a]] [(min r a) (min g a) (min b a) a]) px)
          bgra (mapv (fn [[r g b a]] [b g r a]) pre)
          run  (fn [rows src]
                 (let [s (raster/slot)]
                   (fr/frame! (mr/raster-sink s :bgra-pre {:src-layout src})
                              (dtt/->tensor [rows] :datatype :uint8) (count rows) 1 0)
                   (vec (:bytes (raster/take! s)))))]
      (= (run pre :rgba) (run bgra :bgra-pre)))))

(def ^:private camera-patch-stub ::camera-bgra-pre)

(deftest source-plan-picks-native-bytes-only-for-a-native-host
  (is (= {:src-layout :bgra-pre :patches [camera-patch-stub]}
         (mr/source-plan :bgra-pre :bgra-pre camera-patch-stub true)))
  (is (= {:src-layout :rgba :patches []} (mr/source-plan :rgba :bgra-pre camera-patch-stub true))
      "an :rgba host keeps Manim's RGBA")
  (is (= {:src-layout :rgba :patches []} (mr/source-plan :bgra-pre :bgra-pre camera-patch-stub false))
      "opt-out")
  (is (= {:src-layout :rgba :patches []} (mr/source-plan :bgra-pre nil camera-patch-stub true))
      "big-endian host: no native layout")
  (is (= :bgra-pre (mr/native-layout java.nio.ByteOrder/LITTLE_ENDIAN)))
  (is (nil? (mr/native-layout java.nio.ByteOrder/BIG_ENDIAN))))
