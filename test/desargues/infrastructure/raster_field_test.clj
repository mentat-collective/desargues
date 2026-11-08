(ns desargues.infrastructure.raster-field-test
  "One field form, every field-raster backend. The :jvm backend runs the
   form as an evaluated primitive fn and is held to a hand-written closure of
   the same shader; :raster (a deftm over bands of rows) and :gpu (an OpenCL
   kernel) are held to :jvm within one level per channel. :raster needs
   -A:dynamics, :gpu also an OpenCL device; without either the tests skip
   loudly."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.backend :as backend]
            [desargues.wallpaper.field :as fld]
            [desargues.wallpaper.render :as r])
  (:import (java.awt.image BufferedImage DataBufferInt)))

(def ^:private raster? (backend/available? :raster))

(def ^:private gpu?
  (let [ok (and raster? (backend/available? :gpu)
                ((requiring-resolve 'desargues.infrastructure.gpu-solver/device-available?)))]
    (cond
      (not raster?) (println "[raster-field-test] raster not on classpath (use -A:dynamics); skipping :raster and :gpu")
      (not ok) (println "[raster-field-test] no OpenCL device runs a kernel; skipping :gpu"))
    ok))

(defn- pixels ^ints [^BufferedImage img]
  (.getData ^DataBufferInt (.getDataBuffer (.getRaster img))))

(defn- max-level-diff
  "Largest per-channel difference between two images, in levels."
  [a b]
  (reduce max 0 (map (fn [^long p ^long q]
                       (reduce max (for [s [16 8 0]]
                                     (Math/abs (- (bit-and (bit-shift-right p s) 255)
                                                  (bit-and (bit-shift-right q s) 255))))))
                     (pixels a) (pixels b))))

(defn- render [backend shader opts]
  (r/field-raster backend (r/image 96 54) shader opts))

(def ^:private rings
  "Every op of the vocabulary, in one shader."
  {:params '[u k]
   :body '(let [r2 (+ (* x x) (* y y))
                a (atan2 y x)
                w (fract (+ (* k (sqrt r2)) u))]
            [(clamp (mix 0.1 0.9 w) 0 1)
             (+ 0.5 (* 0.5 (sin (+ (* 3 a) (pow (abs x) 1.5)))))
             (/ (+ (exp (- r2)) (* 0.2 (floor (* 2 (cos (- y (tan 0.3)))))) (max 0 (min 1 (log (+ 1 r2))))) 3)])})

(deftest the-form-on-jvm-is-the-closure
  (let [closure (fn ^long [^double x ^double y]
                  (let [r2 (+ (* x x) (* y y))
                        a (Math/atan2 y x)
                        t (+ (* 2.0 (Math/sqrt r2)) 0.25)
                        w (- t (Math/floor t))]
                    (fld/pack (Math/min (Math/max (+ 0.1 (* (- 0.9 0.1) w)) 0.0) 1.0)
                              (+ 0.5 (* 0.5 (Math/sin (+ (* 3.0 a) (Math/pow (Math/abs x) 1.5)))))
                              (/ (+ (+ (Math/exp (- r2)) (* 0.2 (Math/floor (* 2.0 (Math/cos (- y (Math/tan 0.3)))))))
                                    (Math/max 0.0 (Math/min 1.0 (Math/log (+ 1.0 r2)))))
                                 3.0))))]
    (doseq [ss [1 2]]
      (testing (str "ss " ss)
        (is (java.util.Arrays/equals (pixels (render :jvm closure {:ss ss}))
                                     (pixels (render :jvm (fld/bind rings 0.25 2.0) {:ss ss}))))))))

(deftest raster-matches-jvm
  (if-not raster?
    (is true)
    (doseq [[label form args] [["rings" rings [0.25 2.0]] ["quasicrystal" fld/quasicrystal [0.4 3.0]]]
            ss [1 2]]
      (testing (str label " ss " ss)
        (let [bf (apply fld/bind form args)]
          (is (<= (max-level-diff (render :jvm bf {:ss ss}) (render :raster bf {:ss ss})) 1)))))))

(deftest gpu-matches-jvm
  (if-not gpu?
    (is true)
    (try
      (doseq [[label form args] [["rings" rings [0.25 2.0]] ["quasicrystal" fld/quasicrystal [0.4 3.0]]]
              ss [1 2]]
        (testing (str label " ss " ss)
          (let [bf (apply fld/bind form args)]
            (is (<= (max-level-diff (render :jvm bf {:ss ss}) (render :gpu bf {:ss ss})) 1)))))
      (finally ((requiring-resolve 'desargues.infrastructure.raster-field/close-gpu!))))))

(deftest raster-backends-refuse-closures
  (if-not raster?
    (is true)
    (let [e (is (thrown? clojure.lang.ExceptionInfo (render :raster (fn ^long [^double _ ^double _] 0) {})))]
      (is (= :not-a-field-form (:reason (ex-data e)))))))
