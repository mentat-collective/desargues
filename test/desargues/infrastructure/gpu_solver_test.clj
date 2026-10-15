(ns desargues.infrastructure.gpu-solver-test
  "The :gpu backend against the JVM stepper: batched RK4 as raster.par
   kernels on an OpenCL device. Same time grid exactly; values within
   rounding (the device's sin and fused multiply-adds). Needs -A:dynamics AND
   a device (an OpenCL ICD); without either every test skips loudly. The
   par kernels' JVM build (map-void! as a loop) is held to the CPU kernel bit
   for bit whenever raster is present, device or not."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.backend :as backend]
            [desargues.videos.lagrangian :as lag]
            [desargues.videos.physics :as phys]))

(def ^:private raster? (backend/available? :gpu))

(def ^:private device?
  (let [ok (and raster? ((requiring-resolve 'desargues.infrastructure.gpu-solver/device-available?)))]
    (cond
      (not raster?) (println "[gpu-solver-test] :gpu backend not on classpath (use -A:dynamics); skipping")
      (not ok) (println "[gpu-solver-test] no GPU device (an OpenCL ICD is needed); skipping device tests"))
    ok))

(def ^:private opts {:dt 0.01 :duration 3.0})

(def ^:private tolerance
  "Max |gpu - stepper| over 3 s of a chaotic double pendulum: measured
   ~5e-13 on an RTX 4070 (device sin, FMA contraction, chaos amplifying)."
  1e-10)

(defn- max-diff [a b]
  (reduce max 0.0 (for [[pa pb] (map vector a b)
                        [k v] (:state pa)]
                    (Math/abs (- (double v) (double (get-in pb [:state k])))))))

(def ^:private spherical
  {:l1 1.4 :l2 1.2 :theta1 1.3 :phi1 0.0 :theta2 1.6 :phi2 0.8 :phi1-dot 1.4 :phi2-dot 0.9})

(deftest par-kernels-are-the-cpu-kernels-on-the-jvm
  (if-not raster?
    (is true)
    (let [gpu-kernel @(requiring-resolve 'desargues.infrastructure.raster-kernels/gpu-kernel)
          kernels @(requiring-resolve 'desargues.infrastructure.raster-kernels/kernels)
          s (lag/double-pendulum {})
          src (:kernel-source s)
          {:keys [var n]} (gpu-kernel src)
          nb 3 steps 50
          u0s (double-array (apply concat (repeat nb (seq (:state (phys/array-kernel s))))))
          cpu (double-array (* nb (inc steps) n))
          par (double-array (* nb (inc steps) n))]
      ((:rk4 (kernels src)) u0s nb cpu steps 0.01)
      (@var u0s par (double-array (* nb n)) (double-array (* nb n)) (double-array (* nb 4 n)) steps 0.01 nb)
      (is (java.util.Arrays/equals cpu par)))))

(deftest gpu-tracks-the-stepper
  (if-not device?
    (is true)
    (with-open [gs ^java.io.Closeable (phys/solver :gpu)]
      (doseq [[label s] [["planar double pendulum" (lag/double-pendulum {})]
                         ["spherical double pendulum" (lag/spherical-double-pendulum spherical)]]]
        (testing label
          (let [ref (vec (phys/evolve s opts))
                g (phys/evolve s (assoc opts :solver gs))]
            (is (= (map :time ref) (map :time g)) "the stepper's time grid, exactly")
            (is (< (max-diff ref g) tolerance))))))))

(deftest gpu-batches-sweeps-and-falls-back
  (if-not device?
    (is true)
    (with-open [gs ^java.io.Closeable (phys/solver :gpu)]
      (let [sweep (vec (for [i (range 6)]
                         (phys/make-pendulum {:length (+ 1.0 (* 0.5 i)) :damping (* 0.05 i)
                                              :initial-theta (+ 0.2 (* 0.1 i))})))
            planars (vec (for [i (range 4)] (lag/double-pendulum {:theta1 (+ 1.5 (* 0.1 i))})))
            mixed [(phys/make-pendulum {:length 3.0}) (lag/double-pendulum {}) (phys/make-spring-mass {:initial-x 1.0})]
            close-all? (fn [systems out] (every? #(< % tolerance) (map (fn [s b] (max-diff (vec (phys/evolve s opts)) b)) systems out)))]
        (testing "a pendulum parameter sweep (per-trajectory g/L and damping) in one launch"
          (is (close-all? sweep (phys/evolve-batch sweep (assoc opts :solver gs)))))
        (testing "N initial conditions of one Lagrangian in one launch"
          (is (close-all? planars (phys/evolve-batch planars (assoc opts :solver gs)))))
        (testing "a system without a GPU kernel goes to the stepper, unchanged and in order"
          (let [out (phys/evolve-batch mixed (assoc opts :solver gs))]
            (is (close-all? (subvec mixed 0 2) (subvec out 0 2)))
            (is (= (vec (phys/evolve (mixed 2) opts)) (out 2)))))
        (testing "a call leaves no device buffer behind"
          (is (empty? (:buffers @@(:session gs)))))))))

(deftest a-closed-solver-releases-the-device
  (if-not device?
    (is true)
    (let [gs (phys/solver :gpu)
          s (lag/double-pendulum {})]
      (phys/evolve s (assoc opts :solver gs))
      (is (some? @(:session gs)))
      (.close ^java.io.Closeable gs)
      (is (nil? @(:session gs)))
      (testing "and reopens if used again"
        (is (= (count (phys/evolve s opts)) (count (phys/evolve s (assoc opts :solver gs)))))
        (is (some? @(:session gs)))
        (.close ^java.io.Closeable gs)))))

(deftest an-unusable-device-is-named
  ;; raster opens sessions lazily and does not check an :ocl:N index, so the
  ;; device that cannot open here is one of a backend raster does not know.
  (if-not raster?
    (is true)
    (let [gs ((requiring-resolve 'desargues.infrastructure.gpu-solver/gpu-solver) :nope:0)
          e (is (thrown? clojure.lang.ExceptionInfo (phys/evolve (lag/double-pendulum {}) (assoc opts :solver gs))))]
      (is (= :nope:0 (:device (ex-data e))))
      (is (= :no-device (:reason (ex-data e))))
      (is (false? ((requiring-resolve 'desargues.infrastructure.gpu-solver/device-available?) :nope:0))))))
