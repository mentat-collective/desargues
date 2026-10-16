(ns desargues.infrastructure.wasm-runtime-test
  "The :wasm backend: Emmy-derived trajectories compiled to WebAssembly by
   raster and run on the JVM by Chicory, against the JVM stepper. Same time
   grid exactly; values within rounding (the module's sin/cos are
   desargues.kernel.libm's, 1 ulp from Math, so they are not bit-identical).
   Needs -A:dynamics; without raster and Chicory every test skips loudly."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.backend :as backend]
            [desargues.physics :as lag]
            [desargues.physics :as phys]))

(def ^:private wasm?
  (let [ok (backend/available? :wasm)]
    (when-not ok
      (println "[wasm-runtime-test] :wasm backend not on classpath (use -A:dynamics); skipping"))
    ok))

(def ^:private opts {:dt 0.01 :duration 3.0})

(def ^:private tolerance
  "Max |wasm - stepper| over 3 s of a chaotic double pendulum: the measured
   deviation is ~1e-13, and raster's own sin/cos would put it at 3e-3."
  1e-11)

(defn- max-diff [a b]
  (reduce max 0.0 (for [[pa pb] (map vector a b)
                        [k v] (:state pa)]
                    (Math/abs (- (double v) (double (get-in pb [:state k])))))))

(def ^:private spherical
  {:l1 1.4 :l2 1.2 :theta1 1.3 :phi1 0.0 :theta2 1.6 :phi2 0.8 :phi1-dot 1.4 :phi2-dot 0.9})

(deftest wasm-tracks-the-stepper
  (if-not wasm?
    (is true)
    (doseq [[label s] [["planar double pendulum" (lag/double-pendulum {})]
                       ["spherical double pendulum" (lag/spherical-double-pendulum spherical)]]]
      (testing label
        (let [ref (vec (phys/evolve s opts))
              w (phys/evolve s (assoc opts :solver :wasm))]
          (is (= (map :time ref) (map :time w)) "the stepper's time grid, exactly")
          (is (= (set (keys (:state (first ref)))) (set (keys (:state (first w))))))
          (is (< (max-diff ref w) tolerance)))))))

(deftest wasm-batches-and-falls-back
  (if-not wasm?
    (is true)
    (let [ws (phys/solver :wasm)
          planars (vec (for [i (range 4)] (lag/double-pendulum {:theta1 (+ 1.5 (* 0.1 i))})))
          mixed [(phys/make-pendulum {:length 3.0}) (lag/double-pendulum {}) (phys/make-spring-mass {:initial-x 1.0})]]
      (testing "N initial conditions of one Lagrangian in one module call"
        (is (every? #(< % tolerance)
                    (map (fn [s b] (max-diff (vec (phys/evolve s opts)) b))
                         planars (phys/evolve-batch planars (assoc opts :solver ws))))))
      (testing "systems without an Emmy kernel go to the stepper, unchanged and in order"
        (let [out (phys/evolve-batch mixed (assoc opts :solver ws))]
          (is (= (vec (phys/evolve (mixed 0) opts)) (out 0)))
          (is (< (max-diff (vec (phys/evolve (mixed 1) opts)) (out 1)) tolerance))
          (is (= (vec (phys/evolve (mixed 2) opts)) (out 2))))))))

(deftest the-module-is-portable-wasm
  (if-not wasm?
    (is true)
    (let [module @(requiring-resolve 'desargues.infrastructure.wasm-runtime/trajectory-module)
          lowered @(requiring-resolve 'desargues.infrastructure.raster-kernels/lower-emmy-kernel)
          s (lag/double-pendulum {})
          {:keys [bytes export n work labels]} (module s)]
      (testing "a wasm binary (magic + version 1) exporting the rk4 entry point"
        (is (= [0 97 115 109 1 0 0 0] (vec (take 8 bytes))))
        (is (= "rk4" export))
        (is (= 4 n))
        (is (= [4 4 16] work))
        (is (= (vec (phys/state-labels s)) labels)))
      (testing "systems without an Emmy kernel have no module"
        (is (nil? (module (phys/make-pendulum {})))))
      (testing "the lowered derivative indexes its state with integers (wasm rejects a double index)"
        (let [idx (for [[_ e] (:bindings (lowered (:kernel-source s)))
                        f (tree-seq coll? seq e)
                        :when (and (seq? f) (= 'aget (first f)))]
                    (nth f 2))]
          (is (seq idx))
          (is (every? integer? idx)))))))
