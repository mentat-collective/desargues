;; The pendulum trajectory through a Zig wasm kernel vs interpreted phys/evolve,
;; both on cljw. Run from the repo root after building dev/cljw/kernel/pendulum.wasm:
;;   cljw -cp src dev/cljw/kernel_bench.clj
(require '[desargues.physics :as phys])

(defn now-ms [] (/ (double (System/nanoTime)) 1e6))

(def k (wasm/load "dev/cljw/kernel/pendulum.wasm"))
(def buf (wasm/call k "buf_addr"))

(defn kernel-samples
  "theta/omega every 10 steps of 800 RK4 steps (dt 0.01), as the deck samples."
  []
  (wasm/call k "set_params" (/ 9.8 3.0) 0.12 0.01)
  (let [n (wasm/call k "pendulum_rk4" 1.1 0.0 800 10)]
    (partition 2 (wasm/mem-read k :f64 buf (* 2 n)))))

(def p (phys/make-pendulum {:length 3.0 :damping 0.12 :initial-theta 1.1 :top-point [0 2.2 0]}))

(defn bench [label n f]
  (f)
  (let [t0 (now-ms)]
    (dotimes [_ n] (f))
    (println (format "%-36s %10.4f ms/call (n=%d)" label (/ (- (now-ms) t0) n) n))))

(let [ks (kernel-samples)]
  (spit "target/cljw/kernel.edn" (pr-str (mapv vec ks)))
  (println "kernel samples:" (count ks) "first three:" (vec (take 3 ks))
           "-> target/cljw/kernel.edn"))
(bench "zig wasm kernel, 800 RK4 + read 81" 1000 #(count (kernel-samples)))
(bench "wasm/call crossing only (buf_cap)" 10000 #(wasm/call k "buf_cap"))
(bench "interpreted phys/evolve, 800 RK4" 20 #(count (phys/evolve p {:dt 0.01 :duration 8.0})))
