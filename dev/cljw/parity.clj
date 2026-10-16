;; Compare the cljw and JVM pendulum scene graphs: identical structure, numbers
;; within 1e-9. Run on the JVM after both pendulum_scene runs:
;;   clojure -M dev/cljw/parity.clj
(require '[clojure.edn :as edn])

(defn diff-paths
  "Paths where a and b differ (numbers compared with tolerance)."
  [a b path]
  (cond
    (and (number? a) (number? b))
    (if (< (Math/abs (- (double a) (double b))) 1e-9) [] [[path a b]])

    (and (map? a) (map? b))
    (if (not= (set (keys a)) (set (keys b)))
      [[path :keys (set (keys a)) (set (keys b))]]
      (mapcat #(diff-paths (get a %) (get b %) (conj path %)) (keys a)))

    (and (sequential? a) (sequential? b))
    (if (not= (count a) (count b))
      [[path :count (count a) (count b)]]
      (mapcat #(diff-paths %1 %2 (conj path %3)) a b (range)))

    :else (if (= a b) [] [[path a b]])))

(let [cw  (edn/read-string (slurp "target/cljw/pendulum.cljw.edn"))
      jvm (edn/read-string (slurp "target/cljw/pendulum.jvm.edn"))
      ds  (diff-paths cw jvm [])]
  (println (format "steps cljw=%d jvm=%d; differing leaves: %d"
                   (count (:steps cw)) (count (:steps jvm)) (count ds)))
  (doseq [d (take 5 ds)] (prn d))
  ;; The Zig wasm kernel (dev/cljw/kernel_bench.clj) against JVM phys/evolve.
  (require '[desargues.physics :as phys])
  (when (.exists (java.io.File. "target/cljw/kernel.edn"))
    (let [ks  (edn/read-string (slurp "target/cljw/kernel.edn"))
          p   ((resolve 'phys/make-pendulum)
               {:length 3.0 :damping 0.12 :initial-theta 1.1 :top-point [0 2.2 0]})
          jvm (mapv (comp (juxt :theta :omega) :state)
                    (take-nth 10 ((resolve 'phys/evolve) p {:dt 0.01 :duration 8.0})))
          n   (min (count ks) (count jvm))
          err (reduce max (map (fn [a b]
                                 (max (Math/abs (- (double (first a)) (double (first b))))
                                      (Math/abs (- (double (second a)) (double (second b))))))
                               (take n ks) (take n jvm)))]
      (println (format "kernel vs JVM evolve: %d/%d samples compared, max |error| %.3e"
                       n (count jvm) err)))))
