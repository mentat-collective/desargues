;; End-to-end on either runtime: integrate the landing-page damped pendulum
;; (phys/evolve, RK4, dt 0.01, 8 s), record it through the scene facade on the
;; RecordingBackend, write the EDN scene graph, print timings.
;;   cljw:  dev/cljw/run.sh pendulum        -> target/cljw/pendulum.cljw.edn
;;   JVM:   clojure -M dev/cljw/pendulum_scene.clj jvm -> target/cljw/pendulum.jvm.edn
;; The construct fn is site/desargues/site/deck.clj pendulum-construct, verbatim.
(require '[desargues.scene :as s]
         '[desargues.scene.data :as rec]
         '[desargues.videos.physics :as phys]
         '[clojure.java.io :as io])

(def tag (or (first *command-line-args*) "cljw"))

(defn now-ms [] (/ (double (System/nanoTime)) 1e6))

(def pendulum
  (phys/make-pendulum {:length 3.0 :damping 0.12 :initial-theta 1.1 :top-point [0 2.2 0]}))

(defn pendulum-construct [stage]
  (let [samples (take-nth 10 (phys/evolve pendulum {:dt 0.01 :duration 8.0}))
        pts     (mapv (fn [{:keys [state]}]
                        (let [[x y] (phys/pendulum-position pendulum state)] [x y]))
                      samples)
        top     [0 2.2]
        title   (-> (s/text "Damped pendulum: phys/evolve, RK4 at dt = 0.01" :font-size 30 :color :white)
                    (s/move-to [0 3.3]))
        pivot   (-> (s/dot :color :grey) (s/move-to top))
        rod     (s/line top (first pts) :color :grey :width 4)
        bob     (-> (s/circle :radius 0.32 :color :gold)
                    (s/move-to (first pts))
                    (s/fill! :gold :opacity 0.7))
        clock   (-> (s/decimal 0 :font-size 30 :color :grey :num-decimal-places 1)
                    (s/move-to [5.2 -3.0]))
        unit    (-> (s/text "s" :font-size 26 :color :grey) (s/move-to [5.9 -3.0]))]
    (s/play! stage (s/together [(s/appear title :run-time 0.5) (s/appear pivot :run-time 0.5)
                                (s/draw rod :run-time 0.5) (s/draw bob :run-time 0.5)
                                (s/appear clock :run-time 0.5) (s/appear unit :run-time 0.5)]))
    (doseq [[i p] (map-indexed vector (rest pts))]
      (s/play! stage (s/together [(s/connect rod top p :run-time 0.1)
                                  (s/glide bob p :run-time 0.1)
                                  (s/count-to clock (* 0.1 (inc i)) :run-time 0.1)])))
    (s/hold! stage 1)))

(defn record []
  (s/with-backend (rec/recording-backend)
    (s/render! "desargues-pendulum" pendulum-construct)))

(defn bench [label n f]
  (f) ; warm
  (let [t0 (now-ms)]
    (dotimes [_ n] (f))
    (println (format "%-34s %9.3f ms/call (n=%d)" label (/ (- (now-ms) t0) n) n))))

(let [t0    (now-ms)
      graph (record)
      dt    (- (now-ms) t0)
      out   (str "target/cljw/pendulum." tag ".edn")]
  (io/make-parents out)
  (spit out (pr-str graph))
  (println (format "[%s] first record %.1f ms: %d nodes, %d steps, %d animations -> %s"
                   tag dt (count (:nodes graph)) (count (:steps graph))
                   (rec/animation-count graph) out))
  ;; two rounds: the first is the JIT's warm-up on the JVM, the second steady state.
  ;; NO_BENCH=1 skips them, for a cold time-to-first-graph wall measurement.
  (doseq [round (if (System/getenv "NO_BENCH") [] [1 2])]
    (println "round" round)
    (bench "phys/evolve 8 s (800 RK4 steps)" 50
           #(count (phys/evolve pendulum {:dt 0.01 :duration 8.0})))
    (bench "record whole scene (evolve+facade)" 50 #(count (:steps (record))))))
