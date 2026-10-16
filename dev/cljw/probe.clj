;; Load probe: try each candidate pure namespace under the current runtime
;; (cljw or JVM) and print one line per namespace: ok/fail, ms, first error line.
;; Run: cljw -cp src dev/cljw/probe.clj   |   clojure -M dev/cljw/probe.clj
(def candidates
  '[desargues.domain.protocols
    desargues.domain.math
    desargues.domain.math-expression
    desargues.domain.services
    desargues.domain.number-theory
    desargues.domain.number-theory-services
    desargues.scene.protocols
    desargues.scene.data
    desargues.scene
    desargues.layout.core
    desargues.geometry.projective
    desargues.geometry.disposition
    desargues.geometry.fit
    desargues.geometry.scene
    desargues.pipeline.emmy
    desargues.physics.protocols
    desargues.physics.systems
    desargues.physics.integrators
    desargues.physics.stepper
    desargues.physics.trajectory
    desargues.physics
    desargues.brachistochrone-pure
    emmy.env])

(defn now-ms [] (/ (double (System/nanoTime)) 1e6))

(defn first-line [s]
  (let [s (str s)
        i (.indexOf s "\n")]
    (if (neg? i) s (subs s 0 i))))

(doseq [n candidates]
  (let [t0 (now-ms)
        r (try (require n) :ok
               (catch Throwable e
                 (str "FAIL " (first-line (or (ex-message e) (str e))))))
        dt (- (now-ms) t0)]
    (println (format "%-42s %8.1f ms  %s" (str n) dt (str r)))))
