(ns desargues.typecheck
  "Runnable Typed Clojure gate: `lein typecheck`.

   Checks the statically-typed core: the contracts (types, protocols), the
   Emmy abstraction barrier (math), and the contract-annotated services layer.

   math-expression and number-theory are intentionally excluded: they trip
   FATAL internal bugs in Typed Clojure 1.3.0 (IFrees not implemented for
   TopFunction on ann-record, and a checker StackOverflow over defrecord+Emmy
   types), which annotation cannot work around. Those namespaces stay
   runtime-verified (test/desargues/domain/pure_test.clj) and carry their
   ann-record/ann-protocol contracts as documentation."
  (:require [typed.clojure :as t]))

(def checked-nses
  '[desargues.types
    desargues.domain.protocols
    desargues.domain.math
    desargues.domain.services])

(defn -main [& args]
  (let [nses (if (seq args) (map symbol args) checked-nses)
        results (doall
                 (for [n nses]
                   (let [r (try (t/check-ns-clj n)
                                (catch Throwable e [:crash (.getName (class e))]))]
                     (println (format "  %-38s => %s" n (pr-str r)))
                     [n r])))
        bad (remove (comp #{:ok} second) results)]
    (if (seq bad)
      (do (println (count bad) "namespace(s) failed typecheck.")
          (shutdown-agents)
          (System/exit 1))
      (do (println "All" (count results) "namespaces type-check :ok.")
          (shutdown-agents)
          (System/exit 0)))))
