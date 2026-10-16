(ns user
  "Development namespace with REPL helpers for type safety and testing.
   
   This namespace is automatically loaded when starting a REPL in the :dev profile.
   It provides convenient functions for:
   - Instrumenting functions with specs
   - Running type checks
   - Exploring specs
   - Running generative tests"
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.repl :refer [doc source]]
            [clojure.pprint :refer [pprint]]))

;; =============================================================================
;; Lazy Loading (avoid slow startup)
;; =============================================================================

(defn require-specs!
  "Load all spec namespaces."
  []
  (require '[desargues.specs.common :as specs.common])
  (require '[desargues.specs.physics :as specs.physics])
  (require '[desargues.specs.api :as specs.api])
  (require '[desargues.specs.generators :as specs.generators])
  (require '[desargues.specs.validation :as specs.validation])
  (println "✓ Specs loaded"))

(defn require-types!
  "Load Typed Clojure namespaces."
  []
  (require '[typed.clojure :as t])
  (require '[desargues.types :as types])
  (println "✓ Types loaded"))

(defn require-physics!
  "Load physics simulation namespace."
  []
  (require '[desargues.physics :as phys])
  (println "✓ Physics loaded"))

(defn require-all!
  "Load all namespaces for development."
  []
  (require-specs!)
  (require-types!)
  (require-physics!)
  (println "✓ All namespaces loaded"))

;; =============================================================================
;; Spec Instrumentation
;; =============================================================================

(defn instrument-all!
  "Instrument all spec'd functions for runtime checking.
   Use during development to catch spec violations."
  []
  (require-specs!)
  (let [instrumented (stest/instrument)]
    (println (str "✓ Instrumented " (count instrumented) " functions"))
    instrumented))

(defn unstrument-all!
  "Remove instrumentation from all functions.
   Use for production or performance testing."
  []
  (let [unstrumented (stest/unstrument)]
    (println (str "✓ Unstrumented " (count unstrumented) " functions"))
    unstrumented))

(defn instrument-ns!
  "Instrument all spec'd functions in a namespace.
   Example: (instrument-ns! 'desargues.physics)"
  [ns-sym]
  (require-specs!)
  (let [syms (->> (stest/instrumentable-syms)
                  (filter #(= (namespace %) (str ns-sym))))]
    (stest/instrument syms)
    (println (str "✓ Instrumented " (count syms) " functions in " ns-sym))
    syms))

;; =============================================================================
;; Type Checking
;; =============================================================================

(defn typecheck!
  "Run Typed Clojure type checker on core namespaces.
   Returns check results or throws on type errors."
  []
  (require-types!)
  (require '[typed.clojure :as t])
  (println "Running type checker...")
  (let [check-ns (resolve 't/check-ns)]
    (try
      (check-ns 'desargues.domain.protocols)
      (check-ns 'desargues.domain.math-expression)
      (println "✓ Type check passed")
      :ok
      (catch Exception e
        (println "✗ Type check failed:")
        (println (.getMessage e))
        :failed))))

(defn typecheck-ns!
  "Type check a specific namespace.
   Example: (typecheck-ns! 'desargues.domain.protocols)"
  [ns-sym]
  (require-types!)
  (require '[typed.clojure :as t])
  (let [check-ns (resolve 't/check-ns)]
    (check-ns ns-sym)))

;; =============================================================================
;; Spec Exploration
;; =============================================================================

(defn list-specs
  "List all specs in a namespace.
   Example: (list-specs \"desargues.specs.physics\")"
  [ns-prefix]
  (->> (s/registry)
       keys
       (filter #(and (keyword? %)
                     (some-> (namespace %) (.startsWith ns-prefix))))
       sort
       vec))

(defn describe-spec
  "Get detailed information about a spec.
   Example: (describe-spec ::specs.physics/pendulum-opts)"
  [spec-key]
  {:key spec-key
   :form (s/form spec-key)
   :description (s/describe spec-key)})

(defn explain
  "Explain why a value doesn't conform to a spec.
   Example: (explain ::specs.physics/length -5)"
  [spec value]
  (s/explain spec value))

(defn valid?
  "Check if a value conforms to a spec.
   Example: (valid? ::specs.physics/length 5.0)"
  [spec value]
  (s/valid? spec value))

;; =============================================================================
;; Generative Testing
;; =============================================================================

(defn check-fn
  "Run generative tests on a single function.
   Example: (check-fn `desargues.physics/make-pendulum)"
  [fn-sym & {:keys [num-tests] :or {num-tests 100}}]
  (require-specs!)
  (require '[desargues.specs.generators :as g])
  ((resolve 'g/register-generators!))
  (stest/check fn-sym {:clojure.spec.test.check/opts {:num-tests num-tests}}))

(defn check-ns
  "Run generative tests on all spec'd functions in a namespace.
   Example: (check-ns 'desargues.physics)"
  [ns-sym & {:keys [num-tests] :or {num-tests 50}}]
  (require-specs!)
  (require '[desargues.specs.generators :as g])
  ((resolve 'g/register-generators!))
  (let [syms (->> (stest/checkable-syms)
                  (filter #(= (namespace %) (str ns-sym))))]
    (stest/check syms {:clojure.spec.test.check/opts {:num-tests num-tests}})))

(defn summarize-check
  "Summarize results of generative testing.
   Example: (summarize-check (check-ns 'desargues.physics))"
  [results]
  (let [passed (filter #(true? (-> % :clojure.spec.test.check/ret :pass?)) results)
        failed (remove #(true? (-> % :clojure.spec.test.check/ret :pass?)) results)]
    {:total (count results)
     :passed (count passed)
     :failed (count failed)
     :failures (mapv :sym failed)}))

;; =============================================================================
;; Physics Testing Helpers
;; =============================================================================

(defn test-pendulum
  "Create and evolve a test pendulum.
   Returns trajectory for inspection."
  [& {:keys [length gravity damping initial-theta duration]
      :or {length 1.0 gravity 9.8 damping 0.0 initial-theta 0.3 duration 5.0}}]
  (require-physics!)
  (let [make-pendulum (resolve 'phys/make-pendulum)
        evolve (resolve 'phys/evolve)
        pendulum (make-pendulum {:length length
                                 :gravity gravity
                                 :damping damping
                                 :initial-theta initial-theta})]
    {:pendulum pendulum
     :trajectory (evolve pendulum {:dt 0.01 :duration duration})}))

(defn test-spring
  "Create and evolve a test spring-mass system.
   Returns trajectory for inspection."
  [& {:keys [k mu mass initial-x duration]
      :or {k 3.0 mu 0.0 mass 1.0 initial-x 1.0 duration 5.0}}]
  (require-physics!)
  (let [make-spring (resolve 'phys/make-spring-mass)
        evolve (resolve 'phys/evolve)
        spring (make-spring {:k k :mu mu :mass mass :initial-x initial-x})]
    {:spring spring
     :trajectory (evolve spring {:dt 0.01 :duration duration})}))

;; =============================================================================
;; Validation Helpers
;; =============================================================================

(defmacro with-validation
  "Execute body with validation enabled.
   Example: (with-validation (make-pendulum {:length -5}))"
  [& body]
  `(do
     (require '[desargues.specs.validation :as v#])
     (binding [desargues.specs.validation/*validate* true
               desargues.specs.validation/*throw-on-invalid* true
               desargues.specs.validation/*explain-on-invalid* true]
       ~@body)))

(defmacro without-validation
  "Execute body with validation disabled (for performance).
   Example: (without-validation (do-heavy-computation))"
  [& body]
  `(do
     (require '[desargues.specs.validation :as v#])
     (binding [desargues.specs.validation/*validate* false]
       ~@body)))

;; =============================================================================
;; REPL Startup Message
;; =============================================================================

(defn help
  "Show available development commands."
  []
  (println "
╔══════════════════════════════════════════════════════════════════╗
║                   Desargues Development REPL                      ║
╠══════════════════════════════════════════════════════════════════╣
║  Loading:                                                         ║
║    (require-all!)       - Load all namespaces                    ║
║    (require-specs!)     - Load spec namespaces                   ║
║    (require-types!)     - Load Typed Clojure namespaces          ║
║    (require-physics!)   - Load physics namespace                 ║
║                                                                   ║
║  Instrumentation:                                                 ║
║    (instrument-all!)    - Enable runtime spec checking           ║
║    (unstrument-all!)    - Disable spec checking                  ║
║    (instrument-ns! ns)  - Instrument specific namespace          ║
║                                                                   ║
║  Type Checking:                                                   ║
║    (typecheck!)         - Run Typed Clojure on core namespaces   ║
║    (typecheck-ns! ns)   - Type check specific namespace          ║
║                                                                   ║
║  Spec Exploration:                                                ║
║    (list-specs \"prefix\")  - List specs in namespace            ║
║    (describe-spec key)     - Describe a spec                     ║
║    (valid? spec val)       - Check if value conforms             ║
║    (explain spec val)      - Explain non-conformance             ║
║                                                                   ║
║  Generative Testing:                                              ║
║    (check-fn 'fn-sym)      - Test a function generatively        ║
║    (check-ns 'ns)          - Test all functions in namespace     ║
║    (summarize-check res)   - Summarize test results              ║
║                                                                   ║
║  Physics Testing:                                                 ║
║    (test-pendulum :length 2.0 :damping 0.1)                      ║
║    (test-spring :k 5.0 :mu 0.2)                                  ║
║                                                                   ║
║  Quick Start:                                                     ║
║    (require-all!)                                                 ║
║    (instrument-all!)                                              ║
║    (typecheck!)                                                   ║
╚══════════════════════════════════════════════════════════════════╝
"))

(println "\nDesargues Development REPL loaded. Type (help) for commands.\n")
