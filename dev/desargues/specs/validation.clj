(ns desargues.specs.validation
  "Validation helpers for runtime spec checking.
   
   Provides:
   - Configurable validation (enable/disable for performance)
   - Exception-throwing validators
   - Human-readable error messages
   - Integration with the spec system
   
   Use these helpers at API boundaries and during development."
  (:require [clojure.spec.alpha :as s]))

;; =============================================================================
;; Dynamic Configuration
;; =============================================================================

(def ^:dynamic *validate*
  "When true, validation is performed. Set to false for production performance."
  true)

(def ^:dynamic *throw-on-invalid*
  "When true, validation failures throw exceptions. When false, returns nil."
  true)

(def ^:dynamic *explain-on-invalid*
  "When true, prints explanation on validation failure (in addition to throwing)."
  false)

;; =============================================================================
;; Core Validation Functions
;; =============================================================================

(defn valid?
  "Check if value conforms to spec. Returns boolean.
   Respects *validate* flag - returns true when validation disabled."
  [spec value]
  (if *validate*
    (s/valid? spec value)
    true))

(defn explain-str
  "Get human-readable explanation of why value doesn't conform to spec."
  [spec value]
  (s/explain-str spec value))

(defn explain-data
  "Get structured data explaining why value doesn't conform to spec."
  [spec value]
  (s/explain-data spec value))

(defn conform
  "Conform value to spec. Returns ::s/invalid if doesn't conform.
   Respects *validate* flag - returns value unchanged when disabled."
  [spec value]
  (if *validate*
    (s/conform spec value)
    value))

;; =============================================================================
;; Validation Macros
;; =============================================================================

(defmacro validate!
  "Validate value against spec. Throws if invalid (when *throw-on-invalid*).
   Returns value if valid, allowing use in threading macros.
   
   Example:
     (-> opts
         (validate! ::pendulum-opts)
         make-pendulum)"
  [spec value]
  `(if *validate*
     (if (s/valid? ~spec ~value)
       ~value
       (let [explain-data# (s/explain-data ~spec ~value)]
         (when *explain-on-invalid*
           (s/explain ~spec ~value))
         (if *throw-on-invalid*
           (throw (ex-info (str "Spec validation failed: " ~(str spec))
                           {:spec ~(str spec)
                            :value ~value
                            :explain explain-data#}))
           nil)))
     ~value))

(defmacro validate-args!
  "Validate function arguments against spec.
   Use at the start of public functions.
   
   Example:
     (defn make-pendulum [opts]
       (validate-args! ::pendulum-opts opts)
       ...)"
  [spec value]
  `(validate! ~spec ~value))

(defmacro validate-return!
  "Validate function return value against spec.
   Use before returning from public functions.
   
   Example:
     (defn compute-result [x]
       (-> (do-computation x)
           (validate-return! ::result)))"
  [spec value]
  `(validate! ~spec ~value))

;; =============================================================================
;; Assertion-Style Validators
;; =============================================================================

(defn assert-valid!
  "Assert that value conforms to spec.
   Always throws on failure, regardless of *throw-on-invalid*.
   Use for invariants that must always hold."
  [spec value message]
  (when-not (s/valid? spec value)
    (throw (ex-info (str message ": " (s/explain-str spec value))
                    {:spec (str spec)
                     :value value
                     :explain (s/explain-data spec value)}))))

(defn check-invariant!
  "Check an invariant condition. Throws if predicate returns false.
   Use for conditions that should always be true."
  [pred value message]
  (when-not (pred value)
    (throw (ex-info message {:value value}))))

;; =============================================================================
;; Optional Validation (Returns Option Type)
;; =============================================================================

(defn validate-option
  "Validate value, returning {:ok value} or {:error explain-data}.
   Never throws."
  [spec value]
  (if (s/valid? spec value)
    {:ok value}
    {:error (s/explain-data spec value)}))

(defn ok? [{:keys [ok]}]
  "Check if validation result is ok."
  (some? ok))

(defn error? [{:keys [error]}]
  "Check if validation result is an error."
  (some? error))

(defn unwrap!
  "Unwrap validation result. Throws if error."
  [{:keys [ok error] :as result}]
  (if error
    (throw (ex-info "Validation failed" {:explain error}))
    ok))

;; =============================================================================
;; Validation Context Management
;; =============================================================================

(defmacro with-validation
  "Execute body with validation enabled."
  [& body]
  `(binding [*validate* true]
     ~@body))

(defmacro without-validation
  "Execute body with validation disabled (for performance)."
  [& body]
  `(binding [*validate* false]
     ~@body))

(defmacro with-strict-validation
  "Execute body with strict validation (throws + explains)."
  [& body]
  `(binding [*validate* true
             *throw-on-invalid* true
             *explain-on-invalid* true]
     ~@body))

;; =============================================================================
;; Spec Registration Helpers
;; =============================================================================

(defn describe-spec
  "Get a human-readable description of a spec."
  [spec-key]
  (when-let [form (s/form spec-key)]
    {:key spec-key
     :form form
     :doc (when-let [doc (-> spec-key s/registry meta :doc)]
            doc)}))

(defn list-specs
  "List all registered specs matching a namespace prefix."
  [ns-prefix]
  (->> (s/registry)
       keys
       (filter #(when (keyword? %)
                  (.startsWith (namespace %) ns-prefix)))
       sort))

;; =============================================================================
;; Validation Statistics (for debugging)
;; =============================================================================

(def ^:private validation-stats (atom {:passes 0 :failures 0}))

(defn reset-validation-stats!
  "Reset validation statistics."
  []
  (reset! validation-stats {:passes 0 :failures 0}))

(defn get-validation-stats
  "Get current validation statistics."
  []
  @validation-stats)

(defn validate-with-stats!
  "Validate and track statistics. Useful for debugging validation frequency."
  [spec value]
  (if (s/valid? spec value)
    (do (swap! validation-stats update :passes inc)
        value)
    (do (swap! validation-stats update :failures inc)
        (when *throw-on-invalid*
          (throw (ex-info "Validation failed"
                          {:spec (str spec)
                           :explain (s/explain-data spec value)})))
        nil)))
