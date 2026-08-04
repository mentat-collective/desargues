# ADR-001: Code Quality Standards and Architectural Patterns

**Status:** Accepted  
**Date:** 2025-12-13  
**Context:** Desargues Project Phase 2 Architectural Refinement  

---

## Summary

This Architecture Decision Record (ADR) documents the code quality standards, architectural patterns, and conventions established for the Desargues codebase. **This document is specifically designed for LLMs** to understand and follow when implementing new features or modifying existing code.

---

## Decision Drivers

1. **Maintainability**: Code should be easy to understand, modify, and extend
2. **Testability**: All code should be testable in isolation
3. **Extensibility**: New features should be addable without modifying existing code
4. **Consistency**: Patterns should be applied uniformly across the codebase

---

## Architectural Principles

### 1. SOLID Principles in Clojure

#### Single Responsibility Principle (SRP)
- **Each namespace has ONE reason to change**
- Split large namespaces by responsibility
- Pure API facades vs stateful workflow namespaces

```clojure
;; GOOD: core.clj is pure API facade
(ns desargues.devx.core
  "API facade - no mutable state, only delegates to services")

;; GOOD: repl.clj contains stateful REPL workflow
(ns desargues.devx.repl
  "Stateful REPL workflow - file watching, hot-reload")
```

#### Open/Closed Principle (OCP)
- **Use registry patterns for extensibility**
- New behaviors via registration, not modification

```clojure
;; GOOD: Quality presets are extensible via registration
(defonce preset-registry (atom {}))

(defn register-preset!
  "Register a new quality preset without modifying existing code"
  [preset-key settings]
  {:pre [(keyword? preset-key)
         (map? settings)]}
  (swap! preset-registry assoc preset-key settings)
  preset-key)

;; Usage: Add new preset without touching existing code
(register-preset! :4k {:quality "fourk_quality" :fps 60 :height 2160})
```

#### Interface Segregation Principle (ISP)
- **Small, focused protocols**
- Clients depend only on methods they use

```clojure
;; GOOD: Separate protocols for different concerns
(defprotocol IRenderBackend
  (backend-name [this])
  (render-segment [this segment opts])
  (preview-segment [this segment]))

(defprotocol IGraphRepository
  (save-graph [this id graph])
  (load-graph [this id]))

;; BAD: One giant protocol with everything
(defprotocol IEverything
  (render [this])
  (save [this])
  (validate [this])
  (transform [this])
  ...)
```

---

## Design Patterns

### 2. Protocol-Based Strategy Pattern

**When to use:** Multiple interchangeable implementations of a behavior.

```clojure
;; 1. Define protocol
(defprotocol IRenderBackend
  "Strategy interface for rendering backends"
  (backend-name [this] "Returns keyword identifier")
  (init-backend! [this] "Initialize the backend")
  (render-segment [this segment opts] "Render a segment"))

;; 2. Implement with defrecord
(defrecord ManimBackend [initialized? config]
  IRenderBackend
  (backend-name [_] :manim)
  (init-backend! [this]
    (reset! initialized? true)
    this)
  (render-segment [_ segment opts]
    ;; Implementation
    ))

;; 3. Registry for runtime selection
(defonce backend-registry (atom {}))

(defn register-backend!
  [backend]
  {:pre [(satisfies? IRenderBackend backend)]}
  (swap! backend-registry assoc (backend-name backend) backend))

;; 4. Factory function
(defn create-manim-backend
  ([] (create-manim-backend {}))
  ([config]
   (->ManimBackend (atom false) config)))
```

### 3. Observer Pattern for Events

**When to use:** Decouple event producers from consumers.

```clojure
;; 1. Event registry
(defonce observer-registry (atom {}))

;; 2. Registration with optional filters
(defn register-observer!
  "Register an observer with optional event filtering"
  [observer-id handler-fn & {:keys [filter]}]
  {:pre [(keyword? observer-id)
         (fn? handler-fn)]}
  (swap! observer-registry assoc observer-id
         {:handler handler-fn
          :filter (or filter (constantly true))})
  observer-id)

;; 3. Emit with error isolation
(defn emit!
  "Emit event to all matching observers. Errors are isolated."
  [event]
  {:pre [(map? event)
         (keyword? (:type event))]}
  (doseq [[_id {:keys [handler filter]}] @observer-registry]
    (when (filter event)
      (try
        (handler event)
        (catch Exception e
          ;; Log but don't propagate - observer errors shouldn't break emitter
          (println (format "Observer error: %s" (.getMessage e))))))))

;; 4. Pre-built observers for common cases
(defn logging-observer
  "Creates observer that logs events"
  [log-fn]
  (fn [event]
    (log-fn (format "[%s] %s" (:type event) (:description event "")))))
```

### 4. Repository Pattern for Persistence

**When to use:** Abstract data storage from domain logic.

```clojure
;; 1. Repository protocol
(defprotocol IGraphRepository
  "Repository interface for graph persistence"
  (repo-name [this] "Returns repository identifier")
  (save-graph [this id graph] "Persist graph, returns id")
  (load-graph [this id] "Load graph by id, returns graph or nil")
  (graph-exists? [this id] "Check if graph exists")
  (list-graphs [this] "List all graph ids")
  (delete-graph [this id] "Delete graph, returns boolean"))

;; 2. In-memory implementation (for testing/development)
(defrecord MemoryRepository [storage]
  IGraphRepository
  (repo-name [_] :memory)
  (save-graph [_ id graph]
    (swap! storage assoc id graph)
    id)
  (load-graph [_ id]
    (get @storage id))
  (graph-exists? [_ id]
    (contains? @storage id))
  (list-graphs [_]
    (keys @storage))
  (delete-graph [_ id]
    (let [existed (contains? @storage id)]
      (swap! storage dissoc id)
      existed)))

;; 3. Factory function
(defn create-memory-repository
  "Create an in-memory repository (useful for tests)"
  []
  (->MemoryRepository (atom {})))
```

### 5. Specification Pattern for Queries

**When to use:** Composable, reusable query predicates.

```clojure
;; 1. Specification protocol
(defprotocol ISpecification
  "Specification pattern for composable predicates"
  (satisfied? [this segment] "Test if segment satisfies specification")
  (spec-description [this] "Human-readable description"))

;; 2. Composite specifications
(defrecord AndSpec [specs]
  ISpecification
  (satisfied? [_ segment]
    (every? #(satisfied? % segment) specs))
  (spec-description [_]
    (str "(" (clojure.string/join " AND " (map spec-description specs)) ")")))

(defrecord OrSpec [specs]
  ISpecification
  (satisfied? [_ segment]
    (some #(satisfied? % segment) specs))
  (spec-description [_]
    (str "(" (clojure.string/join " OR " (map spec-description specs)) ")")))

(defrecord NotSpec [spec]
  ISpecification
  (satisfied? [_ segment]
    (not (satisfied? spec segment)))
  (spec-description [_]
    (str "NOT " (spec-description spec))))

;; 3. Factory functions for composition
(defn and-spec [& specs] (->AndSpec specs))
(defn or-spec [& specs] (->OrSpec specs))
(defn not-spec [spec] (->NotSpec spec))

;; 4. Concrete specifications
(defrecord DirtySpec []
  ISpecification
  (satisfied? [_ segment]
    (= :dirty (:state segment)))
  (spec-description [_] "dirty?"))

(defn dirty? [] (->DirtySpec))

;; 5. Query function
(defn find-segments
  "Find all segments matching specification"
  [graph spec]
  (filter #(satisfied? spec %) (vals (:segments graph))))

;; Usage:
;; (find-segments graph (and-spec (dirty?) (independent?)))
```

---

## Naming Conventions

### 6. Bang Convention for Side Effects

**CRITICAL:** The `!` suffix indicates side effects. This is non-negotiable.

```clojure
;; SIDE EFFECTS (use bang!)
(defn register-preset!   [k v] (swap! registry assoc k v))  ; Mutates atom
(defn emit!              [event] ...)                        ; Triggers handlers
(defn init-backend!      [backend] ...)                      ; Initializes state
(defn save-graph         [repo id g] ...)                    ; I/O operation

;; PURE FUNCTIONS (no bang)
(defn resolve-quality    [q] ...)                            ; Pure lookup
(defn find-segments      [g spec] ...)                       ; Pure filter
(defn create-segment     [id expr] ...)                      ; Pure construction
(defn satisfied?         [spec seg] ...)                     ; Pure predicate

;; EXCEPTION: Protocol methods that are pure queries
(defprotocol IGraphRepository
  (load-graph [this id])    ; I/O but protocol method - bang optional
  (graph-exists? [this id])) ; Query - no bang
```

### 7. Function Naming Patterns

| Pattern | Meaning | Example |
|---------|---------|---------|
| `create-*` | Factory function | `create-segment`, `create-memory-repository` |
| `register-*!` | Add to registry | `register-preset!`, `register-backend!` |
| `unregister-*!` | Remove from registry | `unregister-observer!` |
| `get-*` | Lookup/retrieve | `get-preset`, `get-backend` |
| `resolve-*` | Polymorphic lookup | `resolve-quality` |
| `find-*` | Query/search | `find-segments` |
| `*?` | Predicate | `dirty?`, `graph-exists?`, `satisfied?` |
| `*->*` | Conversion | `segment->render-opts` |
| `with-*` | Scoped binding macro | `with-backend` |

---

## Code Organization

### 8. Namespace Structure

```
src/desargues/devx/
├── core.clj          ; API facade (public interface)
├── repl.clj          ; Stateful REPL workflow
├── graph.clj         ; Graph data structure
├── segment.clj       ; Segment operations
├── renderer.clj      ; Rendering coordination
├── quality.clj       ; Quality presets (OCP)
├── events.clj        ; Observer pattern
├── specs.clj         ; Specification pattern
├── repository.clj    ; Repository pattern
├── backend.clj       ; Strategy pattern interface
└── backend/
    └── manim.clj     ; Manim implementation
```

### 9. Namespace Header Template

```clojure
(ns desargues.devx.module-name
  "Single-line description of responsibility.
  
  Longer description if needed, explaining:
  - Primary purpose
  - Key abstractions
  - Usage patterns"
  (:require
   ;; Clojure core libraries first
   [clojure.string :as str]
   ;; Third-party libraries
   [libpython-clj2.python :as py]
   ;; Project namespaces (alphabetical)
   [desargues.devx.protocols :as p]
   [desargues.devx.segment :as seg]))
```

---

## Testing Standards

### 10. Test File Structure

```clojure
(ns desargues.devx.module-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [desargues.devx.module :as sut]))  ; sut = System Under Test

;; Fixtures for setup/teardown
(defn reset-state [f]
  (let [prev @some-atom]
    (try
      (f)
      (finally
        (reset! some-atom prev)))))

(use-fixtures :each reset-state)

;; Group related tests
(deftest feature-tests
  (testing "happy path"
    (is (= expected (sut/function input))))
  
  (testing "edge case"
    (is (nil? (sut/function nil))))
  
  (testing "error case"
    (is (thrown? Exception (sut/function invalid)))))
```

### 11. Test Requirements

| Requirement | Standard |
|-------------|----------|
| Coverage | Every public function must have tests |
| Assertions | Minimum 2 assertions per test function |
| Naming | `deftest function-name-tests` |
| Structure | Use `testing` for sub-cases |
| Isolation | Tests must not depend on each other |
| Fixtures | Use fixtures for state reset |

### 12. Test Patterns

```clojure
;; Pattern: Registry tests
(deftest registry-tests
  (testing "registration"
    (sut/register-thing! :key value)
    (is (some? (sut/get-thing :key))))
  
  (testing "default exists"
    (is (some? (sut/get-thing :default))))
  
  (testing "unknown returns nil"
    (is (nil? (sut/get-thing :nonexistent)))))

;; Pattern: Protocol implementation tests
(deftest implementation-tests
  (let [impl (sut/create-implementation)]
    (testing "protocol method 1"
      (is (= expected (sut/method-1 impl arg))))
    
    (testing "protocol method 2"
      (is (= expected (sut/method-2 impl arg))))))

;; Pattern: Composition tests (Specification pattern)
(deftest composition-tests
  (let [spec1 (sut/predicate-a)
        spec2 (sut/predicate-b)
        combined (sut/and-spec spec1 spec2)]
    (testing "both satisfied"
      (is (sut/satisfied? combined matching-item)))
    
    (testing "one fails"
      (is (not (sut/satisfied? combined partial-item))))))
```

---

## Error Handling

### 13. Preconditions with :pre

```clojure
(defn register-preset!
  [preset-key settings]
  {:pre [(keyword? preset-key)
         (map? settings)
         (string? (:quality settings))
         (number? (:fps settings))]}
  ;; Implementation
  )

(defn emit!
  [event]
  {:pre [(map? event)
         (keyword? (:type event))]}
  ;; Implementation
  )
```

### 14. Error Isolation in Observers

```clojure
;; Observers should not break emitters
(defn emit! [event]
  (doseq [[id {:keys [handler]}] @observer-registry]
    (try
      (handler event)
      (catch Exception e
        ;; Log but continue - one bad observer shouldn't break others
        (log-error id e)))))
```

---

## State Management

### 15. Atom Usage Patterns

```clojure
;; Use defonce for registries (survives reloads)
(defonce preset-registry (atom {}))
(defonce observer-registry (atom {}))

;; Use def for current state (can be reset)
(def current-backend (atom nil))

;; Initialization with swap!
(defn init-registries!
  []
  (swap! preset-registry merge default-presets)
  (swap! observer-registry empty))
```

### 16. Thread-Safe Registry Pattern

```clojure
;; Atomic registration
(defn register!
  [key value]
  (swap! registry assoc key value)
  key)  ; Return key for chaining

;; Atomic unregistration
(defn unregister!
  [key]
  (let [result (atom nil)]
    (swap! registry
           (fn [r]
             (reset! result (contains? r key))
             (dissoc r key)))
    @result))  ; Return whether key existed
```

---

## Macros

### 17. Scoped Binding Macro Pattern

```clojure
(defmacro with-backend
  "Execute body with specified backend, restoring previous after"
  [backend & body]
  `(let [prev# (current-backend-impl)
         impl# (get-backend ~backend)]
     (try
       (set-backend-impl! impl#)
       ~@body
       (finally
         (set-backend-impl! prev#)))))

;; IMPORTANT: Macros cannot access private vars at expansion time
;; Always use public accessor functions in macros
```

---

## Documentation

### 18. Docstring Requirements

```clojure
;; Functions: What it does, params, returns
(defn resolve-quality
  "Resolve quality specification to settings map.
  
  Accepts:
  - keyword: looks up in preset registry
  - map: returns as-is (custom settings)
  - IQualityProvider: calls quality-settings protocol
  
  Returns settings map or nil if not found."
  [quality]
  ...)

;; Protocols: Purpose and contract
(defprotocol IRenderBackend
  "Strategy interface for rendering backends.
  
  Implementations must be thread-safe and handle their own initialization."
  (backend-name [this] "Returns keyword identifier for this backend")
  (render-segment [this segment opts] "Render segment, returns updated segment"))
```

---

## Checklist for New Features

When implementing a new feature, verify:

- [ ] **SRP**: Does each namespace have a single responsibility?
- [ ] **OCP**: Can this be extended without modification? (Use registries)
- [ ] **ISP**: Are protocols small and focused?
- [ ] **Bang Convention**: Do all side-effecting functions end with `!`?
- [ ] **Preconditions**: Are inputs validated with `:pre`?
- [ ] **Tests**: Does every public function have tests?
- [ ] **Docstrings**: Are all public functions documented?
- [ ] **Fixtures**: Do tests clean up state?
- [ ] **Error Isolation**: Do observers/handlers isolate errors?
- [ ] **Thread Safety**: Is atom usage atomic?

---

## Anti-Patterns to Avoid

### DON'T: Giant protocols
```clojure
;; BAD
(defprotocol IEverything
  (render [this])
  (save [this])
  (load [this])
  (validate [this])
  (transform [this]))
```

### DON'T: Direct atom access in macros
```clojure
;; BAD - private var access fails at macro expansion
(defmacro with-thing [& body]
  `(let [prev# @private-atom]  ; Will fail!
     ...))
```

### DON'T: Missing bang on side effects
```clojure
;; BAD - misleading, looks pure
(defn register-observer [id handler]
  (swap! registry assoc id handler))  ; SIDE EFFECT!

;; GOOD
(defn register-observer! [id handler]
  (swap! registry assoc id handler))
```

### DON'T: Hardcoded behavior
```clojure
;; BAD - not extensible
(defn get-quality [k]
  (case k
    :low {:fps 15}
    :high {:fps 60}))

;; GOOD - extensible via registry
(defn get-preset [k]
  (get @preset-registry k))
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2025-12-13 | Initial ADR from Phase 2 refactoring |

---

## References

- [SOLID Principles](https://en.wikipedia.org/wiki/SOLID)
- [Domain-Driven Design](https://en.wikipedia.org/wiki/Domain-driven_design)
- [Gang of Four Design Patterns](https://en.wikipedia.org/wiki/Design_Patterns)
- [Clojure Style Guide](https://github.com/bbatsov/clojure-style-guide)
