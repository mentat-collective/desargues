# Architecture: SOLID + DDD Design

Clean architecture for mathematical animations using SOLID principles and Domain-Driven Design.

## Architecture Overview

```
┌─────────────────────────────────────────────────────┐
│              API Layer (Facade)                     │
│         desargues.api - Clean, simple interface       │
└──────────────────┬──────────────────────────────────┘
                   │
┌──────────────────┴──────────────────────────────────┐
│         Application Layer (Use Cases)                │
│      Workflows, High-level operations                │
└──────────────────┬──────────────────────────────────┘
                   │
┌──────────────────┴──────────────────────────────────┐
│            Domain Layer (Core Logic)                 │
│  ┌─────────────────────────────────────────────┐   │
│  │ Entities: MathExpression, MathFunction      │   │
│  │ Value Objects: LaTeX, PythonCode, Point     │   │
│  │ Services: Mathematical operations            │   │
│  │ Protocols: IMathematicalObject, IEvaluable  │   │
│  └─────────────────────────────────────────────┘   │
└──────────────────┬──────────────────────────────────┘
                   │
┌──────────────────┴──────────────────────────────────┐
│       Infrastructure Layer (External Systems)        │
│  ┌─────────────────────────────────────────────┐   │
│  │ Manim Adapter - Rendering, Animation        │   │
│  │ Emmy Bridge - Symbolic computation          │   │
│  │ Python Bridge - libpython-clj integration   │   │
│  └─────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────┘
```

## SOLID Principles

### Single Responsibility Principle (SRP)

Each class/namespace has one reason to change:

**Domain Layer:**
- `math-expression.clj` - Define mathematical objects
- `protocols.clj` - Define contracts/interfaces
- `services.clj` - Mathematical operations

**Infrastructure:**
- `manim_adapter.clj` - Manim integration only

**API:**
- `api.clj` - User-facing interface only

### Open/Closed Principle (OCP)

Open for extension, closed for modification:

**Protocols allow extension:**
```clojure
;; Define new mathematical object type
(defrecord ComplexNumber [real imag]
  p/IMathematicalObject
  (to-latex [this] ...)
  (simplify [this] ...))

;; No need to modify existing code!
```

**Animation types are extensible:**
```clojure
;; Add new animation type
(p/create-animation expr :CustomAnimation {})
```

### Liskov Substitution Principle (LSP)

Any implementer of a protocol can be substituted:

```clojure
;; Both expressions and functions implement IMathematicalObject
(to-latex math-expression)  ;; Works
(to-latex math-function)     ;; Also works

;; Both can be animated
(animate-expression math-expr)
(animate-expression math-func)
```

### Interface Segregation Principle (ISP)

Clients depend only on interfaces they use:

**Specific protocols:**
- `IMathematicalObject` - Basic math operations
- `IEvaluable` - Only for evaluable objects
- `IDifferentiable` - Only for differentiable objects
- `IRenderable` - Only for visual objects
- `IAnimatable` - Only for animatable objects

**No fat interfaces!** Each protocol is focused.

### Dependency Inversion Principle (DIP)

Depend on abstractions, not concretions:

**Domain depends on protocols, not implementations:**
```clojure
;; Domain service accepts any IEvaluable
(defn evaluate-expression [math-expr point]
  ;; Works with any object implementing IEvaluable
  ...)

;; Infrastructure implements the protocol
(extend-type MathExpression
  p/IEvaluable
  (evaluate [this point] ...))
```

**Dependency injection:**
```clojure
;; Service accepts converter as dependency
(defn expression-to-python [math-expr latex-converter]
  ;; Inject the converter, don't hardcode it
  (latex-converter (expression-to-latex math-expr)))
```

## Domain-Driven Design (DDD)

### Entities

Objects with identity that persist over time:

**MathExpression:**
```clojure
(defrecord MathExpression [id expr latex python-code metadata])
```
- Has unique `id`
- Can change state (add latex, python-code lazily)
- Identity matters

**MathFunction:**
```clojure
(defrecord MathFunction [id name f domain codomain metadata])
```
- Has unique `id` and `name`
- Represents a mathematical function
- Identity matters

### Value Objects

Immutable objects where equality is based on value:

**LaTeX:**
```clojure
(defrecord LaTeX [content])
```
- Immutable
- Two LaTeX objects with same content are equal
- No identity

**Point:**
```clojure
(defrecord Point [variable value])
```
- Represents a point for evaluation
- Immutable
- Value-based equality

### Aggregates

Cluster of entities/value objects treated as a unit:

**Scene** (implicit aggregate root):
- Contains objects (entities)
- Contains animations (value objects)
- Maintains consistency

### Domain Services

Operations that don't naturally belong to an entity:

```clojure
;; Domain services in services.clj
(defn differentiate-expression [math-expr] ...)
(defn compare-at-points [functions points] ...)
(defn tabulate-function [func points] ...)
```

### Repositories

Access to collections of entities:

```clojure
(defprotocol IExpressionRepository
  (save-expression [this expr])
  (find-expression [this id])
  (find-all-expressions [this]))
```

*(Not yet implemented, but protocol defined for future use)*

### Factories

Create complex objects:

```clojure
;; Factory functions
(defn create-expression [expr metadata] ...)
(defn create-function [name f domain codomain metadata] ...)
(defn create-scene-builder [] ...)
```

## Design Patterns Used

### 1. Facade Pattern

**API Layer** provides simple interface to complex subsystem:

```clojure
;; Simple facade
(require '[desargues.api :as api])

(api/init!)
(def e (api/expr '(sin x)))
(api/animate-expression e)
```

Hides complexity of domain, services, and infrastructure.

### 2. Builder Pattern

**Scene construction:**

```clojure
(-> (api/scene)
    (api/show expr1)
    (api/wait 1)
    (api/transform expr1 expr2)
    (api/render!))
```

Fluent API for step-by-step construction.

### 3. Adapter Pattern

**ManimAdapter** adapts Manim to our protocols:

```clojure
(defrecord ManimMobject [py-obj type metadata]
  p/IRenderable
  (to-mobject [this] ...)

  p/IAnimatable
  (create-animation [this type options] ...))
```

### 4. Strategy Pattern

**Protocol-based strategies:**

```clojure
;; Different rendering strategies
(extend-type MathExpression
  p/IRenderable
  (render [this options] ...))

(extend-type MathFunction
  p/IRenderable
  (render [this options] ...))
```

### 5. Factory Pattern

**Factory functions for object creation:**

```clojure
(defn expr [form] ...)
(defn func [name f] ...)
(defn point [var val] ...)
```

### 6. Repository Pattern

**Defined protocols for data access:**

```clojure
(defprotocol IExpressionRepository
  (save-expression [this expr])
  (find-expression [this id]))
```

## Layer Responsibilities

### Domain Layer

**Responsibilities:**
- Define core mathematical concepts
- Pure business logic
- No external dependencies
- Domain services
- Protocols/interfaces

**Files:**
- `domain/math_expression.clj`
- `domain/protocols.clj`
- `domain/services.clj`

**Rules:**
- No I/O
- No UI/rendering concerns
- No framework dependencies
- Pure Clojure + Emmy

### Infrastructure Layer

**Responsibilities:**
- Manim integration
- Python bridging
- File I/O (if needed)
- External system adapters

**Files:**
- `infrastructure/manim_adapter.clj`

**Rules:**
- Implements domain protocols
- Depends on domain, not vice versa
- Contains all Python/Manim code

### Application Layer

**Responsibilities:**
- Use cases
- Workflows
- Orchestration
- Transaction boundaries

**Files:**
- Currently in `api.clj` (combined with API for simplicity)

**Could be separated into:**
- `application/use_cases.clj`
- `application/workflows.clj`

### API Layer

**Responsibilities:**
- User-facing interface
- Convenience functions
- Macros
- Facade to lower layers

**Files:**
- `api.clj`

**Rules:**
- Simple, intuitive API
- Hides complexity
- Documentation-friendly

## Usage Examples

### Basic Usage (Facade)

```clojure
(require '[desargues.api :as api])

;; Simple workflow
(api/init!)
(def e (api/expr '(square (sin x))))
(api/animate-expression e)
```

### Advanced Usage (Builder)

```clojure
(require '[desargues.api :as api])

(api/init!)

(def e1 (api/expr '(sin x)))
(def e2 (api/derivative e1))

(-> (api/scene)
    (api/show e1)
    (api/wait 1)
    (api/show e2)
    (api/wait 1)
    (api/transform e1 e2)
    (api/wait 2)
    (api/render!))
```

### Expert Usage (Direct Protocol Use)

```clojure
(require '[desargues.domain.math-expression :as expr])
(require '[desargues.domain.protocols :as p])
(require '[desargues.domain.services :as svc])

;; Create expression
(def e (expr/create-expression '(sin x)))

;; Use protocols directly
(p/to-latex e)
(p/simplify e)

;; Use domain services
(svc/differentiate-expression e)
```

## Benefits

### 1. Testability

Each layer can be tested independently:

```clojure
;; Test domain logic (no infrastructure)
(deftest test-differentiation
  (let [e (expr '(square x))
        de (svc/differentiate-expression e)]
    (is (= ... (p/to-latex de)))))

;; Test infrastructure with mocks
(deftest test-manim-adapter
  (with-mocked-manim
    (let [mobject (p/to-mobject expr)]
      (is ...))))
```

### 2. Maintainability

Clear separation makes changes easier:

- Change Manim version? → Only touch `manim_adapter.clj`
- Add new math operation? → Only touch `services.clj`
- Change API? → Only touch `api.clj`

### 3. Extensibility

Easy to add new features:

- New math object type? → Implement protocols
- New animation type? → Extend `IAnimatable`
- New rendering backend? → Implement `IRenderable`

### 4. Reusability

Domain logic is reusable:

- Use in different UI (CLI, web, etc.)
- Use with different rendering engines
- Use with different Python bridges

## Next Steps

### Implement

- [ ] Expression repository (save/load)
- [ ] Scene repository (save/render later)
- [ ] More animation types
- [ ] Plot integration
- [ ] Complex numbers
- [ ] Matrices

### Refactor

- [ ] Separate application layer from API
- [ ] Add more domain services
- [ ] Improve error handling
- [ ] Add logging

### Test

- [ ] Unit tests for domain
- [ ] Integration tests for infrastructure
- [ ] End-to-end tests

## Summary

This architecture provides:

✅ **Clean separation** of concerns
✅ **SOLID principles** throughout
✅ **DDD patterns** for domain modeling
✅ **Testable** components
✅ **Extensible** design
✅ **Maintainable** codebase
✅ **Reusable** domain logic

All while providing a **simple, intuitive API** for users!
