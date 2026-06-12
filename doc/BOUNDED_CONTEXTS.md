# Bounded Contexts in Desargues

This document describes the Domain-Driven Design (DDD) bounded contexts in the Desargues codebase.

## Overview

Desargues is organized into three primary bounded contexts:

```
+------------------+     +------------------+     +------------------+
|     Domain       |     |   Rendering      |     |     DevX         |
|   (Math Core)    |---->|   (Animation)    |---->|  (Developer UX)  |
+------------------+     +------------------+     +------------------+
```

## 1. Domain Context (Mathematical Core)

**Location:** `src/desargues/domain/`

**Purpose:** Pure mathematical operations and symbolic computation.

### Ubiquitous Language
- **Expression** - A symbolic mathematical expression (e.g., `sin(x)`)
- **Function** - A callable mathematical function
- **Derivative** - Result of symbolic differentiation
- **LaTeX** - Typeset representation of math
- **Simplification** - Algebraic simplification of expressions

### Key Entities
| Entity | Location | Description |
|--------|----------|-------------|
| `MathExpression` | `math_expression.clj` | Immutable value object wrapping Emmy expressions |
| `MathFunction` | `math_expression.clj` | Callable mathematical function with metadata |
| `Point` | `math_expression.clj` | 2D/3D coordinate value object |

### Protocols (Interfaces)
| Protocol | Purpose |
|----------|---------|
| `ILatexConvertible` | Convert to LaTeX string |
| `IPythonConvertible` | Convert to Python code |
| `ISimplifiable` | Algebraic simplification |
| `IEvaluable` | Numeric/symbolic evaluation |
| `IDifferentiable` | Symbolic differentiation |

### Domain Services
- `differentiate-expression` - Compute symbolic derivatives
- `evaluate-expression` - Evaluate at points
- `simplify-expression` - Algebraic simplification
- `tabulate-function` - Generate value tables

### External Dependencies
- Emmy (sicmutils) - Symbolic mathematics library

### Boundaries
- **No I/O operations** - Pure functions only
- **No rendering concerns** - Output format agnostic
- **No Python/Manim** - Platform independent

---

## 2. Rendering Context (Animation)

**Location:** `src/desargues/infrastructure/`, `src/desargues/manim/`

**Purpose:** Convert mathematical objects to visual animations.

### Ubiquitous Language
- **Scene** - A Manim animation scene
- **Mobject** - Manim mathematical object (visual element)
- **Animation** - Transition or effect
- **Quality** - Render resolution/FPS preset
- **Backend** - Rendering engine (Manim, Mock, etc.)

### Key Components
| Component | Location | Description |
|-----------|----------|-------------|
| `ManimBackend` | `backend/manim.clj` | Manim rendering strategy |
| `MockBackend` | `backend.clj` | Testing backend |
| `IRenderBackend` | `backend.clj` | Strategy protocol |

### Anti-Corruption Layer
The `infrastructure/manim_adapter.clj` acts as an anti-corruption layer:
- Translates domain objects to Manim objects
- Isolates Python interop details
- Prevents Manim concepts from leaking into domain

### External Dependencies
- Manim Community Edition (Python)
- libpython-clj (Clojure-Python bridge)
- FFmpeg (video processing)

### Boundaries
- **All Python code isolated here**
- **Implements domain protocols** for rendering
- **No direct domain logic** - only visualization

---

## 3. DevX Context (Developer Experience)

**Location:** `src/desargues/devx/`

**Purpose:** Hot-reload workflow and incremental rendering for development.

### Ubiquitous Language
- **Segment** - Discrete, cacheable animation unit
- **Scene Graph** - DAG of segments with dependencies
- **Dirty** - Segment needs re-rendering
- **Cached** - Segment has valid rendered output
- **Partial** - Individual segment video file
- **Hot Reload** - Automatic re-render on code change

### Key Aggregates
| Aggregate | Root Entity | Description |
|-----------|-------------|-------------|
| Scene Graph | `SceneGraph` | Collection of segments with dependency edges |
| Segment | `Segment` | Individual animation unit with state |

### Supporting Components
| Component | Location | Purpose |
|-----------|----------|---------|
| `segment.clj` | Core | Segment value object |
| `graph.clj` | Core | Scene graph operations |
| `renderer.clj` | Core | Render orchestration |
| `quality.clj` | Core | Extensible quality presets (OCP) |
| `events.clj` | Core | Observer pattern for state changes |
| `backend.clj` | Core | Render backend strategy |
| `repository.clj` | Core | Graph persistence |
| `repl.clj` | Interface | REPL workflow helpers |
| `core.clj` | Interface | Public API facade |

### State Machine: Segment Render State
```
          +----------+
          | pending  |
          +----+-----+
               |
               v
          +----------+     error
          | rendering+------------+
          +----+-----+            |
               |                  v
               v             +----+----+
          +----+----+        |  error  |
          | cached  |        +---------+
          +----+----+
               |
               | (code change)
               v
          +----+----+
          |  dirty  |
          +----+----+
               |
               +---> (back to rendering)
```

### External Dependencies
- clj-reload (namespace reloading)
- hawk (file watching)

### Boundaries
- **Manages mutable state** (atoms for current scene)
- **Orchestrates rendering** but doesn't implement it
- **Platform agnostic** - delegates to backends

---

## Context Map

```
+------------------+
|     Domain       |
|   (Upstream)     |
+--------+---------+
         |
         | Conformist
         v
+--------+---------+
|   Rendering      |
| (Anti-Corruption |
|     Layer)       |
+--------+---------+
         |
         | Customer/Supplier
         v
+--------+---------+
|     DevX         |
|  (Downstream)    |
+------------------+
```

### Integration Patterns

1. **Domain -> Rendering**: Conformist
   - Rendering context conforms to domain protocols
   - `ILatexConvertible`, `IPythonConvertible` implemented by adapters

2. **Rendering -> DevX**: Customer/Supplier
   - DevX requests rendering services
   - Rendering provides `IRenderBackend` interface
   - DevX can swap backends without changes

3. **Domain -> DevX**: Shared Kernel
   - Both use `ISimplifiable`, `IMetadata` protocols
   - Segment metadata may include domain concepts

---

## Module Dependencies (Layer Diagram)

```
+-------------------------------------------------------------+
|                      API Layer                               |
|  +-----------+  +-----------+  +-----------+                |
|  |  api.clj  |  | core.clj  |  | repl.clj  |                |
|  +-----------+  +-----------+  +-----------+                |
+-------------------------------------------------------------+
                           |
                           v
+-------------------------------------------------------------+
|                   Application Layer                          |
|  +---------+  +---------+  +---------+  +---------+         |
|  |renderer |  | quality |  | events  |  |repository|        |
|  +---------+  +---------+  +---------+  +---------+         |
+-------------------------------------------------------------+
                           |
                           v
+-------------------------------------------------------------+
|                    Domain Layer                              |
|  +---------+  +---------+  +---------+  +---------+         |
|  |protocols|  |math_expr|  |services |  | segment |         |
|  +---------+  +---------+  +---------+  +---------+         |
+-------------------------------------------------------------+
                           |
                           v
+-------------------------------------------------------------+
|                 Infrastructure Layer                         |
|  +---------------+  +---------------+  +------------+       |
|  | manim_adapter |  | backend/manim |  |   manim/   |       |
|  +---------------+  +---------------+  +------------+       |
+-------------------------------------------------------------+
```

---

## Guidelines for Developers

### Adding New Features

1. **Mathematical operations** - Add to Domain Context
   - Keep pure, no side effects
   - Implement relevant protocols

2. **Visual elements** - Add to Rendering Context
   - Use anti-corruption layer
   - Don't expose Python details

3. **Workflow improvements** - Add to DevX Context
   - Use events for loose coupling
   - Extend via protocols (OCP)

### Cross-Context Communication

- Use **protocols** at boundaries
- Pass **immutable data** between contexts
- Events for **loose coupling** within DevX

### Testing Strategy

| Context | Test Type | Tools |
|---------|-----------|-------|
| Domain | Unit tests | `clojure.test`, property-based |
| Rendering | Integration | Mock backend |
| DevX | Unit + Integration | `clojure.test`, fixtures |
