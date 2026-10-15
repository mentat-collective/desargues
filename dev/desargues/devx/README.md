# Desargues DevX: Hot Reload System

This module provides Figwheel-style hot reloading for Manim animation development. When you save a file, the system automatically detects changes, reloads the affected code, and re-renders only the segments that changed.

## Verification Status ✅

**Verified: 2025-12-13** - All 5 end-to-end tests pass:

| Test | Status |
|------|--------|
| Python/Manim Initialization | ✅ |
| Scene Graph with source-ns | ✅ |
| File Watcher (Beholder) | ✅ |
| Reload Cycle (clj-reload) | ✅ |
| Hot-Reload Integration | ✅ |

Run verification: `(require 'verify-hot-reload) (verify-hot-reload/run-all-tests!)`

## Architecture

The devx module is split into two main APIs:

- **`desargues.devx.core`** - Pure functions for scene graph manipulation
- **`desargues.devx.repl`** - Stateful REPL workflow with hot-reload

## Quick Start

```clojure
;; Pure API for scene construction
(require '[desargues.devx.core :as devx])

;; Stateful REPL API for hot-reload workflow
(require '[desargues.devx.repl :as repl])

;; Initialize the system
(devx/init!)

;; Define segments with source namespace tracking
(devx/defsegment intro
  "Introduction animation"
  [scene]
  (let [title (create-text "Hello World")]
    (play! scene (write title))))

(devx/defsegment main-content
  "Main animation"
  :deps #{intro}  ; Depends on intro
  [scene]
  (let [circle (create-circle)]
    (play! scene (create circle))))

;; Create scene graph (pure)
(def my-scene (devx/scene [intro main-content]))

;; Load into REPL workflow (stateful)
(repl/set-graph! my-scene)

;; Start watching for file changes
(repl/watch!)  ; Manual render mode
;; OR
(repl/w!)      ; Auto-render on change (recommended)
;; OR  
(repl/W!)      ; Auto-render + auto-combine into final video

;; ... edit your files ...
;; The system will detect changes and mark affected segments dirty

;; Manually render when ready (if not using auto-render)
(repl/r!)

;; Stop watching
(repl/uw!)
```

## How It Works

### 1. Namespace Tracking

Every segment created with `defsegment` automatically captures its source namespace:

```clojure
(devx/defsegment my-animation
  "Description"
  [scene]
  ...)
;; Metadata includes {:source-ns 'my.namespace}
```

### 2. File Watching

The watcher monitors `.clj` and `.cljc` files in configured paths (default: `["src"]`):

```clojure
;; Watch specific paths
(repl/watch! :paths ["src" "dev" "videos"])

;; Check watcher status
(repl/watch-status)
;; => {:running? true
;;     :paths ["src"]
;;     :changes-detected 3
;;     :last-change {:namespace my.scenes, :file "...", :time #inst"..."}}
```

### 3. Reload Cycle

When a file changes:

1. **Detect** - Beholder detects the file modification
2. **Reload** - clj-reload reloads the namespace (preserving `defonce` state)
3. **Find** - Find segments with matching `:source-ns` metadata
4. **Propagate** - Include all transitive dependents in affected set
5. **Hash** - Recompute segment hashes to detect actual changes
6. **Mark** - Mark changed segments as dirty
7. **Render** - (If auto-render) Re-render dirty segments

### 4. Dependency Propagation

If segment B depends on segment A, and A's source file changes:
- Both A and B are marked dirty
- Rendering happens in topological order (A first, then B)

```clojure
;; Check what would be affected by a namespace change
(require '[desargues.devx.ns-tracker :as tracker])
(tracker/affected-segments @devx/current-scene 'my.scenes)
;; => #{:intro :main-content :conclusion}
```

## REPL Commands Reference

All commands below are in the `desargues.devx.repl` namespace (aliased as `repl`).

### Watch Control

| Command | Description |
|---------|-------------|
| `(repl/watch!)` | Start watching (manual render mode) |
| `(repl/watch! :auto-render? true)` | Auto-render dirty segments on change |
| `(repl/watch! :auto-combine? true)` | Auto-combine partials after render |
| `(repl/watch! :paths ["src" "dev"])` | Watch specific directories |
| `(repl/unwatch!)` | Stop file watching |
| `(repl/watching?)` | Check if watcher is running |
| `(repl/watch-status)` | Show detailed watcher statistics |

### Quick Shortcuts

| Shortcut | Equivalent |
|----------|------------|
| `(repl/w!)` | `(repl/watch! :auto-render? true)` |
| `(repl/W!)` | `(repl/watch! :auto-render? true :auto-combine? true)` |
| `(repl/uw!)` | `(repl/unwatch!)` |

### Rendering

| Command | Description |
|---------|-------------|
| `(repl/r!)` | Render all dirty segments |
| `(repl/render! :quality :high)` | Render with specific quality |
| `(repl/p! :segment-id)` | Preview single segment (last frame only) |
| `(repl/export! "output.mp4")` | Export combined video |

### Inspection

| Command | Description |
|---------|-------------|
| `(repl/s!)` | Show scene status |
| `(repl/tracking-report)` | Show namespace tracking coverage |
| `(repl/reload!)` | Force manual reload cycle |

## Configuration

### Auto-render Behavior

```clojure
;; Enable/disable auto-render programmatically
(require '[desargues.devx.reload :as reload])
(reload/set-auto-render! true)
(reload/set-auto-combine! true)
```

### Protected Namespaces

These namespaces are never unloaded during reload (preserves Python state):
- `desargues.manim.core`
- `desargues.manim-quickstart`
- `user`

### Custom Notifications

```clojure
;; Set custom notification handler
(reload/set-notify-fn!
  (fn [event-type message & data]
    (println (format "[%s] %s" (name event-type) message))))
```

## Workflow Examples

### Basic Development Loop

```clojure
;; 1. Start REPL and initialize
(require '[desargues.devx.core :as devx])
(require '[desargues.devx.repl :as repl])
(devx/init!)

;; 2. Load your scene
(require '[my.video.scenes :as scenes])
(repl/set-graph! scenes/my-animation)

;; 3. Start watching with auto-render
(repl/w!)

;; 4. Edit src/my/video/scenes.clj in your editor
;;    Console shows:
;;    [change] File changed: src/my/video/scenes.clj
;;    [reload] Reloading my.video.scenes...
;;    [success] Reloaded: [my.video.scenes]
;;    [info] 2 segment(s) marked dirty
;;    [render] Auto-rendering dirty segments...

;; 5. Check status anytime
(repl/s!)

;; 6. When done
(repl/uw!)
```

### Debugging Namespace Tracking

```clojure
;; See which namespaces are tracked
(repl/tracking-report)
;; => === Namespace Tracking Report ===
;;    Total segments: 5
;;    Tracked: 5 (100%)
;;    Untracked: 0
;;    Namespaces: 2
;;    
;;    Namespace -> Segments:
;;      my.video.intro: [:title :subtitle]
;;      my.video.main: [:step-1 :step-2 :conclusion]

;; Manually check affected segments (using tracker directly)
(require '[desargues.devx.ns-tracker :as tracker])
(tracker/affected-segments (repl/get-graph) 'my.video.intro)
;; => #{:title :subtitle :step-1}  ; step-1 depends on intro segments
```

### Manual Reload (No Watcher)

```clojure
;; When you want control over when to reload
(repl/reload!)                    ; Reload + render
(repl/reload! :render? false)     ; Reload only, don't render
(repl/reload! :combine? true)     ; Reload + render + combine
```

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     File System                             │
│                         │                                   │
│                    [file change]                            │
│                         ▼                                   │
│  ┌──────────────────────────────────────────────────────┐  │
│  │              watcher.clj (Beholder)                  │  │
│  │   • Watches .clj/.cljc files                         │  │
│  │   • Debounces rapid changes                          │  │
│  │   • Maps file path → namespace symbol                │  │
│  └──────────────────────────────────────────────────────┘  │
│                         │                                   │
│                    [on-change event]                        │
│                         ▼                                   │
│  ┌──────────────────────────────────────────────────────┐  │
│  │              reload.clj (Coordinator)                │  │
│  │   • Reloads namespace via clj-reload                 │  │
│  │   • Finds affected segments                          │  │
│  │   • Recomputes hashes                                │  │
│  │   • Marks dirty segments                             │  │
│  │   • Triggers render (if auto-render)                 │  │
│  └──────────────────────────────────────────────────────┘  │
│                         │                                   │
│            ┌────────────┴────────────┐                     │
│            ▼                         ▼                      │
│  ┌─────────────────┐      ┌─────────────────────────┐      │
│  │  ns_tracker.clj │      │     renderer.clj        │      │
│  │  • :source-ns   │      │  • render-dirty!        │      │
│  │    tracking     │      │  • combine-partials!    │      │
│  │  • affected     │      │  • Manim integration    │      │
│  │    segments     │      │                         │      │
│  └─────────────────┘      └─────────────────────────┘      │
└─────────────────────────────────────────────────────────────┘
```

## Troubleshooting

### "No segments affected by this change"

The changed namespace doesn't match any segment's `:source-ns`. Check:
1. Are you using `defsegment` macro? (captures source-ns automatically)
2. Run `(tracking-report)` to see tracked namespaces
3. For manually created segments, add `:metadata {:source-ns 'your.ns}`

### Watcher not detecting changes

1. Check watcher is running: `(watching?)`
2. Check watched paths: `(watch-status)`
3. Ensure file is in watched directory
4. Try `(devx/restart-watcher!)`

### Python state lost after reload

Protected namespaces should prevent this. If issues persist:
1. Check `desargues.manim-quickstart` is in `:no-unload` set
2. Re-run `(devx/init!)` to reinitialize Python

### Render errors during auto-render

Errors are caught and logged. The segment stays dirty for retry:
1. Fix the code error
2. Save again - will retry rendering
3. Or manually: `(r!)` after fixing
