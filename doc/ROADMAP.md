# Desargues Development Experience Roadmap

> Improving hot-reload, smart re-rendering (figwheel-style), and parallel execution

## Executive Summary

This roadmap transforms Desargues from a "render everything on every change" workflow to a **figwheel-style development experience** where only changed segments re-render, with live preview and parallel execution.

### Key Insights from Research

1. **Manim's Partial Movie System**: Each `play()` call creates a separate partial movie file that gets combined at the end. We can leverage this for segment-based caching.

2. **Content-Addressable Caching**: Manim computes hashes per `play()` call. If hash matches cached output, it can be reused. We'll extend this to Clojure-side segments.

3. **Existing DSL Foundation**: The `desargues.dsl.core` and `desargues.videos.timeline` namespaces already model animations as DATA. This is perfect for change detection and incremental rendering.

4. **OpenGL Live Preview**: Manim's OpenGL renderer supports `force_window=true` for realtime preview - ideal for figwheel-style feedback.

---

## Phase 1: Foundation - Scene Graph & Segment Model
**Priority: HIGH** | *Enables all other improvements*

### 1.1 Scene Segment Abstraction

Create a `Segment` record that wraps a portion of a scene:

```clojure
(defrecord Segment 
  [id           ; unique keyword like :step-7
   hash         ; content-addressable hash for change detection
   dependencies ; set of segment IDs this depends on
   render-state ; :pending | :rendering | :cached | :dirty
   partial-file ; path to partial movie file when cached
   construct-fn ; function that builds this segment's animations
   metadata])   ; timing, duration, description
```

### 1.2 Scene Graph Data Structure

Build a DAG (Directed Acyclic Graph) of segments:

```clojure
(defrecord SceneGraph
  [segments     ; map of id -> Segment
   edges        ; adjacency list for dependencies  
   root-order]) ; topologically sorted segment IDs

;; Operations
(defn add-segment [graph segment])
(defn mark-dirty! [graph segment-id])
(defn dirty-segments [graph])
(defn render-order [graph]) ; respects dependencies
```

### 1.3 Segment Hashing

Implement content-addressable hashing:

```clojure
(defn segment-hash [segment]
  ;; Hash includes:
  ;; - Source code of construct-fn (via serialization)
  ;; - All referenced mobject definitions
  ;; - Animation parameters
  ;; - Timing/duration
  (hash-combine
    (hash-fn (:construct-fn segment))
    (hash-fn (:dependencies segment))
    (:metadata segment)))
```

### 1.4 Partial Movie File Management

Map segments to Manim's partial movie files:

```clojure
(defn partial-file-path [segment]
  (str "media/partial_movies/" (:id segment) ".mp4"))

(defn cached? [segment]
  (and (fs/exists? (partial-file-path segment))
       (= (:hash segment) (read-cached-hash segment))))

(defn combine-partials! [scene-graph output-path]
  ;; Use ffmpeg to concatenate in render-order
  )
```

---

## Phase 2: Hot Reload Infrastructure  
**Priority: HIGH** | *File watching and REPL-driven re-rendering*

### 2.1 File Watcher Integration

Use [hawk](https://github.com/wkf/hawk) or [dirwatch](https://github.com/Ramblurr/dirwatch):

```clojure
(defn start-watcher! [scene-graph]
  (hawk/watch! [{:paths ["src/"]
                 :filter hawk/file?
                 :handler (fn [ctx {:keys [file kind]}]
                            (when (= kind :modify)
                              (on-file-change! scene-graph file)))}]))
```

### 2.2 Namespace Dependency Tracking

Track which segments depend on which namespaces:

```clojure
(defn ns-dependencies [segment]
  ;; Analyze construct-fn to find required namespaces
  ;; Use clojure.tools.namespace for dependency graph
  )

(defn segments-affected-by [scene-graph changed-ns]
  (->> (:segments scene-graph)
       (filter #(contains? (ns-dependencies %) changed-ns))
       (map :id)))
```

### 2.3 Incremental Re-render

The hot-reload cycle:

```clojure
(defn on-file-change! [scene-graph file]
  (let [ns (file->namespace file)]
    ;; 1. Reload the namespace
    (require ns :reload)
    
    ;; 2. Find affected segments
    (let [affected (segments-affected-by scene-graph ns)]
      
      ;; 3. Recompute hashes, mark dirty if changed
      (doseq [seg-id affected]
        (let [seg (get-segment scene-graph seg-id)
              new-hash (segment-hash seg)]
          (when (not= new-hash (:hash seg))
            (mark-dirty! scene-graph seg-id))))
      
      ;; 4. Re-render dirty segments
      (render-dirty! scene-graph)
      
      ;; 5. Recombine if needed
      (when (any-dirty? scene-graph)
        (combine-partials! scene-graph "media/videos/output.mp4")))))
```

### 2.4 REPL Commands

Developer-friendly commands:

```clojure
;; Render a specific segment
(render-segment! :step-7)

;; Preview segment (last frame only, fast)
(preview-segment! :step-7)

;; Render all dirty segments
(render-dirty!)

;; Force full re-render
(render-all!)

;; Show segment status
(segment-status)
;; => {:step-1 :cached, :step-2 :cached, :step-7 :dirty, ...}
```

---

## Phase 3: Smart Preview (Figwheel-style)
**Priority: MEDIUM** | *Fast feedback loop with intelligent caching*

### 3.1 Last-Frame Preview Mode

Instant visual feedback by rendering only the final frame:

```clojure
(defn preview-segment! [segment-id]
  (let [seg (get-segment @scene-graph segment-id)]
    ;; Use Manim's -s equivalent
    (py/call-attr manim-config "save_last_frame" true)
    (render-segment-internal! seg)
    (open-image! (str "media/images/" segment-id ".png"))))
```

### 3.2 OpenGL Live Preview

Integrate Manim's OpenGL renderer for realtime preview:

```clojure
(defn start-live-preview! [segment-id]
  (let [config (py/get-attr (manim/manim) "config")]
    ;; Enable OpenGL with live window
    (py/set-attr! config "renderer" "opengl")
    (py/set-attr! config "write_to_movie" false)
    (py/set-attr! config "preview" true)
    
    ;; Render - window stays open
    (render-segment-internal! (get-segment @scene-graph segment-id))))
```

### 3.3 Segment Selector UI

Simple TUI for segment navigation:

```
╔═══════════════════════════════════════════════════════╗
║  Desargues Preview - Brachistochrone                  ║
╠═══════════════════════════════════════════════════════╣
║  [1] intro          ✓ cached     0:05                 ║
║  [2] problem-setup  ✓ cached     0:12                 ║
║  [3] energy         ● dirty      0:18                 ║
║  [4] arc-length     ✓ cached     0:15                 ║
║  [5] functional     ○ pending    0:20                 ║
║  ...                                                  ║
╠═══════════════════════════════════════════════════════╣
║  [p] Preview  [r] Render  [R] Render All  [q] Quit    ║
╚═══════════════════════════════════════════════════════╝
```

### 3.4 Quality Modes

Automatic quality switching:

```clojure
(def ^:dynamic *preview-quality* :low)
(def ^:dynamic *render-quality* :high)

(defn with-quality [quality f]
  (let [config (py/get-attr (manim/manim) "config")]
    (py/set-attr! config "quality" 
                  (case quality
                    :low "low_quality"      ; 480p15
                    :medium "medium_quality" ; 720p30  
                    :high "high_quality"))   ; 1080p60
    (f)))
```

---

## Phase 4: Parallel Execution
**Priority: MEDIUM** | *Render independent segments concurrently*

### 4.1 Dependency Analysis

Identify parallelizable segments:

```clojure
(defn independent-groups [scene-graph]
  ;; Find segments with no interdependencies
  ;; These can render in parallel
  (let [dirty (dirty-segments scene-graph)]
    (->> dirty
         (group-by #(transitive-deps scene-graph %))
         (vals)
         (filter #(> (count %) 1)))))
```

### 4.2 Worker Pool

Create pool of render workers (each with own Python process):

```clojure
(defonce worker-pool (atom nil))

(defn init-workers! [n]
  (reset! worker-pool
    (repeatedly n 
      (fn []
        ;; Each worker gets its own Python interpreter
        ;; libpython-clj supports this via separate GIL contexts
        {:id (gensym "worker-")
         :status :idle
         :python-ctx (py/create-python-context!)}))))

(defn submit-render! [worker segment]
  ;; Run in worker's Python context
  (py/with-python-context (:python-ctx worker)
    (render-segment-internal! segment)))
```

### 4.3 Parallel Render Orchestrator

Schedule across workers respecting dependencies:

```clojure
(defn render-parallel! [scene-graph]
  (let [order (render-order scene-graph)  ; topological sort
        levels (partition-by-depth order)] ; group by dependency level
    (doseq [level levels]
      ;; All segments in same level can run in parallel
      (let [futures (doall 
                      (for [seg-id level]
                        (let [worker (acquire-worker!)]
                          (future 
                            (try
                              (submit-render! worker (get-segment scene-graph seg-id))
                              (finally
                                (release-worker! worker)))))))]
        ;; Wait for level to complete before next
        (doseq [f futures] @f)))))
```

### 4.4 Progress Reporting

Real-time progress display:

```clojure
(defn render-with-progress! [scene-graph]
  (let [total (count (dirty-segments scene-graph))
        completed (atom 0)
        start-time (System/currentTimeMillis)]
    (add-watch completed :progress
      (fn [_ _ _ n]
        (let [elapsed (- (System/currentTimeMillis) start-time)
              per-seg (/ elapsed n)
              remaining (* per-seg (- total n))]
          (println (format "[%d/%d] %.1fs elapsed, ~%.1fs remaining"
                           n total (/ elapsed 1000.0) (/ remaining 1000.0))))))
    (render-parallel! scene-graph 
                      :on-complete #(swap! completed inc))))
```

---

## Phase 5: DSL Completion & Integration
**Priority: MEDIUM** | *Connect existing DSL to hot-reload system*

### 5.1 Timeline-to-Segments Compiler

Compile Timeline DSL to segment graph:

```clojure
(defn timeline->segments [timeline]
  (let [events (get-events timeline)]
    (->> events
         (partition-by :segment-boundary?)
         (map-indexed 
           (fn [idx segment-events]
             (->Segment 
               (keyword (str "segment-" idx))
               (hash segment-events)
               (deps-from-events segment-events)
               :pending
               nil
               (fn [scene] (render-events! scene segment-events))
               {:duration (total-duration segment-events)}))))))

;; Usage with DSL
(def my-scene
  (-> (timeline)
      (at 0 (create circle) :segment-boundary true)
      (then (write equation))
      (wait 2)
      (at 5 (transform circle square) :segment-boundary true)))

(def scene-graph (timeline->segments my-scene))
```

### 5.2 Reactive Bindings

Connect ReactiveValue to Manim's ValueTracker:

```clojure
(defn bind-to-manim! [reactive-val]
  (let [tracker (py/call (manim/get-class "ValueTracker") 
                         @(:value reactive-val))]
    (add-watch (:value reactive-val) :manim-sync
      (fn [_ _ _ new-val]
        (py/call-attr tracker "set_value" new-val)))
    tracker))
```

### 5.3 Animation Combinator Optimization

Optimize segment boundaries for combinators:

```clojure
;; >> (sequential) - each animation is own segment
;; || (parallel) - all animations in same segment  
;; |> (staggered) - single segment with internal timing

(defmethod optimize-for-segments :sequential [animations]
  (map #(->Segment % {:boundary :hard}) animations))

(defmethod optimize-for-segments :parallel [animations]  
  [(->Segment animations {:boundary :soft})])
```

### 5.4 Scene Serialization

Save/load scene state for resumable rendering:

```clojure
(defn save-scene-graph! [scene-graph path]
  (spit path (pr-str 
    {:segments (update-vals (:segments scene-graph)
                            #(dissoc % :construct-fn))
     :edges (:edges scene-graph)
     :hashes (zipmap (keys (:segments scene-graph))
                     (map :hash (vals (:segments scene-graph))))})))

(defn load-scene-graph! [path construct-fns]
  ;; Restore with provided construct functions
  ;; Check hashes to determine what's still valid
  )
```

---

## Phase 6: Developer Experience Polish
**Priority: LOW** | *Quality of life improvements*

### 6.1 Error Recovery

Graceful Python error handling:

```clojure
(defn safe-render-segment! [segment]
  (try
    (render-segment-internal! segment)
    (catch Exception e
      (let [error-info (parse-python-error e)]
        ;; Show error in preview window
        (show-error-frame! segment error-info)
        ;; Log but don't crash watcher
        (log/error "Render failed for" (:id segment) error-info)
        ;; Mark segment as errored, not dirty
        (mark-errored! @scene-graph (:id segment) error-info)))))
```

### 6.2 Render Cache Management

```clojure
;; Clear all cached segments
(clear-cache!)

;; Clear specific segment
(clear-cache! :step-7)

;; Inspect cache state
(cache-info)
;; => {:total-size "245MB"
;;     :segments {:step-1 {:size "12MB" :cached-at #inst"..."}
;;                :step-2 {:size "8MB" :cached-at #inst"..."}}}

;; Manually invalidate (force re-render)
(invalidate! :step-7)
```

### 6.3 Performance Metrics

```clojure
(defn render-with-metrics! [scene-graph]
  (let [metrics (atom {})]
    (doseq [seg (render-order scene-graph)]
      (let [start (System/nanoTime)
            _ (render-segment! seg)
            elapsed (/ (- (System/nanoTime) start) 1e9)]
        (swap! metrics assoc (:id seg) 
               {:render-time elapsed
                :frame-count (get-frame-count seg)
                :fps (/ (get-frame-count seg) elapsed)})))
    
    ;; Report bottlenecks
    (println "\n=== Performance Report ===")
    (doseq [[id m] (sort-by (comp - :render-time val) @metrics)]
      (println (format "%-20s %6.2fs  %4d frames  %.1f fps"
                       id (:render-time m) (:frame-count m) (:fps m))))))
```

### 6.4 Documentation & Examples

- Hot-reload workflow tutorial
- Best practices for segment boundaries
- Example project with complex scene
- Troubleshooting guide

---

## Implementation Order

```
Phase 1 (Foundation) ──┬──> Phase 2 (Hot Reload) ──> Phase 3 (Preview)
                       │
                       └──> Phase 4 (Parallel) 
                       
Phase 5 (DSL) can proceed independently

Phase 6 (Polish) comes last
```

## Quick Wins (Can Do Now)

1. **Low-quality iteration mode**: Add `--low` flag (already exists!)
2. **Single segment rendering**: `render-step` function exists
3. **Manual parallel**: Run multiple `clojure -M:run` in separate terminals

## Research Sources

- [Manim Deep Dive - Internals](https://docs.manim.community/en/stable/guides/deep_dive.html)
- [Manim Animation System](https://deepwiki.com/ManimCommunity/manim/2.3-animation-system)
- [Manim Core Architecture](https://deepwiki.com/ManimCommunity/manim/2-core-architecture)
- [Manim Mobject System](https://deepwiki.com/ManimCommunity/manim/2.1-mobject-system)
- [Manim Scene System](https://deepwiki.com/ManimCommunity/manim/2.2-scene-and-animation-system)
