# Live frames: Manim cairo frames to a Clojure sink (DESARGUES-PERF-LIVE-1)

`desargues.manim.frames` wraps `SceneFileWriter.write_frame` (the call
`CairoRenderer.add_frame` makes for every frame) so every frame reaches an
`IFrameSink` (`desargues.manim.frame-protocols`) before Manim drops it. With
`render-live!` the scene renders with `write_to_movie`, `save_last_frame` and
caching off: no encoder, no file, no hashing of play calls.

```clojure
(require '[desargues.manim.frames :as fr])
(def sink (fr/collecting-sink))          ; or (fr/null-sink), or any (fn [rgba w h n])
(fr/render-live! #(MyScene) sink)        ; => frames delivered; hook removed after
(fr/collected sink)                      ; {:count :indices :w :h :last :first-ns}
```

A held frame (a wait with nothing moving, `num_frames` k) arrives k times,
one per index. Frame `n` is at scene time `(fr/frame-time n fps)`; the pure
pacing helpers are `frame-time`, `frame-index`, `frame-count` (animation,
`np.arange` length), `held-frame-count` (wait, truncating), `frame-due-ms`,
and `frame-lag`.

The `rgba` handed to the sink is a dtype-next tensor `[h w 4] :uint8`. By
default it is a zero-copy view of the numpy buffer (same address), valid only
during the call. A sink that keeps a frame clones it, as `CollectingSink`
does. Install with `{:copy? true}` to get a JVM-heap clone instead.

## Numbers

Warm JVM, Manim CE 0.20.1, 480p (`low_quality`, 854x480 @15). The scene is
Create(Circle) for 1 s, then a 0.5 s wait (22 frames). Each "edit" is a fresh
Scene class with a different construct. There were 30 runs, interleaved with
the devx last-frame PNG preview. The bench is
`desargues.bench.live-frames/run!` and the raw data is in
`bench/results/live-frames.edn`.

| path | median | MAD | min |
|---|---|---|---|
| edit -> first frame at the sink | **14.6 ms** | 1.9 | 11.1 |
| edit -> all 22 frames at the sink | 123.6 ms | 20.1 | 81.8 |
| edit -> devx `render-preview!` (last frame, PNG) | 26.5 ms | 5.7 | 18.5 |

The target was a first frame under 300 ms warm at 480p. The median is about
20 times under it. The first live frame arrives before the PNG preview
finishes: the preview skips every animation and renders only the final
state, while the live path starts streaming at frame 0.

Per-frame crossing of one 854x480 RGBA frame (1.64 MB) from Python into a
`NullSink`, timed on the Python side over 2000 calls:

| crossing | per frame |
|---|---|
| zero-copy (tensor view of the numpy buffer) | 0.86 ms |
| copy (`dt/clone` onto the JVM heap) | 2.92 ms |

The zero-copy cost is fixed per-call overhead: the libpython-clj callable
boundary, wrapping the array, and reading `__array_interface__`. It does not
depend on frame size. The copy adds a memcpy of about 2 ms. At 15 fps
(66 ms per frame) both are small. At 60 fps (16.7 ms) zero-copy uses about
5% of the frame budget.
