# Benchmarks

```sh
bb bench                    # every suite -> bench/results/latest.edn + doc/bench/*.svg
bb bench --only hotpaths    # one or more of core,hotpaths,geometry
bb bench --manim            # add Manim render timings (needs the conda env)
bb bench --charts-only      # redraw the charts from latest.edn
bb bench:check              # fresh run vs bench/results/baseline.edn, exit 1 on regression
bb bench:check --tolerance 0.4
bb bench:baseline           # fresh run saved as baseline.edn (commit it)
```

All tasks run `clojure -M:bench:dynamics`, so the raster rows (ODE solver and
geometry kernel) are included. The geometry suite still has its own main,
`clojure -M:bench:dynamics -m desargues.bench.geometry`, which writes
`bench/results/geometry.edn`.

## Method

`desargues.bench.sample/sample` first warms the JIT up, then takes `runs`
samples. Each sample is a batch of calls sized to last about `target-ms`.
Every row records the median and median absolute deviation (MAD) per call,
plus mean, std and min. The charts draw median ± MAD.

`bb bench:check` (`desargues.bench.check`) flags a row as a **regression** only
when all three of these hold:

- the fresh median is more than `tolerance` (default 25%) above the baseline median;
- the slowdown is more than 3 × (MAD of the baseline + MAD of the fresh run);
- the slowdown is more than 2 µs.

A row that appears in only one of the two reports is listed but never fails the
check. Raster rows, for example, appear only under `:dynamics`.

## Suites

| suite | group | what | source |
|---|---|---|---|
| core | `emmy` | `pipeline.emmy/derivative-spec`: f → f′ → LaTeX | `bench/desargues/bench.clj` |
| core | `layout`, `scene-graph`, `recording` | layout resolve, `layout->scene-graph`, imperative `render!` through the RecordingBackend, n = 4…1024 | same |
| core | `solver` | pendulum 10 s at dt 0.01 with every TrajectorySolver (Clojure euler/rk4, raster euler/rk4/tsit5/dp5) | same |
| hotpaths | `physics` | `videos.physics/evolve`, 800 steps on the cljw-lane pendulum: Clojure RK4, Clojure Euler, raster RK4 | `bench/desargues/bench/hotpaths.clj` |
| hotpaths | `lagrangian` | `videos.lagrangian` compile (planar, spherical double pendulum) and 8 s evolve (Clojure RK4 and raster RK4) | same |
| hotpaths | `pipeline` | `pipeline.emmy/python-code`: Emmy → LaTeX → Python (`dsl.math/latex->python`) | same |
| hotpaths | `scene` | records the landing page's 3D pendulum (`site/desargues/site/pendulum3d`) through the RecordingBackend, then prints the graph as EDN | same |
| geometry | `h-cloud` | applying H to an n-point cloud: Emmy per point vs the array and raster GeometryKernels | `bench/desargues/bench/geometry.clj` |
| geometry | `geometry` | `fit-homography`, `fit-keyframes`, `disposition/solve-keyframes`, `valid-trajectory?`, for each kernel | same |
| --manim | `manim` | wall time of one Manim render at 480p15, 720p30 and 1080p60 | `bench/desargues/bench.clj` |

## Current numbers

These are the baseline in `bench/results/baseline.edn`, measured 2026-10-11 on
Linux amd64 with 22 cores, JDK 25.0.4.1 and Clojure 1.12.3. Each figure is the
median per call, with the MAD after ±.

| group | subject | median | ± MAD |
|---|---|---|---|
| physics | clojure rk4, 800 steps | 7.24 ms | 1.64 |
| physics | clojure euler, 800 steps | 1.49 ms | 0.54 |
| physics | raster rk4, 800 steps | 2.84 ms | 0.28 |
| solver | clojure rk4, 1000 steps | 13.5 ms | 1.86 |
| solver | raster dp5, 10 s | 3.05 ms | 0.45 |
| lagrangian | compile planar | 33.6 ms | 10.7 |
| lagrangian | compile spherical | 459 ms | 69 |
| lagrangian | evolve planar, rk4 ×800 | 33.4 ms | 8.4 |
| lagrangian | evolve spherical, rk4 ×800 | 102 ms | 27 |
| lagrangian | evolve planar, raster rk4 ×800 | 17.3 ms | 4.2 |
| lagrangian | evolve spherical, raster rk4 ×800 | 66.6 ms | 5.1 |
| pipeline | Emmy → LaTeX → Python, sin x | 23 µs | 12 |
| pipeline | Emmy → LaTeX → Python, x⁵ + 3x³ − 2x | 80 µs | 12 |
| emmy | derivative-spec, x⁵ + 3x³ − 2x | 0.42 ms | 0.24 |
| scene | record landing pendulum (80 frames) | 109 ms | 11 |
| scene | landing graph → EDN (530 kB) | 27.1 ms | 2.9 |
| recording | render!, 3074 animations | 3.42 ms | 0.64 |
| layout | resolve, 1024 elements | 8.48 ms | 1.45 |
| h-cloud | 1000 points: Emmy / array / raster | 24.7 ms / 4.7 µs / 4.9 µs | |
| geometry | fit-homography :projective, array / raster | 91.5 / 46.3 ms | 1.0 / 7.8 |
| geometry | solve-keyframes 3, array / raster | 297 / 150 ms | 128 / 25 |
| geometry | valid-trajectory? 3 × 24, array | 0.80 ms | 0.11 |

`bench/results/baseline.edn` has every row, and `doc/bench/*.svg` charts them.
These rows are the noisiest on this machine: `solve-keyframes` (a core.logic
search) and the Lagrangian compile. The 3 × MAD rule is what keeps them from
tripping the check.

## Reference: the cljw lane (not run by `bb bench`)

These figures are copied from [cljw-lane.md](cljw-lane.md): the same 800-step
RK4 pendulum, plus 81 samples read back. `bb bench` does not run them, and they
add no runtime dependency.

| runtime | ms/call |
|---|---|
| Zig wasm kernel via cljw FFI | 1.3–1.9 |
| interpreted `phys/evolve` on cljw | 147–262 |
| `phys/evolve` on the warm JVM (that doc) | 11 |
| `phys/evolve` on the warm JVM (this baseline, `physics` clojure rk4 x800) | 7.2 |
