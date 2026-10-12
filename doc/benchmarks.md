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

These are the baseline in `bench/results/baseline.edn`, measured 2026-10-12 on
Linux amd64 with 22 cores, JDK 25.0.4.1 and Clojure 1.12.3, while other jobs
held the machine at a load of about 17. Each figure is the median per call,
with the MAD after ±. The "2026-10-11" column is the first baseline, taken
before the two performance passes.

| group | subject | median | ± MAD | 2026-10-11 |
|---|---|---|---|---|
| physics | clojure rk4, 800 steps | 2.33 ms | 0.34 | 7.24 ms |
| physics | clojure euler, 800 steps | 0.66 ms | 0.12 | 1.49 ms |
| physics | raster rk4, 800 steps | 3.82 ms | 0.69 | 2.84 ms |
| solver | clojure rk4, 1000 steps | 2.83 ms | 0.31 | 13.5 ms |
| solver | raster dp5, 10 s | 2.67 ms | 0.31 | 3.05 ms |
| lagrangian | compile planar (cold) | 22.2 ms | 2.9 | 33.6 ms |
| lagrangian | compile spherical (cold) | 343 ms | 57 | 459 ms |
| lagrangian | construct planar / spherical (cached) | 1.9 / 2.2 µs | 0.5 | n/a |
| lagrangian | evolve planar, rk4 ×800 | 2.16 ms | 0.28 | 33.4 ms |
| lagrangian | evolve spherical, rk4 ×800 | 10.5 ms | 2.0 | 102 ms |
| lagrangian | evolve planar, raster rk4 ×800 | 10.3 ms | 1.9 | 17.3 ms |
| lagrangian | evolve spherical, raster rk4 ×800 | 24.7 ms | 3.9 | 66.6 ms |
| pipeline | Emmy → LaTeX → Python, sin x | 8.2 µs | 1.5 | 23 µs |
| pipeline | Emmy → LaTeX → Python, x⁵ + 3x³ − 2x | 88 µs | 22 | 80 µs |
| emmy | derivative-spec, x⁵ + 3x³ − 2x | 0.15 ms | 0.06 | 0.42 ms |
| scene | record landing pendulum (80 frames) | 9.9 ms | 1.0 | 109 ms |
| scene | landing graph → EDN (530 kB) | 14.3 ms | 2.1 | 27.1 ms |
| recording | render!, 3074 animations | 2.31 ms | 0.40 | 3.42 ms |
| layout | resolve, 1024 elements | 5.06 ms | 0.67 | 8.48 ms |
| h-cloud | 1000 points: Emmy / array / raster | 12.9 ms / 2.1 µs / 3.2 µs | | 24.7 ms / 4.7 µs / 4.9 µs |
| geometry | fit-homography :projective, array / raster | 4.5 / 6.2 ms | 0.3 / 0.8 | 91.5 / 46.3 ms |
| geometry | solve-keyframes 3, array / raster | 24.4 / 27.0 ms | 0.2 / 2.5 | 297 / 150 ms |
| geometry | valid-trajectory? 3 × 24, array | 0.53 ms | 0.07 | 0.80 ms |

`bench/results/baseline.edn` has every row, and `doc/bench/*.svg` charts them.
The two baselines were taken under different machine load, so rows that no
code change touched (pipeline, layout, h-cloud) still moved; read a change of
less than about 2x in those as noise. The large changes come from the code:
primitive RK4 steps, double-array kernels for the Lagrangian state derivative,
the array Nelder-Mead in the fits and the disposition solver, and the cached
Lagrangian compile. The noisiest row is the cold Lagrangian compile; the
3 × MAD rule is what keeps it from tripping the check.

## The raster kernel solver (opt-in, `:dynamics`)

`(desargues.videos.physics/solver :raster {:kernel? true})` (the adapter's
`kernel-solver`, reached through the backend key) runs the whole RK4
integration inside one raster `deftm`: the Lagrangian state derivative is
Emmy's compiled kernel source lowered to raster, and the damped pendulum has
a hand-written kernel. Its trajectories are bit-identical to the array path
(the tests compare them with `=`). `phys/evolve-batch` hands it N systems,
which it integrates in one kernel call. These A/B figures come from a single
JVM (2026-10-12, same machine, load around 17), as medians in ms. They are not
part of the baseline; the new rows in `hotpaths.clj` / `geometry.clj` measure
them on the next run.

| subject | array (default) | old raster rk4 | raster kernel |
|---|---|---|---|
| pendulum, 800 steps | 2.2–3.9 | 1.2 | 0.19–0.37 |
| planar double pendulum, 800 steps | 1.4–1.7 | 1.6–7.1 | 0.45–0.52 |
| spherical double pendulum, 800 steps | 5.1–7.5 | 3.2 | 2.7–3.3 |
| pendulum sweep, 64 × 800 (evolve-batch) | 145–151 | | 12–14 |
| planar, 64 initial conditions × 800 | 91–129 | | 45 |
| one-time kernel compile, planar / spherical | | | ~100 / 300–430 |

Raster wins clearly on every evolve, and by 10x on batches. It is still not
the default: compiling the kernel takes 100–430 ms once per system, which
makes a single cold evolve slower than the array path. Pass it as `:solver`
when you evolve the same system many times or run sweeps.

Adaptive steppers, error measured on the 0.01 grid against an RK4 reference
at dt = 1e-4 over 8 s:

| system | method | max error | median |
|---|---|---|---|
| pendulum | RK4 kernel, dt 0.01 | 1.1e-8 | 0.25 ms |
| pendulum | raster tsit5, tol 1e-8 | 1.2e-8 | 1.7 ms |
| planar (chaotic) | RK4 kernel, dt 0.01 | 2.7 (diverged) | 0.45 ms |
| planar | RK4 kernel, dt 0.001 | 1.0e-4 | 5.8 ms |
| planar | raster tsit5, tol 1e-8 | 3.2e-4 | 3.8 ms |
| planar | raster dp5, tol 1e-8 | 8.1e-3 | 3.1 ms |
| planar | raster tsit5, default tol 1e-3 | 22 (diverged) | 1.5 ms |

On the smooth pendulum, fixed RK4 already reaches tsit5's accuracy for less
time. On the chaotic double pendulum, tsit5 at 1e-8 is about 1.5x cheaper
than an RK4 run of similar accuracy. That is a modest edge, so adaptive
stepping stays opt-in.

H over a point cloud (array vs raster `deftm`): 1e4 points take 0.054 vs
0.028 ms, 1e5 points 0.27 vs 0.28 ms, and 1e6 points 3.0 vs 2.3 ms. At figure
size raster is still about 10x slower, so the geometry kernel stays `:array`.

## Reference: the cljw lane (not run by `bb bench`)

These figures are copied from [cljw-lane.md](cljw-lane.md): the same 800-step
RK4 pendulum, plus 81 samples read back. `bb bench` does not run them, and they
add no runtime dependency.

| runtime | ms/call |
|---|---|
| Zig wasm kernel via cljw FFI | 1.3–1.9 |
| interpreted `phys/evolve` on cljw | 147–262 |
| `phys/evolve` on the warm JVM (that doc) | 11 |
| `phys/evolve` on the warm JVM (this baseline, `physics` clojure rk4 x800) | 2.3 |
