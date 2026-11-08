# DESARGUES-PERF-WASMRASTER: a wasm raster kernel for the live path (negative result)

Question: can a sandboxed wasm kernel (Zig compiled to wasm32, hosted in the JVM through
the zwasm runtime and Panama FFM) take over a raster step of the live IRaster path
(`desargues.manim.raster`, LIVE-2), and what does wasm cost compared with the same kernel
built natively?

Answer: **no**. Cairo is not where the frame time goes. The step that could be replaced
(the RGBA to premultiplied BGRA conversion) runs about 2x *slower* in zwasm than in the
current Clojure loop. zwasm also crashes the JVM when it runs next to Manim's embedded
CPython. Only this measurement ships. The experimental kernel and shim (described below)
were not committed.

## Where a live frame spends its time

Warm JVM and Python, 854x480 (`low_quality`), the `desargues.bench.live-raster` scene
(37 frames). The time is per frame.

| step | ms / frame | how measured |
|---|---|---|
| Cairo fill + stroke (inside Manim) | ~0.1 | cProfile: `stroke_preserve` + `fill_preserve` = 4 ms over 31 frames |
| Manim frame bookkeeping (`update_frame`, `set_pixel_array`, numpy copies) | ~0.9 | cProfile |
| Python -> JVM crossing (`as-tensor` on the numpy frame) | 1.05 | `frames/crossing-ns`, null sink |
| RasterSink conversion (int copy 0.07 + swizzle 0.43 + to bytes 0.08 + slot) | 0.92 | `crossing-ns` with the RasterSink minus the null sink |

So the candidate kernel is the format conversion (with an optional dirty rect), not Bezier
fill with AA strokes. Cairo spends about 0.1 ms per frame on the paths.

## Kernel and arms

The experiment's `kernel.zig` held `convert` (an `@Vector(8,u32)` opaque fast path
that swaps R and B, and a scalar premultiply `(a*c+127)/255` that is exact) and
`dirty` (the bounding rectangle of the change). The same file is built for three arms:

- **native**: `wr_native_*` in `libwasmraster.so`, called through FFM (`Linker.Option.critical`, on heap arrays).
- **wasm SIMD**: `guest.wasm` (wasm32 with simd128) embedded in the shim and run on the zwasm 2.7.0 JIT
  (`zig fetch git+https://github.com/BuddhiLW/zwasm`, commit e9d7493). The JVM copies pixels into
  linear memory at the addresses `wr_wasm_buffers` returns.
- **wasm scalar**: the same guest built with `-Dguest-simd=false`.

It was built with `zig build -Doptimize=ReleaseFast` (zig 0.16.0). The shim exported a C ABI:
`wr_native_convert`, `wr_wasm_open(kind)`, `wr_wasm_buffers`, `wr_wasm_convert` and `wr_wasm_frame`.

## Pixel error against the Cairo/Clojure path

Every arm is bit-identical to `mr/convert-words!` on a real Manim frame and on 409,920
random (translucent) words: **max abs error 0, PSNR infinite**.

## Per-frame convert time (ms, median of 100, each arm in its own JVM; host load about 30 on 22 cores)

| arm | opaque Manim frame | random translucent |
|---|---|---|
| native Zig via FFM | 0.14 | 1.7 - 2.0 |
| Clojure (current `convert-words!`) | 0.75 - 0.82 | 3.5 - 4.3 |
| zwasm JIT, SIMD guest: kernel / with copies | 1.18 - 1.5 / 1.4 - 1.7 | 5.6 / 7.8 |
| zwasm JIT, scalar guest: kernel / with copies | 2.8 / 3.3 | 7.6 / 7.9 |

The wasm tax against native is about 10x on opaque frames and 3-4x on translucent ones.
SIMD in the guest halves the opaque case and leaves the translucent case unchanged (the
JIT does not close the gap on the scalar premultiply).

## End to end (live render into a RasterSink twin, 9 runs after 3 warmups, one JVM per arm)

| arm | conversion ms / frame | CPU-s / render | wall ms / render |
|---|---|---|---|
| Clojure | 1.13 - 1.36 | 0.22 - 0.28 | 164 - 204 |
| native Zig | 0.71 - 0.99 | 0.22 - 0.28 | 130 - 227 |
| zwasm JIT | JVM aborted: `zwasm: internal error, caught a fatal signal` (exit 70) | | |

The CPU-s difference between Clojure and native is inside the noise, because Manim's
Python dominates the render. zwasm in a bare JVM is fine. In the JVM that hosts
libpython, numpy and the render thread it dies, and the interpreter engine
(`ZWASM_ENGINE_INTERP`) traps on the same call. zwasm documents its engine as
single-threaded per process, with process-global unwinder state.

## Decision

Do not add an `IRasterKernel` port or a wasm kernel:

1. wasm loses to the code it would replace;
2. the whole conversion is under 1 ms of a 66 ms (15 fps) or 16.7 ms (60 fps) budget;
3. in-process zwasm is unsafe beside Manim.

Even the native kernel saves only 0.3 - 0.6 ms per frame and needs a native build that CI
does not have. The lever that remains is the 1 ms crossing plus Manim's own numpy
bookkeeping, not rasterisation.
