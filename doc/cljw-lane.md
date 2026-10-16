# The cljw lane: desargues' pure half on ClojureWasm

Measured 2026-10-11 with ClojureWasm (cljw) v1.14.11 (ReleaseSafe), Clojure 1.12.3 on
the JVM, Zig 0.16.0, on a 22-core Linux box. It follows on from the 2026-09-06 check (cljw
1.13.4), which found that `desargues.layout.core` and `desargues.scene.data` load
and that `layout->scene-graph` runs about 6x slower than on the JVM.

The scripts are in `dev/cljw/`. Every number below comes from running them.

## What loads

`dev/cljw/probe.clj` requires each candidate namespace and prints ok/FAIL with the first
error line. `dev/cljw/lay-cp.sh` unpacks the Maven jars the pure half needs into
`target/cljw-cp/` (cljw's `-cp` only takes directories, and it skips Maven coordinates)
and puts two probe-only shims first (`dev/cljw/shim/`).

| namespace | `-cp src` | + jars + shims | decisive error |
|---|---|---|---|
| `desargues.scene.protocols`, `scene.data`, `scene`, `layout.core` | ok | ok | |
| `desargues.physics` (RK4, `TrajectorySolver`; measured as `desargues.videos.physics`, since moved) | ok | ok | loads, but computes the wrong numbers (see below) |
| `desargues.domain.number-theory(-services)` | FAIL / ok | ok | `typed.clojure` missing; with the real runtime: `Unable to resolve symbol: 'clojure.lang.RT'` |
| `desargues.domain.protocols` | FAIL | ok with the typed shim | as above |
| `desargues.domain.math`, `math-expression`, `services` | FAIL | FAIL | Emmy |
| `desargues.pipeline.emmy`, `geometry.projective`, `geometry.fit`, `geometry.scene`, `videos.lagrangian`, `brachistochrone-pure` | FAIL | FAIL | Emmy |
| `desargues.geometry.disposition` | FAIL | FAIL | core.logic: `__extend-type!: expected protocol, got type_descriptor` at `clojure/core/logic.clj:74`, a `deftype` that implements the Java interface `java.util.Map$Entry` |

**Typed Clojure.** The annotations are the only thing keeping `domain.protocols` and
`number-theory` off cljw. `typed.clj.runtime` reaches into `clojure.lang.RT`. A 9-line
shim (`dev/cljw/shim/typed/clojure.clj`) turns `ann`, `ann-protocol`, `ann-record`,
`ann-datatype` and `defalias` into no-ops, and with it both namespaces load.

**Emmy** (v0.32.0) does not load. Walking up its namespaces (`dev/cljw/emmy_probe.clj`):

1. `emmy.util.def` requires `cljs.analyzer.api` with no reader conditional around it.
   On the JVM, ClojureScript arrives as a transitive dependency. A stub in the shim gets past it.
2. `emmy.util/sci-macro` expands to `(clojure.core/defmacro ...)`, and cljw fails with
   `macro Var 'defmacro' root binding is not callable`. **This is a cljw bug.** cljw only
   recognises a `defmacro` head when it is unqualified. Repro: `dev/cljw/repro_shadow.clj`
   (second form). With `CLJW_PATCH_EMMY=1`, `lay-cp.sh` shadows `emmy.util` with a copy
   whose head is unqualified, and `emmy.util`, `emmy.util.def` and `emmy.value` then load.
3. `emmy.ratio` fails in `emmy.generic` at the first `defgeneric`:
   `macro return value of tag 'type_descriptor' cannot be re-analysed as a form`.
   Emmy's generic dispatch puts host class objects (`Keyword`, `RT`, `BigInt`, `Ratio`)
   inside its macro output. Emmy also relies on commons-math3 (Java) and
   `clojure.lang` internals. This is where the probe stopped. Getting Emmy onto cljw
   would mean porting it, and shims will not do it.

So the parts of desargues that run on cljw are the scene facade, the RecordingBackend,
the layout algebra and the hand-written physics. Every Emmy-backed path stays on the JVM:
the derivative conveyor, the Lagrangian pendulums, projective geometry and the fits.

## End to end: the landing-page pendulum, recorded on cljw

`dev/cljw/pendulum_scene.clj` is the `pendulum-construct` from `site/desargues/site/deck.clj`,
copied verbatim. It integrates a damped pendulum with `phys/evolve` (RK4, dt 0.01, 8 s),
records it through `desargues.scene` on the RecordingBackend and writes the EDN scene graph.
It **runs on cljw** and produces a graph of the same shape: 6 nodes, 82 steps, 246
animations.

**The numbers are wrong.** `dev/cljw/parity.clj` compares the two graphs. 320 leaves
differ, every bob and rod coordinate from the first sample on: 2.6556 on cljw against
2.6536 on the JVM at t = 0.1 s. `dev/cljw/rk4_diag.clj` traces this to one step. On cljw,
RK4 collapses to an Euler-like step: θ stays put and ω moves by exactly dt·k1. The cause is
a **cljw semantic bug**:

```clojure
(defrecord R [a]
  P
  (f [_ {:keys [a]}] a))   ; JVM: the destructured param  cljw: the record FIELD
```

`Pendulum/derivatives` is written `[this {:keys [theta omega]}]`, and `theta` and `omega`
are also field names. On cljw the fields win, so k2, k3 and k4 are all evaluated at the
start state. A plain parameter `[_ a]` and a `let` destructure both shadow correctly. Only
destructuring in the method's own parameter vector goes wrong. Repro:
`dev/cljw/repro_shadow.clj` (prints `[:field :param :param]` on cljw and
`[:param :param :param]` on the JVM). Nothing throws. The animation looks plausible
and is wrong.

### Timings (pendulum scene)

| | cljw | JVM |
|---|---|---|
| cold process to first graph written (`NO_BENCH=1`) | **0.21 s wall**, 0.16 s user, 31 MB RSS | 8.2 s wall, 10.7 s user, 313 MB RSS |
| first `record` inside the process | 135–213 ms | 179–185 ms |
| `phys/evolve`, 800 RK4 steps, steady | 147–267 ms/call | 11 ms/call (2nd round of 50) |
| record the whole scene, steady | 186–257 ms/call | 13 ms/call |

At steady state cljw interprets about 15–20x slower than the warm JIT. Earlier
`layout->scene-graph` measurements put it at 6x. Numeric loops over maps and records are
cljw's worst case. To first output, though, cljw is about **40x faster** and uses a tenth
of the memory. For a one-shot render that is the number that counts.

## A wasm ODE kernel (Zig), measured

`dev/cljw/kernel/pendulum.zig` (11 KB of wasm, built with the command in its header) runs
the same RK4 on the same model as plain f64 code. It writes (θ, ω) every 10th step into a
buffer the guest owns. `dev/cljw/kernel_bench.clj` drives it through `wasm/load`,
`wasm/call` and `wasm/mem-read`.

| 800 RK4 steps + 81 samples back | ms/call |
|---|---|
| Zig wasm kernel via cljw FFI (load once, call, read buffer) | **1.3–1.9** |
| interpreted `phys/evolve` on cljw | 147–262 |
| `phys/evolve` on the warm JVM | 11 |
| one bare `wasm/call` crossing | 0.003 |

The kernel agrees with JVM `phys/evolve` to **2.2e-16** over all 81 samples (`parity.clj`).
It is right, and the interpreted cljw path is wrong. It runs about 100–150x faster than
interpreting on cljw and about 6x faster than the warm JVM.

Two cljw FFI limits shaped the kernel. Both are documented and neither is a bug:

- The JIT engine dispatches exports with at most 5 parameters. A 7-parameter
  `pendulum_rk4` trapped, so the constants go in through `set_params` first.
- A void export that takes f64 has no JIT call shape, so `set_params` returns a `u32`.

## What a JVM-free trajectory renderer would need

1. **A correct physics core on cljw.** That means either a fix for the field-shadowing
   bug upstream, or a desargues change that renames the destructured locals in `Pendulum/derivatives`
   (one line, e.g. `{th :theta om :omega}`), plus any other record method that
   destructures one of its own field names. The other records were not audited. Before any cljw output is
   trusted, the parity check (`parity.clj`) has to be a test.
2. **No Emmy at render time.** Anything that needs equations Emmy derived, such as the
   Lagrangian double pendulums, would have to compile them ahead of time on the JVM and
   hand them over as data (coefficients, or a generated kernel). It cannot happen inside
   the cljw process.
3. **A consumer for the EDN.** The RecordingBackend graph is already what plato plays in
   the browser. A JVM-free *video* still needs Manim, which means CPython. So "JVM-free"
   realistically means "EDN scene graph → plato/browser", or a new rasteriser. It does not
   mean mp4 through Manim.
4. **Typed Clojure as a shim, or `:cljw` reader conditionals**, if `desargues.domain`
   should ride along. The shim works today. It does not belong in `src/`.

## Recommendation: no-go for now, with conditions to revisit

- **Do not build the cljw lane now.** The only half that runs is the facade, the layout
  and hand-written physics. That half gives *wrong numbers* without any error, because of
  a cljw semantic bug. Today the trajectory cost on the JVM is 11 ms, so no performance
  problem exists for it to solve. cljw's real advantage is start-up: 0.2 s against 8 s.
  The plato prerender already uses that wherever cljw is installed.
- **The wasm ODE kernel is the most promising piece**, and its case is correctness and
  portability, not JVM speed. It is about 6x faster than the warm JVM and 100x faster
  than interpreting on cljw, and it agrees with the JVM to the last bit. Keep it as a
  `TrajectorySolver` provider idea, next to raster. It is not a reason to move off the JVM.
- **Revisit when all of these hold:**
  (a) cljw fixes destructured-parameter shadowing of record fields, and qualified
  `clojure.core/defmacro`. Both repros are in `dev/cljw/repro_shadow.clj`.
  (b) There is a concrete JVM-free consumer: a single-binary CLI that writes scene EDN
  for plato, or a serverless prerender where 8 s of JVM cold start hurts.
  (c) The trajectories it needs can be written without Emmy at runtime, either as
  hand-written models or as Emmy-compiled kernels exported ahead of time.
  When all three hold, ship the scene facade, the RecordingBackend and a wasm
  `TrajectorySolver` as a `cljw build` binary, with `parity.clj` as its gate.

## Update 2026-10-12: the five issues are fixed in open ClojureWasm PRs

The measurements above are on cljw 1.14.11. Fixes for all five issues listed below
are on branches of `BuddhiLW/ClojureWasm`, open as pull requests and not yet in a
release:

| issue | fix | PR |
|---|---|---|
| 1, 2 | destructured method params shadow fields; `clojure.core/`-qualified `defmacro`/`binding`/`ns` | #52 |
| 3 | a macro may return a class object (re-formed as its class symbol) | #53 (stacked on #52) |
| 4, 5 | `java.util.Map$Entry` on a deftype via an `IMapEntry` protocol; script args reach `*command-line-args*` | #54 |

On a cljw built from #53, `parity.clj` reports `steps cljw=82 jvm=82; differing
leaves: 0` (it was 320): the recorded pendulum scene matches the JVM exactly, so
revisit condition (a) holds once those PRs are released. Conditions (b) and (c) are
unchanged, and so is the recommendation.

Emmy now loads through `emmy.generic` without the `CLJW_PATCH_EMMY` workaround. It
stops at `emmy.complex` (`emmy/complex/impl.cljc:149`): cljw's regex compiler caps a
pattern at 7 capture groups and that pattern has 9. core.logic gets past its `Pair`
type and stops at `logic.clj:716`, `(. clojure.lang.RT (nextID))`.

Since these measurements the JVM side also moved: the 800-step pendulum takes about
2–5 ms and the Emmy-derived planar double pendulum about 1–2 ms
(see [benchmarks.md](benchmarks.md)).

## cljw issues found (reported here; ClojureWasm was not modified)

1. Destructuring in a defrecord method's parameter vector does not shadow a record field
   with the same name. Wrong value, no error. `dev/cljw/repro_shadow.clj`.
2. A `clojure.core/defmacro` head is not recognised as `defmacro`
   (`macro Var 'defmacro' root binding is not callable`). `dev/cljw/repro_shadow.clj`.
3. A macro whose expansion embeds a host type object cannot be re-analysed
   (`macro return value of tag 'type_descriptor' cannot be re-analysed as a form`).
   Found via Emmy's `defgeneric`. This may be by design.
4. `deftype` cannot implement a JVM interface such as `java.util.Map$Entry`
   (`__extend-type!: expected protocol, got type_descriptor`). Found via core.logic. This is
   expected given that cljw has no JVM interfaces, but it means core.logic does not load.
5. `cljw script.clj extra-arg` treats the trailing argument as another file to open
   (`Error opening cljw: FileNotFound`). Script arguments need `-M`/`-m`.

## Reproduce

```sh
# what loads (bare, then with jars and shims, then with the Emmy patch)
cljw -cp src dev/cljw/probe.clj
cljw -cp "$(dev/cljw/lay-cp.sh)" dev/cljw/probe.clj
cljw -cp "$(CLJW_PATCH_EMMY=1 dev/cljw/lay-cp.sh)" dev/cljw/emmy_probe.clj

# end to end + parity + kernel
NO_BENCH=1 cljw -cp src dev/cljw/pendulum_scene.clj      # cold, cljw
cljw -cp src dev/cljw/pendulum_scene.clj                 # benchmarks, cljw
clojure -M dev/cljw/pendulum_scene.clj jvm               # same on the JVM
(cd dev/cljw/kernel && zig build-exe pendulum.zig -target wasm32-freestanding \
   -O ReleaseFast -fno-entry -rdynamic -fstrip -femit-bin=pendulum.wasm)
cljw -cp src dev/cljw/kernel_bench.clj
clojure -M dev/cljw/parity.clj                           # graph diff + kernel check
cljw dev/cljw/repro_shadow.clj; clojure -M dev/cljw/repro_shadow.clj
```
