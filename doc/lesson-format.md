# Lesson bundles

A **lesson bundle** is one EDN value: a lesson made of slides, each slide made
of blocks. Every block is plain data that desargues, Emmy or emmy-viewers
already understands. Nothing in it is a function, an atom or a host object.
Because it is only data, a bundle can be checked, written out, read back and
rebuilt to the same bytes.

From a bundle, `bb lesson:build` produces a Clerk notebook, a lossless JSON
copy, rendered media and a **manifest** of sha256 hashes. `bb lesson:verify`
rebuilds the bundle and checks those hashes.

The worked example is [lessons/area-under-curve.edn](../lessons/area-under-curve.edn),
six slides on the definite integral and the shell method. Its manifest,
[lessons/area-under-curve.manifest.edn](../lessons/area-under-curve.manifest.edn),
is committed so you can compare your hashes with ours. A second, smaller
example, [lessons/constructions-demo.edn](../lessons/constructions-demo.edn),
draws a board construction; its manifest is
[lessons/constructions-demo.manifest.edn](../lessons/constructions-demo.manifest.edn).

Namespaces:

| Namespace | Role |
|---|---|
| `desargues.lesson.schema` | the bundle schema (malli): `validate`, `explain`, `valid?`, `pure-data?` |
| `desargues.lesson.export` | `build!`, `verify!`, the JSON convention, the notebook generator |
| `desargues.scene.serialize` | canonical EDN, used for every text file the build writes |

## Shape

```clojure
{:lesson/id      :area-under-curve          ; keyword, also names dist/lessons/<id>
 :lesson/title   "Area under a curve, and the shell method"
 :lesson/version "1.0.0"
 :lesson/slides
 [{:slide/id :question
   :slide/title "The question"
   :slide/blocks [{:block/id :intro :block/kind :prose :md "..."} ...]}
  ...]}
```

Slide ids must be unique, and block ids must be unique across the whole
lesson. Every block has `:block/id` and `:block/kind`, plus the payload key
for its kind.

## Block kinds

### `:prose`: markdown

```clojure
{:block/id :intro :block/kind :prose
 :md "How much area lies under y = x^2 between 0 and 1?\n\nWe will *see* it, then compute it."}
```

In the notebook, prose becomes Clerk markdown comments.

### `:tex`: a frozen Emmy expression

```clojure
{:block/id :integral :block/kind :tex
 :expr (integral (expt x 2) x 0 1)}
```

`:expr` holds numbers, symbols and strings, plus lists headed by a symbol,
nested. Vectors are allowed for structures. The expression is **not**
evaluated. The notebook typesets it with
`(clerk/tex (desargues.lesson.tex/->TeX '<expr>))`: Emmy's `->TeX`, extended
through Emmy's extensible renderer (`emmy.expression.render/TeX-renderer`)
with rules for integrals. Any form Emmy's `->TeX` accepts works, including
`(= a b c)`, `(/ 1 3)`, `(* 2 pi ...)` and `(expt x 2)`, and in addition:

| Expression | TeX |
|---|---|
| `(integral f x a b)` | `\int_{a}^{b} f \, dx` |
| `(integral f x)` | `\int f \, dx` |

`integrate` is accepted as a synonym of `integral`. A sum or difference
integrand is parenthesized. Any other operator Emmy has no TeX rule for is
set as a sans-serif function name, e.g. `\mathsf{foo}(x)`. Further rules go
in `desargues.lesson.tex/integral-handlers`.

### `:figure`: a calc figure

```clojure
{:block/id :area-fig :block/kind :figure
 :figure {:title "area under y = x^2 on [0, 1]"
          :view {:view :plane :x-range [-0.5 1.5] :y-range [-0.5 1.5]
                 :x-length 8.0 :y-length 6.0 :center [0.0 0.0]}
          :params {:f :parabola :a 0.0 :b 1.0 :sweep 1.0}
          :animate {:param :sweep :from 0.0 :to 1.0 :duration 4.0}
          :objects [{:kind :area-under :id "area" :f {:calc/param :f}
                     :a {:calc/param :a} :b {:calc/param :b}
                     :upto {:calc/param :sweep} :color :teal}
                    {:kind :graph :id "graph" :f {:calc/param :f} :a -0.5 :b 1.5 :color :blue}]}}
```

The figure is a `desargues.objects.calc.core` figure, and it must pass
`calc/figure-problems`. Functions are named by catalog key (`:parabola`,
`:root-like`, ... see `desargues.objects.calc.functions`) and never given as a
bare fn. `{:calc/param k}` reads `k` from `:params`. A 3D figure uses
`:view {:view :space ...}`; the example's shell-method figure is a
`:revolution` object with `:method :shell`. In the notebook, a plane figure
goes through `calc-emmy/figure->mafs` and a space figure through
`calc-emmy/figure->mathbox`.

To get the data form of a figure built with the `c/figure` helpers, `prn` it.
`desargues.objects.calc.demos/area-figure` and `revolution-figure` are good
starting points.

### `:construction`: a board construction

```clojure
{:block/id :tri :block/kind :construction
 :construction {:kind :construction :id :tri
                :points [{:id :A :at [0 0]} {:id :B :at [2 0]} {:id :M :op :mid :of [:A :B]}]
                :draw [[:segment :A :B] [:point :M {:label "M"}]]}}
```

This is `desargues.board.construction` EDN. The schema checks its structure
only; the build checks the rest by sampling it.

A construction is drawn at its params' `:init` values. The build samples it
with `desargues.board/sample-board` through the `:jvm` kernel compiler
(`desargues.board.jvm`): the same kernel source raster compiles to wasm, run
as plain Clojure, so **no `:dynamics` alias and no raster** are needed.
`desargues.board.picture` then reads the board's layers against the sampled
frame: points (free points too), segments, lines, polygons, paths and traces,
and each check as a `label = value` readout. In the notebook the block is
its data followed by a static Mafs figure,
`(ev/fragment '[mafs.core/Mafs {:view-box ...} ...])`, over the
construction's `:window` (default x in [-6, 6], y in [-3.5, 3.5]). The
figure is computed at build time, so it is in the notebook source and its
hash. Nothing is draggable there. For a draggable board, compile it with
`desargues.board/compile-board!` (raster). A 3D construction (`:view`) is
projected orthographically by its camera's initial yaw and pitch.
Coordinates are rounded to 6 decimals, so the notebook text is the same on
every host. [lessons/constructions-demo.edn](../lessons/constructions-demo.edn)
is the worked example.

### `:viewer`: an emmy-viewers fragment

```clojure
{:block/id :mafs :block/kind :viewer
 :fragment (reagent.core/with-let [!p (reagent.core/atom [0.5 0.25])]
             [mafs.core/Mafs {:view-box {:x [-0.5 1.5] :y [-0.5 1.5]}}
              [mafs.coordinates/Cartesian {}]
              [mafs.plot/OfX {:y (fn [x] (* x x))}]
              [mafs.core/MovablePoint {:atom !p :constrain (fn [[x _]] [x (* x x)])}]])}
```

A fragment is the **quoted** Reagent form that the `emmy.mafs` /
`emmy.mathbox` / `emmy.viewer` builders return. The `(fn ...)` forms inside
it are symbols and lists, not JVM functions, and they are evaluated in the
browser. To produce one, call the builder in a REPL and print the result, for
example `(ev/with-let [!p [0.5 0.25]] (mafs/mafs ...))`. Replace the gensym
with a readable name such as `!p`. The notebook wraps the form in
`(emmy.viewer/fragment '<form>)`.

### `:scene`: a recorded scene graph

```clojure
{:block/id :dot :block/kind :scene
 :scene {:scene "dot"
         :nodes {1 {:node :dot :id 1 :opts {:color :gold} :at [0 0]}}
         :steps [{:step :play :anims [{:anim :appear :target 1 :opts {}}]}
                 {:step :hold :seconds 1}]
         :node-count 1}}
```

This is a `desargues.scene.data` recording: the output of
`(desargues.scene.record/record name construct)`. Record the scene in a REPL,
print it with `desargues.scene.serialize/->canonical-edn` and paste the text in.
The example's Riemann-sum scene comes from
[dev/desargues/lesson/area_under_curve.clj](../dev/desargues/lesson/area_under_curve.clj),
via `(print-scene)`. In the notebook, the scene is shown as data and then as
its last frame, painted by the headless player.

### `:media`: a rendered asset

```clojure
{:block/id :area-png    :block/kind :media :from :area-fig      :format :png}
{:block/id :riemann-gif :block/kind :media :from :riemann-scene :format :gif}
```

A media block renders the block named by `:from` to `media/<name>.<format>`
at 960×540, and the notebook embeds the rendered bytes as a `data:` URI.
`<name>` is the block id in lower case, with every run of characters
outside `[a-z0-9]` replaced by one `-` (`:riemann-gif` gives `riemann-gif`,
`:fig/a` gives `fig-a`). The same name gives the notebook var `block-<name>`
for figure, scene and construction blocks. The mapping is lossy, so the
build refuses a lesson in which two block ids give the same name (for
example `:fig/a` and `:fig.a`, or `:Area` and `:area`). The error names both
ids. `:from` must name an existing block that is not itself a media
block. Rendering is done by the headless PNG host
(`desargues.player.headless`, the path `bb showcase` uses). A PNG is the
figure or scene at its last instant. `:gif` and `:mp4` are 12 fps frame
sequences encoded by ffmpeg. `:figure`, `:scene` and `:construction`
blocks have a render path. A construction is painted as the same picture
the notebook shows (`desargues.board.picture/->scene`), fitted to the frame,
with no alias needed. If a block cannot be rendered (`:prose`, `:tex` or `:viewer`, recorded as
`:no-render-path`; no AWT; no ffmpeg),
it is **skipped**: the build warns on stderr, records it under the manifest's
`:skipped`, and the notebook says so. The build does not fail.

## Author, build, verify

1. Write `lessons/<name>.edn`. [`test/desargues/lesson/tiny_lesson.edn`](../test/desargues/lesson/tiny_lesson.edn)
   is a minimal template.

2. Validate it in a REPL. `explain` returns nil when the lesson is valid, and
   otherwise a map with `:impure`, `:schema` and/or `:references` problems:

   ```clojure
   (require '[desargues.lesson.export :as lx] '[desargues.lesson.schema :as ls])
   (ls/explain (lx/read-lesson "lessons/area-under-curve.edn"))   ;; => nil
   ```

   The build runs `ls/validate` as well and refuses an invalid lesson.

3. Build. The default output directory is `dist/lessons/<id>`, which is
   gitignored. An optional second argument gives a different one.

   ```sh
   bb lesson:build lessons/area-under-curve.edn
   # same as: clojure -M -m desargues.lesson.export build lessons/area-under-curve.edn [out-dir]
   ```

   It prints one `<sha256> <file>` line per file it wrote, and a `SKIPPED`
   line for each media block it could not render. The bundle directory
   contains:

   | File | Content |
   |---|---|
   | `lesson.edn` | the lesson as canonical EDN |
   | `lesson.json` | the lesson as lossless JSON (below) |
   | `notebook.clj` | a generated Clerk notebook, one section per slide |
   | `media/*.png\|gif\|mp4` | one file per rendered media block |
   | `manifest.edn` | hashes and pins (below) |

4. Verify:

   ```sh
   bb lesson:verify dist/lessons/area-under-curve
   # lesson:verify dist/lessons/area-under-curve REPRODUCIBLE
   ```

   Verify re-reads `lesson.edn`, rebuilds it in a temp directory, and compares
   three things for every file: the manifest hash, the rebuilt hash and the
   hash of the file on disk. It also reports files in the directory that the
   manifest does not list, and manifest entries (`:lesson/id`, `:deps`,
   `:notebook-deps`, `:skipped`) that differ from the rebuild. On any
   mismatch it prints `MISMATCH` with each difference and exits 1.

   `lesson:build` writes only into an empty directory or a previous bundle
   (one holding `manifest.edn`). It removes files there that the new manifest
   does not list. It refuses the working directory, any ancestor of it, and
   a git work tree root.

5. Open the notebook. It needs the `:notebooks` classpath, because it
   requires `desargues.calc-emmy` (in `notebooks/`), emmy-viewers and Clerk.
   Rendered media is **embedded** in `notebook.clj` as `data:` URIs
   (`clerk/html` with an `img`, or a `video` for MP4). The notebook reads no
   file at run time and holds no path. So it can be opened from any working
   directory, shown with `clerk/show!` or built statically with
   `clerk/build!`. The `media/` files stay in the bundle as hashed
   artifacts. Clerk caches a cell by the hash of its form, so new media
   bytes give a new form and are never served from a stale cache entry. For
   example, from the repo root:

   ```sh
   clojure -A:notebooks
   ```

   ```clojure
   (require '[nextjournal.clerk :as clerk])
   (clerk/serve! {})
   (clerk/show! "dist/lessons/area-under-curve/notebook.clj")
   ```

   `notebook.clj` is generated output. Edit the lesson and rebuild instead
   of editing the notebook.

## JSON

`lesson.json` holds the same value as `lesson.edn`, in the **emmy-viewers
fragment JSON encoding, v1**: the one `emmy.viewer.export` writes for an
exported fragment ([encoding table in emmy-viewers'
DEVELOPING.md](https://github.com/BuddhiLW/emmy-viewers/blob/main/DEVELOPING.md),
source in [`emmy/viewer/export.cljc`](https://github.com/BuddhiLW/emmy-viewers/blob/main/src/emmy/viewer/export.cljc)).
Desargues and emmy-viewers share one convention, so one decoder reads both
lesson.json and a fragment's `"form"` / `"meta"`.

| EDN | JSON |
|---|---|
| `nil`, `true`, `false`, string | `null`, `true`, `false`, string (never escaped) |
| integer, double | number (integral doubles keep `.0`) |
| `##Inf`, `##-Inf`, `##NaN` | `{"~double": "Infinity"}` etc. |
| ratio, bigint, bigdec | `{"~ratio": "1/2"}`, `{"~bigint": "1"}`, `{"~bigdec": "1.5"}` |
| char `\a` | `{"~char": "a"}` |
| keyword `:a/b`, symbol `a/b` | `{"~kw": "a/b"}`, `{"~sym": "a/b"}` |
| vector | array |
| list, set | `{"~list": [...]}`, `{"~set": [...]}` |
| map (any keys) | `{"~map": [[k, v], ...]}` |

Every JSON object in the encoding has exactly one key, one of these tags.
Map entries and set members are in canonical order (type rank, then printed
form), so the output is as deterministic as `lesson.edn`. The file is a
document mirroring the fragment one:

```json
{"format": "desargues/lesson", "version": 1, "lesson": J(lesson)}
```

(a fragment is `{"format": "emmy-viewers/fragment", "version": 1, "form": ..., "meta": ...}`).
The codec is [`desargues.lesson.json`](../src/desargues/lesson/json.cljc),
pure and free of any emmy-viewers dependency; `lesson->json` /
`json->lesson` write and read the document, `->json-data` / `<-json-data`
the bare encoding. Its tests check it byte for byte against
`emmy.viewer.export` when emmy-viewers is on the classpath
(`clojure -M:test:notebooks`).

### Related: emmy-viewers fragment export

A `:viewer` block holds a bare fragment form. Outside a lesson, emmy-viewers
fragments can be exported on their own with `emmy.viewer.export`, released in
[`io.github.buddhilw/emmy-viewers 0.5.0-fork.1`](https://clojars.org/io.github.buddhilw/emmy-viewers)
on Clojars, which is the version `:notebooks` pins.
It writes a fragment together with its Clerk viewer metadata, with the viewer
stored as a registered symbol: `fragment->edn` / `edn->fragment` and
`fragment->json` / `json->fragment`. It also offers `export-notebook!` for a
static Clerk build of a single notebook. Its JSON uses the same encoding as
`lesson.json` (above).

## What the manifest guarantees

```clojure
{:lesson/id :area-under-curve
 :generated-by "bb lesson:build lessons/area-under-curve.edn"
 :deps {emmy-lib {:git/sha "..."} malli-lib {:mvn/version "..."} ...}   ; deps.edn :deps
 :notebook-deps {...}                                                    ; :notebooks :extra-deps
 :files {"lesson.edn" "<sha256>" "lesson.json" "..." "notebook.clj" "..." "media/area-png.png" "..."}
 :skipped []}
```

**Guaranteed:**

- The **text outputs** (`lesson.edn`, `lesson.json`, `notebook.clj`,
  `manifest.edn`) are **byte-identical** on every build of the same lesson
  with the same code. They contain no timestamps and no absolute paths, maps
  are written in canonical order, and the layout is a pure function of the
  data. They do not depend on the host.
- The **dependency versions** used for the build are pinned in the manifest:
  each git dep's `:git/sha` and each Maven dep's `:mvn/version`, for both the
  library and the notebook alias.
- `lesson:verify` checks all of this. A changed lesson, a hand-edited output
  or a code change that alters the output gives `MISMATCH`.

**Not guaranteed:**

- **Canonical text is byte-identical on the JVM only.**
  `desargues.scene.serialize` is `.cljc`, but under ClojureScript a
  1-character string is also `char?` (so it sorts as a char) and a whole
  double prints without its decimal point (`1.0` as `1`). CLJS output reads
  back to an equal value, but its bytes can differ. Bundles and manifests are
  built and verified on the JVM.
- **Media bytes depend on the host.** PNGs are painted with Java2D, so the
  JDK's font rasteriser and antialiasing affect the pixels. GIF and MP4 also
  depend on the ffmpeg version. On a single host, rebuilds are reproducible
  (verify checks the media too). Another JDK or ffmpeg may produce different
  hashes for `media/*` while every text hash still matches. When you compare
  with the committed manifest, compare the text files and treat media
  differences as a host difference.
- **The manifest does not cover rendering.** It hashes the notebook
  *source*. How Clerk and emmy-viewers render it is checked separately, by
  `bb lesson:e2e` (below). That check needs npm and the network, and its
  result is not part of `lesson:verify`.

## Browser check: `bb lesson:e2e`

```sh
bb lesson:e2e                       # lessons/area-under-curve.edn
bb lesson:e2e lessons/<name>.edn
```

The task runs three steps and exits non-zero if any of them fails:

1. `lesson:build` of the lesson into `dist/lessons/<id>`.
2. A static Clerk build of its `notebook.clj` into `dist/lesson-e2e/<id>`,
   made by `clojure -M:notebooks:lesson-e2e -m desargues.lesson.static-build
   <notebook> <out>`. This step goes through `emmy.clerk/build!`, with a
   viewer bundle that shadow-cljs compiles here from
   `notebooks/desargues/lesson/sci_extensions.cljs`: Clerk's static app plus
   emmy-viewers' SCI namespaces. The prebuilt emmy-viewers bundle on
   Clerk's CDN, which is `emmy.clerk/build!`'s default, throws on load and
   leaves the page blank. The npm deps go into the root `package.json` and
   `node_modules/`, both gitignored. `three` is pinned to 0.150.0: newer
   releases call `process.emitWarning` in the browser, which stops Mafs from
   mounting. The first build takes several minutes.
3. `node dev/e2e/lesson_render.mjs <lesson.edn> <static dir>`. It uses
   `playwright-core` (`dev/e2e/package.json`, installed on first run into
   `dev/e2e/node_modules`) with Playwright's downloaded Chromium headless
   shell (`~/.cache/ms-playwright`; set `CHROMIUM` to use another binary).
   It serves the build on a local port, opens it, and checks:
   - no page errors and no console errors;
   - every `:slide/title` of the lesson is a heading on the page;
   - KaTeX rendered the integrals: a `.katex` element whose text holds `∫`;
   - the embedded PNG and GIF media and every other `<img>` loaded
     (`naturalWidth > 0`);
   - the `:viewer` fragment's Mafs mounted: an `<svg>` inside `.MafsView`;
   - the recorded scene's last frame (Clerk's `_data/*.png`) is on the page.

   It writes a full-page screenshot to `dist/lesson-e2e-<id>.png`.
