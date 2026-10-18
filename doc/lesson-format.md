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
is committed so you can compare your hashes with ours.

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
evaluated. The notebook typesets it with `(clerk/tex (emmy.env/->TeX '<expr>))`,
so any form `->TeX` accepts will work, including `(= a b c)`, `(/ 1 3)`,
`(* 2 pi ...)` and `(expt x 2)`. An operator Emmy has no TeX rule for, such
as `integral`, is set as a sans-serif function name: `integral(x², x, 0, 1)`.

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
only, because compiling it needs the kernel toolchain.

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

A media block renders the block named by `:from` to `media/<block-id>.<format>`
at 960×540. `:from` must name an existing block that is not itself a media
block. Rendering is done by the headless PNG host
(`desargues.player.headless`, the path `bb showcase` uses). A PNG is the
figure or scene at its last instant. `:gif` and `:mp4` are 12 fps frame
sequences encoded by ffmpeg. Only `:figure` and `:scene` blocks have a
render path. If a block cannot be rendered (other kind, no AWT, no ffmpeg),
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
   hash of the file on disk. On any mismatch it prints `MISMATCH` with the
   differing files and exits 1.

5. Open the notebook. It needs the `:notebooks` classpath, because it
   requires `desargues.calc-emmy` (in `notebooks/`), emmy-viewers and Clerk.
   Its `:media` cells refer to `media/<file>` **relative to the bundle
   directory**, and `clerk/image` reads that path when the cell is evaluated.
   So run the JVM with the bundle directory as its working directory, using
   the repo's `:notebooks` classpath:

   ```sh
   CP=$(clojure -Spath -A:notebooks | tr ':' '\n' \
        | sed "s|^\([^/]\)|$PWD/\1|" | paste -sd:)   # make relative entries absolute
   cd dist/lessons/area-under-curve
   java -cp "$CP" clojure.main
   ```

   ```clojure
   (require '[nextjournal.clerk :as clerk])
   (clerk/serve! {})
   (clerk/show! "notebook.clj")
   ```

   If you evaluate it from the repo root instead, every cell runs except the
   `clerk/image` ones, which fail with "Can't get input stream from URL!".

   `notebook.clj` is generated output. Edit the lesson and rebuild instead
   of editing the notebook.

## JSON

`lesson.json` holds the same value as `lesson.edn`, encoded transit-style with
`~` as the escape character:

| EDN | JSON |
|---|---|
| keyword `:a/b` | `"~:a/b"` |
| symbol `a/b` | `"~$a/b"` |
| char, bigint, ratio, bigdec | `"~c…"`, `"~n…"`, `"~r…"`, `"~m…"` |
| a string starting with `~` | one more `~` |
| list, set | `{"~list": [...]}`, `{"~set": [...]}` |
| map with non-string-like keys | `{"~map": [[k, v], ...]}` |

Objects keep canonical key order, and doubles always print with `.` or `E`.
The full table is the "JSON convention" comment at the top of
[`desargues.lesson.export`](../src/desargues/lesson/export.clj). `->json` and
`<-json` round-trip it.

### Related: emmy-viewers fragment export

A `:viewer` block holds a bare fragment form. Outside a lesson, emmy-viewers
fragments can be exported on their own with `emmy.viewer.export`, on the
[`feat/fragment-export`](https://github.com/BuddhiLW/emmy-viewers/tree/feat/fragment-export)
branch of BuddhiLW/emmy-viewers (not yet in the version pinned by `:notebooks`).
It writes a fragment together with its Clerk viewer metadata, with the viewer
stored as a registered symbol: `fragment->edn` / `edn->fragment` and
`fragment->json` / `json->fragment`. It also offers `export-notebook!` for a
static Clerk build of a single notebook. Its JSON encoding differs from the
lesson one: every value is a one-key tagged object (`{"~kw": ...}`,
`{"~sym": ...}`) rather than a `~`-prefixed string. The two formats do not
interchange directly.

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

- **Media bytes depend on the host.** PNGs are painted with Java2D, so the
  JDK's font rasteriser and antialiasing affect the pixels. GIF and MP4 also
  depend on the ffmpeg version. On a single host, rebuilds are reproducible
  (verify checks the media too). Another JDK or ffmpeg may produce different
  hashes for `media/*` while every text hash still matches. When you compare
  with the committed manifest, compare the text files and treat media
  differences as a host difference.
- **Constructions have no render path yet.** A `:construction` block appears
  in the notebook only as data, and a `:media` block whose `:from` is a
  construction is skipped (`:no-render-path`). Drawing one requires
  compiling a board kernel.
- Nothing is verified in a browser. The manifest hashes the notebook
  *source*. It does not check how Clerk or emmy-viewers render it, and it
  does not check that the quoted `:viewer` code runs.
