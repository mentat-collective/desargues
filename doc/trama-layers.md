# Trama layers (fosfora)

fosfora renders with wgpu and loads **trama sources**: one WGSL file per node,
which naga validates. It has no GLSL or ISF loader. desargues therefore
compiles field forms (`desargues.wallpaper.field`) straight to WGSL (the
`:wgsl` source target, `desargues.wallpaper.wgsl`) and packages them as trama
sources (`desargues.wallpaper.trama`), reached through
`desargues.wallpaper.emit` with `:target :trama`. The ISF/GLSL layers
(`doc/glsl-layers.md`) are unchanged and stay useful for other VJ hosts.
For fosfora's effect catalog (full layer effects, `.pfx` + `.wgsl`), use
`:target :pfx`, which shares this WGSL body but delivers the rate integral in
an extra slot: see `doc/pfx-effects.md`.

## Emit a trama source

```clojure
(require '[desargues.wallpaper.emit :as emit]
         '[desargues.wallpaper.field :as fld])

(emit/emit! {:id :quasicrystal                ; file stem; - becomes _
             :title "7-fold quasicrystal"     ; manifest "name"
             :target :trama
             :form fld/quasicrystal           ; {:params [u scale] :body ..}
             :phase 'u                        ; the form's loop phase (default u)
             :period 12                       ; loop_rate units per loop
             :view [14.22 8.0]                ; scene window, fitted to the output
             :blend :screen                   ; hint comment only
             :params [['scale {:default 3.0 :min 1.0 :max 8.0}]]}
            "out/trama")
;; => {:ok [".../out/trama/quasicrystal.wgsl"]}
;;    or a named err; nothing is written
```

`emit/layer-files` returns `{"<id>.wgsl" text}` and touches no disk.
`emit/emit-all!` emits a list of layers. The bare compiler is
`(fld/source :wgsl form {})`, which returns `{:ok {:function :helpers :params :ir}}`.

## The file

```wgsl
/*! trama
{ "name": "7-fold quasicrystal", "id": "quasicrystal", "kind": "source", "inputs": 0,
  "params": [ {"type":"Float","name":"loop_rate","default":1.0,"min":0.0,"max":4.0},
              {"type":"Float","name":"scale","default":3.0,"min":1.0,"max":8.0} ],
  "rates": ["loop_rate"], "integers": [] }
*/
// desargues: blend=screen
fn desargues_field(fx: f32, fy: f32, p0: f32, p1: f32) -> vec3f { ... }
@fragment
fn fs_main(@builtin(position) frag_coord: vec4f) -> @location(0) vec4f { ... }
```

The file declares only the manifest comment, the blend hint, its own helpers
(all prefixed `desargues_`), `desargues_field` and `fs_main`. fosfora's
preamble supplies `u` (with `u.resolution`), `param(i)` and the bindings, and
the file declares none of them again.

## The manifest mapping

- **Keys.** The manifest has exactly `name id kind inputs params rates
  integers`, in that order. fosfora reads it with `deny_unknown_fields`, so
  there are no extra keys: the blend mode lives only in the
  `// desargues: blend=<mode>` comment, and the period and view only in a
  plain comment.
- **id.** `:id` with `-` read as `_`. It must match `[a-z0-9_]+` and is the
  file stem.
- **kind and inputs.** Always `"source"` and `0`.
- **Slot 0 is `loop_rate`.** It is a Float with default 1, range 0..4, and it
  is the only entry in `"rates"`. fosfora hands the shader its running
  integral, so the shader computes `phase = fract(param(0u) / period)`. The
  layer loops seamlessly and never uses `u.time * speed`. Setting `loop_rate`
  to 0 freezes the loop.
- **Slots 1..15** are the layer's `:params`, in their declared order, each a
  Float with `default`, `min` and `max`. Param names are snake_case
  (`[a-z][a-z0-9_]*`, `-` read as `_`). `:label` is not emitted, because the
  manifest has no key for it.
- **Integer controls.** Add `:integer? true` to a param spec, for example
  `['bands {:default 3 :min 1 :max 8 :integer? true}]`. Its snake_case name is
  included in `"integers"`, in declaration order, so Fosfora can show an
  integer-stepped Float slider. The Float type and its scalar slot do not change.
  The shader also reads `floor(param(slot) + 0.5)` (nearest integer, ties toward
  positive infinity), so automation or a host ignoring the UI hint cannot send
  fractional values into the field. Continuous controls, phase and loop_rate
  are not rounded. Omitted or false keeps a continuous control.
  Defaults and bounds must be integral signed 32-bit values (e.g. `3.0` is
  accepted), with the default in range; invalid metadata returns `:layer/invalid`.
  Float32 precision still applies to very large integers.
- **Pixel to scene.** This works as `desargues.wallpaper.render` does it:
  `frag_coord.xy` is centred on `u.resolution` and divided by
  `min(res.x / view_w, res.y / view_h)`, with y up. The whole `:view` stays
  visible and the longer axis extends, so 2560x1080 (21:9) shows a wider
  plane instead of a stretched one.
- **Output.** `vec4f(clamp(rgb, 0, 1), 1.0)`, always opaque. fosfora's blend
  modes do the mixing.

## Append-only params

fosfora packs params into slots by declaration order, and saved bindings and
presets refer to slots. Param order is therefore explicit and stable:

- `:params` is an **ordered vector** of `[symbol spec]` pairs, and its order is
  the slot order. Neither the form's `:params` order nor map iteration order
  is ever used.
- The legacy map shape `{'scale spec}` is accepted only with
  `:order ['scale ..]` naming every key exactly once. Without it the emitter
  returns `{:error :layer/unordered-params}`.
- `loop_rate` is always slot 0, the same in every layer.
- **Add new params at the end only.** Never reorder, rename or remove a
  published param, because each of those shifts or breaks the slots that
  saved bindings point to. To retire a param, keep it in place and stop
  using it.
- Emitting the same spec again gives byte-identical output, and the tests
  check this.

## What compiles

The whole field vocabulary compiles, the same as for `:glsl`: `+ - * /`,
`sin cos tan exp log sqrt pow atan2 abs floor min max`, `fract mix clamp let`,
`frac wave osc`, `smoothstep`, `cospal`, `pi` and `tau`. The WGSL notes:

- Every literal is an f32 with a decimal point. WGSL has no implicit
  int-to-float conversion.
- Sub-trees made only of literals are folded on the JVM. WGSL evaluates
  constant expressions when the shader is created and rejects a fold that
  overflows or leaves the domain.
- `pow` compiles to `desargues_pow`, which follows `Math/pow`: an integer
  exponent keeps the sign of a negative base, NaN becomes 0 and infinity
  becomes 1e30. It is written with `select` because WGSL has no `?:`.
- `atan2` is WGSL's `atan2`.

## Limits and refusals

Every refusal is a hive-dsl err, and no file is written:

| err | when |
|---|---|
| `:layer/invalid` | the spec fails `LayerSpec`, there is no such phase, a param has no `:default`, a param is declared twice or the form lacks it |
| `:layer/unordered-params` | `:params` is a map without a valid `:order` |
| `:trama/bad-id` | the id (with `-` read as `_`) is not `[a-z0-9_]+` |
| `:trama/too-many-params` | more than 16 slots, counting `loop_rate`, so at most 15 params |
| `:trama/bad-param` | a name is not snake_case or is `loop_rate`, or `:min`/`:max` is missing, or the default lies outside them |
| `:field/unsupported` (`:target :wgsl`) | a construct WGSL cannot express, such as a non-finite literal, a literal beyond the f32 range, a constant fold to NaN or infinity, or a lowered op with no WGSL mapping; `:construct` names it |
| `:field/bad` | the form does not lower (unknown op, bad let) |

Other limits:

- Params use Float slots, including integer-stepped controls. fosfora's Bool,
  Color and Point2D would need new field types.
- Only field forms are emitted. `:frame`, `:splat`, `:light`, `:post` and
  `:scene` are not.
- Precision is f32. The tests read the WGSL tree on the CPU in double and in
  float32 and hold both to the JVM shader within one level of 255.

## Validation

`test/desargues/wallpaper/trama_test.clj` covers:

- CPU parity of the WGSL tree against the JVM shader;
- the golden manifest (`golden/quasicrystal.trama-manifest.json`);
- slot order, append-only growth and byte-identical re-emission;
- every refusal;
- the file's structure: it declares no preamble names and every helper
  carries the `desargues_` prefix.

When `naga` is found (`$DESARGUES_NAGA`, then the system property
`desargues.naga`, then `PATH`), each emitted file is validated behind
`golden/trama-stub-preamble.wgsl`. That stub is a minimal stand-in for
fosfora's preamble (`u` with `time`, `resolution`, `params: array<vec4f,4>`,
`param()`, and a fullscreen vertex stage). It is not fosfora's preamble.
Without naga the test says so and runs only the structural checks. fosfora
validates every file itself on load, and a broken file is logged while the
last good version keeps running.

## Delivering to fosfora

The files go to a directory you choose, such as `out/trama/` or tessera's
`out/layers/`. Getting them into fosfora is Pedro's call:

- copy them into fosfora's watched `assets/trama/effects/`, or
- push from his nREPL with `(f/put-trama! "id" (slurp ".../id.wgsl"))`, or
- add a configurable watch directory to fosfora.
