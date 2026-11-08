# GLSL layers

A field form (`desargues.wallpaper.field`) is a per-pixel shader written as
data. The same form already runs on the `:jvm`, `:raster` and `:gpu` (OpenCL)
backends. It also compiles to GLSL, and `desargues.wallpaper.emit` packages it
as a self-contained **layer**. VJ hosts and compositors load the layer as one
of these:

| file             | what                                                     |
|------------------|----------------------------------------------------------|
| `<id>.fs`        | ISF v2 (Interactive Shader Format: VDMX, Resolume, Synesthesia, ISF editors) |
| `<id>.frag`      | plain GLSL ES 3.00 fragment shader (WebGL2, GLES)        |
| `<id>.330.frag`  | plain desktop GLSL 330 core                              |
| `<id>.json`      | manifest of the plain shaders: uniforms, roles, ranges, blend hint |

## Emit a layer

```clojure
(require '[desargues.wallpaper.emit :as emit]
         '[desargues.wallpaper.field :as fld])

(emit/emit! {:id :quasicrystal
             :title "7-fold quasicrystal"
             :form fld/quasicrystal          ; {:params [u scale] :body ..}
             :period 12                      ; seconds per loop on the host clock
             :blend :screen                  ; hint for the host's layer stack
             :view [14.22 8.0]               ; scene window, as in a loop spec
             :params [['scale {:default 3.0 :min 1.0 :max 8.0 :label "Wave number"}]]}
            "out/layers")
;; => {:ok ["out/layers/quasicrystal.fs" ...]}
;;    or {:error :layer/invalid ...} / {:error :field/unsupported ...}; nothing is written
```

`emit/layer-files` returns the same files as a map of name to text and touches
no disk. `emit/emit-all!` emits a list of layers. The compiler on its own is
`(fld/source :glsl form {})`, which returns `{:ok {:function :helpers :params :ir}}`.

`:params` is an ordered vector of `[symbol spec]` pairs, and its order is the
input order. Add new params at the end only (append-only), because host
bindings and presets refer to input positions. The older map shape
`{'scale spec}` is accepted only together with `:order ['scale ..]` naming every
key once; without it the emitter returns `{:error :layer/unordered-params}`,
because map iteration order is not stable.

Every non-phase param of the form needs a `:default`. `:min`, `:max` and
`:label` are optional. Param names must be GLSL identifiers (`-` becomes `_`)
and must not collide with ISF built-ins, GLSL keywords, or the GLSL functions
the field calls.

## The ISF mapping

- **Header.** The header carries `ISFVSN "2"`, a `DESCRIPTION` (title plus
  `[blend: screen; loops every 12 s at rate 1]`), `CATEGORIES` (default
  `["Generator"]`) and `INPUTS`. It also has a `DESARGUES` block: id, blend,
  phase name, period, view and generator. Hosts ignore unknown keys.
- **Inputs, in order:**
  - the phase `u`: float 0..1, default 0;
  - `rate`: float 0..4, default 1;
  - every other param: float with its default and range.
- **Phase.** The shader computes `phase = fract(u + rate * TIME / period)`.
  - With the defaults, the layer loops seamlessly on the host clock: at
    `TIME = period` the picture is the one at `TIME = 0`.
  - With `rate = 0`, the host drives the phase through `u` (a beat, an LFO).
  - The form's own phase is periodic in u, as every loop in
    `desargues.wallpaper.loop` is, so any binding of u keeps the loop closed.
- **Pixel to scene.** The pixel is `isf_FragNormCoord * RENDERSIZE`, or
  `gl_FragCoord.xy` in the plain shaders. It maps to scene coordinates the way
  `desargues.wallpaper.render` maps it: the whole `:view` stays visible, the
  scene is centred with y up, and the longer axis extends. A 2560x1080 output
  therefore shows more of the plane and never stretches it.
- **Output.** The output is `vec4(clamp(field, 0, 1), 1)`. Blending is the
  host's job, which is why the blend mode is only a hint.
- **Plain shaders.** The `.frag` files declare `uniform vec2 RENDERSIZE`,
  `uniform float TIME` and one `uniform float` per input, and write
  `out vec4 fragColor`. The `.json` manifest lists the uniforms with
  `role` (`phase`, `rate` or `param`), `default`, `min`, `max` and `label`.

## What compiles

The whole field vocabulary compiles:

- arithmetic: `+ - * /`
- functions: `sin cos tan exp log sqrt pow atan2 abs floor min max`
- derived ops: `fract mix clamp let`
- loop helpers: `frac wave osc`
- `smoothstep`, and `cospal` (one cosine-palette channel, a + b cos 2π(ct + d))
- the constants `pi` and `tau`

The derived ops and helpers lower to the primitive ones before any backend
sees them, so `:jvm`, `:raster`, `:gpu` and `:glsl` agree by construction.
Palettes are inlined as cosine palettes (`cospal`, one per channel). A palette
is a few numbers, so it needs no uniform array.

## Limits

- **Refusals.** The compiler refuses what GLSL cannot express and never drops
  it. A non-finite literal such as `(/ 1.0 0.0)` folded into the code returns
  `{:error :field/unsupported :construct ##Inf :message "GLSL cannot express ..."}`,
  and so does any lowered op without a GLSL mapping. A form that does not lower
  at all (unknown op, bad let) returns `{:error :field/bad :form <the offending form>}`.
- **`pow`.** GLSL's `pow(x, y)` is undefined for x < 0. Every `pow` compiles to
  a `d_pow` helper that follows `Math/pow`: an integer exponent of a negative
  base keeps its sign. Where `Math/pow` gives NaN, `d_pow` gives 0, and where
  it gives infinity, `d_pow` gives 1e30.
- **Precision.** GPUs compute in float32. The tests read the emitted
  expression tree on the CPU, once in double and once rounding every step to
  float32, and hold both to the JVM shader within one level of 255 over a
  64x36 grid. Very large phase arguments (e.g. `k x` with huge `k`) lose
  precision on a GPU before they do on the JVM.
- **Scope.** Only field forms become layers. `:frame` shapes, `:splat`,
  `:light`, `:post` effects and `:scene` (LuxCore) are not field forms and are
  not emitted. Supersampling (`:ss`) is the host's job (MSAA or render scale).
- **Inputs.** Inputs are floats only. Colour and point inputs would need new
  field types.

## Validation

`test/desargues/wallpaper/emit_test.clj` compiles every variant of four layers
(quasicrystal, Lieb contours, graphene bands, and `rings`, which uses every
op) with `glslangValidator`. The ISF file is checked behind the preamble an
ISF host declares. The test looks for the validator in `$DESARGUES_GLSLANG`,
then the system property `desargues.glslang`, then `PATH`. Without one, the
test prints that it is absent and runs only the structural checks: balanced
code, no JVM leftovers, inputs, and the golden ISF header in
`test/desargues/wallpaper/golden/`.
