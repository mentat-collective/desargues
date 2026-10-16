# 3D on the stage

The player's 3D frame vocabulary, the ports that use it, and which
alexandria MathBox gaps it covers.

## Strata

| Namespace | Stratum | Role |
|---|---|---|
| `desargues.player.space` | Core + Pure (.cljc) | camera, `:surface3` `:curve3` `:points3` `:axes3`, projection, validation (`problems`), flattening (`->2d`) |
| `desargues.player.orbit` | Pure (.cljc) | turns the camera from pointer drags and zooms it |
| `desargues.player.present` | Protocol (.cljc) | picks the port: `ISpace/-present-space!` for a native-3D host, else `ISurface/-present!` with the frame projected |
| `desargues.player.protocols/ISpace` | Protocol | opt-in capability (ISP); 2D hosts do not change |
| `desargues.player.space-extras` | Core + Pure (.cljc) | `:arrow3` `:plane3` `:contour3` `:label3`, registered on `space`'s multimethods (no edit to `space`) |
| `desargues.player.contour` | Pure (.cljc) | marching squares: level curves of z = f(x, y) as polylines |
| `desargues.player.camera-path` | Pure (.cljc) | camera keyframes interpolated over time (Hermite / Catmull-Rom) |
| `desargues.player.space-demos` | demo source | a saddle with a spiral drawn on it, orbitable, with two sliders |
| `desargues.player.calculus-demos` | demo source | partial derivatives of z = f(x, y), interactive and animated (below) |
| `desargues.player.calculus-symbolic` | Boundary (JVM, Emmy) | f_x and f_y by Emmy, memoized; `surface-catalog` macro carries them into ClojureScript |
| `desargues.studio.spaces` | registry (.cljc) | the 3D sources by key, shared by the JavaFX REPL and the browser page |
| `studio/desargues/player/javafx3d.clj` | Boundary | JavaFX: SubScene + PerspectiveCamera under the 2D host's canvas, composed over `desargues.player.javafx` |

A 3D frame is an ordinary Frame with a `:camera` and 3D shapes. 2D shapes
in it are an overlay. Hosts without `ISpace` (headless PNG, browser
Canvas2D) receive `space/->2d`: depth-sorted closed `:path` faces with flat
shading, `:line` segments, `:dot` points and `:text` axis labels. Golden
images therefore cover 3D (`test/resources/.../space-demo-t4.png`).

The JavaFX field of view is set from `space/project`, so a point appears in
the window where the pure projection places it. Hit-testing and orbit stay
pure: the host stores the flattened frame for `hit/hit-px`, and the player
turns `:camera` in its state (`player/set-camera!` from the REPL).

```clojure
(require '[desargues.player :as player]
         '[desargues.player.javafx3d :as fx3]
         '[desargues.player.space-demos :as sd])
(def p (player/open-source! (fx3/host) sd/source-fn
                            {:title "saddle" :params sd/defaults :controls sd/controls}))
(player/set-camera! p {:yaw 0.4 :pitch 0.9})   ; or drag on empty space
```

## More 3D kinds (`desargues.player.space-extras`)

| Kind | Data | Flattens to |
|---|---|---|
| `:arrow3` | `{:from p :to q :stroke {:color :width} :opts {:head 0.25}}` | shaft `:curve3` + cone `:surface3` (`:draw` grows it from `:from`; zero length draws nothing) |
| `:plane3` | `{:point p :normal n :size s}` or `{:point p :u u :v v :size [a b]}` | a `:surface3` patch point + s u + t v, \|s\| <= a, \|t\| <= b |
| `:contour3` | `{:lines [[p ...] ...]}` | one `:curve3` per polyline |
| `:label3` | `{:at p :text "P" :opts {:font-size :color :anchor}}` | a `:text` where p projects |

The first three are composites: `sx/expand` turns them into base kinds,
and both the flattening (`space/primitives`) and the native JavaFX host
(`node!` :default) draw that expansion, so a new composite kind is one
`expand` method plus its `shape-problems`. `space/three-d-shape?` counts any
kind with a `primitives` method as 3D, which is what lets these live outside
`space`. `sx/level-curves` runs marching squares (`desargues.player.contour`)
over f and lifts each curve to a height: the floor, or the surface itself.

## Camera paths (`desargues.player.camera-path`)

A path is keyframes `{:t seconds :camera {...}}`. `camera-at` interpolates
yaw, pitch, distance and each target coordinate along a cubic Hermite spline
(Catmull-Rom tangents inside, zero at the ends): it passes through every
keyframe, eases in and out, and clamps outside the path. `:projection` steps.
A `:zoom` is interpolated too when any keyframe carries one (1 where one is
missing); when none does, the result carries no `:zoom`.
`fly-around` builds the keyframes of one or more turns around a target. A
source animates its camera by putting `(camera-at path t)` in each frame; an
orbit drag still overrides it (the player's camera wins).

## Partial derivatives (`desargues.player.calculus-demos`)

`?demo=calculus` in the browser, `(open-space :calculus)` in
`studio/desargues/studio/repl.clj` (section 9).

- **Surfaces.** A select picks f among paraboloid, saddle, sin x cos y,
  gaussian bump, monkey saddle, dipole, ripple
  (`desargues.player.calculus-surfaces`). Emmy computes f_x and f_y
  symbolically (`calculus-symbolic/partials`, memoized per surface form). The
  catalog is built by the `surface-catalog` macro, so the derivation runs at
  macroexpansion time on the JVM and the browser bundle carries only the
  infix strings and numeric functions, with no Emmy in the page.
- **The point.** The x0 and y0 sliders move it, and so does a drag:
  P moves in the horizontal plane through it, its shadow Q moves on the floor
  (drag specs with `:plane`).
- **Toggles.** Tangent plane and normal (P + s(1,0,f_x) + t(0,1,f_y)), the
  x-slice y = y0 with its tangent line of slope f_x, the y-slice x = x0 with
  slope f_y, the gradient as an arrow on the floor, level curves (floor and
  surface), wireframe.
- **Readout.** f, df/dx and df/dy as Emmy writes them, with their values and
  |grad f| at the point. It updates on every change (`readout`; the browser
  exposes it as `desargues.browser.main.readout()` for `check.mjs`).
- **Buttons.** "Step gradient ascent" moves (x0, y0) by 0.3 grad f and holds
  the point still. "Reset" restores the point and the morph.
- **Tour (20 s, looping).** The surface draws in, then the level curves.
  The x-slice sweeps across the domain to y = y0 and shows its tangent, then
  the y-slice does the same. The tangent plane and gradient appear. P then
  travels around a circle or up the gradient-ascent path, and the tangent
  plane, slices and gradient follow it. The camera flies around, and with
  "morph over time" f morphs into a second surface and back. The `morph`
  slider holds (1 - m) f + m g at any m. Since differentiation is linear,
  the partials are mixed the same way. Every frame is `(frame-at params t)`.
  With the tour off, t is ignored and the frame is a still.

The golden image `test/resources/desargues/player/golden/calculus-t14.png`
pins one frame (sin x cos y at t = 14). Refresh it after an intended
visual change with `DESARGUES_UPDATE_GOLDEN=1 clojure -M:test -n
desargues.player.calculus-test`.

## Gap-report notebooks (alexandria docs/stage-coverage)

The vocabulary expresses every scene below, but the scenes are not ported
yet: each still needs an alexandria source built on `space/surface`,
`space/curve` and `:points3`.

| Era | Scene | What it needs from the vocabulary |
|---|---|---|
| greek | `:delian.tie/archytas`, `:delian.play/archytas` | cylinder, cone and torus as `:surface3` grids, their meeting curve as `:curve3`; dragging the play point also needs a 3D drag (see below) |
| greek | `:archimedes.tie/sphere` | sphere and cylinder grids |
| greek | `:apollonius/symptom`, `:apollonius/cone`, `:apollonius/playground` | double cone grid, cutting plane as a two-row grid, the section as `:curve3` |
| greek | `:conics/cutting-plane` | the same as Apollonius' cone |
| medieval | `:cubic/cardano-cube` | boxes as `:vertices`/`:faces` meshes (quads) |
| revolution | `:kepler-mysterium/nest-widget` | spheres as grids, Platonic solids as face meshes |
| enlightenment | `:euler/polyhedron` | face meshes plus `:points3` vertices |
| enlightenment | `:brach/design-mathbox`, `:euler/terms-steps` | curves and points in space (`:curve3`, `:points3`, `:axes3`) |
| rigour | `:gauss/egregium-playground` | helicoid to catenoid as a parametric grid over a parameter |
| rigour | `:non-euclidean/pseudosphere` | pseudosphere and sphere grids |
| rigour | `:riemann-hypothesen/geodesics` | sphere, saddle and torus grids with geodesics as `:curve3` |

`:apollonius/notation-steps` is a stepped HTML figure, not a 3D scene, so
this vocabulary does not cover it.

## Not done yet

- A drag that moves a point in 3D. `drag.cljc` works in the 2D scene
  plane. Points in `:points3` keep `:opts :name`, so a press hits them, but
  `->params` receives a screen point. Unprojecting it onto a plane or curve
  would be a pure function in `space`.
- The browser player projects 3D frames to 2D. It does not orbit, because
  its pointer port is not wired.
- The flattened view has no per-pixel depth. Painter's sort at face
  granularity can misorder faces that intersect; the JavaFX host is exact.
