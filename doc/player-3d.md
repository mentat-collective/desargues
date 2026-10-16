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
| `desargues.player.space-demos` | demo source | a saddle with a spiral drawn on it, orbitable, with two sliders |
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
