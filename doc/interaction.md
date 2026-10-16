# Interacting with the player: zoom, pan, orbit, reset

Every player host feeds raw input through one pure router,
`desargues.player.pointer/step`, which hands gestures to
`desargues.player.navigate`. Hosts only translate their own events
(`desargues.browser.input`, `desargues.player.javafx-input`), so a gesture does
the same thing in every host that has the port for it.

What the viewer looks through is the *look*, `{:camera :view}`:

- a **3D frame** (one with a `:camera` or 3D shapes, see `doc/player-3d.md`)
  is seen through an orbited camera. Zooming multiplies the camera's `:zoom`
  (the target plane's magnification); a perspective camera also walks in
  (`distance / m`), as a real camera would, so near things grow faster than
  far ones. Panning slides the camera's target in its right/up plane.
- a **2D frame** is seen through a viewport, `desargues.player.view`:
  `{:center [x y] :scale k}`, applied to the Frame before painting. Positions
  and geometric extents (circle radii, rectangles, SVG paths) scale; dots,
  text, stroke widths and dashes keep their size on screen.

Zooming keeps the scene point under the pointer where it is (zoom to cursor).
Pans move the picture with the pointer. Magnification is clamped
(`view/scale-limits`, `orbit/zoom-limits`, `orbit/distance-limits`).

`nil` in the look means the source's own camera or framing. The look is kept
apart from the parameters, so moving a slider, typing a formula or dragging a
handle never resets the viewer's zoom. Hit-testing runs on the painted frame,
and a 2D drag takes pointer positions back through the viewport, so dragging
a handle works the same at any zoom.

## Gestures and keys

| Action | Browser canvas | JavaFX window (2D and 3D host) |
|---|---|---|
| Zoom in / out at the pointer | mouse wheel | mouse wheel (ScrollEvent) |
| Zoom (trackpad) | pinch (the browser reports ctrl + wheel) | pinch (ZoomEvent) |
| Zoom in / out about the middle | `+` or `=` / `-` or `_` | same keys, canvas focused |
| Pan | shift + drag, middle or right drag, or a plain drag on empty 2D space | same |
| Pan by key | arrow keys (the view moves that way) | same |
| Orbit (3D) | drag on empty space | same |
| Orbit by key (3D) | `W` `A` `S` `D`, or Alt + arrows | same |
| Reset to the source's own view | double click, or `0` | same |
| Drag a handle (drag specs) | drag the shape (primary button, shift allowed) | same |

Priority, in `pointer/step`: a gesture (wheel, pinch, key, double click) goes
to navigation; a pan in progress keeps the pointer; a primary press on a
draggable shape drags it, with or without shift; a middle or right press, a
shift press, or a press on empty 2D space pans; a press on empty 3D space
orbits.

Keys: in the browser they are read page-wide, except while typing in a
widget, and never with ctrl or meta (those belong to the browser). In JavaFX
they are read while the canvas has focus (a press on it takes the focus).
A key the player uses has its default action stopped (no page scroll, no
focus traversal). The bindings are data: `desargues.player.navigate/bindings`.

Other hosts: the headless PNG host and the native (cljrs) host take no input,
but they paint what the player presents, so the REPL calls below zoom and pan
them too.

## REPL API

The same on the JVM player (`desargues.player`) and the browser player
(`desargues.browser.player`):

```clojure
(player/zoom! pl 2.0)          ; magnify 2x about the middle
(player/zoom! pl 0.5 [1.0 2.0]); zoom out about scene point [1 2]
(player/pan! pl [1.0 0.0])     ; move the picture 1 scene unit right
(player/reset-view! pl)        ; back to the source's own camera / viewport
(player/view pl)               ; => {:three-d? false :camera nil
                               ;     :view {:center [..] :scale 2.0} :zoom 2.0}
(player/set-camera! pl {:yaw 0.3 :pitch 0.5})  ; 3D: merge over the current camera
```

The browser page's probe, `desargues.browser.main.status()`, reports `zoom`
and `moved` (whether the viewer left the source's own view); `browser/check.mjs`
wheels, pinches, pans and resets on a 2D demo and on `?demo=space`.

## Extending

- A new 2D node kind that carries extents gets a `desargues.player.view/shape`
  method (the default maps `:at :from :to :points`).
- A new host implements `IPointer` (with `:wheel`, `:pinch`, `:double` events,
  `:button` and `:mods` on presses) and `IKeyboard` (DOM key names); nothing
  above the ports changes.
