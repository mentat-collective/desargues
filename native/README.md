# desargues-native

The player's native host. A Rust binary that embeds a cljrs (clojurust) runtime.
It loads the player's pure `.cljc` strata unchanged from `../src`: frame,
timeline, session, graph-source and svg-path. Frames are painted with tiny-skia
(text through ab_glyph and DejaVu Sans) into a winit + softbuffer X11 window.

| Port        | Native side (`cljrs/desargues/native/host.cljrs`)        |
|-------------|----------------------------------------------------------|
| `ISurface`  | a tiny-skia canvas painted by `desargues.native.paint`   |
| `IWindow`   | winit window via softbuffer; `-snapshot!` writes the PNG  |
| `IControls` | sliders painted in a 260 px side panel, mouse-dragged    |
| `IClock`    | `host/run!`: pump events, tick with elapsed s, present   |

The Rust functions are `#[cljrs_interop::export]`s in the namespaces
`desargues.native.gfx`, `desargues.native.window`, `desargues.native.stdin` and
`desargues.native.sys`.

## Build and run

```sh
cargo build --release          # needs ~/PP/clojurust beside mentat-collective
B=target/release/desargues-native
$B desargues.native.main play ../browser/public/scenes/pendulum.edn       # window
$B desargues.native.main snapshot ../browser/public/scenes/pendulum.edn 3 640 360 /tmp/t3.png
$B desargues.native.selfcheck  # list the native exports
```

## From the JVM player

`desargues.player.native/host` implements the same four ports by driving
`desargues-native desargues.native.main serve`. Frames go out as EDN, one map
per line. Ticks, slider changes and snapshots come back the same way. Nothing
above the ports changes:

```clojure
(player/open! (native/host) demos/pendulum {:controls demos/pendulum-controls})
```

`test/desargues/player/native_test.clj` checks the following:

- cljrs replays the recorded pendulum graph with the same step count.
- The native frame at t = 3 matches the headless golden PNG.
- The bridged host passes through `player/open!`.

The test is skipped when the binary is not built.

## Why not GTK4 or wgpu

This machine has the GTK4 runtime but not its development files (`pkg-config
gtk4` fails), so gtk-rs cannot link. A wgpu window would work, but it adds a
GPU dependency and changes nothing at the ports. softbuffer gives a real X11
window with no GPU. tiny-skia does the same CPU anti-aliased raster that the
golden comparison needs.
