(ns desargues.player.protocols
  "PROTOCOL stratum of the player: the ports a renderer host implements.

   The player core (frame, timeline) never names a toolkit. A host adapter
   implements these ports over whatever it reaches: JavaFX on the JVM, GTK or
   wgpu through cljrs, a browser canvas through cljw or cljs. Any adapter is
   substitutable for any other (LSP): the player only ever calls these methods.

   Segregated on purpose (ISP): a headless adapter that only writes PNGs
   implements ISurface and nothing else; a window without widgets skips
   IControls. Host-free .cljc.")

(defprotocol ISource
  "What the stage plays: anything with a duration and a Frame at every time.
   Pure on both methods. A recorded desargues.scene graph is one source
   (desargues.player.graph-source); an alexandria.medium scene, a function
   plot or a live simulation are others. The player cannot tell them apart
   (LSP), so a new kind of content is a new source, never a player edit."
  (-duration [src]
    "Seconds the source lasts (0 for a still).")
  (-frame-at [src t]
    "The desargues.player.frame Frame at time `t`, 0 <= t <= duration."))

(defprotocol ISurface
  "Something a Frame can be painted on."
  (-present! [surface frame]
    "Paint `frame` (desargues.player.frame), replacing what was shown. Called
     from any thread; the adapter marshals onto its UI thread if it needs to.")
  (-size [surface]
    "Current [w h] in pixels."))

(defprotocol IWindow
  "A top-level window holding a surface."
  (-show! [window opts] "Open or raise the window. opts: {:title :w :h}.")
  (-close! [window] "Close the window and release host resources.")
  (-snapshot! [window path] "Write what the window shows to a PNG at `path`."))

(defprotocol IControls
  "Interactive widgets bound to player parameters."
  (-control! [window spec on-change]
    "Add a widget from `spec` {:id :kind (:slider | :toggle | :button) :label
     :min :max :value}. `on-change` is (fn [id value]), called on user input.")
  (-set-control! [window id value]
    "Move widget `id` to `value` without firing on-change (REPL-driven)."))

(defprotocol IClock
  "The host's frame clock (an AnimationTimer, requestAnimationFrame, a GTK
   tick callback). The player owns no thread; the host calls it back."
  (-on-tick! [host f]
    "Call (f dt-seconds) once per displayed frame until -stop-ticks!.")
  (-stop-ticks! [host]
    "Stop calling the tick fn."))
