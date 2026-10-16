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

(defprotocol ISpace
  "A surface that renders 3D frames natively (desargues.player.space: a
   :camera plus :surface3 / :curve3 / :points3 / :axes3 shapes). Opt-in
   capability (ISP): a host without it is handed the frame projected to 2D
   (desargues.player.space/->2d) through ISurface, so every 2D host shows 3D
   content without knowing it exists."
  (-present-space! [surface frame]
    "Paint 3D `frame` (its 3D shapes in space, its 2D shapes as an overlay),
     replacing what was shown. Same threading contract as -present!."))

(defprotocol IWindow
  "A top-level window holding a surface."
  (-show! [window opts] "Open or raise the window. opts: {:title :w :h}.")
  (-close! [window] "Close the window and release host resources.")
  (-snapshot! [window path] "Write what the window shows to a PNG at `path`."))

(defprotocol IControls
  "Interactive widgets bound to player parameters. The control model (what a
   spec means, what an input does to the params) is pure and shared by every
   host: desargues.player.controls. A host only draws one widget per kind
   and reports raw input."
  (-control! [window spec on-change]
    "Add a widget from `spec` {:id :kind :label ...}, kind one of :slider
     {:min :max :value}, :toggle {:value bool}, :select {:options
     [{:value :label}] :value}, :text {:value string} or :button (see
     desargues.player.controls; the player hands specs already normalised by
     controls/widget-spec). `on-change` is (fn [id raw]), called on user
     input with the widget's raw value: the number, the boolean, the chosen
     option's :value, the typed string, or nil for a button press.")
  (-set-control! [window id value]
    "Move widget `id` to `value` (controls/display of the param) without
     firing on-change (REPL-driven). A button ignores it."))

(defprotocol IControlFeedback
  "Telling the user an input was refused (a :text that does not parse).
   Segregated from IControls (ISP): a host may draw widgets and stay silent."
  (-control-error! [window id msg]
    "Show `msg` beside widget `id`, or clear it when `msg` is nil."))

(defprotocol IPointer
  "Pointer input on the surface (mouse, pen, touch, trackpad). Segregated
   from IControls (ISP): a host can paint and take clicks without having
   widgets, or the reverse."
  (-on-pointer! [host f]
    "Call (f event) on every pointer event over the surface, where event is
     {:type (:down | :move | :up) :at [x y] :hit id-or-nil} plus, when the
     host knows them, :button (:primary | :middle | :secondary) and :mods (a
     set of :shift :alt :ctrl :meta). :at is in scene units of the frame last
     presented (y up, desargues.player.frame/px->scene); :hit is the id of
     the shape under the pointer (desargues.player.hit/hit), or nil.
     Navigation gestures go to the same f (desargues.player.navigate):
     {:type :wheel :at :dy} (dy in pixels, > 0 scrolling down, a line or page
     scaled to pixels), {:type :pinch :at :factor} (> 1 spreading the
     fingers; a browser reports it as a wheel with ctrlKey) and
     {:type :double :at} (a double click)."))

(defprotocol IKeyboard
  "Keys pressed while the surface has focus. Segregated (ISP): a host may
   take pointer input and no keys."
  (-on-key! [host f]
    "Call (f event) on every key press, event {:type :key :key k :mods
     #{...}}, k a DOM KeyboardEvent.key name (\"ArrowLeft\", \"+\", \"0\",
     \"a\"); a host translates its own key codes. f returns true when it used
     the key, so the host may stop the key's default action (a page scroll).
     Keys typed into a widget are the widget's, not f's."))

(defprotocol IClock
  "The host's frame clock (an AnimationTimer, requestAnimationFrame, a GTK
   tick callback). The player owns no thread; the host calls it back."
  (-on-tick! [host f]
    "Call (f dt-seconds) once per displayed frame until -stop-ticks!.")
  (-stop-ticks! [host]
    "Stop calling the tick fn."))
