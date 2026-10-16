(ns desargues.player
  "BOUNDARY / composition stratum of the player (the stage): plays any
   desargues.player.protocols/ISource live in any host that implements the
   host ports (ISurface, IWindow, IControls, IClock).

   Two entry points:
     open-source!  a source-fn (fn [params] source): an alexandria scene, a
                   plot, anything implementing ISource.
     open!         a desargues construct (fn [params] (fn [stage] ...)), the
                   same value desargues.scene/render! turns into an mp4. It is
                   recorded through the RecordingBackend and played as a
                   desargues.player.graph-source.

   Changing a parameter (a widget or the REPL) rebuilds the source and the
   session keeps its time, so the picture updates in place.

   REPL surface: open!, open-source!, close!, play!, pause!, seek!,
   set-param!, params, snapshot!. One atom per player is the only state."
  (:require [desargues.player.graph-source :as graph-source]
            [desargues.player.protocols :as p]
            [desargues.player.session :as session]
            [desargues.scene.record :as recorder]
            [desargues.player.pointer :as pointer]
            [desargues.player.controls :as controls]
            [desargues.player.present :as present]
            [desargues.player.space :as space]))

(defn record
  "The EDN scene graph of construct `construct` (fn [stage]), named `scene-name`
   (desargues.scene.record, the recorder every host shares)."
  [scene-name construct]
  (recorder/record scene-name construct))

(defn construct-source
  "A source-fn playing parametric construct `construct-fn`
   (fn [params] (fn [stage] ...)), recorded as `scene-name`."
  [scene-name construct-fn]
  (fn [params]
    (graph-source/graph-source (record scene-name (construct-fn params)))))

(defn- source-for [{:keys [source-fn params]}] (source-fn params))

(defn- present!
  "Show the session's current frame on the host, seen through the orbited
   camera when there is one (desargues.player.present picks the port)."
  [{:keys [host state]}]
  (let [{:keys [session camera]} @state]
    (present/present! host (session/current-frame session) camera)))

(defn- on-tick [player dt]
  (swap! (:state player) update :session session/tick dt)
  (present! player))

(declare set-param!)

(declare control-input!)

(defn- bind-controls!
  "Add one widget per entry of `controls` to the host, if it has widgets (ISP:
   a host without IControls just plays). Each spec is drawn at the value the
   params hold (desargues.player.controls/widget-spec). Drag specs
   ({:id :drag {...}}) are moved by the pointer, not by a widget, so they are
   skipped here."
  [player controls]
  (when (satisfies? p/IControls (:host player))
    (doseq [spec (filter controls/widget? controls)]
      (p/-control! (:host player)
                   (controls/widget-spec spec (:params @(:state player)))
                   (fn [id raw] (control-input! player id raw))))))

(declare set-params!)

(defn on-pointer
  "Feed one pointer `event` to `player` through the pure
   desargues.player.pointer/step: a drag on a shape named by a drag spec in
   `controls` re-records the construct with the new params, like a slider
   (a :points3 point moves in world space, seen through the orbited
   camera); on a 3D frame, a drag on empty space orbits the camera
   (desargues.player.orbit). Returns the player. Hosts call it through
   IPointer; tests and the REPL call it directly with scene-unit events."
  [player controls event]
  (let [st  @(:state player)
        out (pointer/step st controls (session/current-frame (:session st)) event)]
    (swap! (:state player) assoc :drag (:drag out) :orbit (:orbit out))
    (when-let [ps (:params out)]
      (set-params! player ps))
    (when-let [cam (:camera out)]
      (swap! (:state player) assoc :camera cam)
      (present! player))
    player))

(defn- bind-pointer!
  "Wire the host's pointer to the drag specs in `controls` and, for a 3D
   source, to the orbit, if the host takes pointer input (ISP)."
  [player controls]
  (when (and (satisfies? p/IPointer (:host player))
             (pointer/wants-pointer? controls (session/current-frame (:session @(:state player)))))
    (p/-on-pointer! (:host player) (fn [ev] (on-pointer player controls ev)))))

(defn open-source!
  "Play `source-fn` (fn [params] ISource) in `host`.
   opts: {:title :w :h :params {} :controls [widget-spec] :autoplay? true}.
   Widget kinds (:slider :toggle :select :text :button) are described in
   desargues.player.controls; params not given start at each control's
   default. A control spec with :drag ({:id :a :drag {:node \"A\"}}) is
   moved by dragging that shape (desargues.player.drag) instead of by a
   widget. Returns the player, the handle every REPL fn takes."
  [host source-fn {:keys [title w h params controls autoplay?]
                   :or {title "desargues" w 1280 h 720 params {} autoplay? true}}]
  (let [controls (controls/validate (vec controls))
        params   (merge (controls/defaults controls) params)
        spec     {:source-fn source-fn :params params :controls controls}
        sess     (cond-> (session/session (source-for spec)) autoplay? session/play)
        player   {:host host :state (atom (assoc spec :session sess))}]
    (p/-show! host {:title title :w w :h h})
    (bind-controls! player controls)
    (bind-pointer! player controls)
    (p/-on-tick! host (fn [dt] (on-tick player dt)))
    (present! player)
    player))

(defn open!
  "Play parametric construct `construct-fn` (fn [params] (fn [stage] ...)) in
   `host`. Same opts as open-source!, plus :scene-name."
  [host construct-fn {:keys [scene-name] :or {scene-name "Live"} :as opts}]
  (open-source! host (construct-source scene-name construct-fn) opts))

(defn close! [player]
  (p/-stop-ticks! (:host player))
  (p/-close! (:host player)))

(defn- update-session! [player f & args]
  (apply swap! (:state player) update :session f args)
  (present! player)
  player)

(defn play!  [player] (update-session! player session/play))
(defn pause! [player] (update-session! player session/pause))
(defn seek!  [player t] (update-session! player session/seek t))

(defn params [player] (:params @(:state player)))

(defn current-frame
  "The Frame `player` shows now (plain data: hit-test it, inspect it)."
  [player]
  (session/current-frame (:session @(:state player))))

(defn- move-widget!
  "Move widget `id` to show parameter value `v`, if the host has widgets."
  [player id v]
  (when (satisfies? p/IControls (:host player))
    (let [spec (controls/spec-by-id (:controls @(:state player)) id)]
      (p/-set-control! (:host player) id (if spec (controls/display spec v) v)))))

(defn set-camera!
  "Look at a 3D source through camera `cam` (merged over the current one:
   {:yaw :pitch :distance :target :projection}), as an orbit drag does.
   nil returns to the source's own camera."
  [player cam]
  (swap! (:state player) update :camera #(when cam (merge (or %1 (space/camera (current-frame player))) cam)))
  (present! player)
  player)

(defn set-param!
  "Set parameter `k` to `v`, rebuild the source and keep playing from the
   same time. Moves the matching widget too when called from the REPL."
  [player k v]
  (let [st (swap! (:state player) assoc-in [:params k] v)]
    (swap! (:state player) update :session session/reload (source-for st))
    (move-widget! player k v)
    (present! player)
    player))

(defn set-params!
  "Replace the parameters with `ps` and rebuild the source once, keeping time
   (a drag moves several at once, or a non-numeric one like a point [x y])."
  [player ps]
  (let [st (swap! (:state player) assoc :params ps)]
    (swap! (:state player) update :session session/reload (source-for st))
    (present! player)
    player))

(defn control-input!
  "Apply raw input `raw` from widget `id` (what a host's on-change reports):
   the pure desargues.player.controls/step decides the new params; the
   source is rebuilt once, keeping time; other widgets whose param changed
   (a preset button) are moved; a refused input (a :text that does not
   parse) leaves everything alone and is reported through IControlFeedback
   when the host has it. Returns the step outcome {:params :error :move}.
   Tests and the REPL press buttons through it: (control-input! pl :step nil)."
  [player id raw]
  (let [{:keys [controls params]} @(:state player)
        out (controls/step controls params id raw)]
    (when (satisfies? p/IControlFeedback (:host player))
      (p/-control-error! (:host player) id (:error out)))
    (when (and (nil? (:error out)) (not= params (:params out)))
      (set-params! player (:params out))
      (when (satisfies? p/IControls (:host player))
        (doseq [[k v] (:move out)]
          (p/-set-control! (:host player) k v))))
    out))

(defn snapshot!
  "Write the current picture to a PNG at `path` (hosts implementing IWindow)."
  [player path]
  (p/-snapshot! (:host player) path))

(defn press!
  "Press button `id` of `player` (the REPL twin of clicking it)."
  [player id]
  (control-input! player id nil))
