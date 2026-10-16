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
            [desargues.player.drag :as drag]))

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

(defn- present! [{:keys [host state]}]
  (p/-present! host (session/current-frame (:session @state))))

(defn- on-tick [player dt]
  (swap! (:state player) update :session session/tick dt)
  (present! player))

(declare set-param!)

(defn- bind-controls!
  "Add one widget per entry of `controls` to the host, if it has widgets (ISP:
   a host without IControls just plays). Drag specs ({:id :drag {...}}) are
   moved by the pointer, not by a widget, so they are skipped here."
  [player controls]
  (when (satisfies? p/IControls (:host player))
    (doseq [spec (remove drag/drag-spec? controls)]
      (p/-control! (:host player) spec (fn [id v] (set-param! player id v))))))

(declare set-params!)

(defn on-pointer
  "Feed one pointer `event` to `player`'s drag state: a drag on a shape named
   by a drag spec in `controls` re-records the construct with the new params,
   like a slider. Returns the player. Hosts call it through IPointer; tests
   and the REPL call it directly with scene-unit events."
  [player controls event]
  (let [{:keys [drag params session]} @(:state player)
        f   (session/current-frame session)
        out (drag/pointer drag controls f params event)]
    (swap! (:state player) assoc :drag (:drag out))
    (when-let [ps (:params out)]
      (set-params! player ps))
    player))

(defn- bind-pointer!
  "Wire the host's pointer to the drag specs in `controls`, if the host takes
   pointer input (ISP) and any spec is draggable."
  [player controls]
  (when (and (satisfies? p/IPointer (:host player))
             (some drag/drag-spec? controls))
    (p/-on-pointer! (:host player) (fn [ev] (on-pointer player controls ev)))))

(defn open-source!
  "Play `source-fn` (fn [params] ISource) in `host`.
   opts: {:title :w :h :params {} :controls [widget-spec] :autoplay? true}.
   A control spec with :drag ({:id :a :drag {:node \"A\"}}) is moved by
   dragging that shape (desargues.player.drag) instead of by a widget.
   Returns the player, the handle every REPL fn takes."
  [host source-fn {:keys [title w h params controls autoplay?]
                   :or {title "desargues" w 1280 h 720 params {} autoplay? true}}]
  (let [spec   {:source-fn source-fn :params params}
        sess   (cond-> (session/session (source-for spec)) autoplay? session/play)
        player {:host host :state (atom (assoc spec :session sess))}]
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

(defn set-param!
  "Set parameter `k` to `v`, rebuild the source and keep playing from the
   same time. Moves the matching widget too when called from the REPL."
  [player k v]
  (let [st (swap! (:state player) assoc-in [:params k] v)]
    (swap! (:state player) update :session session/reload (source-for st))
    (when (satisfies? p/IControls (:host player))
      (p/-set-control! (:host player) k v))
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

(defn snapshot!
  "Write the current picture to a PNG at `path` (hosts implementing IWindow)."
  [player path]
  (p/-snapshot! (:host player) path))
