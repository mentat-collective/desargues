(ns desargues.player
  "BOUNDARY / composition stratum of the player: plays a desargues construct
   live in any host that implements desargues.player.protocols.

   The construct is the SAME (fn [stage] ...) that desargues.scene/render!
   turns into an mp4. Here it is recorded once through the RecordingBackend
   into the EDN scene graph, and the graph is played by the pure session and
   timeline strata. The host (JavaFX today; GTK or wgpu via cljrs, a browser
   via cljw or cljs tomorrow) only paints Frames and calls back on each tick.

   A parametric construct is (fn [params] (fn [stage] ...)): changing a
   parameter (from a widget or from the REPL) re-records it and the session
   keeps its time, so the picture updates in place.

   REPL surface: open!, close!, play!, pause!, seek!, set-param!, params,
   snapshot!. This ns holds the only mutable state (one atom per player)."
  (:require [desargues.player.protocols :as p]
            [desargues.player.session :as session]
            [desargues.scene :as scene]
            [desargues.scene.data :as data]))

(defn record
  "The EDN scene graph of construct `construct` (fn [stage]), named `scene-name`."
  [scene-name construct]
  (scene/with-backend (data/recording-backend)
    (scene/render! scene-name construct)))

(defn- graph-for [{:keys [scene-name construct-fn params]}]
  (record scene-name (construct-fn params)))

(defn- present! [{:keys [host state]}]
  (p/-present! host (session/current-frame (:session @state))))

(defn- on-tick [player dt]
  (swap! (:state player) update :session session/tick dt)
  (present! player))

(declare set-param!)

(defn- bind-controls!
  "Add one widget per entry of `controls` to the host, if it has widgets (ISP:
   a host without IControls just plays)."
  [player controls]
  (when (satisfies? p/IControls (:host player))
    (doseq [spec controls]
      (p/-control! (:host player) spec (fn [id v] (set-param! player id v))))))

(defn open!
  "Play `construct-fn` (fn [params] (fn [stage] ...)) in `host`.
   opts: {:title :w :h :params {} :controls [widget-spec] :autoplay? true
          :scene-name}. Returns the player, the handle every REPL fn takes."
  [host construct-fn {:keys [title w h params controls autoplay? scene-name]
                      :or {title "desargues" w 1280 h 720 params {}
                           autoplay? true scene-name "Live"}}]
  (let [spec   {:scene-name scene-name :construct-fn construct-fn :params params}
        sess   (cond-> (session/session (graph-for spec)) autoplay? session/play)
        player {:host host :state (atom (assoc spec :session sess))}]
    (p/-show! host {:title title :w w :h h})
    (bind-controls! player controls)
    (p/-on-tick! host (fn [dt] (on-tick player dt)))
    (present! player)
    player))

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

(defn set-param!
  "Set parameter `k` to `v`, re-record the construct and keep playing from
   the same time. Moves the matching widget too when called from the REPL."
  [player k v]
  (let [st (swap! (:state player) assoc-in [:params k] v)
        g  (graph-for st)]
    (swap! (:state player) update :session session/reload g)
    (when (satisfies? p/IControls (:host player))
      (p/-set-control! (:host player) k v))
    (present! player)
    player))

(defn snapshot!
  "Write the current picture to a PNG at `path` (hosts implementing IWindow)."
  [player path]
  (p/-snapshot! (:host player) path))
