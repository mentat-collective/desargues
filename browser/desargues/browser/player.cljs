(ns desargues.browser.player
  "BOUNDARY / composition stratum: the stage in a browser. The ClojureScript
   twin of desargues.player (which is .clj because it records through the JVM
   scene facade): plays any desargues.player.protocols/ISource in any host
   implementing the host ports, with the same pure session underneath.

   Entry points:
     open-source!  a source-fn (fn [params] ISource): anything at all.
     open-graph!   a recorded scene graph (EDN data, e.g. written by
                   `clojure -M:browser-record` or desargues.player/record), so
                   viewing needs no JVM.
     open!         a parametric construct (fn [params] (fn [stage] ...)) over
                   desargues.browser.stage, re-recorded in the page on every
                   parameter change.
     load-graph!   fetch an EDN scene graph by URL, then open-graph!.

   REPL surface: play!, pause!, seek!, set-param!, params, snapshot!, close!.
   One atom per player is the only state."
  (:require [cljs.reader :as reader]
            [desargues.browser.stage :as stage]
            [desargues.player.graph-source :as graph-source]
            [desargues.player.protocols :as p]
            [desargues.player.session :as session]))

(defn- source-for [{:keys [source-fn params]}] (source-fn params))

(defn- present! [{:keys [host state]}]
  (p/-present! host (session/current-frame (:session @state))))

(defn- on-tick [player dt]
  (swap! (:state player) update :session session/tick dt)
  (present! player))

(declare set-param!)

(defn- bind-controls! [player controls]
  (when (satisfies? p/IControls (:host player))
    (doseq [spec controls]
      (p/-control! (:host player) spec (fn [id v] (set-param! player id v))))))

(defn open-source!
  "Play `source-fn` (fn [params] ISource) in `host`.
   opts: {:title :w :h :params {} :controls [widget-spec] :autoplay? true}."
  [host source-fn {:keys [title w h params controls autoplay?]
                   :or {title "desargues" w 1280 h 720 params {} autoplay? true}}]
  (let [spec   {:source-fn source-fn :params params}
        sess   (cond-> (session/session (source-for spec)) autoplay? session/play)
        player {:host host :state (atom (assoc spec :session sess))}]
    (p/-show! host {:title title :w w :h h})
    (bind-controls! player controls)
    (p/-on-tick! host (fn [dt] (on-tick player dt)))
    (present! player)
    player))

(defn open-graph!
  "Play recorded scene graph `graph` (EDN data) in `host`. Same opts as
   open-source!; a recorded graph has no parameters, so no controls."
  [host graph opts]
  (open-source! host (constantly (graph-source/graph-source graph)) (dissoc opts :controls)))

(defn construct-source
  "A source-fn re-recording parametric construct `construct-fn` in the page."
  [construct-fn]
  (fn [params] (graph-source/graph-source (stage/record (construct-fn params)))))

(defn open!
  "Play parametric construct `construct-fn` (fn [params] (fn [stage] ...))
   over desargues.browser.stage in `host`. Same opts as open-source!."
  [host construct-fn opts]
  (open-source! host (construct-source construct-fn) opts))

(defn read-graph
  "A scene graph from its EDN text."
  [text]
  (reader/read-string text))

(defn load-graph!
  "Fetch the EDN scene graph at `url` and play it in `host`. Returns a
   promise of the player."
  [host url opts]
  (-> (js/fetch url)
      (.then (fn [r] (if (.-ok r) (.text r) (throw (js/Error. (str url ": " (.-status r)))))))
      (.then (fn [text] (open-graph! host (read-graph text) opts)))))

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
  "Set parameter `k` to `v`, rebuild the source and keep playing from the
   same time. Moves the matching widget too when called from the REPL."
  [player k v]
  (let [st (swap! (:state player) assoc-in [:params k] v)]
    (swap! (:state player) update :session session/reload (source-for st))
    (when (satisfies? p/IControls (:host player))
      (p/-set-control! (:host player) k v))
    (present! player)
    player))

(defn snapshot!
  "Offer the current picture as a PNG download named `path`."
  [player path]
  (p/-snapshot! (:host player) path))
