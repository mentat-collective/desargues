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
            [desargues.scene.record :as recorder]
            [desargues.player.graph-source :as graph-source]
            [desargues.player.protocols :as p]
            [desargues.player.session :as session]
            [desargues.player.controls :as controls]))

(defn- source-for [{:keys [source-fn params]}] (source-fn params))

(defn- present! [{:keys [host state]}]
  (p/-present! host (session/current-frame (:session @state))))

(defn- on-tick [player dt]
  (swap! (:state player) update :session session/tick dt)
  (present! player))

(declare set-param!)

(declare control-input!)

(defn- bind-controls!
  "One widget per control spec (desargues.player.controls), drawn at the
   value the params hold, if the host has widgets (ISP)."
  [player controls]
  (when (satisfies? p/IControls (:host player))
    (doseq [spec (filter controls/widget? controls)]
      (p/-control! (:host player)
                   (controls/widget-spec spec (:params @(:state player)))
                   (fn [id raw] (control-input! player id raw))))))

(defn open-source!
  "Play `source-fn` (fn [params] ISource) in `host`.
   opts: {:title :w :h :params {} :controls [widget-spec] :autoplay? true}.
   Widget kinds: desargues.player.controls (:slider :toggle :select :text
   :button); params not given start at each control's default."
  [host source-fn {:keys [title w h params controls autoplay?]
                   :or {title "desargues" w 1280 h 720 params {} autoplay? true}}]
  (let [controls (controls/validate (vec controls))
        params   (merge (controls/defaults controls) params)
        spec     {:source-fn source-fn :params params :controls controls}
        sess     (cond-> (session/session (source-for spec)) autoplay? session/play)
        player   {:host host :state (atom (assoc spec :session sess))}]
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
  (fn [params] (graph-source/graph-source (recorder/record (construct-fn params)))))

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
      (let [spec (controls/spec-by-id (:controls st) k)]
        (p/-set-control! (:host player) k (if spec (controls/display spec v) v))))
    (present! player)
    player))

(defn- set-params! [player ps]
  (let [st (swap! (:state player) assoc :params ps)]
    (swap! (:state player) update :session session/reload (source-for st))
    (present! player)
    player))

(defn control-input!
  "Apply raw input `raw` from widget `id` through the pure
   desargues.player.controls/step: rebuild once, move the other widgets a
   preset changed, report a refusal through IControlFeedback. Returns the
   step outcome {:params :error :move}."
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

(defn press!
  "Press button `id` (the REPL twin of clicking it)."
  [player id]
  (control-input! player id nil))

(defn snapshot!
  "Offer the current picture as a PNG download named `path`."
  [player path]
  (p/-snapshot! (:host player) path))
