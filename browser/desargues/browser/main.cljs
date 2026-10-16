(ns desargues.browser.main
  "BOUNDARY: the demo page's entry. Reads ?demo=<key> (one of the eight
   studio object demos, desargues.studio.objects-demos, the same registry the
   JavaFX stage opens, played live with sliders, one of the widget demos,
   desargues.studio.widget-demos, driven by buttons, a select, a text field
   and a toggle, or a 3D source from `spaces`, orbited by dragging on empty
   space) or ?scene=<url> (a recorded EDN scene graph, e.g.
   scenes/pendulum.edn), builds a menu and mounts the canvas host into #stage.

   window.desargues exposes the player and a paint probe for the headless
   check (browser/check.mjs): desargues.status() -> {demo, t, painted, yaw}.
   Every widget carries data-control=<id>, which the check drives."
  (:require [clojure.string :as str]
            [desargues.browser.canvas :as canvas]
            [desargues.browser.player :as player]
            [desargues.studio.objects-demos :as demos]
            [desargues.studio.widget-demos :as widget-demos]
            [desargues.studio.spaces :as studio-spaces]
            [desargues.studio.blackboard :as blackboard]))

(defonce ^:private current (atom nil))

(def scenes
  "Recorded scene graphs shipped beside the page (EDN, no JVM at view time)."
  {"pendulum" {:title "damped pendulum (recorded)" :url "scenes/pendulum.edn"}})

(def spaces
  "3D sources the page plays as they are (ISources, not constructs): the
   shared registry desargues.studio.spaces (:space, :calculus)."
  studio-spaces/demos)

(defn- query []
  (let [ps (js/URLSearchParams. (.. js/window -location -search))]
    {:demo (.get ps "demo") :scene (.get ps "scene") :t (.get ps "t")}))

(def ^:private registry
  "Every demo the page opens by ?demo=<key>: the slider object demos, the
   widget demos (buttons, select, text field, toggle) and the blackboard
   (?demo=blackboard, desargues.studio.blackboard)."
  (merge demos/demos widget-demos/demos blackboard/demos))

(defn- menu! []
  (when-let [nav (.getElementById js/document "menu")]
    (set! (.-innerHTML nav)
          (str/join " | "
                    (concat
                     (for [[k {:keys [title]}] (concat (sort-by key (merge demos/demos spaces))
                                                       (sort-by key widget-demos/demos)
                                                       blackboard/demos)]
                       (str "<a href=\"?demo=" (name k) "\">" title "</a>"))
                     (for [[k {:keys [title]}] scenes]
                       (str "<a href=\"?scene=" k "\">" title "</a>")))))))

(defn- opts [title]
  {:title (str "desargues: " title) :w 960 :h 540})

(defn- after-open! [p t]
  (reset! current p)
  (when t
    (player/pause! p)
    (player/seek! p (js/parseFloat t))))

(defn ^:export start []
  (menu!)
  (let [mount (.getElementById js/document "stage")
        {:keys [demo scene t]} (query)
        host  (canvas/host mount)
        k     (keyword (or demo "tangent"))]
    (cond
      scene
      (let [{:keys [title url]} (get scenes scene {:title scene :url scene})]
        (-> (player/load-graph! host url (opts title))
            (.then #(after-open! % t))))

      (contains? spaces k)
      (let [{:keys [title source-fn params controls]} (spaces k)]
        (after-open! (player/open-source! host source-fn
                                          (assoc (opts title) :params params :controls controls))
                     t))

      ;; params start at each control's default (desargues.player.controls)
      :else
      (let [{:keys [title construct controls]} (get registry k (:tangent registry))]
        (after-open! (player/open! host construct
                                   (assoc (opts title) :controls controls))
                     t)))))

(defn ^:export status
  "What the headless check reads: the current time, how many pixels of the
   stage canvas differ from the background, how many KaTeX labels the
   overlay shows (TeX typesets to DOM, not to the canvas), the orbited
   camera's yaw (null until an orbit), and the view's magnification and
   whether the viewer has moved it from the source's own (zoom, moved)."
  []
  (if-let [p @current]
    (let [{:keys [canvas tex-cache tick-error]} @(:state (:host p))
          v (player/view p)]
      #js {"t" (:t (:session @(:state p)))
           "painted" (if canvas (canvas/painted-pixels canvas) 0)
           "tex" (count (filter #(not= "none" (.. % -style -display))
                                (vals (some-> tex-cache deref))))
           "yaw" (some-> (:camera @(:state p)) :yaw)
           "zoom" (:zoom v)
           "moved" (boolean (or (:camera v) (:view v)))
           "tickError" (some-> tick-error str)})
    #js {"t" nil "painted" 0 "tex" 0}))

(defn ^:export seek [t]
  (when-let [p @current] (player/pause! p) (player/seek! p t) (status)))

(defn ^:export set-param [k v]
  (when-let [p @current] (player/set-param! p (keyword k) v) (status)))

(defn ^:export readout
  "The text shapes whose id starts with \"readout\" in the frame last
   painted, one per line (the calculus demo's live readout; the headless
   check asserts it follows the widgets)."
  []
  (if-let [p @current]
    (->> (:shapes (:last-frame @(:state (:host p))))
         (filter #(str/starts-with? (str (:id %)) "readout"))
         (map :text)
         (str/join "\n"))
    ""))
