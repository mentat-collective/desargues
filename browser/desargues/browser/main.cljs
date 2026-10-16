(ns desargues.browser.main
  "BOUNDARY: the demo page's entry. Reads ?demo=<key> (one of the eight
   desargues.browser.demos, played live with sliders) or ?scene=<url> (a
   recorded EDN scene graph, e.g. scenes/pendulum.edn), builds a menu and
   mounts the canvas host into #stage.

   window.desargues exposes the player and a paint probe for the headless
   check (browser/check.mjs): desargues.status() -> {demo, t, painted}."
  (:require [clojure.string :as str]
            [desargues.browser.canvas :as canvas]
            [desargues.browser.demos :as demos]
            [desargues.browser.player :as player]))

(defonce ^:private current (atom nil))

(def scenes
  "Recorded scene graphs shipped beside the page (EDN, no JVM at view time)."
  {"pendulum" {:title "damped pendulum (recorded)" :url "scenes/pendulum.edn"}})

(defn- query []
  (let [ps (js/URLSearchParams. (.. js/window -location -search))]
    {:demo (.get ps "demo") :scene (.get ps "scene") :t (.get ps "t")}))

(defn- menu! []
  (when-let [nav (.getElementById js/document "menu")]
    (set! (.-innerHTML nav)
          (str/join " | "
                    (concat
                     (for [[k {:keys [title]}] (sort-by key demos/demos)]
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
        host  (canvas/host mount)]
    (if scene
      (let [{:keys [title url]} (get scenes scene {:title scene :url scene})]
        (-> (player/load-graph! host url (opts title))
            (.then #(after-open! % t))))
      (let [k (keyword (or demo "tangent"))
            {:keys [title construct controls]} (get demos/demos k (:tangent demos/demos))]
        (after-open! (player/open! host construct
                                   (assoc (opts title)
                                          :params (demos/defaults k)
                                          :controls controls))
                     t)))))

(defn ^:export status
  "What the headless check reads: the current time, how many pixels of the
   stage canvas differ from the background, and how many KaTeX labels the
   overlay shows (TeX typesets to DOM, not to the canvas)."
  []
  (if-let [p @current]
    (let [{:keys [canvas tex-cache tick-error]} @(:state (:host p))]
      #js {"t" (:t (:session @(:state p)))
           "painted" (if canvas (canvas/painted-pixels canvas) 0)
           "tex" (count (filter #(not= "none" (.. % -style -display))
                                (vals (some-> tex-cache deref))))
           "tickError" (some-> tick-error str)})
    #js {"t" nil "painted" 0 "tex" 0}))

(defn ^:export seek [t]
  (when-let [p @current] (player/pause! p) (player/seek! p t) (status)))

(defn ^:export set-param [k v]
  (when-let [p @current] (player/set-param! p (keyword k) v) (status)))
