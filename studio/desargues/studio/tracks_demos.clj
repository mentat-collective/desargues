(ns desargues.studio.tracks-demos
  "The Lagrangian teach path on the live stage: each lesson (pendulum,
   spinning top) is integrated through desargues.physics/evolve, written as
   time tracks on a scene graph, and played by desargues.player.track-source.
   The energy-drift badge is a value on the tracks; the overlay here only
   prints it.

   Start a REPL with `clojure -A:dev:studio:dynamics` for :solver :raster
   (the JVM RK4 needs no alias), then evaluate the comment block."
  (:require [desargues.player :as player]
            [desargues.player.track-source :as track-source]
            [desargues.tracks.lagrangian :as tl]))

(defn- badge-text
  "The overlay label for a teach result's energy-drift badge."
  [{:keys [max-relative solver dt duration]}]
  (format "energy drift %.2e  (%s, dt %s, %s s)" (double max-relative) (name solver) dt duration))

(defn lesson-source
  "A source-fn (fn [params] ISource) for lesson `k`: params are the lesson's
   physics params plus :solver :dt :duration :every."
  [k]
  (fn [params]
    (let [{:keys [graph drift]} (tl/teach k (merge {:every 2} params))
          title (:scene graph)]
      (track-source/track-source
       graph
       {:overlay (fn [_t]
                   [{:node :text :id :title :text (str title ", Emmy Lagrangian") :at [0.0 3.4]
                     :opts {:font-size 30 :color :white}}
                    {:node :text :id :badge :text (badge-text drift) :at [0.0 -3.4]
                     :opts {:font-size 22 :color :grey}}])}))))

(def demos
  "lesson key -> {:title :controls}, the sliders each lesson binds."
  {:pendulum {:title "pendulum"
              :controls [{:id :theta0 :kind :slider :label "theta0 (rad)" :min 0.1 :max 3.0 :value 1.0}
                         {:id :l :kind :slider :label "length (m)" :min 0.3 :max 1.5 :value 1.0}]}
   :top {:title "spinning top"
         :controls [{:id :psidot0 :kind :slider :label "spin (rad/s)" :min 5.0 :max 60.0 :value 20.0}
                    {:id :theta0 :kind :slider :label "tilt (rad)" :min 0.1 :max 1.4 :value 0.5}]}})

(defn open-lesson!
  "Play lesson `k` in `host` with its sliders."
  [host k params]
  (let [{:keys [title controls]} (demos k)]
    (player/open-source! host (lesson-source k)
                         {:title (str "desargues: " title " (tracks)")
                          :params params
                          :controls controls})))

(comment
  (require '[desargues.player.javafx :as fx])
  (def p (open-lesson! (fx/host) :pendulum {}))
  (player/set-param! p :theta0 2.5)       ; re-integrates, new badge
  (player/set-param! p :solver :raster)   ; needs :dynamics
  (player/snapshot! p "tracks-pendulum.png")
  (player/close! p)

  (def q (open-lesson! (fx/host) :top {}))
  (player/set-param! q :psidot0 8.0)      ; slow spin: wide nutation
  (player/snapshot! q "tracks-top.png")
  (player/close! q))
