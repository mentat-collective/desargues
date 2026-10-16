(ns desargues.studio.repl
  "REPL walkthrough for the live player. Start a REPL with
   `clojure -A:dev:studio` (add :dynamics for the raster solver) and evaluate
   the forms in the comment block one by one.

   The same construct value (demos/pendulum params) renders an mp4 through
   desargues.scene/render! with the Manim backend; here it plays live."
  (:require [desargues.player :as player]
            [desargues.player.javafx :as fx]
            [desargues.player.session :as session]
            [desargues.player.timeline :as timeline]
            [desargues.studio.demos :as demos]
            [desargues.studio.objects-demos :as od]))

(comment
  ;; 1. Open a window. The host is the only toolkit-specific value here: any
  ;;    other host implementing desargues.player.protocols drops in unchanged.
  (def p (player/open! (fx/host) demos/pendulum
                       {:title "desargues: live pendulum"
                        :params {:length 2.0 :damping 0.15 :theta0 1.2}
                        :controls demos/pendulum-controls}))

  ;; 2. Drive it. Each set-param! re-records the construct (RK4 re-integrates)
  ;;    and moves the matching slider; the session keeps its time.
  (player/set-param! p :damping 0.0)
  (player/set-param! p :theta0 2.9)
  (player/params p)

  ;; 3. Transport.
  (player/pause! p)
  (player/seek! p 4.0)
  (player/play! p)

  ;; 4. Integrate on raster instead of the JVM RK4 (needs the :dynamics alias).
  (player/set-param! p :solver :raster)

  ;; 5. Look inside: the scene graph is plain EDN, the frame at t is plain data.
  (def g (player/record "Pendulum" (demos/pendulum (player/params p))))
  (count (:steps g))
  (timeline/frame-at g (timeline/schedule g) 3.0)

  ;; 6. Picture to disk, then close.
  (player/snapshot! p "studio.png")
  (player/close! p)

  ;; 7. The math object vocabulary (desargues.objects): one demo per object,
  ;;    each with its own sliders. Keys: :axes :plot :parametric :polar
  ;;    :field :tangent :riemann :tex.
  (defn open-demo [k]
    (let [{:keys [construct controls title]} (od/demos k)]
      (player/open! (fx/host) construct {:title (str "desargues: " title)
                                         :scene-name (name k)
                                         :params (od/defaults k)
                                         :controls controls})))
  (def q (open-demo :tangent))
  (player/set-param! q :x0 -1.0)
  (player/set-param! q :h 0.1)          ; the secant closes onto the tangent
  (player/close! q)

  (def q (open-demo :riemann))
  (player/set-param! q :n 30)
  (player/set-param! q :rule 1)         ; midpoint rule
  (player/close! q)

  (def q (open-demo :field))
  (player/set-param! q :curl 0.0)       ; a pure source
  (player/close! q)

  (doseq [k [:axes :plot :parametric :polar :tex]]
    (let [q (open-demo k)] (Thread/sleep 4000) (player/close! q)))

  ;; The same objects build any construct, on any backend:
  ;;   (o/add! stage (ax/axes cs {:x-label "x"}))
  ;;   (o/sweep! stage handle #(calc/tangent cs f %) [0.0 0.5 1.0])
  ;; and the step count matches the RecordingBackend graph the stage plays:
  (count (:steps (player/record "Tangent" ((:construct (od/demos :tangent))
                                          (od/defaults :tangent))))))
