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
            [desargues.studio.demos :as demos]))

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
  (player/close! p))
