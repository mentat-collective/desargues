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
            [desargues.studio.objects-demos :as od]
            [desargues.studio.widget-demos :as wd]
            [desargues.studio.blackboard :as bb]))

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

  ;; 4. Integrate on raster instead of the JVM RK4 (needs the :dynamics alias):
  ;;    flip the "raster solver" row in the panel, or from the REPL:
  (player/set-param! p :raster 1.0)
  (player/set-param! p :solver :raster)  ; an explicit :solver wins over the row

  ;; 4b. The same player, native: desargues-native (cljrs, tiny-skia, winit)
  ;;     implements the same ports. Build once: cargo build --release in native/.
  ;;     (require '[desargues.player.native :as native])
  ;;     (def n (player/open! (native/host) demos/pendulum
  ;;                          {:params {:raster 1.0} :controls demos/pendulum-controls}))
  ;;     (player/close! n)

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
                                          (od/defaults :tangent)))))

  ;; 8. Widgets that are not sliders (desargues.player.controls kinds :button
  ;;    :select :text :toggle), one demo each in desargues.studio.widget-demos.
  ;;    press! and control-input! go through the same pure step a click in the
  ;;    panel does (a refusal shows under the widget); set-param! skips the
  ;;    step and moves the widget to the value. Params start at the controls'
  ;;    defaults, so open! needs no :params.
  (defn open-widgets [k]
    (let [{:keys [construct controls title]} (wd/demos k)]
      (player/open! (fx/host) construct {:title (str "desargues: " title)
                                         :scene-name (name k)
                                         :controls controls})))

  ;; Turing: Step / Run / Reset buttons over one param, :steps.
  (def t (open-widgets :turing))
  (player/press! t :step)               ; one step of the busy beaver
  (player/press! t :step)
  (player/params t)                     ; => {:steps 2}
  (player/press! t :run)                ; to the halt: 14 steps, six 1s
  (player/press! t :reset)              ; a preset: {:steps 0}
  (player/close! t)

  ;; Kepler: a select (the solid), a slider (the turn), a toggle (the spheres).
  (def k (open-widgets :kepler))
  (player/set-param! k :solid :dodecahedron)      ; the select moves too
  (player/control-input! k :solid :sphere)        ; => {:error ":sphere is not an option" ...}
  (player/set-param! k :turn 2.0)                 ; the slider moves too
  (player/set-param! k :spheres false)            ; the toggle unticks
  (player/close! k)

  ;; Goedel: a text field whose :parse refuses symbols outside the alphabet;
  ;; the refusal shows under the field and the picture stays.
  (def g (open-widgets :goedel))
  (player/control-input! g :formula "0=q")        ; => {:error "not in Goedel's alphabet: q" ...}
  (player/set-param! g :formula "(Ex)(x=s0)")     ; the field shows it
  (wd/godel-number "0=0")                         ; => "243000000" = 2^6 3^5 5^6
  (player/close! g))

(comment
  ;; 9. The blackboard (desargues.studio.blackboard): a worked integral
  ;;    written step by step in chalk, the explanation under each line, side
  ;;    work in the margin, a figure pane, auto-scroll. Widgets: problem and
  ;;    method selects, next / previous / show all / restart, explanations
  ;;    and auto-play toggles. Buttons move the clock (:seek) to where the
  ;;    step starts being written.
  (def b (player/open! (fx/host) bb/blackboard {:title "desargues: blackboard"
                                                :scene-name "blackboard"
                                                :controls bb/controls}))
  (player/control-input! b :problem :by-parts)
  (player/press! b :next)                         ; step mode: one line at a time
  (player/press! b :next)
  (player/press! b :prev)
  (player/press! b :all)                          ; everything, boxed answer, Emmy check
  (player/control-input! b :path 1)               ; the tabular method
  (player/control-input! b :explain false)
  (player/control-input! b :auto true)            ; write the whole lesson again
  (player/snapshot! b "blackboard.png")
  (player/close! b))
