(ns desargues.site.deck
  "The landing page: a plato deck built BY desargues. The scenes on it are real
   RecordingBackend scene graphs and the LaTeX comes out of the Emmy conveyor,
   both computed when this namespace loads.

     bb site   ->  dist/site/index.html  (plato.cli, --live-scenes --math)"
  (:require [desargues.layout.core :as l]
            [desargues.pipeline.emmy :as pipe]
            [desargues.scene :as s]
            [desargues.scene.data :as rec]
            [emmy.env :as e]
            [plato.content :as content]
            [plato.deck :as deck]
            [plato.desargues :as desargues]))

;; ── scenes, computed by the library ─────────────────────────────────────────

(defn construct
  "The same construct fn renders to Manim (mp4) or records to EDN (this page)."
  [stage]
  (let [title (-> (s/text "A scene is a function of the stage" :font-size 34 :color :white)
                  (s/move-to [0 2.6]))
        circles (mapv (fn [[x c]] (-> (s/circle :radius 1.0 :color c)
                                      (s/move-to [x 0])
                                      (s/fill! c :opacity 0.25)
                                      (s/stroke! :color c :width 4)))
                      [[-3.6 :teal] [0 :gold] [3.6 :green]])
        counters (mapv (fn [c] (-> (s/decimal 0 :font-size 36 :color :white :num-decimal-places 0)
                                   (s/next-to c :down)))
                       circles)
        caption (-> (s/text "play! / hold! against a protocol, never against Manim" :font-size 24 :color :grey)
                    (s/move-to [0 -2.8]))]
    (s/play! stage (s/appear title :run-time 0.7))
    (s/play! stage (s/stagger (map #(s/draw % :run-time 0.8) circles) :lag-ratio 0.3))
    (s/play! stage (s/together (map #(s/appear % :run-time 0.4) counters)))
    (s/play! stage (s/together (map-indexed (fn [i d] (s/count-to d (* 100 (inc i)) :run-time 1.5)) counters)))
    (s/play! stage (s/appear caption :run-time 0.6))
    (s/play! stage (s/together (map #(s/glide % [0 0] :run-time 1.2) circles)))
    (s/hold! stage 1)))

(def construct-source
  "(defn construct [stage]
  (let [circles (mapv (fn [[x c]] (-> (s/circle :radius 1.0 :color c)
                                      (s/move-to [x 0])
                                      (s/fill! c :opacity 0.25)))
                      [[-3.6 :teal] [0 :gold] [3.6 :green]])
        nums    (mapv #(s/next-to (s/decimal 0) % :down) circles)]
    (s/play! stage (s/stagger (map s/draw circles) :lag-ratio 0.3))
    (s/play! stage (s/together
                     (map-indexed #(s/count-to %2 (* 100 (inc %1))) nums)))
    (s/play! stage (s/together (map #(s/glide % [0 0]) circles)))
    (s/hold! stage 1)))")

(def render-source
  ";; Manim: writes scene.mp4 (the lazy default backend)
(s/render! \"scene\" construct)

;; Pure data, no Python: the scene on the previous slide
(s/with-backend (rec/recording-backend)
  (s/render! \"scene\" construct))
;; => {:scene \"scene\" :nodes {1 {:node :circle ...} ...}
;;     :steps [{:step :play :anims [...]} ... {:step :hold :seconds 1}]}")

(def animation-graph
  "The scene above, recorded. Pure data: :scene / :nodes / :steps."
  (s/with-backend (rec/recording-backend)
    (s/render! "desargues-scene" construct)))

(def layout-tree
  (l/column {:width :fill :height :fill :padding 0.8 :spacing 0.5 :align :center-x :font-size 32}
    (l/text "Layout algebra" {:font-size 44 :color :gold})
    (l/row {:width :fill :height [:portion 2] :spacing 0.6}
      (l/box {:width [:portion 1] :height :fill :border :white :padding 0.4}
        (l/column {:spacing 0.3 :align :center-x}
          (l/text "Bank A")
          (l/text "ΔM = 1000" {:color :teal})))
      (l/box {:width [:portion 1] :height :fill :border [:teal 3] :padding 0.4}
        (l/column {:spacing 0.3 :align :center-x}
          (l/text "Reserves")
          (l/text "r · D" {:color :gold}))))
    (l/text "measure → resolve → realize, on any backend" {:font-size 24 :color :grey})))

(def layout-source
  "(v/render-layout! \"balance\"
  (v/column {:padding 0.8 :spacing 0.5 :align :center-x}
    (v/text \"Layout algebra\" {:font-size 44 :color :gold})
    (v/row {:width :fill :spacing 0.6}
      (v/box {:width [:portion 1] :border :white}
        (v/text \"Bank A\")
        (v/text \"ΔM = 1000\" {:color :teal}))
      (v/box {:width [:portion 1] :border [:teal 3]}
        (v/text \"Reserves\")
        (v/text \"r · D\" {:color :gold})))))")

(def layout-graph
  (rec/layout->scene-graph "desargues-layout" layout-tree {:hold 1}))

;; ── the Emmy conveyor, run for real ─────────────────────────────────────────

(def derivative
  (pipe/derivative-spec (fn [x] (e/exp (e/sin x)))))

(defn- tex [s] (str "\\(" s "\\)"))

;; ── sources quoted on slides ────────────────────────────────────────────────

(def deps-source
  ";; deps.edn
{:deps {io.github.mentat-collective/desargues
        {:git/tag \"v0.1.1\" :git/sha \"6d26609\"}}}")

(def conda-source
  "conda create -n manim -c conda-forge python=3.12 manim
conda activate manim          # sets CONDA_PREFIX; desargues.config picks it up
clojure -M:doctor             # JVM ok? Manim env ok? LaTeX? ffmpeg?")

(def pure-source
  "(require '[desargues.scene :as s]
         '[desargues.scene.data :as rec]
         '[desargues.layout.core :as l])

(rec/layout->scene-graph \"hello\"
  (l/column {:padding 0.6 :spacing 0.4 :align :center-x}
    (l/text \"Reserves\")
    (l/math \"\\\\Delta M = 1000\"))
  {})
;; => {:scene \"hello\" :nodes {1 {:node :text ...} 2 {:node :math ...}}
;;     :steps [{:step :play ...} {:step :hold :seconds 3}] ...}")

(def emmy-source
  "(pipe/derivative-spec (fn [x] (e/exp (e/sin x))))
;; => {:expr            (exp (sin x))
;;     :derivative-expr (* (cos x) (exp (sin x)))
;;     :func-latex      \"e^{\\\\sin\\\\left(x\\\\right)}\"
;;     :deriv-latex     \"\\\\cos\\\\left(x\\\\right)\\\\,e^{\\\\sin\\\\left(x\\\\right)}\"}")

(def solver-source
  "(require '[desargues.videos.physics :as phys]
         '[desargues.infrastructure.raster-adapter :as ra])

(def pendulum (phys/make-pendulum {:length 3.0 :damping 0.1}))

(phys/evolve pendulum {:dt 0.01 :duration 10.0})                          ; RK4 in Clojure
(phys/evolve pendulum {:dt 0.01 :duration 10.0 :solver (ra/raster-solver :tsit5)}) ; adaptive, resampled onto the dt grid")

;; ── the deck ────────────────────────────────────────────────────────────────

(def model
  (deck/deck
   {:title "desargues"
    :description "desargues — backend-neutral mathematical animation in Clojure: Emmy symbolic math, Manim video, or pure scene-graph data for the browser."
    :math? true
    :config {:hash true :history true :controls true :progress true
             :center true :slide-number "c/t" :transition :slide}
    :slides
    [(deck/slide
      :welcome
      [:div
       (content/kicker "Clojure · Emmy · Manim")
       [:h1 "desargues"]
       [:p "Symbolic mathematics in, mathematical animation out. Emmy computes, "
        "LaTeX carries, and a backend-neutral scene facade renders — to Manim video, "
        "or to pure data a browser can play."]
       [:p.fragment "Every scene on this page was recorded by desargues when the page was built."]]
      {:background-color "#0b0e13"
       :notes "Space or arrows to advance. The two scenes further on are live and scrub-able."})

     (deck/slide
      :scene
      (desargues/scene animation-graph {:controls? true :autoplay? true})
      {:transition :fade
       :notes "A RecordingBackend scene graph: the same construct fn that writes an mp4 under Manim."})

     (deck/stack
      :construct
      [(deck/slide
        :construct-fn
        [:div
         [:h3 "One construct"]
         (content/code :clojure construct-source {:highlight "1-6|7-10|11"})]
        {:overflow :shrink
         :notes "play!/hold! talk to desargues.scene.protocols. Manim and the recorder both implement it, so a scene never names Manim."})
       (deck/slide
        :construct-render
        [:div
         [:h3 "Two backends"]
         (content/code :clojure render-source {:highlight "1-2|4-8"})
         [:p "The facade is " [:code "desargues.scene"] "; the seam is " [:code "desargues.scene.protocols"]
          ". Swap the backend, keep the scene."]]
        {:overflow :shrink})])

     (deck/slide
      :layout
      (desargues/scene layout-graph {:controls? false :autoplay? true})
      {:transition :fade
       :notes "desargues.scene/render-layout!: an elm-ui style tree, measured by a pure extent estimator, resolved into boxes."})

     (deck/slide
      :layout-source
      [:div
       [:h3 "Layout algebra"]
       (content/code :clojure layout-source)
       [:p "column / row / box / text / math / image with " [:code ":fill"] ", " [:code "[:portion n]"]
        " and padding: measure → resolve → realize, on either backend."]]
      {:overflow :shrink})

     (deck/stack
      :install
      [(deck/slide
        :install-lib
        [:div
         [:h3 "Install"]
         [:p "A git coordinate. The pure half runs on the JVM alone."]
         (content/code :clojure deps-source)
         [:p "Or from a clone: " [:code "git clone https://github.com/mentat-collective/desargues && cd desargues && bb doctor"]]]
        {:overflow :shrink})
       (deck/slide
        :install-manim
        [:div
         [:h3 "The Manim backend"]
         [:p "One conda env. desargues derives every path from " [:code "CONDA_PREFIX"] "; nothing is hardcoded."]
         (content/code :bash conda-source)
         (content/note "Not the active env? Set DESARGUES_CONDA_PREFIX. Any single path can be overridden with DESARGUES_MANIM_PYTHON / _LIBPYTHON / _SITEPACKAGES." {:tone :info})]
        {:overflow :shrink})
       (deck/slide
        :install-pure
        [:div
         [:h3 "Sixty seconds, no Python"]
         (content/code :clojure pure-source {:highlight "1-3|5-9|10-11"})]
        {:overflow :shrink})])

     (deck/slide
      :emmy
      [:div
       [:h3 "The Emmy conveyor"]
       [:p "Collect the symbolic facts, promote them to LaTeX. This line was computed by the build:"]
       [:p {:style "font-size:1.15em"}
        (tex (str "\\frac{d}{dx}\\," (:func-latex derivative) " = " (:deriv-latex derivative)))]
       (content/code :clojure emmy-source)]
      {:overflow :shrink
       :notes "desargues.pipeline.emmy is pure CPPB: Collect, Promote, Pipeline, Boundary. The Python code step is the only boundary."})

     (deck/slide
      :physics
      [:div
       [:h3 "Trajectories behind a port"]
       [:p [:code "TrajectorySolver"] " is a protocol. Clojure RK4 ships; the "
        [:code ":dynamics"] " alias adds raster's Tsit5 and DP5, resampled onto the frame grid."]
       (content/code :clojure solver-source)]
      {:overflow :shrink})

     (deck/stack
      :bench
      [(deck/slide
        :bench-scaling
        [:div
         [:h3 "Benchmarks"]
         (content/image "assets/bench/scaling.svg"
                        {:alt "log-log chart: layout resolve, scene-graph expansion and recording render time versus size"
                         :caption "Pure backend: linear in the number of elements and animations."})])
       (deck/slide
        :bench-solvers
        [:div
         [:h3 "Solvers"]
         (content/image "assets/bench/solvers.svg"
                        {:alt "bar chart: pendulum trajectory time per solver"
                         :caption "A damped pendulum, 10 s at dt = 0.01, per solver."})])
       (deck/slide
        :bench-emmy
        [:div
         [:h3 "Emmy conveyor"]
         (content/image "assets/bench/emmy-conveyor.svg"
                        {:alt "bar chart: differentiate and render to LaTeX, per function"
                         :caption "Differentiate and render both to LaTeX, per function."})])
       (deck/slide
        :bench-how
        [:div
         [:h3 "Measure it yourself"]
         (content/code :bash "bb bench            # clojure -M:bench:dynamics\nbb bench --manim    # adds a Manim render per quality (needs the env)")
         [:p "Writes " [:code "bench/results/latest.edn"] " and regenerates these charts under " [:code "doc/bench/"] "."]])])

     (deck/slide
      :architecture
      [:div
       [:h3 "Layered, dependency arrow inward"]
       (content/cards
        [{:title "desargues.api"        :body "High-level facade: expr, func, derivative, animate-derivative, render-layout!"}
         {:title "desargues.scene"      :body "Backend-neutral animation DSL — the DIP seam (desargues.scene.protocols)"}
         {:title "scene.manim / scene.data" :body "Two backends: libpython-clj → Manim, or an EDN recorder"}
         {:title "desargues.layout"     :body "elm-ui layout algebra with a pure extent estimator"}
         {:title "desargues.domain"     :body "Pure Clojure + Emmy, Typed-Clojure annotated"}
         {:title "desargues.config"     :body "Everything about the environment, resolved once at the boundary"}]
        {:columns 3})])

     (deck/slide
      :links
      [:div
       [:h3 "Go"]
       (content/bullets
        [[:span [:a {:href "https://github.com/mentat-collective/desargues"} "github.com/mentat-collective/desargues"] " — source, docs, changelog"]
         [:span [:a {:href "https://github.com/mentat-collective/emmy"} "Emmy"] " — the symbolic engine"]
         [:span [:a {:href "https://www.manim.community/"} "Manim Community"] " — the renderer"]
         [:span [:a {:href "https://github.com/BuddhiLW/plato"} "plato"] " — this page, and the player for the scenes on it"]])
       [:p "EPL-2.0 or GPL-2.0-or-later with Classpath exception."]])]}))
