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
            [plato.desargues :as desargues]
            [desargues.videos.physics :as phys]
            [clojure.string :as str]))

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

(def f-sin-exp (fn [x] (e/exp (e/sin x))))

(defn- tex-of [expr] (:content (pipe/promote-latex (e/simplify expr))))

(def derivative-tower
  "f, f', f'', f''' of e^{sin x}, each differentiated and simplified by Emmy."
  (mapv (fn [k] (tex-of ((nth (iterate e/D f-sin-exp) k) 'x))) (range 4)))

(def taylor-terms
  "The first five Taylor terms of e^{sin x} at 0 (the zero term dropped)."
  (->> ((e/taylor-series f-sin-exp 0) 'x)
       (take 5)
       (map e/simplify)
       (remove #(and (number? %) (zero? %)))
       (mapv tex-of)))

(def derivatives-source
  "(def f (fn [x] (e/exp (e/sin x))))

(mapv #(e/->TeX (e/simplify ((nth (iterate e/D f) %) 'x))) (range 4))
;; f, f', f'', f'''  as LaTeX, ready for KaTeX or for Manim's MathTex

(take 5 ((e/taylor-series f 0) 'x))
;; => (1 x (* 1/2 (expt x 2)) 0 (* -1/8 (expt x 4)))")

(def curve-f (fn [x] (e/sin x)))
(def curve-df (e/D curve-f))

(defn derivative-construct
  "y = sin x drawn as dots; a gold point walks the curve while a seven-dot
   tangent follows the slope Emmy's D returns and a readout shows f'(x)."
  [stage]
  (let [sx 1.0 sy 1.6 oy -0.4
        P (fn [x] [(* sx x) (+ oy (* sy (curve-f x)))])
        tangent-pts (fn [x] (let [m (curve-df x)]
                              (mapv (fn [i] (let [h (* i 0.28)] [(* sx (+ x h)) (+ oy (* sy (+ (curve-f x) (* m h))))]))
                                    (range -3 4))))
        title  (-> (s/text "f(x) = sin x, and Emmy's f'(x) = cos x as a slope" :font-size 30 :color :white)
                   (s/move-to [0 3.3]))
        curve  (mapv (fn [x] (-> (s/dot :color :grey :radius 0.04) (s/move-to (P x))))
                     (range -6.0 6.01 0.2))
        xs     (vec (range -6.0 6.01 0.3))
        tang   (mapv (fn [p] (-> (s/dot :color :teal :radius 0.06) (s/move-to p))) (tangent-pts (first xs)))
        point  (-> (s/circle :radius 0.18 :color :gold) (s/move-to (P (first xs))) (s/fill! :gold :opacity 0.9))
        label  (-> (s/text "f'(x) =" :font-size 30 :color :grey) (s/move-to [4.4 -3.0]))
        slope  (-> (s/decimal (curve-df (first xs)) :font-size 30 :color :gold :num-decimal-places 2)
                   (s/move-to [5.7 -3.0]))]
    (s/play! stage (s/appear title :run-time 0.5))
    (s/play! stage (s/stagger (map #(s/appear % :run-time 0.2) curve) :lag-ratio 0.02))
    (s/play! stage (s/together (concat [(s/draw point :run-time 0.4) (s/appear label :run-time 0.4) (s/appear slope :run-time 0.4)]
                                       (map #(s/appear % :run-time 0.4) tang))))
    (doseq [x (rest xs)]
      (s/play! stage (s/together (concat [(s/glide point (P x) :run-time 0.15)
                                          (s/count-to slope (curve-df x) :run-time 0.15)]
                                         (map (fn [d p] (s/glide d p :run-time 0.15)) tang (tangent-pts x))))))
    (s/hold! stage 1)))

(def derivative-graph
  (s/with-backend (rec/recording-backend)
    (s/render! "desargues-derivative" derivative-construct)))

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

(def pendulum
  (phys/make-pendulum {:length 3.0 :damping 0.12 :initial-theta 1.1 :top-point [0 2.2 0]}))

(defn pendulum-construct
  "The pendulum integrated by phys/evolve (RK4, dt 0.01), sampled every 0.1 s;
   each sample is one glide of the bob while a clock counts the seconds."
  [stage]
  (let [samples (take-nth 10 (phys/evolve pendulum {:dt 0.01 :duration 8.0}))
        pts     (mapv (fn [{:keys [state]}]
                        (let [[x y] (phys/pendulum-position pendulum state)] [x y]))
                      samples)
        title   (-> (s/text "Damped pendulum: phys/evolve, RK4 at dt = 0.01" :font-size 30 :color :white)
                    (s/move-to [0 3.3]))
        pivot   (-> (s/dot :color :grey) (s/move-to [0 2.2]))
        bob     (-> (s/circle :radius 0.32 :color :gold)
                    (s/move-to (first pts))
                    (s/fill! :gold :opacity 0.7))
        clock   (-> (s/decimal 0 :font-size 30 :color :grey :num-decimal-places 1)
                    (s/move-to [5.2 -3.0]))
        unit    (-> (s/text "s" :font-size 26 :color :grey) (s/move-to [5.9 -3.0]))]
    (s/play! stage (s/together [(s/appear title :run-time 0.5) (s/appear pivot :run-time 0.5)
                                (s/draw bob :run-time 0.5) (s/appear clock :run-time 0.5)
                                (s/appear unit :run-time 0.5)]))
    (doseq [[i p] (map-indexed vector (rest pts))]
      (s/play! stage (s/together [(s/glide bob p :run-time 0.1)
                                  (s/count-to clock (* 0.1 (inc i)) :run-time 0.1)])))
    (s/hold! stage 1)))

(def pendulum-graph
  (s/with-backend (rec/recording-backend)
    (s/render! "desargues-pendulum" pendulum-construct)))

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

     (deck/stack
      :derivatives
      [(deck/slide
        :derivatives-tower
        [:div
         [:h3 "Differentiate until it stops being obvious"]
         [:p "Four derivatives of " (tex "e^{\\sin x}") ", each computed and simplified by Emmy when this page was built:"]
         (into [:div {:style "text-align:left;font-size:0.95em"}]
               (map-indexed (fn [k t]
                              [:p {:class (when (pos? k) "fragment")}
                               (tex (str "f" (apply str (repeat k "'")) "(x) = " t))])
                            derivative-tower))]
        {:overflow :shrink
         :notes "(iterate e/D f) on the JVM, then ->TeX, then KaTeX in the page. Nothing here is typed by hand."})
       (deck/slide
        :derivatives-scene
        (desargues/scene derivative-graph {:controls? true :autoplay? true})
        {:transition :fade
         :notes "The teal dots are the tangent: slope from ((e/D sin) x) at every step, the readout counts the same number."})
       (deck/slide
        :derivatives-taylor
        [:div
         [:h3 "The Taylor series, term by term"]
         [:p "Emmy's " [:code "taylor-series"] " is a lazy stream of terms; the deck takes the first five:"]
         [:p {:style "font-size:1.2em"}
          (tex (str "e^{\\sin x} = "
                    (str/replace (str/join " + " taylor-terms) "+ \\frac{-1}" "- \\frac{1}")
                    " + \\cdots"))]
         (content/code :clojure derivatives-source)]
        {:overflow :shrink})])

     (deck/slide
      :physics
      [:div
       [:h3 "Trajectories behind a port"]
       [:p [:code "TrajectorySolver"] " is a protocol. Clojure RK4 ships; the "
        [:code ":dynamics"] " alias adds raster's Tsit5 and DP5, resampled onto the frame grid."]
       (content/code :clojure solver-source)]
      {:overflow :shrink})

     (deck/slide
      :physics-sim
      (desargues/scene pendulum-graph {:controls? true :autoplay? true})
      {:transition :fade
       :notes "phys/evolve with the Clojure RK4 stepper, dt 0.01, sampled every 0.1 s; each sample is one glide of the bob. Same trajectory the Manim backend would draw."})

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
