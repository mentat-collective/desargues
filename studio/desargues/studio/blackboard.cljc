(ns desargues.studio.blackboard
  "The BLACKBOARD: a worked solution written step by step like chalk on a
   slate board. Each step is an equation line written left to right with a
   plain-English line under it in smaller italic chalk; substitutions go to
   a side margin; u and du are chalked in yellow, dv and v in blue, the
   answer is boxed in pink; a small figure pane on the right shows the curve
   (and the area, for a definite integral). When the board fills, the lines
   written so far slide up so the step being written stays in view.

   One parametric construct, (fn [params] (fn [stage] ...)), over the scene
   facade only, so the JavaFX window, the browser page and the headless PNG
   host all play it. Params:

     :problem  lesson id (desargues.studio.blackboard-lessons)
     :path     which pathway (solution method), 0-based
     :step     steps written so far in step mode (0 = just the problem)
     :explain  true writes the explanation lines
     :auto     true writes the whole lesson over time (auto-play)

   Widgets (`controls`): problem and method selects, next / previous / show
   all / restart buttons, an explanations toggle and an auto-play toggle.
   Buttons carry :seek (desargues.player.controls) so the clock jumps to
   where the new step starts being written.

   The layout and the timeline are one pure value, `plan`, which the
   construct plays and the :seek fns read. Host-free .cljc."
  (:require [clojure.string :as str]
            [desargues.objects :as o]
            [desargues.objects.axes :as ax]
            [desargues.objects.calculus :as calc]
            [desargues.objects.coords :as c]
            [desargues.objects.plot :as pl]
            [desargues.player.controls :as controls]
            [desargues.scene :as s]
            [desargues.studio.blackboard-lessons :as bl]))

;; ── chalk ─────────────────────────────────────────────────────────────────────

(def chalk
  "Chalk colours on the slate."
  {:white "#F4F1E8" :dim "#CFC9B8" :grey "#8FA096" :yellow "#FFE27A"
   :blue "#8CCBF2" :pink "#F6A6B2" :green "#A8E6A1"})

(def slate "#1E3A2F")
(def wood "#6B4A2B")

(def ^:private macro-chalk {"yel" :yellow "blu" :blue "pnk" :pink "grn" :green})

(defn chalk-tex
  "TeX `tex` with the chalk macros \\yel{..} \\blu{..} \\pnk{..} \\grn{..}
   expanded to \\textcolor (JLaTeXMath and KaTeX both read it)."
  [tex]
  (str/replace (str tex) #"\\(yel|blu|pnk|grn)\{"
               (fn [[_ k]] (str "\\textcolor{" (chalk (macro-chalk k)) "}{"))))

(defn- escape-text [s] (str/replace s #"[&%#_{}]" (fn [m] (str "\\" m))))

(defn- text-run [p]
  (cond (= "" p)        ""
        (str/blank? p)  "\\ "
        :else (str (when (str/starts-with? p " ") "\\ ")
                   "\\textit{" (escape-text (str/trim p)) "}"
                   (when (str/ends-with? p " ") "\\ "))))

(defn explain->tex
  "A plain-English explanation as TeX: italic text, with math between $...$."
  [s]
  (apply str (map-indexed (fn [i p] (if (odd? i) (chalk-tex p) (text-run p)))
                          (str/split (str s) #"\$" -1))))

;; ── layout and timeline: one pure value ───────────────────────────────────────

(def geom
  "Board geometry in scene units (Manim's 14.22 x 8 frame, y up). :eq-font
   and :ex-font are the chalk sizes of an equation line and of the
   explanation under it."
  {:left -6.45 :num-x -6.8 :top 2.25 :bottom -3.6 :fade-above 2.2
   :eq-h 0.98 :eq-h-flat 0.76 :ex-h 0.54 :gap 0.12 :eq-font 48 :ex-font 31
   :col-x 2.75 :side-x 2.95 :side-top 1.75 :side-h 0.6
   :fig-center [4.85 -2.3] :fig-w 3.6 :fig-h 2.2})

(defn- tall? [tex] (boolean (re-find #"\\frac|\\int|\\left|\\Big|\\boxed|\\tfrac" tex)))

(defn- write-time [s per lo hi] (-> (+ 0.5 (* per (count s))) (max lo) (min hi)))

(defn lesson-of [{:keys [problem path]}]
  (let [l (bl/lesson problem)] [l (bl/pathway l path)]))

(defn shown-steps
  "How many steps params `ps` put on the board."
  [{:keys [step auto] :as ps}]
  (let [n (count (:steps (second (lesson-of ps))))]
    (if auto n (-> (long (or step 0)) (max 0) (min n)))))

(defn- step-rows
  "The rows of step `st` (index `i`) at unscrolled top `y`: [rows bottom].
   An equation row is taller when it holds a fraction or an integral; so is
   an explanation with one in its math."
  [{:keys [left num-x eq-h eq-h-flat ex-h gap eq-font ex-font]} explain? i st y]
  (let [tex  (cond-> (chalk-tex (:tex st)) (:answer? st) (#(str "\\boxed{" % "}")))
        h    (if (tall? tex) eq-h eq-h-flat)
        eq   {:key [i :eq] :tex tex :at [left (- y (/ h 2))] :font eq-font
              :color (cond (:answer? st) :pink (:check? st) :green :else :white)}
        num  {:key [i :num] :text (if (:check? st) "" (str (inc i) ".")) :at [num-x (- y (/ h 2))]}
        y1   (- y h)
        exh  (if (re-find #"\\frac|\\int" (str (:explain st))) (+ ex-h 0.2) ex-h)
        ex   (when (and explain? (:explain st))
               {:key [i :ex] :tex (explain->tex (:explain st)) :at [(+ left 0.3) (- y1 (/ exh 2))]
                :font ex-font :color :dim})]
    [(cond-> [eq num] ex (conj ex)) (- (if ex (- y1 exh) y1) gap)]))

(defn plan
  "The board for params `ps`: {:lesson :pathway :items [..] :acts [..]
   :starts [t ...] :end t :duration t}. Items are what gets chalked (TeX or
   text at a point); acts are the timeline, each {:act :dur ...}; :starts[i]
   is when step i starts being written and :end when the last write ends."
  [{:keys [explain auto] :or {explain true} :as ps}]
  (let [g (merge geom (:geom ps))
        [lesson {:keys [steps label]}] (lesson-of ps)
        k     (shown-steps ps)
        check (when (and (= k (count steps)) (:check lesson))
                {:tex (str "\\checkmark\\ \\ " (:check lesson)) :check? true
                 :explain "Checked with Emmy: differentiate the answer, get the integrand back."})
        sts   (cond-> (vec (take k steps)) check (conj check))
        fig   (:figure lesson)
        side-n (atom 0)
        header [{:act :board :dur 0.3} {:act :header :dur 1.6}]
        header (cond-> header fig (conj {:act :figure :dur 1.4}))
        header (conj header {:act :hold :dur 0.4})]
    (loop [i 0 y (:top g) off 0.0 items [] acts header starts []]
      (if (= i (count sts))
        (let [t-of (reductions + 0.0 (map :dur acts))
              end  (last t-of)
              acts (conj acts {:act :hold :dur (if auto 6.0 30.0)})]
          {:lesson lesson :label label :steps sts :items items :acts acts :geom g
           :starts (mapv #(nth t-of %) starts) :end end :duration (+ end (if auto 6.0 30.0))})
        (let [st (nth sts i)
              [rows bot] (step-rows g explain i st y)
              off'  (max off (- (:bottom g) bot))
              scroll (when (> off' off)
                       {:act :scroll :dur 0.7
                        :moves (vec (for [{:keys [key at gone?]} items
                                          :when (and (not gone?) (not= :side (second key)))
                                          :let [[x y0] at y1 (+ y0 (- off' off))]]
                                      [key [x y1] (> y1 (:fade-above g))]))})
              rows  (mapv (fn [r] (let [[x y0] (:at r) at [x (+ y0 off')]]
                                    (assoc r :at at :at0 at)))
                          rows)
              side  (when (:side st)
                      (let [j (dec (swap! side-n inc))]
                        {:key [i :side] :tex (chalk-tex (:side st)) :font 26 :color :white
                         :at0 [(:side-x g) (- (:side-top g) (* j (:side-h g)))]}))
              side  (when side (assoc side :at (:at0 side)))
              eq-dur (write-time (:tex st) 0.035 1.0 2.4)
              new-acts (cond-> []
                         scroll (conj scroll)
                         true   (conj {:act :write :keys [[i :eq] [i :num]] :dur eq-dur})
                         side   (conj {:act :write :keys [[i :side]] :dur 1.1})
                         (some #(= [i :ex] (:key %)) rows)
                         (conj {:act :write :keys [[i :ex]] :dur (write-time (:explain st) 0.02 0.9 1.8)})
                         (and fig (:shade? st))  (conj {:act :shade :dur 1.2})
                         (and fig (:F fig) (:answer? st)) (conj {:act :antiderivative :dur 1.4})
                         true   (conj {:act :hold :dur 0.6}))
              moved (if scroll
                      (let [m (into {} (map (fn [[kk at gone?]] [kk [at gone?]])) (:moves scroll))]
                        (mapv #(if-let [[at gone?] (m (:key %))] (assoc % :at at :gone? gone?) %) items))
                      items)]
          (recur (inc i) bot off' (cond-> (into moved rows) side (conj side))
                 (into acts new-acts) (conj starts (count acts))))))))

(defn step-start
  "When step index `i` starts being written under params `ps` (0 for i < 0)."
  [ps i]
  (let [{:keys [starts]} (plan ps)]
    (if (and (>= i 0) (< i (count starts))) (nth starts i) 0.0)))

(defn board-end
  "When the last chalk of params `ps` is down: everything written, still."
  [ps]
  (:end (plan ps)))

;; ── the construct ─────────────────────────────────────────────────────────────

(defn- chalk-item
  "The hidden mobject of plan item `it`, where it is first written (:at0).
   Margin work is chalked a little larger than its plan font so it reads
   beside the bigger equation lines."
  [{:keys [tex text font color key] at :at0}]
  (if tex
    (-> (s/tex tex :font-size (if (= :side (second key)) (+ font 6) font)
               :color (chalk color) :anchor "start" :write true)
        (s/move-to at))
    (-> (s/text text :font-size 28 :color (chalk :grey)) (s/move-to at))))

(defn- figure-coords [{:keys [x-range y-range]} g]
  (c/coord-system {:x-range (conj (vec x-range) 1) :y-range (conj (vec y-range) 1)
                   :x-length (:fig-w g) :y-length (:fig-h g) :center (:fig-center g)}))

(defn- board!
  "The slate in its wooden frame, a chalk tray and a stub of chalk on it."
  [stage]
  (let [bg   (-> (s/rectangle :width 14.3 :height 8.1 :color wood)
                 (s/fill! slate :opacity 1.0)
                 (s/stroke! :color wood :width 26)
                 (s/move-to [0 0]))
        tray (-> (s/rectangle :width 14.3 :height 0.14 :color wood)
                 (s/fill! wood :opacity 1.0)
                 (s/move-to [0 -3.93]))
        stub (-> (s/rounded-rectangle :width 0.5 :height 0.1 :corner-radius 0.04 :color (chalk :white))
                 (s/fill! (chalk :white) :opacity 1.0)
                 (s/move-to [5.6 -3.9]))]
    (s/play! stage (s/together (mapv #(s/appear % :run-time 0.3) [bg tray stub])))))

(defn- header! [stage {:keys [lesson label geom steps]} dur]
  (let [idx  (inc (.indexOf (mapv :id bl/lessons) (:id lesson)))
        tag  (-> (s/text (str "Problem " idx "  ·  " (:title lesson)) :font-size 22
                         :color (chalk :grey) :anchor "start")
                 (s/move-to [-6.75 3.58]))
        meth (-> (s/text (str "method: " label) :font-size 22 :color (chalk :grey) :anchor "end")
                 (s/move-to [6.75 3.58]))
        prob (-> (s/tex (str "\\textit{Evaluate}\\quad " (chalk-tex (:problem lesson)))
                        :font-size 44 :color (chalk :white) :anchor "start" :write true)
                 (s/move-to [-6.75 3.02]))
        rule (s/line [-6.8 2.55] [6.8 2.55] :color (chalk :dim) :width 2)
        col  (s/line [(:col-x geom) 2.4] [(:col-x geom) -3.75] :color (chalk :grey) :width 1.5)
        side (-> (s/text "side work" :font-size 20 :color (chalk :grey) :anchor "start")
                 (s/move-to [(:side-x geom) 2.25]))]
    (s/play! stage (s/together [(s/appear tag :run-time 0.4) (s/appear meth :run-time 0.4)
                                (s/draw prob :run-time (* 0.75 dur))
                                (s/draw rule :run-time dur) (s/draw col :run-time dur)
                                (s/appear side :run-time dur)]))))

(defn- figure! [stage {:keys [lesson geom]} dur]
  (let [fig (:figure lesson)
        cs  (figure-coords fig geom)
        [cx cy] (:fig-center geom)
        legend (if (:area fig)
                 "\\textcolor{#8CCBF2}{y = \\sin x}"
                 "\\textcolor{#8CCBF2}{f(x)}\\quad \\textcolor{#FFE27A}{F(x)}")
        lab (-> (s/tex legend :font-size 24 :color (chalk :white)) (s/move-to [cx (+ cy 1.42)]))
        a   (o/create (ax/axes cs {:labels? false :color (chalk :grey) :width 2 :tick-size 0.06})
                      {:run-time (* 0.5 dur)})
        f   (o/create (pl/plot cs (:f fig) {:color (chalk :blue) :width 3}) {:run-time dur})]
    (s/play! stage (s/together [(:reveal a) (:reveal f) (s/appear lab :run-time dur)]))))

(defn- act! [stage nodes p {:keys [act dur] :as a}]
  (let [fig (:figure (:lesson p))]
    (case act
      :board  (board! stage)
      :header (header! stage p dur)
      :figure (figure! stage p dur)
      :hold   (s/hold! stage dur)
      :write  (s/play! stage (s/together (vec (for [k (:keys a) :let [n (nodes k)] :when n]
                                                (if (= :num (second k))
                                                  (s/appear n :run-time dur)
                                                  (s/draw n :run-time dur))))))
      :scroll (s/play! stage (s/together
                              (vec (mapcat (fn [[k at gone?]]
                                             (when-let [n (nodes k)]
                                               (cond-> [(s/glide n at :run-time dur)]
                                                 gone? (conj (s/vanish n :run-time dur)))))
                                           (:moves a)))))
      :shade  (let [cs (figure-coords fig (:geom p))
                    [lo hi] (:area fig)]
                (s/play! stage (:reveal (o/create (calc/area cs (:f fig) lo hi
                                                             {:color (chalk :yellow) :fill-opacity 0.3})
                                                  {:run-time dur}))))
      :antiderivative (let [cs (figure-coords fig (:geom p))]
                        (s/play! stage (:reveal (o/create (pl/plot cs (:F fig) {:color (chalk :yellow) :width 3})
                                                          {:run-time dur}))))
      nil)))

(defn blackboard
  "The blackboard construct for params (see the ns doc)."
  [ps]
  (let [p (plan ps)]
    (fn [stage]
      (let [nodes (atom {})]
        (doseq [a (:acts p)]
          (when (= :board (:act a))
            (act! stage {} p a)
            ;; every chalk line exists (hidden) from here, in draw order
            (reset! nodes (into {} (map (fn [it] [(:key it) (chalk-item it)])) (:items p))))
          (when-not (= :board (:act a))
            (act! stage @nodes p a)))))))

;; ── widgets ───────────────────────────────────────────────────────────────────

(defn step-count [ps] (count (:steps (second (lesson-of ps)))))

(defn next-step [ps]
  (-> ps (assoc :auto false) (update :step #(min (step-count ps) (inc (long (or % 0)))))))

(defn prev-step [ps]
  (-> ps (assoc :auto false) (update :step #(max 0 (dec (min (step-count ps) (long (or % 0))))))))

(def controls
  "Problem and method selects, the step buttons, two toggles."
  [{:id :problem :kind :select :label "problem"
    :options (vec (map-indexed (fn [i l] {:value (:id l) :label (str (inc i) ". " (:title l))})
                               bl/lessons))
    :value (:id (first bl/lessons)) :seek 0}
   {:id :path :kind :select :label "method"
    :options [{:value 0 :label "first method"} {:value 1 :label "second method"}]
    :value 0 :seek 0}
   {:id :next :kind :button :label "next step" :on-press next-step
    :seek (fn [ps] (step-start ps (dec (shown-steps ps))))}
   {:id :prev :kind :button :label "previous step" :on-press prev-step :seek board-end}
   {:id :all :kind :button :label "show all"
    :on-press (fn [ps] (assoc ps :auto false :step (step-count ps))) :seek board-end}
   {:id :restart :kind :button :label "restart" :on-press #(assoc % :step 0) :seek 0}
   {:id :explain :kind :toggle :label "explanations" :value true}
   {:id :auto :kind :toggle :label "auto-play" :value true :seek 0}])

(def demos
  {:blackboard {:title "Blackboard (worked integrals)" :construct blackboard :controls controls}})

(defn defaults [] (assoc (controls/defaults controls) :step 0))
