;; # Integration, step by step
;;
;; Worked integrals in the style of a classic problem book: every move is
;; licensed by a rule, phrased by its construction (*Let …*, *By the
;; formula …*, *Substitute back …*), and the answer is only marked correct
;; after Emmy differentiates it back. The figure under each problem is the
;; area the integral measures: drag a or b and the shading and the value
;; follow.
(ns solutions.integration
  {:nextjournal.clerk/visibility {:code :hide :result :hide}}
  (:require [clojure.walk :as walk]
            [desargues.steps.examples :as ex]
            [desargues.steps.present :as present]
            [desargues.steps.verify :as verify]
            [desargues.steps.viewers :as viewers]
            [emmy.clerk :as ec]
            [emmy.expression.compile :as xc]
            [emmy.viewer :as ev]
            [nextjournal.clerk :as clerk]))

;; Mafs and the other emmy-viewers widgets render through these viewers;
;; the page needs the emmy-viewers JS bundle (deps.edn, :notebooks).
(ec/install!)

;; ## How a solution is shown
;; Each step's words come from its construction, a multimethod. The
;; renderer below only lays the parts out; the blackboard window takes the
;; same parts.

(defn- step-hiccup [i s]
  [:div {:style {:display "flex" :gap "0.8em" :align-items "baseline" :margin "0.2em 0"}}
   [:span {:style {:color "#888" :min-width "1.6em" :font-family "monospace"}} (str (inc i) ".")]
   [:div {:style {:flex "1" :min-width "0"}} (clerk/md (present/->md (present/present s)))]])

(defn- badge [{:keys [ok? symbolic?]}]
  (clerk/html
   [:span {:style {:padding "0.15em 0.6em" :border-radius "1em" :font-size "0.85em"
                   :background (if ok? "#1f6f43" "#8a1c1c") :color "white"}}
    (cond (and ok? symbolic?) "verified by Emmy: symbolic + numeric"
          ok?                 "verified by Emmy: numeric"
          :else               "NOT verified")]))

;; ### The figure: the area over [a, b]
;; A Mafs plot (emmy-viewers): the curve, the region over [a, b] shaded
;; (below the axis it shades down, the integral's sign), a and b as points
;; to drag along the axis and as sliders, and the value: Simpson's rule
;; beside F(b) − F(a) from the derived antiderivative. Emmy compiles f and F
;; to JavaScript, so the browser recomputes both on every move. The same
;; figure is desargues' ->emmy-viewers rendering of an :area-under object
;; (notebooks/desargues/calc_viewers.clj on the calculus-objects branch).

(defn- js-fn
  "A form evaluating, in the browser, to Emmy fn `f` compiled to JS."
  [f]
  (let [[arg body] (xc/compile-state-fn f false [0] {:mode :js :calling-convention :native :arity 1})]
    (list 'js/Function. arg body)))

(defn- finite-on? [f x0 x1 cap]
  (every? (fn [i] (let [x (+ x0 (* (- x1 x0) (/ i 40.0)))
                        y (try (double (f x)) (catch Exception _ ##NaN))]
                    (and (Double/isFinite y) (< (Math/abs y) cap))))
          (range 41)))

(defn- domain
  "Slider range: a quarter of the width beyond each end where f stays tame
   (1/(x²−1) on [1.5, 4] keeps a = 1.5 clear of the pole at 1)."
  [f a b]
  (let [w (* 0.25 (- b a))
        cap (* 4.0 (reduce max 1.0 (map #(Math/abs (double (f %))) [a b (/ (+ a b) 2)])))]
    [(if (finite-on? f (- a w) a cap) (- a w) a)
     (if (finite-on? f b (+ b w) cap) (+ b w) b)]))

(defn- y-range [f x0 x1]
  (let [ys (for [i (range 201)] (double (f (+ x0 (* (- x1 x0) (/ i 200.0))))))
        lo (min 0.0 (reduce min ys)) hi (max 0.0 (reduce max ys))
        m (* 0.12 (max 1e-6 (- hi lo)))]
    [(- lo m) (+ hi m)]))

(def ^:private area-template
  '(reagent.core/with-let
     [f ?f
      F ?F
      !s (reagent.core/atom {:a [?a 0] :b [?b 0]})
      fmt (fn [v] (.toFixed (js/Number v) 6))
      clamp (fn [x] (max ?lo (min ?hi x)))
      simpson (fn [a b]
                (let [n 400 h (/ (- b a) n)]
                  (* (/ h 3)
                     (reduce + (map (fn [i]
                                      (* (cond (or (zero? i) (= i n)) 1 (odd? i) 4 :else 2)
                                         (f (+ a (* i h)))))
                                    (range (inc n)))))))
      slider (fn [k label color]
               (let [x (first (get @!s k))]
                 [:label {:style {:display "flex" :align-items "center" :gap "0.6em"
                                  :font-family "monospace" :font-size "0.9em"}}
                  [:span {:style {:color color :min-width "1.2em"}} label]
                  [:input {:type "range" :min ?lo :max ?hi :step ?step :value x
                           :style {:flex "1"}
                           :on-change (fn [e] (swap! !s assoc k [(js/parseFloat (.. e -target -value)) 0]))}]
                  [:span {:style {:min-width "5em" :text-align "right"}} (.toFixed x 3)]]))]
     (let [a (clamp (first (:a @!s)))
           b (clamp (first (:b @!s)))
           l (min a b) r (max a b)
           in? (fn [x] (and (>= x l) (<= x r)))]
       [:div {:style {:margin "0.6em 0"}}
        [mafs.core/Mafs {:height 300 :pan false :zoom false
                         :preserve-aspect-ratio false
                         :view-box {:x [?lo ?hi] :y [?y0 ?y1] :padding 12}}
         [mafs.coordinates/Cartesian {:subdivisions 2}]
         [mafs.plot/Inequality
          {:y {:<= (fn [x] (if (in? x) (max 0 (f x)) 0))
               :>= (fn [x] (if (in? x) (min 0 (f x)) 0))}
           :color :blue :fill-opacity 0.3 :upper-opacity 0 :lower-opacity 0}]
         [mafs.plot/OfX {:y f :color :yellow :weight 2.5}]
         [mafs.line/Segment {:point1 [a 0] :point2 [a (f a)] :color :pink :style "dashed" :opacity 0.6}]
         [mafs.line/Segment {:point1 [b 0] :point2 [b (f b)] :color :green :style "dashed" :opacity 0.6}]
         [mafs.core/MovablePoint {:atom !s :path :a :constrain "horizontal" :color :pink}]
         [mafs.core/MovablePoint {:atom !s :path :b :constrain "horizontal" :color :green}]]
        [slider :a "a" "#f472b6"]
        [slider :b "b" "#4ade80"]
        [:div {:style {:font-family "monospace" :margin-top "0.4em" :display "flex"
                       :gap "2em" :flex-wrap "wrap"}}
         [:span (str "area over [a, b] \u2248 " (fmt (simpson a b)) "  (Simpson, n = 400)")]
         (when F [:span (str "F(b) \u2212 F(a) = " (fmt (- (F b) (F a))) "  (exact)")])]])))

(defn- area-figure
  "The interactive area of example `f` (Emmy fn) over [a, b], with the exact
   readout from antiderivative `F` when given."
  [f F a b]
  (let [a (double a) b (double b)
        [lo hi] (domain f a b)
        [y0 y1] (y-range f lo hi)]
    (ev/fragment
     (walk/postwalk-replace
      {'?f (js-fn f) '?F (when F (js-fn F)) '?a a '?b b '?lo lo '?hi hi
       '?y0 y0 '?y1 y1 '?step (/ (- hi lo) 400.0)}
      area-template))))

(defn- verdict [{:keys [f F points bounds value]}]
  (if bounds
    (let [[a b] bounds] (verify/definite (fn [x] (f x)) a b value))
    (verify/antiderivative f F points)))

(defn- pathway-view [{:keys [title derivation]}]
  (clerk/html
   (into [:details {:style {:margin "0.4em 0" :padding "0.3em 0.8em" :border-left "3px solid #4aa3c7"}}
          [:summary {:style {:cursor "pointer" :color "#4aa3c7"}} title]]
         (concat (map-indexed step-hiccup (:steps derivation))
                 [[:div (clerk/tex (str "\\boxed{" (present/present-form (:answer derivation)) "}"))]]))))

(defn- problem-view [{:keys [title section derivation alternatives figure f F] :as example}]
  (let [v (verdict example)]
    (apply clerk/col
           (concat
            [(clerk/md (str "## " title "\n*" section "*"))
             (clerk/tex (present/present-form (-> derivation :steps first :from)))
             (viewers/show-steps derivation)
             (badge v)]
            (when (seq alternatives)
              (cons (clerk/md (str "**Other pathways** (" (count alternatives) "), each reaching the same answer:"))
                    (map pathway-view alternatives)))
            [(area-figure f F (:a figure) (:b figure))]))))

;; ## The problems
^{:nextjournal.clerk/visibility {:result :show}}
(apply clerk/col (map problem-view ex/all))
