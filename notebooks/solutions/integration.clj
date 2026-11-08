;; # Integration, step by step
;;
;; Worked integrals in the style of a classic problem book: every move is
;; licensed by a rule, phrased by its construction (*Let …*, *By the
;; formula …*, *Substitute back …*), and the answer is only marked correct
;; after Emmy differentiates it back. The figure under each problem is the
;; area the integral measures, swept from left to right.
(ns solutions.integration
  {:nextjournal.clerk/visibility {:code :hide :result :hide}}
  (:require [desargues.steps.examples :as ex]
            [desargues.steps.numeric :as numeric]
            [desargues.steps.phrase :as phrase]
            [desargues.steps.protocols :as p]
            [desargues.steps.render :as render]
            [desargues.steps.verify :as verify]
            [nextjournal.clerk :as clerk]
            [desargues.steps.viewers :as viewers]
            [desargues.steps.present :as present]))

;; ## How a solution is shown
;; Each step's words come from its construction, a multimethod. The
;; renderer below only lays the parts out; the blackboard window takes the
;; same parts.

(defn- step-hiccup [i s]
  [:div {:style {:display "flex" :gap "0.8em" :align-items "baseline" :margin "0.2em 0"}}
   [:span {:style {:color "#888" :min-width "1.6em" :font-family "monospace"}} (str (inc i) ".")]
   [:div {:style {:flex "1" :min-width "0"}} (clerk/md (present/->md (present/present s)))]])

(defn- step-view [i s] (clerk/html (step-hiccup i s)))

(defn- badge [{:keys [ok? symbolic? numeric?]}]
  (clerk/html
   [:span {:style {:padding "0.15em 0.6em" :border-radius "1em" :font-size "0.85em"
                   :background (if ok? "#1f6f43" "#8a1c1c") :color "white"}}
    (cond (and ok? symbolic?) "verified by Emmy: symbolic + numeric"
          ok?                 "verified by Emmy: numeric"
          :else               "NOT verified")]))

;; ### The figure: the area, swept
;; An SVG with an SMIL animation, so it needs no JavaScript. Samples come
;; from the numeric backend (`:jvm` here; `:raster` is one key away).

(defn- area-svg [f a b]
  (let [W 520 H 220 pad 28
        pts  (p/sample (numeric/current) (fn [x] (f x)) a b 160)
        ys   (map second pts)
        lo   (min 0.0 (apply min ys)) hi (max 0.0 (apply max ys))
        sx   (fn [x] (+ pad (* (- W (* 2 pad)) (/ (- x a) (- b a)))))
        sy   (fn [y] (- H pad (* (- H (* 2 pad)) (/ (- y lo) (max 1e-9 (- hi lo))))))
        line (apply str (map (fn [[x y]] (str (sx x) "," (sy y) " ")) pts))
        area (str (sx a) "," (sy 0) " " line (sx b) "," (sy 0))
        value (p/integrate (numeric/current) (fn [x] (f x)) a b)]
    (clerk/html
     [:svg {:width W :height H :style {:background "#10181d" :border-radius "8px"}}
      [:defs [:clipPath {:id (str "sweep" (hash [a b]))}
              [:rect {:x pad :y 0 :width 0 :height H}
               [:animate {:attributeName "width" :from 0 :to (- W (* 2 pad))
                          :dur "3s" :repeatCount "indefinite"}]]]]
      [:line {:x1 pad :y1 (sy 0) :x2 (- W pad) :y2 (sy 0) :stroke "#567" :stroke-width 1}]
      [:polygon {:points area :fill "#4aa3c7" :fill-opacity 0.45
                 :clip-path (str "url(#sweep" (hash [a b]) ")")}]
      [:polyline {:points line :fill "none" :stroke "#f2c14e" :stroke-width 2.5}]
      [:text {:x pad :y 18 :fill "#cfd8dc" :font-size 13}
       (format "area over [%.3f, %.3f] = %.6f" (double a) (double b) value)]])))

(defn- verdict [{:keys [f F points bounds value]}]
  (if bounds
    (let [[a b] bounds] (verify/definite (fn [x] (f x)) a b value))
    (verify/antiderivative f F points)))

(defn- steps-view [derivation]
  (apply clerk/col (map-indexed step-view (:steps derivation))))

(defn- pathway-view [{:keys [title derivation]}]
  (clerk/html
   (into [:details {:style {:margin "0.4em 0" :padding "0.3em 0.8em" :border-left "3px solid #4aa3c7"}}
          [:summary {:style {:cursor "pointer" :color "#4aa3c7"}} title]]
         (concat (map-indexed step-hiccup (:steps derivation))
                 [[:div (clerk/tex (str "\\boxed{" (present/present-form (:answer derivation)) "}"))]]))))

(defn- problem-view [{:keys [title section derivation alternatives figure f] :as example}]
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
            [(area-svg f (:a figure) (:b figure))]))))

;; ## The problems
^{:nextjournal.clerk/visibility {:result :show}}
(apply clerk/col (map problem-view ex/all))
