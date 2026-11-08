(ns desargues.calc-viewers
  "BOUNDARY stratum (JVM, notebooks): the ->emmy-viewers target of calculus
   objects (desargues.objects.calc.core). Each method turns a resolved object
   into an emmy-viewers fragment (a quoted Reagent form, Mafs inside) that
   Clerk renders in the browser. The page's JS bundle must carry
   emmy-viewers' SCI namespaces (emmy.viewer.sci/install!); Clerk's CDN
   bundle does not.

   :area-under is a Mafs plot of y = f(x) with the region over [a, b] shaded
   (signed: below the axis shades down), a and b as movable points on the
   x-axis and as range sliders, and a live readout: the numeric area
   (composite Simpson, in the browser) beside the exact F(b) - F(a) when an
   antiderivative F is known (:F of the object or of the opts). Emmy
   compiles f and F to JavaScript, so the readout follows every drag.

     (->emmy-viewers (calc/area-under (fn [x] (e/sin x)) 0 Math/PI)
                     {:F (fn [x] (e/- (e/cos x)))})"
  (:require [clojure.walk :as walk]
            [desargues.objects.calc.core :as calc]
            [emmy.clerk]
            [emmy.expression.compile :as xc]
            [emmy.viewer :as ev]))

;; ── Emmy -> JavaScript ──────────────────────────────────────────────────────

(defn js-fn
  "A form that evaluates, in the browser, to Emmy fn `f` of one number
   compiled to a JS Function (Emmy's :js compile)."
  [f]
  (let [[arg body] (xc/compile-state-fn f false [0] {:mode :js :calling-convention :native :arity 1})]
    (list 'js/Function. arg body)))

;; ── Domain and view, sampled on the JVM ─────────────────────────────────────

(defn- finite-on?
  "f finite and below `cap` in magnitude at 41 samples of [x0 x1]."
  [f x0 x1 cap]
  (every? (fn [i] (let [x (+ x0 (* (- x1 x0) (/ i 40.0)))
                        y (try (double (f x)) (catch Exception _ ##NaN))]
                    (and (Double/isFinite y) (< (Math/abs y) cap))))
          (range 41)))

(defn domain
  "The slider range of [a b]: a quarter of the width beyond each end where f
   stays finite and tame there, else the bound itself (1/(x^2-1) on [1.5 4]
   keeps a = 1.5 away from the pole at 1)."
  [f a b]
  (let [a (double a) b (double b) w (* 0.25 (- b a))
        cap (* 4.0 (reduce max 1.0 (map #(Math/abs (double (f %))) [a b (/ (+ a b) 2)])))]
    [(if (finite-on? f (- a w) a cap) (- a w) a)
     (if (finite-on? f b (+ b w) cap) (+ b w) b)]))

(defn y-range
  "The y window of f over [x0 x1], with the axis and a margin."
  [f x0 x1]
  (let [ys (for [i (range 201)] (double (f (+ x0 (* (- x1 x0) (/ i 200.0))))))
        lo (min 0.0 (reduce min ys)) hi (max 0.0 (reduce max ys))
        m (* 0.12 (max 1e-6 (- hi lo)))]
    [(- lo m) (+ hi m)]))

;; ── The fragment ────────────────────────────────────────────────────────────

(def ^:private area-template
  "The client-side body; ?-symbols are filled in by `area-fragment`."
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
        [mafs.core/Mafs {:height ?height :pan false :zoom false
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
         [:span (str "area \u222b f dx over [a, b] \u2248 " (fmt (simpson a b)) "  (Simpson, n = 400)")]
         (when F [:span (str "F(b) \u2212 F(a) = " (fmt (- (F b) (F a))) "  (exact)")])]])))

(defn area-fragment
  "The interactive area figure: {:f :a :b} required (f an Emmy fn); :F
   (antiderivative, Emmy fn) for the exact readout, :domain [lo hi] for the
   sliders, :height in pixels."
  [{:keys [f F a b height] :or {height 300} :as opts}]
  (let [a (double a) b (double b)
        [lo hi] (or (:domain opts) (domain f a b))
        [y0 y1] (y-range f lo hi)]
    (ev/fragment
     (walk/postwalk-replace
      {'?f (js-fn f) '?F (when F (js-fn F)) '?a a '?b b '?lo lo '?hi hi
       '?y0 y0 '?y1 y1 '?step (/ (- hi lo) 400.0) '?height height}
      area-template))))

;; ── The target ──────────────────────────────────────────────────────────────

(defn emmy-fn
  "An Emmy-compilable (fn [x]) for function slot `k` of `o`: a catalog
   entry's :body (a form in x over emmy.env) when it has one, else the
   slot's :f (a bare Emmy fn)."
  [o k]
  (let [{:keys [body f]} (calc/fn-entry o k)]
    (if body
      (binding [*ns* (the-ns 'emmy.env)] (eval (list 'fn '[x] body)))
      f)))

(defmulti ->emmy-viewers
  "The emmy-viewers fragment of resolved calculus object `o` (opts: :F, an
   antiderivative for exact readouts, and per-kind display options)."
  (fn [o _opts] (:kind o)))

(defmethod ->emmy-viewers :area-under [o opts]
  (area-fragment (merge {:f (emmy-fn o :f) :a (:a o) :b (:b o) :F (:F o)}
                        (select-keys opts [:F :domain :height]))))
