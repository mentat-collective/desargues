(ns desargues.objects.calculus
  "PURE stratum: calculus objects driven by a parameter, so a slider moves
   them: a point on a curve, its tangent line, a secant, Riemann rectangles
   and the shaded area under a curve.

   Derivatives are numeric (central difference) unless the spec carries :df,
   so any (fn [x]) works, an Emmy-derived derivative included."
  (:require [desargues.objects.coords :as c]
            [desargues.objects.parts :as parts]
            [desargues.objects.sample :as sample]))

(defn derivative
  "Central-difference derivative of f at x, or (df x) when df is given."
  ([f x] (derivative f nil x))
  ([f df x]
   (if df
     (double (df x))
     (let [h (* 1e-5 (max 1.0 (Math/abs (double x))))]
       (/ (- (double (f (+ x h))) (double (f (- x h)))) (* 2.0 h))))))

;; ── point on curve ────────────────────────────────────────────────────────

(defn point-on-curve
  "A dot on y = f(x) at x = `x`, optionally labelled (TeX :label) and with
   dashed-style drop lines to the axes (:drops? true)."
  ([cs f x] (point-on-curve cs f x {}))
  ([cs f x opts]
   (merge {:object :point-on-curve :coords cs :f f :x x :color :yellow :radius 0.08
           :drops? false}
          opts)))

(defmethod parts/parts :point-on-curve [{:keys [coords f x color radius drops? label]}]
  (let [y (double (f x))
        p (c/->scene coords [x y])
        [ox oy] (c/origin coords)]
    (cond-> []
      drops? (into [(parts/line :drop-x p [(first p) oy] {:color color :width 2})
                    (parts/line :drop-y p [ox (second p)] {:color color :width 2})])
      true   (conj (parts/dot :point p {:color color :radius radius}))
      label  (conj (parts/tex :label label (parts/v+ p [0.3 0.3]) {:color color :font-size 30})))))

;; ── tangent and secant ────────────────────────────────────────────────────

(defn- clipped-line
  "Scene segment of the math line through [x0 y0] with `slope`, spanning
   `half` math units either side of x0 in x."
  [cs [x0 y0] slope half]
  (let [xa (- x0 half) xb (+ x0 half)]
    [(c/->scene cs [xa (+ y0 (* slope (- xa x0)))])
     (c/->scene cs [xb (+ y0 (* slope (- xb x0)))])]))

(defn tangent
  "Tangent line to y = f(x) at x. opts: :length (math units along x), :df,
   :color :width, :point? (also draw the touching dot)."
  ([cs f x] (tangent cs f x {}))
  ([cs f x opts]
   (merge {:object :tangent :coords cs :f f :x x :length 3.0 :color :red :width 3
           :point? true}
          opts)))

(defn tangent-slope [{:keys [f df x]}] (derivative f df x))

(defmethod parts/parts :tangent [{:keys [coords f x length color width point?] :as spec}]
  (let [y (double (f x))
        [a b] (clipped-line coords [x y] (tangent-slope spec) (/ length 2.0))]
    (cond-> [(parts/line :tangent a b {:color color :width width})]
      point? (conj (parts/dot :point (c/->scene coords [x y]) {:color color})))))

(defn secant
  "Secant through (x, f x) and (x+h, f(x+h)), extended by :extend math units
   beyond both points. As h -> 0 it becomes the tangent."
  ([cs f x h] (secant cs f x h {}))
  ([cs f x h opts]
   (merge {:object :secant :coords cs :f f :x x :h h :extend 1.0 :color :green :width 3}
          opts)))

(defn secant-slope [{:keys [f x h]}]
  (/ (- (double (f (+ x h))) (double (f x))) (double h)))

(defmethod parts/parts :secant [{:keys [coords f x h extend color width] :as spec}]
  (let [m  (secant-slope spec)
        y  (double (f x))
        x1 (+ x h)
        lo (- (min x x1) extend) hi (+ (max x x1) extend)
        at (fn [u] (c/->scene coords [u (+ y (* m (- u x)))]))]
    [(parts/line :secant (at lo) (at hi) {:color color :width width})
     (parts/dot :p0 (c/->scene coords [x y]) {:color color})
     (parts/dot :p1 (c/->scene coords [x1 (f x1)]) {:color color})]))

;; ── Riemann sums and area ────────────────────────────────────────────────

(def riemann-rules
  "Sample-point rule -> fn of [left right] giving the x where f is sampled.
   An open map: assoc a new rule to extend."
  {:left     (fn [l _] l)
   :right    (fn [_ r] r)
   :midpoint (fn [l r] (/ (+ l r) 2.0))})

(defn riemann
  "n Riemann rectangles of f over [a b] with sample rule :left :right or
   :midpoint. opts: :color :fill-opacity :stroke."
  ([cs f a b n] (riemann cs f a b n {}))
  ([cs f a b n opts]
   (merge {:object :riemann :coords cs :f f :domain [a b] :n n :rule :left
           :color :blue :fill-opacity 0.5 :stroke :white}
          opts)))

(defn rectangles
  "[{:left :right :height}] in math units for a riemann spec."
  [{:keys [f domain n rule]}]
  (let [[a b] domain n (max 1 (long n)) dx (/ (- (double b) (double a)) n)
        pick (get riemann-rules rule (:left riemann-rules))]
    (for [i (range n)]
      (let [l (+ a (* i dx)) r (+ l dx)]
        {:left l :right r :height (double (f (pick l r)))}))))

(defn riemann-sum
  "Signed area of the rectangles of a riemann spec."
  [spec]
  (reduce + 0.0 (map (fn [{:keys [left right height]}] (* (- right left) height))
                     (rectangles spec))))

(defmethod parts/parts :riemann [{:keys [coords color fill-opacity stroke] :as spec}]
  (vec (map-indexed
        (fn [i {:keys [left right height]}]
          (let [[sx0 sy0] (c/->scene coords [left 0])
                [sx1 sy1] (c/->scene coords [right height])]
            (parts/rectangle [:rect i] [(/ (+ sx0 sx1) 2.0) (/ (+ sy0 sy1) 2.0)]
                             (Math/abs (- sx1 sx0)) (Math/abs (- sy1 sy0))
                             {:color stroke :width 1 :fill color :fill-opacity fill-opacity})))
        (rectangles spec))))

(defn area
  "The region between y = f(x) and the x-axis over [a b], one closed filled
   path. opts: :color :fill-opacity, sampler opts."
  ([cs f a b] (area cs f a b {}))
  ([cs f a b opts]
   (merge {:object :area :coords cs :f f :domain [a b] :color :teal :fill-opacity 0.4}
          opts)))

(defmethod parts/parts :area [{:keys [coords f domain color fill-opacity] :as spec}]
  (let [[a b] domain
        curve (first (sample/sample-curve (sample/graph-fn #(c/->scene coords %) f) a b
                                          (select-keys spec [:seed :tol :max-depth])))]
    (if (seq curve)
      [(parts/path :region (concat [(c/->scene coords [a 0])] curve [(c/->scene coords [b 0])])
                   true {:color color :width 1 :fill color :fill-opacity fill-opacity})]
      [])))
