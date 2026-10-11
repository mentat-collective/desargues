(ns desargues.geometry.fit
  "The perspective compute. A figure is a projective object with no canonical
   embedding: displaying it means CHOOSING a representative — a homography
   H in PGL(3) that lands the figure well in the screen frame.

   fit-homography searches a parameterized family (similarity 4-dof, affine
   6-dof, projective 8-dof) for the H minimizing the frame-fit cost of
   H . config, with a degeneracy guard: every figure point must keep w bounded
   away from 0 (no point sent to infinity), and every w must share a sign (no
   point may PASS through infinity on the way in).

   The minimizer evaluates H over the figure's point cloud as double arrays
   through desargues.geometry.kernel/*kernel*; results are returned as Emmy
   matrices.

   Dev/build-time only, like desargues.geometry.disposition: decks consume the
   fitted H (or its effect) as literals."
  (:require [emmy.env :as e]
            [emmy.numerical.multimin.nelder-mead :as nm]
            [desargues.geometry.disposition :as d]
            [desargues.geometry.kernel :as k]
            [desargues.geometry.projective :as g]))

;; =============================================================================
;; Frame cost
;; =============================================================================

(defn frame-cost
  "How badly `points` sit in `frame`: out-of-bounds distance (heavy) +
  margin shortfall (medium) + inverse min-spacing (light). Weights via
  :weights {:oob :margin :spacing}."
  [points & {:keys [frame margin weights]
             :or {frame d/default-frame
                  margin 0.5
                  weights {:oob 100.0 :margin 10.0 :spacing 1.0}}}]
  (let [[[xlo xhi] [ylo yhi]] frame
        ps (vec (if (map? points) (vals points) points))
        oob (reduce + (map (fn [[x y]]
                             (+ (max 0.0 (- xlo x) (- x xhi))
                                (max 0.0 (- ylo y) (- y yhi))))
                           ps))
        short (reduce + (map (fn [[x y]]
                               (+ (max 0.0 (- margin (- x xlo)))
                                  (max 0.0 (- margin (- xhi x)))
                                  (max 0.0 (- margin (- y ylo)))
                                  (max 0.0 (- margin (- yhi y)))))
                             ps))
        inv-spacing (/ 1.0 (+ 1e-6 (d/spacing ps)))]
    (+ (* (:oob weights) oob)
       (* (:margin weights) short)
       (* (:spacing weights) inv-spacing))))

;; =============================================================================
;; Homography families
;; =============================================================================

(def families
  "family -> {:dof n :->H (fn [dof-vector] -> 3x3 Emmy matrix) :->h (the same
  map as a row-major double[9]) :x0 identity-like start}."
  {:similarity {:dof 4 :->H g/similarity-h :->h k/similarity-a :x0 [0.0 1.0 0.0 0.0]}
   :affine {:dof 6 :->H g/affine-h :->h k/affine-a :x0 [1.0 0.0 0.0 1.0 0.0 0.0]}
   :projective {:dof 8 :->H g/projective-h :->h k/projective-a
                :x0 [1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0]}})

;; =============================================================================
;; Fitting
;; =============================================================================

(def ^:private default-weights {:oob 100.0 :margin 10.0 :spacing 1.0})

(defn fit-homography
  "The H in `family` (:similarity | :affine | :projective) minimizing
  frame-cost of H . config + degeneracy penalty. Returns {:H :cost :dof}.
  Deterministic. Costs are evaluated through desargues.geometry.kernel/*kernel*."
  [config & {:keys [family frame margin eps maxiter x0]
             :or {family :similarity frame d/default-frame margin 0.5
                  eps 0.05 maxiter 1600}}]
  (let [{:keys [->H ->h x0] :as fam} (families family)
        _ (when-not fam (throw (ex-info "fit: unknown homography family" {:family family})))
        [xs ys] (k/cloud config)
        cost (fn [dof]
               (k/h-cloud-cost (->h dof) xs ys frame margin default-weights eps))
        {:keys [result value]} (nm/nelder-mead cost (vec x0)
                                               {:maxiter maxiter
                                                :simplex-tolerance 1e-7
                                                :fn-tolerance 1e-9})]
    {:H (->H result) :cost value :dof (vec result) :family family}))

(defn- frobenius-sq
  "Squared Frobenius distance between two row-major double[9] homographies."
  ^double [^doubles A ^doubles B]
  (loop [i 0 acc 0.0]
    (if (< i 9)
      (let [d (- (aget A i) (aget B i))] (recur (inc i) (+ acc (* d d))))
      acc)))

(defn fit-keyframes
  "Fit one H per keyframe configuration, jointly: the total cost adds
  :continuity * ||H_{i+1} - H_i||_F^2 so the display map does not jump
  mid-animation. Returns a vector of {:H :family} fits. Costs are evaluated
  through desargues.geometry.kernel/*kernel*."
  [configs & {:keys [family frame margin eps continuity maxiter]
              :or {family :similarity frame d/default-frame margin 0.5
                   eps 0.05 continuity 20.0 maxiter 4000}}]
  (let [{:keys [dof ->H ->h x0]} (families family)
        clouds (mapv k/cloud configs)
        n (count clouds)
        split (fn [xs] (mapv (fn [i] (subvec xs (* i dof) (* (inc i) dof))) (range n)))
        cost (fn [xs]
               (let [hs (mapv ->h (split (vec xs)))]
                 (+ (reduce + (map (fn [h [cx cy]]
                                     (k/h-cloud-cost h cx cy frame margin default-weights eps))
                                   hs clouds))
                    (* continuity
                       (reduce + (map frobenius-sq hs (rest hs)))))))
        x0* (vec (mapcat identity (repeat n x0)))
        {:keys [result]} (nm/nelder-mead cost x0* {:maxiter maxiter
                                                   :simplex-tolerance 1e-7
                                                   :fn-tolerance 1e-9})]
    (mapv (fn [xs] {:H (->H xs) :dof (vec xs) :family family})
          (split (vec result)))))
