(ns desargues.geometry.fit
  "The perspective compute. A figure is a projective object with no canonical
   embedding: displaying it means CHOOSING a representative — a homography
   H in PGL(3) that lands the figure well in the screen frame.

   fit-homography searches a parameterized family (similarity 4-dof, affine
   6-dof, projective 8-dof) for the H minimizing the frame-fit cost of
   H . config, with a degeneracy guard: every figure point must keep w bounded
   away from 0 (no point sent to infinity), and every w must share a sign (no
   point may PASS through infinity on the way in).

   Dev/build-time only, like desargues.geometry.disposition: decks consume the
   fitted H (or its effect) as literals."
  (:require [emmy.env :as e]
            [emmy.numerical.multimin.nelder-mead :as nm]
            [desargues.geometry.disposition :as d]
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
  "family -> {:dof n :->H (fn [dof-vector] -> 3x3) :x0 identity-like start}."
  {:similarity {:dof 4 :->H g/similarity-h :x0 [0.0 1.0 0.0 0.0]}
   :affine {:dof 6 :->H g/affine-h :x0 [1.0 0.0 0.0 1.0 0.0 0.0]}
   :projective {:dof 8 :->H g/projective-h :x0 [1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0]}})

;; =============================================================================
;; Fitting
;; =============================================================================

(defn- min-abs-w
  "Smallest |w| over the figure's homogeneous images, and whether every w
   shares a sign."
  [H ps]
  (let [ws (mapv (fn [p] (double (get (g/apply-h* H p) 2))) ps)]
    [(reduce min (map #(Math/abs %) ws))
     (or (every? pos? ws) (every? neg? ws))]))

(defn- degeneracy-cost
  "Big unless every transformed point keeps |w| >= eps and all w share a sign
   (no figure point sent to, or through, infinity)."
  [H ps eps]
  (let [[m same-sign?] (min-abs-w H ps)]
    (+ (if same-sign? 0.0 1.0e6)
       (if (< m eps) (* 1.0e6 (- eps m)) 0.0))))

(defn fit-homography
  "The H in `family` (:similarity | :affine | :projective) minimizing
  frame-cost of H . config + degeneracy penalty. Returns {:H :cost :dof}.
  Deterministic."
  [config & {:keys [family frame margin eps maxiter x0]
             :or {family :similarity frame d/default-frame margin 0.5
                  eps 0.05 maxiter 1600}}]
  (let [{:keys [->H x0] :as fam} (families family)
        _ (when-not fam (throw (ex-info "fit: unknown homography family" {:family family})))
        ps (vec (if (map? config) (vals config) config))
        cost (fn [xs]
               (let [H (->H xs)]
                 (+ (frame-cost (mapv #(g/apply-h H %) ps)
                                :frame frame :margin margin)
                    (degeneracy-cost H ps eps))))
        {:keys [result value]} (nm/nelder-mead cost (vec x0)
                                               {:maxiter maxiter
                                                :simplex-tolerance 1e-7
                                                :fn-tolerance 1e-9})]
    {:H (->H result) :cost value :dof (vec result) :family family}))

(defn- frobenius-sq
  "Squared Frobenius distance between two 3x3 Emmy matrices."
  [A B]
  (reduce + (map (fn [ra rb]
                   (reduce + (map (fn [a b] (let [d (- (double a) (double b))] (* d d)))
                                  ra rb)))
                 A B)))

(defn fit-keyframes
  "Fit one H per keyframe configuration, jointly: the total cost adds
  :continuity * ||H_{i+1} - H_i||_F^2 so the display map does not jump
  mid-animation. Returns a vector of {:H :family} fits."
  [configs & {:keys [family frame margin eps continuity maxiter]
              :or {family :similarity frame d/default-frame margin 0.5
                   eps 0.05 continuity 20.0 maxiter 4000}}]
  (let [{:keys [dof ->H x0]} (families family)
        pss (mapv (fn [c] (vec (if (map? c) (vals c) c))) configs)
        n (count pss)
        split (fn [xs] (mapv (fn [i] (subvec xs (* i dof) (* (inc i) dof))) (range n)))
        cost (fn [xs]
               (let [Hs (mapv ->H (split (vec xs)))]
                 (+ (reduce + (map (fn [H ps]
                                     (+ (frame-cost (mapv #(g/apply-h H %) ps)
                                                    :frame frame :margin margin)
                                        (degeneracy-cost H ps eps)))
                                   Hs pss))
                    (* continuity
                       (reduce + (map frobenius-sq Hs (rest Hs)))))))
        x0* (vec (mapcat identity (repeat n x0)))
        {:keys [result]} (nm/nelder-mead cost x0* {:maxiter maxiter
                                                   :simplex-tolerance 1e-7
                                                   :fn-tolerance 1e-9})]
    (mapv (fn [xs] {:H (->H xs) :dof (vec xs) :family family})
          (split (vec result)))))
