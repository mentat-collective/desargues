(ns desargues.geometry.disposition
  "The disposition solver: chooses WHERE a construction's free parameters sit
   so the figure lands well in the frame — what the erlangen deck once did by
   hand-scanning parameters.

   A construction is declared as data:

     {:params {:ra {:kind :scalar :scale 10 :lo 1.2 :hi 2.5} ...}
      :derive (fn [params] -> {:A [x y] ...})
      :hard   [(fn [points] -> boolean) ...]
      :soft   (fn [points] -> cost, lower is better)}

   Solving = logic tier (core.logic clp(FD) over discretized domains,
   generate-and-test with the hard constraints as ground predicates, `run 1`
   for the first satisfying disposition) -> numeric tier (Nelder-Mead polish of
   the soft cost, warm-started from the logic solution) -> trajectory check
   (lerp the PARAMS, not the points, and verify the hard constraints at every
   sampled t).

   This namespace is dev/build-time only: decks consume solved parameters as
   literals. Nothing under desargues.geometry.scene or the deck runtime may
   require it."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [emmy.numerical.multimin.nelder-mead :as nm]
            [desargues.geometry.projective :as g]))

;; =============================================================================
;; Constraint vocabulary: predicates over derived point maps
;; =============================================================================

(def default-frame
  "The centered, y-up scene frame: x in [-7.1 7.1], y in [-4 4]."
  [[-7.1 7.1] [-4.0 4.0]])

(defn- points-of [pts] (if (map? pts) (vals pts) pts))

(defn dist [[x1 y1] [x2 y2]] (Math/hypot (- x2 x1) (- y2 y1)))

(defn within-frame?
  "Every point inside the frame (default [[-7.1 7.1] [-4 4]])."
  ([pts] (within-frame? pts default-frame))
  ([pts [[xlo xhi] [ylo yhi]]]
   (every? (fn [[x y]] (and (<= xlo (double x) xhi) (<= ylo (double y) yhi)))
           (points-of pts))))

(defn spacing
  "Minimum pairwise distance between the points."
  [pts]
  (let [ps (vec (points-of pts))]
    (reduce min Double/MAX_VALUE
            (for [i (range (count ps)) j (range (inc i) (count ps))]
              (dist (ps i) (ps j))))))

(defn min-spacing?
  "Every pair of points at least d apart."
  [pts d]
  (>= (spacing pts) (double d)))

(defn non-collinear?
  "a, b, c not on one line (twice the triangle area above eps)."
  ([a b c] (non-collinear? a b c 1e-6))
  ([a b c eps]
   (> (Math/abs (double (- (* (- (nth b 0) (nth a 0)) (- (nth c 1) (nth a 1)))
                           (* (- (nth b 1) (nth a 1)) (- (nth c 0) (nth a 0))))))
      eps)))

(defn between?
  "b strictly between a and c on the segment, within eps of the line and at
   least eps from both endpoints (parameter measure)."
  ([a b c] (between? a b c 1e-6))
  ([a b c eps]
   (let [ab (dist a b) bc (dist b c) ac (dist a c)]
     (and (> ab eps) (> bc eps)
          (< (Math/abs (double (- (+ ab bc) ac))) eps)))))

(defn no-label-overlap?
  "Approximate labels as w x h boxes centered on their points: no two overlap."
  [pts [w h]]
  (let [ps (vec (points-of pts))]
    (every? (fn [[p q]]
              (or (>= (Math/abs (double (- (nth p 0) (nth q 0)))) (double w))
                  (>= (Math/abs (double (- (nth p 1) (nth q 1)))) (double h))))
            (for [i (range (count ps)) j (range (inc i) (count ps))]
              [(ps i) (ps j)]))))

(defn hard-ok?
  "Every hard constraint satisfied by the derived points. A constraint that
   throws (a degenerate derivation: parallel lines, division by zero) counts
   as violated."
  [hard pts]
  (try
    (boolean (every? (fn [h] (boolean (h pts))) hard))
    (catch Exception _ false)))

;; =============================================================================
;; Discretization: param specs -> flat FD leaves
;; =============================================================================

(defn- leaves
  "Flat leaf list [{:path [k] | [k i] :lo :hi :scale}], sorted ascending by
   discretized domain size: the fd enumeration is leftmost-outermost, so small
   domains first means infeasible regions are abandoned after the least wasted
   work. Spec kinds: {:kind :fixed :value v} (no leaf), {:kind :scalar :scale s
   :lo a :hi b}, {:kind :vector :scale s :los [...] :his [...]}."
  [params]
  (vec (sort-by (fn [{:keys [lo hi scale]}] (* (- hi lo) scale))
                (mapcat (fn [[k s]]
                          (case (:kind s)
                            :fixed []
                            :scalar [{:path [k] :scale (double (:scale s))
                                      :lo (double (:lo s)) :hi (double (:hi s))}]
                            :vector (mapv (fn [i lo hi]
                                            {:path [k i] :scale (double (:scale s))
                                             :lo (double lo) :hi (double hi)})
                                          (range) (:los s) (:his s))))
                        params))))

(defn- fixed-base [params]
  (into {} (keep (fn [[k s]] (when (= :fixed (:kind s)) [k (:value s)])) params)))

(defn- assoc-leaf [m [k i] v]
  (if i
    (update m k (fnil assoc []) i v)
    (assoc m k v)))

(defn- unflatten
  "Flat leaf values in real units -> the param map."
  [params ls values]
  (reduce (fn [m [leaf v]]
            (assoc-leaf m (:path leaf) (double v)))
          (fixed-base params)
          (map vector ls values)))

(defn- decode-solution
  "Offset-encoded fd solution ints -> the param map (real = lo + v/scale)."
  [params ls sol]
  (unflatten params ls
             (mapv (fn [{:keys [lo scale]} v] (+ lo (/ (double v) scale)))
                   ls sol)))

(defn- flatten-params [params ls p]
  (mapv (fn [leaf] (double (get-in p (:path leaf)))) ls))

;; =============================================================================
;; Logic tier: clp(FD) generate-and-test over the discretized domains
;; =============================================================================

(defn- ground-goal
  "Non-relational goal (the `project` pattern, built programmatically since
   the leaf count is only known at runtime): walk every var to ground and hand
   the values to f, which answers a goal."
  [vars f]
  (fn [a]
    (let [vals (mapv #(l/walk* a %) vars)]
      ((f vals) a))))

(defn- too-close?
  "Is the candidate within min-dist (Chebyshev, real units) of any solved
   frame, or in the same discretized cell as an excluded failure?"
  [ls candidate solved min-dist excluded]
  (let [half-cell (fn [leaf] (/ 0.5 (:scale leaf)))]
    (boolean
     (or (some (fn [s]
                 (every? true? (map (fn [leaf x y] (< (Math/abs (double (- x y))) min-dist))
                                    ls candidate s)))
               solved)
         (some (fn [e]
                 (every? true? (map (fn [leaf x y]
                                      (<= (Math/abs (double (- x y))) (half-cell leaf)))
                                    ls candidate e)))
               excluded)))))

(defn- logic-solve
  "First discretized disposition whose derived points satisfy every hard
   constraint: fd intervals per leaf, force-ans to enumerate (deterministically,
   leftmost-ascending), ground test of the derived geometry. `run 1`.

   core.logic's clp(FD) domains are non-negative (fd/interval clamps negative
   bounds to 0), so each leaf is OFFSET-encoded: the fd var ranges over
   [0, round((hi - lo) * scale)] and decodes to lo + v/scale."
  [{:keys [params derive hard]} ls {:keys [solved min-param-dist excluded]
                                    :or {solved [] min-param-dist 0.0 excluded []}}]
  (let [vars (vec (repeatedly (count ls) l/lvar))
        goals (concat
               (map (fn [v {:keys [lo hi scale]}]
                      (fd/in v (fd/interval 0 (Math/round (* (- hi lo) scale)))))
                    vars ls)
               [(l/force-ans vars)
                (ground-goal vars
                             (fn [vs]
                               (let [reals (mapv (fn [{:keys [lo scale]} v]
                                                   (+ lo (/ (double v) scale)))
                                                 ls vs)
                                     p (unflatten params ls reals)]
                                 (if (and (not (too-close? ls reals
                                                           solved min-param-dist excluded))
                                          (hard-ok? hard (derive p)))
                                   l/succeed
                                   l/fail))))])]
    (first (l/run 1 [q] (l/== q vars) (l/and* goals)))))

;; =============================================================================
;; Numeric tier: Nelder-Mead polish, warm-started from the logic solution
;; =============================================================================

(defn- objective
  "Soft cost + 1000 per violated hard constraint + 100 per unit out-of-domain
   (Nelder-Mead is unconstrained; the domains are enforced as penalties) + a
   repulsion barrier keeping the polish min-param-dist (Chebyshev, real units)
   away from every already-solved frame — without it every warm start relaxes
   onto the same optimum and the keyframes collapse to one."
  [{:keys [params derive hard soft]} ls {:keys [solved min-param-dist]
                                         :or {solved [] min-param-dist 0.0}}]
  (fn [xs]
    (let [p (unflatten params ls xs)
          oob (reduce + (map (fn [{:keys [lo hi]} x]
                               (double (max 0.0 (- lo x) (- x hi))))
                             ls xs))
          repel (reduce + (map (fn [s]
                                 (let [d (reduce max 0.0 (map (fn [x y]
                                                                (Math/abs (double (- x y))))
                                                              xs s))]
                                   (* 1.0e4 (max 0.0 (- min-param-dist d)))))
                               solved))
          pts (try (derive p) (catch Exception _ nil))]
      (if (nil? pts)
        1.0e12
        (+ (if soft (double (soft pts)) 0.0)
           (* 1000.0 (count (remove (fn [h] (try (boolean (h pts))
                                                 (catch Exception _ false)))
                                    hard)))
           (* 100.0 oob)
           repel)))))

(defn polish
  "Nelder-Mead on the soft cost, warm-started from p0. Returns the polished
   params; throws if the polish left the feasible region or drifted back into
   an already-solved frame's cell."
  [{:keys [derive hard] :as construction} ls p0
   {:keys [maxiter solved min-param-dist] :or {maxiter 800 solved [] min-param-dist 0.0}}]
  (let [x0 (flatten-params (:params construction) ls p0)
        f (objective construction ls {:solved solved :min-param-dist min-param-dist})
        {:keys [result]} (nm/nelder-mead f x0 {:maxiter maxiter
                                               :simplex-tolerance 1e-7
                                               :fn-tolerance 1e-9})
        p (unflatten (:params construction) ls result)
        flat (flatten-params (:params construction) ls p)]
    (when-not (hard-ok? hard (derive p))
      (throw (ex-info "disposition: numeric polish left the feasible region"
                      {:params p0 :polished p})))
    (when (too-close? ls flat solved min-param-dist [])
      (throw (ex-info "disposition: polish relaxed onto an already-solved frame"
                      {:params p0 :polished p :min-param-dist min-param-dist})))
    p))

;; =============================================================================
;; Solving dispositions and keyframes
;; =============================================================================

(defn solve-disposition
  "Logic tier -> numeric tier. Returns {:params :points :cost :min-spacing}.
   :solved and :excluded are flat leaf vectors in real units (see leaves).
   Throws ex-info when the discretized domain holds no feasible disposition."
  [{:keys [derive hard soft] :as construction}
   & {:keys [polish? solved min-param-dist excluded maxiter]
      :or {polish? true solved [] min-param-dist 0.0 excluded [] maxiter 800}}]
  (let [ls (leaves (:params construction))
        sol (logic-solve construction ls {:solved solved
                                          :min-param-dist min-param-dist
                                          :excluded excluded})]
    (when-not sol
      (throw (ex-info "disposition: no disposition in the discretized domain satisfies the hard constraints"
                      {:params (:params construction)
                       :excluded (count excluded) :solved (count solved)})))
    (let [p0 (decode-solution (:params construction) ls sol)
          p (if polish? (polish construction ls p0 {:maxiter maxiter
                                                   :solved solved
                                                   :min-param-dist min-param-dist})
                          p0)
          pts (derive p)]
      {:params p
       :points pts
       :cost (when soft (double (soft pts)))
       :min-spacing (spacing pts)})))

(def lerp-params
  "Interpolate the construction's PARAMETERS, not its points (alias of
  desargues.geometry.projective/lerp-params)."
  g/lerp-params)

(defn valid-trajectory?
  "Sample the param-lerp between consecutive frames (samples interior t's per
   leg, >= 20 recommended) and require every hard constraint at every sample."
  [{:keys [derive hard]} frames samples]
  (every? (fn [[a b]]
            (every? (fn [i]
                      (hard-ok? hard (derive (lerp-params a b (/ (double i) (inc samples))))))
                    (range 1 (inc samples))))
          (partition 2 1 frames)))

(defn solve-keyframes
  "n keyframes: solve each disposition at least min-param-dist (Chebyshev,
   real units, per leaf) from every previously solved one, then require a
   valid trajectory between consecutive frames (and last -> first when
   :closed?). A frame whose trajectory leg fails is re-solved with the failed
   cell excluded; after `retries` failures the whole search throws.

   Returns a vector of n param maps (deterministic given fixed domains)."
  [construction n
   & {:keys [samples min-param-dist retries closed?]
      :or {samples 24 min-param-dist 0.2 retries 6 closed? false}}]
  (loop [frames [] excluded []]
    (if (= n (count frames))
      (let [ring (if closed? (conj frames (first frames)) frames)]
        (if (valid-trajectory? construction ring samples)
          frames
          (if (< (count excluded) retries)
            ;; The closed leg failed: re-solve the last frame, excluding it.
            (recur (pop frames) (conj excluded (flatten-params (:params construction)
                                                               (leaves (:params construction))
                                                               (peek frames))))
            (throw (ex-info "disposition: no valid trajectory after bounded retries"
                            {:frames frames :closed? closed?})))))
      (let [ls (leaves (:params construction))
            flat (fn [p] (flatten-params (:params construction) ls p))
            attempt (loop [excl excluded k 0]
                      (let [{:keys [params] :as sol}
                            (solve-disposition construction
                                               :solved (mapv flat frames)
                                               :min-param-dist min-param-dist
                                               :excluded excl)
                            ok? (or (empty? frames)
                                    (valid-trajectory? construction [(peek frames) params] samples))]
                        (if ok?
                          [params excl]
                          (if (< k retries)
                            (recur (conj excl (flat params))
                                   (inc k))
                            (throw (ex-info "disposition: no keyframe with a valid incoming trajectory after bounded retries"
                                            {:keyframe (count frames) :retries retries}))))))]
        (recur (conj frames (first attempt)) (second attempt))))))

;; =============================================================================
;; The Desargues figure, as a disposition problem
;; =============================================================================

(defn- cross3 [[a b c] [d e f]]
  [(- (* b f) (* c e)) (- (* c d) (* a f)) (- (* a e) (* b d))])

(defn- meet-d
  "Intersection of line p1p2 with line p3p4, plain doubles. The solver's
   generate-and-test derives thousands of candidates per search; the generic
   emmy path (desargues.geometry.projective) is for placement and proofs, this
   one is for speed."
  [p1 p2 p3 p4]
  (let [l (cross3 (conj p1 1.0) (conj p2 1.0))
        m (cross3 (conj p3 1.0) (conj p4 1.0))
        [x y w] (cross3 l m)]
    [(/ x w) (/ y w)]))

(defn desargues-disposition
  "The free parameters of the axis-first Desargues construction (O and the
   axis fixed, matching the deck's composition): P's height, Q's position on
   the axis, the third ray's angle, and how far along the first ray A and A'
   sit. Hard constraints: inside the frame, the ten points at least
   min-spacing apart, both triangles non-degenerate, A between O and A'. Soft
   cost: maximize the minimum spacing."
  [& {:keys [min-spacing frame] :or {min-spacing 0.45 frame default-frame}}]
  {:params (array-map
            :py {:kind :scalar :scale 10 :lo -3.2 :hi -2.2}
            :qx {:kind :scalar :scale 10 :lo -6.5 :hi -2.5}
            :a3 {:kind :scalar :scale 100 :lo -1.35 :hi -0.85}
            :ra {:kind :scalar :scale 10 :lo 1.2 :hi 2.5}
            :ra' {:kind :scalar :scale 10 :lo 3.4 :hi 4.6})
   :derive (fn [{:keys [py qx a3 ra ra']}]
             (let [O [0.0 3.0] P [1.0 py] Q [qx -3.1]
                   ray (fn [ang r] [(* r (Math/cos ang)) (+ 3.0 (* r (Math/sin ang)))])
                   A (ray -2.25 ra) A' (ray -2.25 ra')
                   ob (ray -1.57 1.0) oc (ray a3 1.0)
                   B (meet-d A P O ob) B' (meet-d A' P O ob)
                   C (meet-d B Q O oc) C' (meet-d B' Q O oc)]
               {:O O :A A :B B :C C :A' A' :B' B' :C' C' :P P :Q Q
                :R (meet-d C A C' A')}))
   :hard [#(within-frame? % frame)
          #(min-spacing? % min-spacing)
          #(non-collinear? (:A %) (:B %) (:C %) 1e-3)
          #(non-collinear? (:A' %) (:B' %) (:C' %) 1e-3)
          #(between? (:O %) (:A %) (:A' %))]
   :soft (fn [pts] (- (double (spacing pts))))})

(defn ->desargues-frame
  "Slim solved params -> the deck's axis-first frame shape {:O :P :Q :angs :ra
  :ra'}."
  [{:keys [py qx a3 ra ra']}]
  {:O [0.0 3.0] :P [1.0 (double py)] :Q [(double qx) -3.1]
   :angs [-2.25 -1.57 (double a3)] :ra (double ra) :ra' (double ra')})
