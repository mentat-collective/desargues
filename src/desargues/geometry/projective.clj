(ns desargues.geometry.projective
  "The projective plane, written once in Emmy's generic arithmetic: the same
   functions place vertices in an animation (on numbers) and prove, on symbols,
   that what the animation shows is true in general.

   Points in the affine chart are [x y]; in homogeneous form they are Emmy
   up-tuples (up x y w). Lines are homogeneous triples (up a b c) meaning
   ax + by + cw = 0. Point/line duality: the line through two points and the
   intersection of two lines are the SAME operation, the cross product.

   This namespace is cljw-friendly: no Java interop beyond Math/* statics, no
   solver dependencies. Solving (core.logic, Nelder-Mead) lives in
   desargues.geometry.disposition / desargues.geometry.fit and never enters the
   deck runtime."
  (:require [emmy.env :as e]))

;; =============================================================================
;; Homogeneous coordinates
;; =============================================================================

(defn homog
  "Affine point [x y] -> homogeneous (up x y 1). Already-homogeneous input
   (a triple) passes through."
  [p]
  (if (= 2 (count p))
    (e/up (nth p 0) (nth p 1) 1)
    p))

(defn dehomog
  "Homogeneous point -> the affine chart w = 1: [x/w y/w]."
  [u]
  (let [w (get u 2)]
    [(e// (get u 0) w) (e// (get u 1) w)]))

(defn- dot* [a b] (reduce e/+ (map e/* a b)))

(defn point-at-infinity?
  "A homogeneous point with w = 0 lies on the line at infinity."
  [u]
  (let [w (e/simplify (get u 2))]
    (if (number? w) (zero? w) (e/zero? w))))

;; =============================================================================
;; Incidence: join and meet are dual (both are the cross product)
;; =============================================================================

(defn join
  "The line through points p and q, as a homogeneous triple."
  [p q]
  (e/cross-product (homog p) (homog q)))

(defn meet
  "The point where lines l and m meet, as a homogeneous triple."
  [l m]
  (e/cross-product l m))

(defn meet-lines
  "Intersection of line p1p2 with line p3p4, in the affine chart."
  [p1 p2 p3 p4]
  (dehomog (meet (join p1 p2) (join p3 p4))))

(defn on?
  "Incidence: does point p lie on line l? Symbolic answers via e/simplify;
   numeric answers within eps."
  ([p l] (on? p l 1e-9))
  ([p l eps]
   (let [d (e/simplify (dot* (homog p) l))]
     (if (number? d)
       (< (Math/abs (double d)) eps)
       (e/zero? d)))))

;; =============================================================================
;; Determinant tests, generic over numbers and symbols
;; =============================================================================

(defn- zeroish?
  "Zero test that works on Emmy expressions (after simplify) and on numbers
   (within eps)."
  ([x] (zeroish? x 1e-9))
  ([x eps]
   (let [v (e/simplify x)]
     (if (number? v)
       (< (Math/abs (double v)) eps)
       (e/zero? v)))))

(defn- det3 [rows] (e/determinant (apply e/matrix-by-rows (map seq rows))))

(defn collinear?
  "p, q, r collinear <=> det of their homogeneous coordinates vanishes."
  ([p q r] (collinear? p q r 1e-9))
  ([p q r eps]
   (zeroish? (det3 [(homog p) (homog q) (homog r)]) eps)))

(defn concurrent?
  "Lines l, m, n concurrent <=> det of their coefficients vanishes (the dual
   of collinear?)."
  ([l m n] (concurrent? l m n 1e-9))
  ([l m n eps]
   (zeroish? (det3 [l m n]) eps)))

;; =============================================================================
;; Cross ratio — the projective invariant
;; =============================================================================

(defn cross-ratio
  "(A,B;C,D) = (AC·BD)/(BC·AD) for four collinear affine points, read along
   the line through A and B. The parameter t(X) = (X - A)·(B - A) is not
   normalized: a uniform rescale cancels between numerator and denominator, so
   no square roots are needed and the function stays generic over symbols."
  [A B C D]
  (let [u (mapv e/- B A)
        t (fn [X] (dot* (mapv e/- X A) u))]
    (e// (e/* (e/- (t C) (t A)) (e/- (t D) (t B)))
         (e/* (e/- (t C) (t B)) (e/- (t D) (t A))))))

;; =============================================================================
;; Homographies: elements of PGL(3) as 3x3 Emmy matrices
;; =============================================================================

(def I3 (e/matrix-by-rows [1 0 0] [0 1 0] [0 0 1]))

(defn apply-h*
  "H · u on homogeneous coordinates, without returning to the affine chart.
   Use this to inspect w (a display map must keep every figure point away from
   w = 0)."
  [H u]
  (e/* H (homog u)))

(defn apply-h
  "Apply homography H (a 3x3 Emmy matrix) to affine point [x y]."
  [H p]
  (dehomog (apply-h* H p)))

(defn h-lerp
  "(1-t)A + tB. Between the identity and H it is a path of homographies (as
   long as no point of the figure is sent to infinity on the way), which is how
   scenes animate one: sample H(t), move every vertex to its image."
  [A B t]
  (e/+ (e/* (e/- 1 t) A) (e/* t B)))

(defn rot-h
  "Rotation about the origin, as a homography."
  [theta]
  (e/matrix-by-rows [(e/cos theta) (e/- (e/sin theta)) 0]
                    [(e/sin theta) (e/cos theta) 0]
                    [0 0 1]))

(defn translate-h
  "Translation by [dx dy], as a homography."
  [[dx dy]]
  (e/matrix-by-rows [1 0 dx] [0 1 dy] [0 0 1]))

(defn scale-h
  "Uniform scaling by s (or anisotropic by [sx sy]), as a homography."
  [s]
  (if (sequential? s)
    (e/matrix-by-rows [(nth s 0) 0 0] [0 (nth s 1) 0] [0 0 1])
    (e/matrix-by-rows [s 0 0] [0 s 0] [0 0 1])))

;; Parameterized families, as flat dof vectors (what a minimizer searches over)

(defn similarity-h
  "4-dof: [theta s tx ty] -> rotate by theta, scale by s, translate by (tx,ty)."
  [[theta s tx ty]]
  (e/matrix-by-rows [(e/* s (e/cos theta)) (e/- (e/* s (e/sin theta))) tx]
                    [(e/* s (e/sin theta)) (e/* s (e/cos theta)) ty]
                    [0 0 1]))

(defn affine-h
  "6-dof: [a b c d tx ty] -> rows [a b tx] [c d ty] [0 0 1]."
  [[a b c d tx ty]]
  (e/matrix-by-rows [a b tx] [c d ty] [0 0 1]))

(defn projective-h
  "8-dof: [a b c d e f g h] -> rows [a b c] [d e f] [g h 1] (fixing the scale
   of PGL(3) by h33 = 1; the fit guards against configurations needing
   h33 = 0 via its degeneracy penalty)."
  [[a b c d e f g h]]
  (e/matrix-by-rows [a b c] [d e f] [g h 1]))

;; =============================================================================
;; Conics: a symmetric 3x3 matrix, points u with uᵀCu = 0
;; =============================================================================

(defn conic
  "The conic a x² + b xy + c y² + d xw + e yw + f w² = 0, as a symmetric 3x3
   matrix."
  [[a b c d e f]]
  (e/matrix-by-rows [a (e// b 2) (e// d 2)]
                    [(e// b 2) c (e// e 2)]
                    [(e// d 2) (e// e 2) f]))

(defn on-conic?
  "Does point p lie on conic C? (uᵀCu = 0, generic zero test.)"
  ([C p] (on-conic? C p 1e-9))
  ([C p eps]
   (let [u (homog p)]
     (zeroish? (dot* u (e/* C u)) eps))))

(defn polar
  "The polar line of point p with respect to conic C: C·p. When p is on C,
   this is the tangent at p."
  [C p]
  (e/* C (homog p)))

(defn pole
  "The pole of line l with respect to conic C: C⁻¹·l."
  [C l]
  (e/* (e/invert C) l))

(defn tangent
  "The tangent to conic C at a point p on it (its polar)."
  [C p]
  (polar C p))

;; =============================================================================
;; Parameter interpolation (animate the construction, not its points)
;; =============================================================================

(defn lerp-params
  "Interpolate a construction's PARAMETERS, not its points: every in-between
   frame is then a true instance of the construction. Numbers lerp, vectors
   elementwise, maps keywise, equal constants pass through."
  [a b t]
  (cond
    (and (number? a) (number? b)) (+ a (* t (- b a)))
    (and (vector? a) (vector? b)) (mapv #(lerp-params %1 %2 t) a b)
    (and (map? a) (map? b)) (into {} (map (fn [[k v]] [k (lerp-params v (b k) t)]) a))
    (= a b) a
    :else (throw (ex-info "cannot lerp unequal constants" {:a a :b b}))))

;; =============================================================================
;; Named configurations, as data + derivations
;; =============================================================================

(defn desargues-configuration
  "Every point of the Desargues figure, built backwards from the axis, the way
   a logic program runs the relation in reverse: A and A' on the first ray
   from O; B = AP ∩ ray₂ and B' = A'P ∩ ray₂; C = BQ ∩ ray₃ and
   C' = B'Q ∩ ray₃. R is then CA ∩ C'A', and it lands on PQ because the
   theorem is true.

   params {:O [x y] :P [x y] :Q [x y] :angs [a b c] :ra r :ra' r'} — generic:
   numbers place the figure, symbols feed the proof. Returns {:points {...}
   :lines {...}} with the derived lines as homogeneous triples so scenes never
   recompute them."
  [{:keys [O P Q angs ra ra']}]
  (let [ray (fn [ang r] [(e/+ (nth O 0) (e/* r (e/cos ang)))
                         (e/+ (nth O 1) (e/* r (e/sin ang)))])
        [aa ab ac] angs
        A (ray aa ra) A' (ray aa ra')
        ob (ray ab 1) oc (ray ac 1)
        B (meet-lines A P O ob) B' (meet-lines A' P O ob)
        C (meet-lines B Q O oc) C' (meet-lines B' Q O oc)]
    {:points {:O O :A A :B B :C C :A' A' :B' B' :C' C' :P P :Q Q
              :R (meet-lines C A C' A')}
     :lines  {:ra (join O A') :rb (join O B') :rc (join O C')
              :ab (join A B) :a'b' (join A' B')
              :bc (join B C) :b'c' (join B' C')
              :ca (join C A) :c'a' (join C' A')
              :axis (join P Q)}}))

(defn pappus-configuration
  "Pappus' hexagon theorem: A, B, C on one line through the origin (direction
   u), A', B', C' on another (direction v) — WLOG, since the theorem is
   projective. P = AB'∩A'B, Q = AC'∩A'C, R = BC'∩B'C are collinear.

   params {:u [ux uy] :v [vx vy] :as [a b c] :bs [a' b' c']}; the scalars place
   the points along their lines. Returns {:points {...} :lines {...}}."
  [{:keys [u v as bs]}]
  (let [scale (fn [k [x y]] [(e/* k x) (e/* k y)])
        [a b c] as [a' b' c'] bs
        A (scale a u) B (scale b u) C (scale c u)
        A' (scale a' v) B' (scale b' v) C' (scale c' v)
        P (meet-lines A B' A' B)
        Q (meet-lines A C' A' C)
        R (meet-lines B C' B' C)]
    {:points {:A A :B B :C C :A' A' :B' B' :C' C' :P P :Q Q :R R}
     :lines  {:l1 (join A B) :l2 (join A' B') :pappus (join P Q)}}))

;; =============================================================================
;; Proofs, computed at load/build time: what each identity simplifies to.
;; Emmy's answers, not ours.
;; =============================================================================

(def proofs
  "Desargues: two triangles in perspective from a point have their three
   axis points collinear. Pappus: the three cross-intersections of a hexagon
   inscribed in two lines are collinear. Both are the vanishing of a
   determinant; both are checked here on symbols."
  (let [;; Desargues: triangles ABC and A'B'C' in perspective from the origin,
        ;; A' = kA, B' = lB, C' = mC. The intersections of corresponding sides
        ;; are collinear iff the three side-pair lines are concurrent.
        A ['a_1 'a_2] B ['b_1 'b_2] C ['c_1 'c_2]
        s* (fn [k [x y]] [(e/* k x) (e/* k y)])
        A' (s* 'k A) B' (s* 'l B) C' (s* 'm C)
        side (fn [P Q P' Q'] (meet (join P Q) (join P' Q')))
        ddet (det3 [(seq (side A B A' B'))
                    (seq (side B C B' C'))
                    (seq (side C A C' A'))])
        ;; Pappus, in homogeneous coordinates (polynomial throughout): points
        ;; on two lines through the origin, cross-intersections without ever
        ;; dehomogenizing.
        sc (fn [k [x y]] [(e/* k x) (e/* k y)])
        PA (sc 'p_1 ['u_1 'u_2]) PB (sc 'p_2 ['u_1 'u_2]) PC (sc 'p_3 ['u_1 'u_2])
        PA' (sc 'q_1 ['v_1 'v_2]) PB' (sc 'q_2 ['v_1 'v_2]) PC' (sc 'q_3 ['v_1 'v_2])
        xi (fn [W X Y Z] (meet (join W X) (join Y Z)))
        pdet (det3 [(seq (xi PA PB' PA' PB))
                    (seq (xi PA PC' PA' PC))
                    (seq (xi PB PC' PB' PC))])]
    {:desargues (e/simplify ddet)
     :pappus (e/simplify pdet)}))
