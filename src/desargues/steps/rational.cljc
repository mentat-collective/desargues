(ns desargues.steps.rational
  "RATIONAL FUNCTIONS: partial fractions over Q, and completing the square.

   A rational integrand p/q in v is read into dense coefficient vectors
   (exact rationals, from Emmy's derivatives at 0), divided when deg p >=
   deg q, and its denominator factored over Q: rational roots by the
   rational root theorem (with multiplicity), the rest kept as one
   irreducible factor of degree <= 2. The ansatz
     A_ij / (v - r_i)^j  and  (B v + C) / q2
   is solved exactly by Gaussian elimination over Q from its values at
   sample points. The result is a sum the linearity rule splits.

   Written Emmy-shaped (plain functions over coefficient vectors) as an
   upstream candidate: Emmy has polynomial arithmetic but no partial
   fraction decomposition (doc/emmy-upstream.md).

   Host-free .cljc."
  (:require [desargues.steps.expr :as x]
            [desargues.steps.model :as m]
            [desargues.steps.rules :as r]
            [desargues.steps.tactics :as t]))

(defn- fact [n] (reduce * 1 (range 1 (inc n))))

(defn- polynomial-form?
  "Built from v, numbers, + - * and natural-number powers only."
  [form v]
  (cond (number? form) true
        (= v form)     true
        (seq? form)    (let [[op & args] form]
                         (case op
                           (+ - *) (every? #(polynomial-form? % v) args)
                           expt    (and (polynomial-form? (first args) v)
                                        (nat-int? (second args)))
                           false))
        :else false))

(defn coefficients
  "[c0 c1 ... cn] when `form` is a polynomial in v with rational
   coefficients (degree <= 12), else nil."
  [form v]
  (when (polynomial-form? form v)
    (loop [k 0 d form cs []]
      (cond (> k 12) nil
            (= 0 d) (let [cs (vec (reverse (drop-while zero? (reverse cs))))] (if (empty? cs) [0] cs))
            :else (let [c (x/evaluate d v 0)]
                    (when (rational? c)
                      (recur (inc k) (x/d d v) (conj cs (/ c (fact k))))))))))

(defn- poly-form [cs v]
  (x/simp (cons '+ (map-indexed (fn [i c] (list '* c (list 'expt v i))) cs))))

(defn- peval [cs t] (reduce (fn [acc c] (+ (* acc t) c)) 0 (rseq cs)))

(defn- pdiv
  "[quotient remainder] of coefficient vectors."
  [n d]
  (let [dn (dec (count d)) lc (peek d)]
    (loop [q (vec (repeat (max 1 (- (count n) dn)) 0)) r n]
      (let [rn (dec (count r))]
        (if (or (< rn dn) (every? zero? r))
          [q r]
          (let [c (/ (peek r) lc) s (- rn dn)
                r' (vec (map-indexed (fn [i ri] (- ri (* c (get d (- i s) 0)))) r))]
            (recur (assoc q s c) (pop r'))))))))

(defn- divisors [n] (let [n (abs n)] (filter #(zero? (mod n %)) (range 1 (inc n)))))

(defn- num-of [c] (if (ratio? c) (numerator c) (long c)))

(defn- rational-roots
  "[roots rest]: the rational roots of integer-coefficient cs, with
   multiplicity, and the cofactor left when they are divided out."
  [cs]
  (loop [cs cs roots []]
    (if (and (> (count cs) 1) (zero? (first cs)))
      (recur (subvec cs 1) (conj roots 0))
      (let [c0 (num-of (first cs)) l (num-of (peek cs))
            cands (when (> (count cs) 1)
                    (distinct (for [p (divisors c0) q (divisors l) s [1 -1]] (* s (/ p q)))))]
        (if-let [r (first (filter #(zero? (peval cs %)) cands))]
          (recur (first (pdiv cs [(- r) 1])) (conj roots r))
          [roots cs])))))

(defn- den-of [c] (if (ratio? c) (denominator c) 1))

(defn- roots+rest [cs]
  (let [scale (reduce (fn [a c] (* a (den-of c))) 1 cs)]
    (rational-roots (mapv #(* scale %) cs))))

(defn- solve-linear
  "Exact solution of rows [[a1 .. an b] ...] (n unknowns), or nil."
  [rows n]
  (loop [rows (vec rows) col 0]
    (if (= col n)
      (mapv #(peek (nth rows %)) (range n))
      (when-let [p (first (filter #(not (zero? (get-in rows [% col]))) (range col (count rows))))]
        (let [rows (assoc rows p (nth rows col) col (nth rows p))
              piv (nth rows col) pv (nth piv col)
              piv (mapv #(/ % pv) piv)
              rows (vec (map-indexed (fn [i row] (if (= i col) piv
                                                   (let [f (nth row col)] (mapv - row (map #(* f %) piv)))))
                                     rows))]
          (recur rows (inc col)))))))

(defn decompose
  "Partial fractions of num/den (coefficient vectors) in v: a seq of forms
   whose sum is num/den, or nil when den has an irreducible factor of
   degree > 2."
  [num den v]
  (let [[q rem] (if (>= (count num) (count den)) (pdiv num den) [[0] num])
        [roots rest] (roots+rest den)
        lc (/ (peek den) (peek rest))
        rest (mapv #(* lc %) rest)]
    (when (<= (count rest) 3)
      (let [mult (frequencies roots)
            terms (concat (for [[r k] (sort mult) j (range 1 (inc k))]
                            (fn [t] (/ 1 (reduce * (repeat j (- t r))))))
                          (when (= 3 (count rest))
                            [(fn [t] (/ 1 (peval rest t))) (fn [t] (/ t (peval rest t)))]))
            forms (concat (for [[r k] (sort mult) j (range 1 (inc k))]
                            (list '/ 1 (list 'expt (list '- v r) j)))
                          (when (= 3 (count rest))
                            [(list '/ 1 (poly-form rest v)) (list '/ v (poly-form rest v))]))
            n (count terms)
            pts (take (+ n 4) (remove #(zero? (peval den %)) (map #(+ (/ % 3) 1/7) (range 60))))
            rows (map (fn [t] (conj (mapv #(% t) terms) (/ (peval rem t) (peval den t)))) pts)
            cs (solve-linear rows n)]
        (when cs
          (->> (cons (when (some (complement zero?) q) (poly-form q v))
                     (map (fn [c f] (when-not (zero? c) (x/simp (list '* c f)))) cs forms))
               (remove nil?)
               vec))))))

(defn- ratio-parts [f]
  (if (and (seq? f) (= '/ (first f)) (= 3 (count f))) [(nth f 1) (nth f 2)] [f 1]))

(def partial-fractions
  ;; The decomposition is not handed back as one sum: Emmy's simplifier would
  ;; put it over a common denominator again. Each fraction is its own
  ;; subgoal, and the move writes the split itself.
  (t/rule :integral/partial-fractions
          (fn [{:keys [kind f var] :as g}]
            (when (= :integral kind)
              (let [[p q] (ratio-parts f) num (coefficients p var) den (coefficients q var)]
                (when (and num den (>= (count den) 3))
                  (when-let [terms (decompose num den var)]
                    (when (> (count terms) 1)
                      [(t/move {:rule :integral/partial-fractions :cost 2
                                :steps [(r/step :integral/partial-fractions :rewrite (m/integral f var)
                                                (m/eq f (apply m/op :+ terms)))
                                        (r/step :integral/linearity :split (m/integral f var)
                                                (apply m/op :+ (map #(m/integral % var) terms)))]
                                :subgoals (mapv #(t/sub-goal g %) terms)
                                :finish (fn [as] {:answer (x/simp (cons '+ as)) :steps []})})]))))))))

(defn- complete-square-move [{:keys [f var] :as g} c0 [c b a]]
  (let [h (/ b (* 2 a)) k (- (/ c a) (* h h)) U 'u back-to (list '+ var h)]
    (when (pos? k)
      (let [h2 (x/simp (list '/ c0 (list '* a (list '+ (list 'expt U 2) k))))]
        (t/move {:rule :integral/complete-square :cost 1.5
                 :steps [(r/step :integral/complete-square :let (m/integral f var) (m/integral h2 U)
                                 :bindings (array-map U back-to (r/dvar U) (r/dvar var)))]
                 :subgoals [(t/sub-goal g h2 U)]
                 :finish (fn [[F]]
                           (let [back (x/simp (x/evaluate F U back-to))]
                             {:answer back
                              :steps [(r/step :integral/back :substitute-back (m/plus-c F)
                                              (m/plus-c back) :bindings {U back-to})]}))})))))

(def complete-square
  (t/rule :integral/complete-square
          (fn [{:keys [kind f var] :as g}]
            (when (= :integral kind)
              (let [[p q] (ratio-parts f) num (coefficients p var) den (coefficients q var)]
                (when (and num den (= 1 (count num)) (= 3 (count den)) (not (zero? (den 1))))
                  (some-> (complete-square-move g (first num) den) vector)))))))
