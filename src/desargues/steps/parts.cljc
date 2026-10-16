(ns desargues.steps.parts
  "INTEGRATION BY PARTS as an IRule: ∫ u dv = u v - ∫ v du.

   Every split of the integrand's factors into u and dv is a move. LIATE
   (Logarithmic, Inverse trig, Algebraic, Trigonometric, Exponential)
   ranks the choices: a u earlier in LIATE costs less, so the search
   prefers it but keeps the others as alternative pathways. v = ∫ dv is
   solved through the goal's :solve port (the same search). When the
   remaining integral ∫ v du is a constant multiple k of the original I
   (after one more pass, as with e^x sin x), the move closes with
   :solve-for: I = R + k I, so I = R / (1 - k).

   Host-free .cljc."
  (:require [desargues.steps.expr :as x]
            [desargues.steps.model :as m]
            [desargues.steps.rules :as r]
            [desargues.steps.tactics :as t]))

(defn liate
  "LIATE rank of a factor (lower: better u)."
  [f v]
  (let [head (when (seq? f) (first f))]
    (cond (= 'log head)                                0
          (contains? '#{atan asin acos} head)          1
          (or (= f v) (and (= 'expt head) (= v (second f)) (number? (nth f 2)))) 2
          (contains? '#{sin cos tan} head)             3
          (or (= 'exp head) (and (= 'expt head) (x/free-of? v (second f)))) 4
          :else 5)))

(def formula
  (m/eq (m/integral 'u 'v) (m/op :- (list '* 'u 'v) (m/integral 'v 'u))))

(defn- splits
  "[u dv] candidates: one factor as u, the rest as dv; a lone log or inverse
   trig function is u with dv = dx."
  [f v]
  (let [fs (x/factors f)]
    (concat
     (when (and (= 1 (count fs)) (<= (liate f v) 1)) [[f 1]])
     (when (> (count fs) 1)
       (for [i (range (count fs))
             :let [u (nth fs i) dv (x/product (concat (take i fs) (drop (inc i) fs)))]
             :when (x/depends-on? v u)]
         [u dv])))))

(defn- reason [u v]
  (str "LIATE: " (["a logarithm" "an inverse trigonometric function" "an algebraic factor"
                   "a trigonometric factor" "an exponential" "this factor"] (liate u v))
       " is taken as u"))

(defn- cycle-coefficient
  "k when g = k f with k a nonzero constant in v, else nil."
  [g f v]
  (let [k (x/ratio g f)]
    (when (and (not= 0 k) (x/free-of? v k)) k)))

(defn- by-parts-moves [{:keys [f var solve] :as g} depth]
  (for [[u dv] (splits f var)
        :let [sv (when solve (solve (t/sub-goal g dv)))]
        :when sv
        :let [V (:answer sv) du (x/d u var) w (x/simp (list '* V du)) uv (x/simp (list '* u V))
              dvar (r/dvar var)
              choose (r/step :integral/by-parts :choose (m/integral f var) nil
                             :bindings (array-map 'u u 'dv (list '* dv dvar) :reason (reason u var)))
              lets (r/step :integral/by-parts :let nil (m/integral dv var)
                           :bindings (array-map 'du (list '* du dvar) 'v V))
              apply (r/step :integral/by-parts :apply-formula (m/integral f var)
                            (m/eq (m/integral f var) (m/op :- uv (m/integral w var)))
                            :bindings {:formula formula})
              k (cycle-coefficient w f var)]]
    (if (and k (not= 1 k))
      ;; ∫ v du = k I: the integral came straight back
      (t/closing :integral/cycle
                 [choose lets apply
                  (r/step :integral/cycle :solve-for nil
                          (m/eq 'I (m/plus-c (x/simp (list '/ uv (list '+ 1 k)))))
                          :bindings {:unknown 'I})]
                 (x/simp (list '/ uv (list '+ 1 k))) (+ 3 (:cost sv)))
      (t/move {:rule :integral/by-parts
               :cost (+ 2 (* 0.5 (liate u var)) (:cost sv))
               :steps (into (vec (:steps sv)) [choose lets apply])
               :subgoals [(t/sub-goal g w)]
               :finish (fn [[W]]
                         (let [F (x/simp (list '- uv W))]
                           {:answer F :steps [(r/step :integral/simplify :hence nil (m/plus-c F))]}))}))))

(def by-parts
  (t/rule :integral/by-parts
          (fn [g] (when (= :integral (:kind g)) (by-parts-moves g 0)))))

(defn- second-pass
  "By parts twice, solving for I when the integral returns (e^x sin x)."
  [{:keys [f var solve] :as g}]
  (for [[u dv] (splits f var)
        :when (>= (liate u var) 3)
        :let [sv (when solve (solve (t/sub-goal g dv)))]
        :when sv
        :let [V1 (:answer sv) w1 (x/simp (list '* V1 (x/d u var))) uv1 (x/simp (list '* u V1))
              [u2 dv2] (first (filter #(= (liate (first %) var) (liate u var)) (splits w1 var)))]
        :when u2
        :let [sv2 (solve (t/sub-goal g dv2))]
        :when sv2
        :let [V2 (:answer sv2) w2 (x/simp (list '* V2 (x/d u2 var))) uv2 (x/simp (list '* u2 V2))
              k (cycle-coefficient w2 f var)]
        ;; I = uv1 - (uv2 - k I)  =>  I = (uv1 - uv2) / (1 - k)
        :when (and k (not= 1 k))
        :let [R (x/simp (list '- uv1 uv2)) F (x/simp (list '/ R (list '- 1 k))) dvar (r/dvar var)]]
    (t/closing :integral/cycle
               [(r/step :integral/by-parts :choose (m/integral f var) nil
                        :bindings (array-map 'u u 'dv (list '* dv dvar) :reason (reason u var)))
                (r/step :integral/by-parts :apply-formula nil
                        (m/eq 'I (m/op :- uv1 (m/integral w1 var))) :bindings {:formula formula})
                (r/step :integral/by-parts :choose nil nil
                        :bindings (array-map 'u u2 'dv (list '* dv2 dvar) :reason "the same pattern again"))
                (r/step :integral/by-parts :then nil
                        (m/eq 'I (m/op :- R (list '* k 'I))))
                (r/step :integral/cycle :solve-for nil (m/eq 'I (m/plus-c F)) :bindings {:unknown 'I})]
               F (+ 5 (:cost sv) (:cost sv2)))))

(def cyclic
  (t/rule :integral/cycle
          (fn [g] (when (= :integral (:kind g)) (second-pass g)))))
