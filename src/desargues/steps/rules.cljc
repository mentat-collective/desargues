(ns desargues.steps.rules
  "RULES of integration as IRule values: the table, linearity, constant
   factors, u-substitution, trigonometric identities and the fundamental
   theorem (FTC) for definite integrals.

   Every rule reads its goal through emmy.pattern (the table rows, the
   identity patterns) or Emmy's algebra (derivatives, simplification) and
   answers Moves (desargues.steps.tactics) whose Steps record the rule, its
   construction and its bindings, so a derivation says which entry, which
   u, which identity licensed each move. By parts and partial fractions
   live in desargues.steps.parts and desargues.steps.rational; `integrator`
   at the bottom is the whole solver as one `choice`.

   Host-free .cljc."
  (:require [desargues.steps.expr :as x]
            [desargues.steps.model :as m]
            [desargues.steps.table :as table]
            [desargues.steps.tactics :as t]
            [emmy.pattern.match :as pm]))

(defn dvar [v] (symbol (str "d" v)))

(defn step [rule construction from to & {:as more}]
  (m/step (merge {:rule rule :construction construction :from from :to to} more)))

(defn- integral [g] (m/integral (:f g) (:var g)))

;; ## Table -----------------------------------------------------------------

(def table-rule
  (t/rule :integral/table
          (fn [{:keys [kind f var] :as g}]
            (when (= :integral kind)
              (when-let [{:keys [row F] :as hit} (table/lookup f var)]
                [(t/closing :integral/table
                            [(step :integral/table :table (integral g) (m/plus-c F)
                                   :bindings {:row row :formula (m/eq (integral g) (m/plus-c F))})]
                            F (if (= :const row) 0.5 1))])))))

;; ## Linearity: sums split, constants come out -----------------------------

(defn- sum-move [rule construction g terms finish-sum]
  (t/move {:rule rule :cost 0.5
           :steps [(step rule construction (integral g)
                         (apply m/op :+ (map #(m/integral % (:var g)) terms)))]
           :subgoals (mapv #(t/sub-goal g %) terms)
           :finish (fn [answers] {:answer (x/simp (finish-sum answers)) :steps []})}))

(def linearity
  (t/rule :integral/linearity
          (fn [{:keys [kind f var] :as g}]
            (when (= :integral kind)
              (if-let [terms (x/sum-terms f)]
                [(sum-move :integral/linearity :split g terms #(cons '+ %))]
                (let [[c rest] (x/const-split var f)]
                  (when (and (not= 1 c) (not= rest f) (x/depends-on? var rest))
                    [(t/move {:rule :integral/constant :cost 0.5
                              :steps [(step :integral/constant :split (integral g)
                                            (m/op :* c (m/integral rest var)))]
                              :subgoals [(t/sub-goal g rest)]
                              :finish (fn [[F]] {:answer (x/simp (list '* c F)) :steps []})})])))))))

;; ## u-substitution ---------------------------------------------------------

(defn- fresh [v] (get {'x 'u 'u 't 't 'w} v (symbol (str v "1"))))

(defn- candidates
  "The u worth trying: inner subforms that depend on v (arguments of
   functions, bases of powers, denominators), never v or f itself."
  [f v]
  (->> (x/subforms f)
       (filter seq?)
       (mapcat rest)
       (filter #(and (seq? %) (x/depends-on? v %) (not= % f)))
       distinct
       (take 8)))

(defn- substituted
  "The integrand in u when f dv = h(u) du with u = g, or nil."
  [f v g U]
  (let [dg (x/d g v)]
    (when (not= 0 dg)
      (let [r (x/ratio f dg)
            [a b] (x/linear-in v g)
            h (if a
                (x/simp (x/evaluate r v (x/simp (list '/ (list '- U b) a))))
                (x/simp (x/subst r g U)))]
        (when (and (x/free-of? v h) (x/depends-on? U h)) [h dg])))))

(def substitution
  (t/rule :integral/substitution
          (fn [{:keys [kind f var] :as g}]
            (when (and (= :integral kind) (not= 'w var))
              (let [U (fresh var)]
                (for [u (candidates f var)
                      :let [[h du] (substituted f var u U)]
                      :when h]
                  (t/move {:rule :integral/substitution
                           :cost (if (x/linear-in var u) 1.5 2)
                           :steps [(step :integral/substitution :let (integral g) (m/integral h U)
                                         :bindings (array-map U u (dvar U) (list '* du (dvar var))))]
                           :subgoals [(t/sub-goal g h U)]
                           :finish (fn [[F]]
                                     (let [back (x/simp (x/evaluate F U u))]
                                       {:answer back
                                        :steps [(step :integral/back :substitute-back (m/plus-c F) (m/plus-c back)
                                                      :bindings {U u})]}))})))))))

;; ## Trigonometric identities, as emmy.pattern rewrites --------------------

(defn- identities [v]
  [[:sin2   (list 'expt (list 'sin v) 2)  [(/ 1 2) (list '* (/ -1 2) (list 'cos (list '* 2 v)))]]
   [:cos2   (list 'expt (list 'cos v) 2)  [(/ 1 2) (list '* (/ 1 2) (list 'cos (list '* 2 v)))]]
   [:tan2   (list '/ (list 'expt (list 'sin v) 2) (list 'expt (list 'cos v) 2))
    [(list '/ 1 (list 'expt (list 'cos v) 2)) -1]]
   [:sincos (list '* (list 'sin v) (list 'cos v)) [(list '* (/ 1 2) (list 'sin (list '* 2 v)))]]])

(def trig-identity
  (t/rule :integral/trig-identity
          (fn [{:keys [kind f var] :as g}]
            (when (= :integral kind)
              (for [[id pat terms] (identities var)
                    :when (not (pm/failed? (pm/match pat f)))
                    :let [rhs (cons '+ terms)]]
                (t/move {:rule :integral/trig-identity :cost 1
                         :steps [(step :integral/trig-identity :rewrite (integral g)
                                       (apply m/op :+ (map #(m/integral % var) terms))
                                       :bindings {:identity (m/eq pat rhs) :name id})]
                         :subgoals (mapv #(t/sub-goal g %) terms)
                         :finish (fn [as] {:answer (x/simp (cons '+ as)) :steps []})}))))))

;; ## FTC: a definite integral is F(b) - F(a) --------------------------------

(def ftc
  (t/rule :integral/ftc
          (fn [{:keys [kind f var bounds] :as g}]
            (when (= :definite kind)
              (let [[a b] bounds]
                [(t/move {:rule :integral/ftc :cost 1
                          :subgoals [(t/sub-goal g f)]
                          :finish (fn [[F]]
                                    (let [Fb (x/simp (x/evaluate F var b))
                                          Fa (x/simp (x/evaluate F var a))
                                          value (x/simp (list '- Fb Fa))]
                                      {:answer value
                                       :steps [(step :integral/ftc :evaluate (m/integral f var bounds)
                                                     (m/eq (m/eval-at F var a b) (m/op :- Fb Fa) value))]}))})])))))
