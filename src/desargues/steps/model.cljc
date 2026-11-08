(ns desargues.steps.model
  "DOMAIN stratum of the step solver: the values a worked solution is made of.

   A Problem is what the student is asked. A Step is one move of a solution:
   the rule that licensed it, the expression before and after, the bindings
   the move introduced (u, du, v, dv, ...) and a plain-English sentence that
   says why. A Derivation is the ordered steps from the problem to the
   answer. All three are records: plain immutable values with value
   equality, so a derivation can be stored, compared, rendered by any host
   and replayed one step at a time.

   Expressions are Emmy expressions (symbolic). Nothing here evaluates,
   renders or prints: those live in the strata above (verify, numeric,
   render). Host-free .cljc."
  (:require [emmy.env :as e]
            [emmy.expression]))

(defrecord Problem [id section kind integrand var bounds prompt])

;; Math forms a step's :from / :to may hold. They are EMMY EXPRESSIONS: the
;; integral sign, the equation chain, + C, the evaluation bracket and the
;; substitution bar are symbolic operators in Emmy's expression tree, built
;; as Emmy literals so they compose with Emmy arithmetic,
;;   (e/- (e/* 'u 'v) (integral 'v 'u))  ;=> (- (* u v) (integral v u))
;; and Emmy typesets them (desargues.steps.notation extends Emmy's renderer
;; with one handler per operator). IForm names a form's kind: the operator
;; at its root.
(defprotocol IForm
  (form-kind [f] "Keyword naming the kind of math form, e.g. :integral."))

(def notation-operators
  "The operators a solution writes that Emmy's algebra has no function for."
  '#{integral plus-C eval-at subst derivative})

(extend-protocol IForm
  emmy.expression.Literal
  (form-kind [l]
    (let [f (e/freeze l)]
      (if (and (seq? f) (contains? notation-operators (first f)))
        (keyword (name (first f)))
        :expr))))

(defn form?
  "True when x is a notation form (an integral, + C, a bracket ...)."
  [x]
  (and (satisfies? IForm x) (not= :expr (form-kind x))))

(defn- ->list [form] (if (seq? form) (apply list (map ->list form)) form))

(defn- lit [form] (e/literal-number (->list form)))

(defn integral
  "∫ integrand d var, or with bounds [a b] the definite integral."
  ([integrand var] (lit (list 'integral (e/freeze integrand) (e/freeze var))))
  ([integrand var bounds]
   (if-let [[a b] bounds]
     (lit (list 'integral (e/freeze integrand) (e/freeze var) (e/freeze a) (e/freeze b)))
     (integral integrand var))))

(defn eq
  "The equation chain a = b = c ..., Emmy's `=` operator left unevaluated."
  [& sides]
  (lit (cons '= (map e/freeze sides))))

(defn plus-c "expr + C." [expr] (lit (list 'plus-C (e/freeze expr))))

(defn eval-at
  "[F]_a^b: F between the bounds a and b of var."
  [F var a b]
  (lit (list 'eval-at (e/freeze F) (e/freeze var) (e/freeze a) (e/freeze b))))

(defn subst
  "F |_{u = g}: F with u replaced by g."
  [F u g]
  (lit (list 'subst (e/freeze F) (e/freeze u) (e/freeze g))))

(defn derivative
  "d/d var of f, not yet taken."
  [f var]
  (lit (list 'derivative (e/freeze f) (e/freeze var))))

(defn op
  "Emmy arithmetic over math forms: (op :- (* u v) (integral v u)) is
   u v - ∫ v du. `op` is :+ :- or :*; kept so a derivation reads as it is
   written, it is (e/- ...) etc. Frozen forms are accepted as arguments."
  [op & args]
  (apply (case op :+ e/+ :- e/- :* e/*)
         (map #(if (seq? %) (lit %) %) args)))

(defn integral-parts
  "[integrand var a b] of an integral form (a b nil when indefinite), else nil."
  [form]
  (let [f (e/freeze form)]
    (when (and (seq? f) (= 'integral (first f)))
      (vec (take 4 (concat (rest f) (repeat nil)))))))

(defrecord Step [rule construction from to bindings text side-work])

(defrecord Derivation [problem steps answer])

(defn problem
  "A Problem. `kind` is :integral, :definite or :derivative; `integrand` is a
   one-argument function of the variable (Emmy builds the expression by
   calling it on a symbol), `bounds` is [a b] for a definite integral."
  [{:keys [id section kind integrand var bounds prompt] :or {var 'x kind :integral}}]
  (->Problem id section kind integrand var bounds prompt))

(defn step
  "A Step. Keys:
     :rule          the rule that licensed the move (:integral/by-parts ...)
     :construction  how the move is phrased: :let :then :hence :apply-formula
                    :substitute-back :evaluate ... (desargues.steps.phrase
                    turns it into words and math, one method per kind)
     :from :to      the working expression before and after
     :bindings      what the move introduced, {u (sin x) du ...}
     :text          optional wording that overrides the construction's own
     :side-work     sub-derivations shown in the margin
   A step needs no text at all: its words come from its construction."
  [{:keys [rule construction from to bindings text side-work]
    :or {construction :then bindings {}}}]
  (->Step rule construction from to bindings text side-work))

(defn derivation
  "A Derivation of `problem` through `steps` ending at `answer`."
  [problem steps answer]
  (->Derivation problem (vec steps) answer))

(defn prefix
  "The first `n` steps of `d`: what a student sees after n moves (hint mode)."
  [d n]
  (update d :steps #(vec (take n %))))
