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
   render). Host-free .cljc.")

(defrecord Problem [id section kind integrand var bounds prompt])

;; Math forms a step's :from / :to may hold besides a bare Emmy expression.
;; Each names its kind through IForm, and desargues.steps.render/tex has one
;; method per kind (OCP: a new form is a record plus a method).
(defprotocol IForm
  (form-kind [f] "Keyword naming the kind of math form, e.g. :integral."))

(defn form? [x] (satisfies? IForm x))

(defrecord Integral [integrand var bounds]    ; ∫ integrand d var, bounds [a b] or nil
  IForm (form-kind [_] :integral))
(defrecord Eq [sides]                         ; a = b = c ...
  IForm (form-kind [_] :eq))
(defrecord Plus-C [expr]                      ; expr + C
  IForm (form-kind [_] :plus-c))

(defn integral
  ([integrand var] (->Integral integrand var nil))
  ([integrand var bounds] (->Integral integrand var bounds)))

(defn eq [& sides] (->Eq (vec sides)))

(defn plus-c [expr] (->Plus-C expr))

(defrecord Op [op args]
  IForm (form-kind [_] :op))

(defn op
  "A display combination of math forms Emmy cannot add, e.g.
   (op :- (* u v) (integral v u)) for u v - ∫ v du. `op` is :+ :- or :*."
  [op & args]
  (->Op op (vec args)))

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
