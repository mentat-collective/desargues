(ns desargues.steps.integrator
  "The rule-based INTEGRATOR: every technique as one `choice` of IRules, run
   by the DP search, and its Solutions turned into Derivations.

     (integrate f 'x)            ; best Derivation of ∫ f dx
     (integrate f 'x {:bounds [0 'pi]})
     (pathways f 'x)             ; every first move, completed at its best

   `f` is an Emmy expression or a frozen form. A technique library
   (desargues.steps.library) can be passed as :library: its lemmas become
   one more rule that closes a goal by citing an earlier result.

   Host-free .cljc."
  (:require [desargues.steps.expr :as x]
            [desargues.steps.model :as m]
            [desargues.steps.parts :as parts]
            [desargues.steps.rational :as rational]
            [desargues.steps.rules :as r]
            [desargues.steps.search :as search]
            [desargues.steps.tactics :as t]
            [emmy.env :as e]
            [desargues.steps.verify :as verify]))

(def techniques
  "The rules of the integrator, in the order a book teaches them."
  [r/table-rule r/linearity r/substitution r/trig-identity
   rational/partial-fractions rational/complete-square
   parts/by-parts parts/cyclic r/ftc])

(defn rule
  "The integrator as one IRule; `extra` rules (a library's :cite) first."
  ([] (rule []))
  ([extra] (apply t/choice (concat extra techniques))))

(defn goal
  ([f v] (goal f v nil))
  ([f v bounds]
   (if bounds
     (t/goal :definite (e/freeze f) v bounds)
     (t/goal :integral (e/freeze f) v))))

(defn ->derivation
  "A Derivation from Solution `s` of problem `prob`."
  [prob s]
  (m/derivation prob (:steps s)
                (if (= :definite (get-in s [:goal :kind])) (:answer s) (m/plus-c (:answer s)))))

(defn- problem [f v bounds]
  (m/problem {:kind (if bounds :definite :integral)
              :integrand (fn [t] (x/evaluate (e/freeze f) v t))
              :var v :bounds bounds}))

(defn integrate
  "The best derivation of ∫ f dv: {:derivation D :solution S :cost c}, or
   nil when no rule sequence closes it. opts: :bounds [a b], :extra rules."
  ([f v] (integrate f v {}))
  ([f v {:keys [bounds extra]}]
   (when-let [s (search/solve (rule extra) (goal f v bounds))]
     {:derivation (->derivation (problem f v bounds) s) :solution s :cost (:cost s)})))

(defn pathways
  "Every way to start ∫ f dv, completed at its best, cheapest first."
  ([f v] (pathways f v {}))
  ([f v {:keys [bounds extra]}]
   (for [{:keys [rule cost solution]} (search/pathways (rule extra) (goal f v bounds))]
     {:rule rule :cost cost :derivation (->derivation (problem f v bounds) solution)})))

(defn- numeric
  "A closed form (pi allowed) as a double."
  [form]
  (double (x/evaluate (x/subst form 'pi Math/PI) 'x 0)))

(defn check
  "Solve corpus problem `p` and verify its answer with
   desargues.steps.verify: an antiderivative by differentiating back at the
   problem's points, a definite integral against quadrature."
  [{:keys [f var points bounds] :as p}]
  (let [r (try (integrate f var {:bounds bounds}) (catch #?(:clj Exception :cljs :default) _ nil))
        F (some-> r :solution :answer)
        f-fn (x/->fn f var)]
    (assoc p :result r :answer F
           :verdict (cond (nil? r) {:ok? false :reason :unsolved}
                          bounds   (let [[a b] (map numeric bounds)]
                                     (verify/definite f-fn a b (numeric F)))
                          :else    (verify/antiderivative f-fn (x/->fn F var) points)))))
