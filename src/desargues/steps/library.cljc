(ns desargues.steps.library
  "The TECHNIQUE LIBRARY: results that persist across problems.

   Solving a problem files a Lemma for its goal (∫ f dx = F) and for every
   subgoal its solution closed, with the rules it used and the earlier
   lemmas it cited. `cite-rule` is an IRule that closes a goal already in
   the library with one :cite step, at a low cost, so a later derivation
   reads 'by Problem s1' instead of repeating the work. The prerequisite
   DAG links each problem to the problems whose lemmas it cited (and to the
   problems that first used each technique), which orders the book.

   A library is a plain value (a map); `learn` returns a new one.

   Host-free .cljc."
  (:require [desargues.steps.integrator :as integrator]
            [desargues.steps.model :as m]
            [desargues.steps.tactics :as t]))

(defrecord Lemma [id goal-key f var answer rules cites])

(def empty-library {:lemmas {} :by-goal {} :first-use {} :order []})

(defn cite-rule
  "An IRule closing any goal the library already holds, with a :cite step."
  [lib]
  (t/rule :integral/cite
          (fn [g]
            (when-let [{:keys [id answer f var]} (get (:by-goal lib) (t/goal-key g))]
              [(assoc (t/closing :integral/cite
                                 [(m/step {:rule :integral/cite :construction :cite
                                           :from (m/integral f var) :to (m/plus-c answer)
                                           :bindings {:lemma id}})]
                                 answer 0.25)
                      :cited #{id})]))))

(defn learn
  "Solve corpus problem `p` with the library's lemmas available, file its
   result as a lemma, and record what it cited and the techniques it first
   used. Returns [library' checked-problem]."
  [lib {:keys [id f var bounds] :as p}]
  (let [extra [(cite-rule lib)]
        r (integrator/integrate f var {:bounds bounds :extra extra})
        s (:solution r)]
    (if-not s
      [lib (assoc p :result nil)]
      (let [k (t/goal-key (:goal s))
            lemma (->Lemma id k (get-in s [:goal :f]) var (:answer s) (:rules s) (:cited s))
            new-rules (remove (:first-use lib) (:rules s))]
        [(-> lib
             (assoc-in [:lemmas id] lemma)
             (assoc-in [:by-goal k] lemma)
             (update :first-use into (map (fn [r] [r id]) new-rules))
             (update :order conj id))
         (assoc p :result r)]))))

(defn learn-all
  "Learn every problem in order: [library results]."
  [problems]
  (reduce (fn [[lib acc] p] (let [[lib' r] (learn lib p)] [lib' (conj acc r)]))
          [empty-library []] problems))

(defn prerequisites
  "The prerequisite DAG as {problem #{problems it depends on}}: the lemmas
   it cited, and for each technique it used, the problem that introduced it."
  [lib]
  (into {}
        (for [[id {:keys [rules cites]}] (:lemmas lib)]
          [id (disj (into (set cites) (keep (:first-use lib) rules)) id)])))
