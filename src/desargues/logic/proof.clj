(ns desargues.logic.proof
  "Step-by-step proofs as data, checked line by line.

   A proof is {:goal formula :lines [line ...]}; a line is

     {:claim formula :by rule ...rule-specific keys}

   Lines are numbered from 0. `justify` is OPEN on :by and returns
   {:ok? bool :level kw :why string}, where :level states the evidence:

     :given        a hypothesis or an assumption
     :definitional one definition unfolded or folded (alpha-equivalent)
     :tautology    follows propositionally from the cited lines, every
                   non-connective subformula read as an atom
     :structural   a quantifier or subproof rule, checked syntactically
     :model        no counterexample among small finite models
     :cited        a numbered result of the book, taken as given

   Rules shipped here:
     :hyp                         a hypothesis of the goal
     :assume  (opens a subproof)  any claim; :depth grows
     :discharge :from [a b]       (implies claim-of-a claim-of-b), closing a's subproof
     :def     :from i             claim is line i with one definition unfolded or folded
     :taut    :from [i ...]       propositional consequence
     :inst    :from i :with {x t} claim is line i's forall body with x := t
     :gen     :from i :vars [x]   claim is (forall [x] claim-of-i)
     :witness :from i :with {x t} claim is (exists [x] B) and line i is B with x := t
     :cite    :result id          a book result instance, not checked
     :model   :from [i ...]       semantic check in finite models"
  (:require [desargues.logic.classes :as cl]
            [desargues.logic.definition :as d]
            [desargues.logic.formula :as f]
            [desargues.logic.model :as m]))

;; ---------------------------------------------------------------------------
;; Terms

(def ^:private binder-ops '#{forall exists exists! class})

(defn alpha
  "form with every bound variable renamed to v0 v1 ... in binding order, so
   alpha-equivalent forms are equal."
  [form]
  (let [n (atom -1)]
    (letfn [(go [form env]
              (cond
                (symbol? form) (get env form form)
                (and (seq? form) (binder-ops (first form)) (vector? (second form)))
                (let [[op bs body] form
                      bs' (mapv (fn [_] (symbol (str "v" (swap! n inc)))) bs)]
                  (list op bs' (go body (merge env (zipmap bs bs')))))
                (seq? form) (apply list (map #(go % env) form))
                (vector? form) (mapv #(go % env) form)
                :else form))]
      (go form {}))))

(defn alpha-atoms
  "formula with each propositional atom alpha-normalised on its own, so the
   same quantified statement is the same atom wherever it occurs."
  [formula]
  (if (f/compound? formula)
    (apply list (first formula) (map alpha-atoms (rest formula)))
    (alpha formula)))

(defn substitute
  "form with free occurrences of the symbols in smap replaced."
  [form smap]
  (cond
    (symbol? form) (get smap form form)
    (and (seq? form) (binder-ops (first form)) (vector? (second form)))
    (let [[op bs body] form]
      (list op bs (substitute body (apply dissoc smap bs))))
    (seq? form) (apply list (map #(substitute % smap) form))
    (vector? form) (mapv #(substitute % smap) form)
    :else form))

(defn- one-step
  "Every form obtained from form by replacing one subterm s with (expand s)
   when that is non-nil."
  [form expand]
  (let [here (when-some [e (expand form)] [e])
        inside (when (seq? form)
                 (for [i (range 1 (count form))
                       s' (one-step (nth form i) expand)]
                   (apply list (assoc (vec form) i s'))))]
    (concat here inside)))

(defn- expand-once [s]
  (or (d/defines s)
      (when (and (seq? s) (= 'in (first s)))
        (let [[_ x c] s
              c (if (#{'empty 'universe} c) (list c) c)]
          (when (seq? c) (cl/unfold x c))))))

(defn definitional-step?
  "True when b is a with one definition unfolded, or a with one folded."
  [a b]
  (let [same? (fn [x y] (= (alpha x) (alpha y)))]
    (boolean (or (some #(same? % b) (one-step a expand-once))
                 (some #(same? % a) (one-step b expand-once))))))

;; ---------------------------------------------------------------------------
;; Rules

(defmulti justify
  "Check one line in context {:proof :index :lines}: {:ok? :level :why}."
  (fn [line _ctx] (:by line)))

(defmethod justify :default [line _]
  {:ok? false :level :unknown :why (str "unknown rule " (:by line))})

(defn- claim-of [ctx i] (:claim (nth (:lines ctx) i)))

(defn hypotheses
  "The hypotheses of a goal (implies (and h ...) c) or (implies h c)."
  [goal]
  (if (and (seq? goal) (= 'implies (first goal)))
    (let [h (second goal)] (if (and (seq? h) (= 'and (first h))) (vec (rest h)) [h]))
    []))

(defmethod justify :hyp [{:keys [claim]} {:keys [proof]}]
  (if (some #(= (alpha %) (alpha claim)) (hypotheses (:goal proof)))
    {:ok? true :level :given :why "hypothesis"}
    {:ok? false :level :given :why "not a hypothesis of the goal"}))

(defmethod justify :assume [_ _] {:ok? true :level :given :why "assumption"})

(defmethod justify :discharge [{:keys [claim from]} ctx]
  (let [[a b] from
        want (list 'implies (claim-of ctx a) (claim-of ctx b))]
    (if (= (alpha want) (alpha claim))
      {:ok? true :level :structural :why "discharge the assumption"}
      {:ok? false :level :structural :why "claim is not assumption => conclusion"})))

(defmethod justify :def [{:keys [claim from]} ctx]
  (if (definitional-step? (claim-of ctx from) claim)
    {:ok? true :level :definitional :why "by definition"}
    {:ok? false :level :definitional :why "not one definition step away"}))

(defmethod justify :taut [{:keys [claim from]} ctx]
  (let [premises (map #(claim-of ctx %) from)
        g (list 'implies (cons 'and (concat premises [true])) claim)]
    (if (f/tautology? (alpha-atoms g))
      {:ok? true :level :tautology :why "propositional consequence"}
      {:ok? false :level :tautology :why "does not follow propositionally"})))

(defmethod justify :inst [{:keys [claim from with]} ctx]
  (let [src (claim-of ctx from)]
    (if (and (seq? src) (= 'forall (first src)))
      (let [[_ bs body] src
            inst (substitute body with)
            remaining (remove (set (keys with)) bs)
            want (if (seq remaining) (list 'forall (vec remaining) inst) inst)]
        (if (= (alpha want) (alpha claim))
          {:ok? true :level :structural :why "instantiate"}
          {:ok? false :level :structural :why "not an instance"}))
      {:ok? false :level :structural :why "cited line is not a forall"})))

(defmethod justify :gen [{:keys [claim from vars]} {:keys [lines index] :as ctx}]
  (let [discharged (set (for [l (take index lines) :when (= :discharge (:by l))] (first (:from l))))
        givens (for [[i l] (map-indexed vector (take index lines))
                     :when (or (= :hyp (:by l)) (and (= :assume (:by l)) (not (discharged i))))]
                 (:claim l))
        free-in? (fn [x form] (not= form (substitute form {x ::probe})))
        captured (filter (fn [x] (some #(free-in? x %) givens)) vars)]
    (cond
      (seq captured)
      {:ok? false :level :structural
       :why (str "not arbitrary: " (pr-str (vec captured)) " occurs in a hypothesis or open assumption")}
      (= (alpha claim) (alpha (list 'forall (vec vars) (claim-of ctx from))))
      {:ok? true :level :structural :why "generalise over an arbitrary element"}
      :else
      {:ok? false :level :structural :why "not the generalisation of the cited line"})))

(defmethod justify :witness [{:keys [claim from with]} ctx]
  (if (and (seq? claim) (= 'exists (first claim)))
    (let [[_ bs body] claim]
      (if (= (alpha (substitute body with)) (alpha (claim-of ctx from)))
        {:ok? true :level :structural :why "a witness exists"}
        {:ok? false :level :structural :why "the cited line is not the witnessed body"}))
    {:ok? false :level :structural :why "claim is not an exists"}))

(defmethod justify :cite [{:keys [result]} _]
  {:ok? true :level :cited :why (str "by " result)})

(defmethod justify :model [{:keys [claim from]} ctx]
  (let [premises (map #(claim-of ctx %) from)
        g (list 'implies (cons 'and (concat premises [true])) claim)
        ce (try (m/counterexample g) (catch Throwable e {::error (.getMessage e)}))]
    (cond (::error ce) {:ok? false :level :model :why (str "model check failed: " (::error ce))}
          ce {:ok? false :level :model :why "counterexample in a finite model" :counterexample ce}
          :else {:ok? true :level :model :why "no counterexample in small models"})))

;; ---------------------------------------------------------------------------
;; Checking a proof

(defn check
  "Every line justified, and the last line's claim against the goal:
   {:ok? :lines [{:index :claim :by :ok? :level :why}] :concludes?}"
  [{:keys [goal lines] :as proof}]
  (let [ctx {:proof proof :lines lines}
        results (vec (map-indexed (fn [i line]
                                    (merge {:index i :claim (:claim line) :by (:by line)}
                                           (justify line (assoc ctx :index i))))
                                  lines))
        concludes? (= (alpha goal) (alpha (:claim (peek lines))))]
    {:ok? (and concludes? (every? :ok? results))
     :concludes? concludes?
     :lines results}))

(defn weakest-level
  "The weakest evidence level among checked lines, by the order
   :given :definitional :tautology :structural :model :cited."
  [{:keys [lines]}]
  (let [order [:given :definitional :tautology :structural :model :cited :unknown]]
    (last (sort-by #(.indexOf ^java.util.List order %) (map :level lines)))))
