(ns desargues.logic.proof.rules
  "The inference rules: `justify` is OPEN on a line's :by.

   (justify line ctx) -> (ok {:evidence e :why s}) or (err {:evidence e :why s})
   with e an Evidence variant and ctx {:lines :index :scope :goal :library}.

     :hyp                          a hypothesis of the goal
     :assume                       opens a subproof
     :discharge :from [a b]        (implies claim-a claim-b), closing a's subproof
     :def       :from i            line i with one definition unfolded or folded
     :taut      :from [i ...]      propositional consequence
     :use       :result id :with {P t} :from [i ...]
                                   an instance of a proved result, applied
     :inst      :from i :with {x t} line i's forall body, x := t
     :gen       :from i :vars [x]  (forall [x] claim-i), x arbitrary
     :witness   :from i :with {x t} (exists [x] B) from B with x := t
     :cite      :result id         a book result taken as given
     :model     :from [i ...]      no counterexample in small finite models"
  (:require [desargues.logic.classes :as cl]
            [desargues.logic.definition :as d]
            [desargues.logic.formula :as f]
            [desargues.logic.model :as m]
            [desargues.logic.proof.library :as lib]
            [desargues.logic.proof.scope :as sc]
            [desargues.logic.term :as t]
            [hive-dsl.result :as r]
            [desargues.logic.definitions]))

(defmulti justify
  "The Result of checking one line in ctx."
  (fn [line _ctx] (:by line)))

(defn- holds [evidence why] (r/ok {:evidence evidence :why why}))

(defn- fails [evidence why & {:as more}] (r/err (merge {:evidence evidence :why why} more)))

(defn- claim-of [{:keys [lines]} i] (:claim (nth lines i)))

(defn- refs [from] (if (sequential? from) (vec from) [from]))

(defmethod justify :default [line _]
  (fails :evidence/unknown (str "unknown rule " (:by line))))

;; ---------------------------------------------------------------------------
;; Givens

(defn hypotheses
  "The hypotheses of a goal (implies (and h ...) c) or (implies h c)."
  [goal]
  (if (and (seq? goal) (= 'implies (first goal)))
    (let [h (second goal)] (if (and (seq? h) (= 'and (first h))) (vec (rest h)) [h]))
    []))

(defmethod justify :hyp [{:keys [claim]} {:keys [goal]}]
  (if (some #(t/alpha= % claim) (hypotheses goal))
    (holds :evidence/given "hypothesis")
    (fails :evidence/given "not a hypothesis of the goal")))

(defmethod justify :assume [_ _] (holds :evidence/given "assumption"))

(defmethod justify :discharge [{:keys [claim from]} ctx]
  (let [[a b] (refs from)]
    (if (t/alpha= claim (list 'implies (claim-of ctx a) (claim-of ctx b)))
      (holds :evidence/structural "discharge the assumption")
      (fails :evidence/structural "claim is not assumption => conclusion"))))

;; ---------------------------------------------------------------------------
;; Definitions and propositional steps

(defn- unfold-once [s]
  (or (d/defines s)
      (when (and (seq? s) (= 'in (first s)))
        (let [[_ x c] s
              c (if (#{'empty 'universe} c) (list c) c)]
          (when (seq? c) (cl/unfold x c))))))

(defn definitional-step?
  "True when b is a with one definition unfolded, or a with one folded."
  [a b]
  (boolean (or (some #(t/alpha= % b) (t/rewrites a unfold-once))
               (some #(t/alpha= % a) (t/rewrites b unfold-once)))))

(defmethod justify :def [{:keys [claim from]} ctx]
  (if (definitional-step? (claim-of ctx from) claim)
    (holds :evidence/definitional "by definition")
    (fails :evidence/definitional "not one definition step away")))

(defn- consequence [premises claim]
  (list 'implies (cons 'and (concat premises [true])) claim))

(defmethod justify :taut [{:keys [claim from]} ctx]
  (if (f/tautology? (t/alpha-atoms (consequence (map #(claim-of ctx %) (refs from)) claim)))
    (holds :evidence/tautology "propositional consequence")
    (fails :evidence/tautology "does not follow propositionally")))

(defn- as-implication
  "[premises conclusion] of a result's statement."
  [s]
  (if (and (seq? s) (= 'implies (first s)))
    (let [[_ h c] s]
      [(if (and (seq? h) (= 'and (first h))) (vec (rest h)) [h]) c])
    [[] s]))

(defmethod justify :use [{:keys [claim from result with]} {:keys [library] :as ctx}]
  (if-let [stmt (and library (lib/statement library result))]
    (let [[premises conclusion] (as-implication (t/substitute stmt with))
          cited (mapv #(claim-of ctx %) (refs from))]
      (if (and (t/alpha= conclusion claim)
               (= (count premises) (count cited))
               (every? true? (map t/alpha= premises cited)))
        (holds :evidence/used (str "by " result))
        (fails :evidence/used (str "not an instance of " result " applied to the cited lines"))))
    (fails :evidence/used (str "no proved result " result " in the library"))))

;; ---------------------------------------------------------------------------
;; Quantifiers

(defmethod justify :inst [{:keys [claim from with]} ctx]
  (let [src (claim-of ctx from)]
    (if (and (seq? src) (= 'forall (first src)))
      (let [[_ bs body] src
            remaining (vec (remove (set (keys with)) bs))
            inst (t/substitute body with)]
        (if (t/alpha= claim (if (seq remaining) (list 'forall remaining inst) inst))
          (holds :evidence/structural "instantiate")
          (fails :evidence/structural "not an instance")))
      (fails :evidence/structural "cited line is not a forall"))))

(defmethod justify :gen [{:keys [claim from vars]} {:keys [scope] :as ctx}]
  (let [captured (filter (fn [x] (some #(t/free-in? x %) (sc/givens scope))) vars)]
    (cond
      (seq captured)
      (fails :evidence/structural
             (str "not arbitrary: " (pr-str (vec captured)) " occurs in a hypothesis or open assumption"))
      (t/alpha= claim (list 'forall (vec vars) (claim-of ctx from)))
      (holds :evidence/structural "generalise over an arbitrary element")
      :else
      (fails :evidence/structural "not the generalisation of the cited line"))))

(defmethod justify :witness [{:keys [claim from with]} ctx]
  (if (and (seq? claim) (= 'exists (first claim)))
    (let [[_ _ body] claim]
      (if (t/alpha= (t/substitute body with) (claim-of ctx from))
        (holds :evidence/structural "a witness exists")
        (fails :evidence/structural "the cited line is not the witnessed body")))
    (fails :evidence/structural "claim is not an exists")))

;; ---------------------------------------------------------------------------
;; Outside evidence

;; ---------------------------------------------------------------------------
;; Equality

(defmethod justify :refl [{:keys [claim]} _]
  (if (and (seq? claim) (= '= (first claim)) (= 3 (count claim)) (t/alpha= (nth claim 1) (nth claim 2)))
    (holds :evidence/structural "a thing equals itself")
    (fails :evidence/structural "claim is not t = t")))

(defn- replace-some
  "Every form obtained from form by replacing any non-empty set of the
   occurrences of a with b."
  [form a b]
  (letfn [(go [g]
            (let [here (if (= g a) [b] [])
                  inside (if (seq? g)
                           (let [opts (map (fn [x] (cons x (go x))) (rest g))]
                             (map #(apply list (first g) %)
                                  (reduce (fn [acc o] (for [p acc x o] (conj p x))) [[]] opts)))
                           [g])]
              (distinct (concat here inside))))]
    (remove #(= % form) (go form))))

(defmethod justify :subst [{:keys [claim from]} ctx]
  (let [[e i] (refs from)
        eq (claim-of ctx e)
        src (claim-of ctx i)]
    (if (and (seq? eq) (= '= (first eq)))
      (let [[_ a b] eq]
        (if (some #(t/alpha= % claim) (concat (replace-some src a b) (replace-some src b a)))
          (holds :evidence/structural "replace equals by equals")
          (fails :evidence/structural "claim is not the cited line with equals replaced")))
      (fails :evidence/structural "first cited line is not an equation"))))

;; ---------------------------------------------------------------------------
;; Existential elimination: choose a witness, reason, conclude

(defmethod justify :choose [{:keys [claim from with]} ctx]
  (let [src (claim-of ctx from)]
    (if (and (seq? src) (= 'exists (first src)))
      (let [[_ _ body] src
            [[x c]] (seq with)
            fresh? (not-any? #(t/free-in? c %) (cons src (sc/givens (:scope ctx))))]
        (cond (not fresh?) (fails :evidence/structural (str c " is not a new name"))
              (t/alpha= claim (t/substitute body {x c})) (holds :evidence/given "let it be such")
              :else (fails :evidence/structural "claim is not the chosen witness's body")))
      (fails :evidence/structural "cited line is not an exists"))))

(defmethod justify :exists-elim [{:keys [claim from with]} ctx]
  (let [[c-line last-line] (refs from)
        c (first (vals with))]
    (cond (t/free-in? c claim) (fails :evidence/structural (str "the conclusion still mentions " c))
          (t/alpha= claim (claim-of ctx last-line)) (holds :evidence/structural "it holds whichever witness")
          :else (fails :evidence/structural (str "claim is not the conclusion reached from line " c-line)))))

(defmethod justify :cite [{:keys [result]} _]
  (holds :evidence/cited (str "by " result)))

(defmethod justify :model [{:keys [claim from]} ctx]
  (let [g (consequence (map #(claim-of ctx %) (refs from)) claim)
        found (r/rescue {::failed true} (m/counterexample g))]
    (cond (::failed found) (fails :evidence/model "the model check could not evaluate the claim")
          found (fails :evidence/model "counterexample in a finite model" :counterexample found)
          :else (holds :evidence/model "no counterexample in small models"))))
