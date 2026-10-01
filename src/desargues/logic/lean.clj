(ns desargues.logic.lean
  "Propositional tautologies as Lean 4 proof terms, as data (ansatz surface
   syntax: Clojure forms).

   The proof IS the truth table: one classical case split per atom,
   Or.elim (Classical.em p), gives one branch per row, and each row is closed
   by a term built connective by connective from the essential cases: a true
   formula is proved, a false one is refuted from its hypothesis.

   Atoms are any non-connective forms; an atom (in x A) is the Lean
   proposition (A x), a class being a predicate on elements. Every implicit
   argument is given explicitly, so the terms elaborate without unification.

   Nothing here needs ansatz on the classpath; desargues.logic.verify checks
   the terms against the kernel through a port."
  (:require [clojure.string :as str]
            [desargues.logic.formula :as f]))

(defn binary
  "formula with n-ary and/or nested right as binary applications."
  [g]
  (if (f/compound? g)
    (let [[op & as] g
          as (map binary as)]
      (cond (and (#{'and 'or} op) (= 1 (count as))) (first as)
            (and (#{'and 'or} op) (> (count as) 2)) (list op (first as) (binary (cons op (rest as))))
            :else (cons op as)))
    g))

(defn- atom->lean [a]
  (if (and (seq? a) (= 'in (first a)) (= 3 (count a)))
    (list (nth a 2) (nth a 1))
    a))

(defn ->lean
  "The Lean proposition of a binary formula."
  [g]
  (cond (true? g) 'True
        (false? g) 'False
        (f/atom? g) (atom->lean g)
        :else (let [[op a b] g]
                (case op
                  and (list 'And (->lean a) (->lean b))
                  or (list 'Or (->lean a) (->lean b))
                  not (list 'Not (->lean a))
                  implies (list 'arrow (->lean a) (->lean b))
                  iff (list 'Iff (->lean a) (->lean b))))))

(defn hyp
  "The hypothesis name of atom a's case split."
  [a]
  (symbol (str "hem_" (str/replace (pr-str (atom->lean a)) #"[^A-Za-z0-9]+" "_"))))

(declare refute-term)

(defn- fresh
  "A new hypothesis name h1, h2, ... from the counter atom names."
  [names]
  (symbol (str "h" (count (swap! names conj :h)))))

(defn- prove-term
  "A proof of g, true under env."
  [names g env]
  (let [L ->lean v #(f/evaluate % env)]
    (cond
      (true? g) 'True.intro
      (f/atom? g) (hyp g)
      :else
      (let [[op a b] g]
        (case op
          and (list 'And.intro (L a) (L b) (prove-term names a env) (prove-term names b env))
          or (if (v a)
               (list 'Or.inl (L a) (L b) (prove-term names a env))
               (list 'Or.inr (L a) (L b) (prove-term names b env)))
          not (let [h (fresh names)] (list 'fn [h :- (L a)] (refute-term names h a env)))
          implies (let [h (fresh names)]
                    (list 'fn [h :- (L a)]
                          (if (v b)
                            (prove-term names b env)
                            (list 'False.elim (L b) (refute-term names h a env)))))
          iff (list 'Iff.intro (L a) (L b)
                    (prove-term names (list 'implies a b) env)
                    (prove-term names (list 'implies b a) env)))))))

(defn- refute-term
  "A proof of False from h, a proof of g, false under env."
  [names h g env]
  (let [L ->lean v #(f/evaluate % env)]
    (cond
      (false? g) h
      (f/atom? g) (list (hyp g) h)
      :else
      (let [[op a b] g]
        (case op
          and (if (v a)
                (refute-term names (list 'And.right (L a) (L b) h) b env)
                (refute-term names (list 'And.left (L a) (L b) h) a env))
          or (let [x (fresh names) y (fresh names)]
               (list 'Or.elim (L a) (L b) 'False h
                     (list 'fn [x :- (L a)] (refute-term names x a env))
                     (list 'fn [y :- (L b)] (refute-term names y b env))))
          not (list h (prove-term names a env))
          implies (refute-term names (list h (prove-term names a env)) b env)
          iff (if (v a)
                (refute-term names (list 'Iff.mp (L a) (L b) h (prove-term names a env)) b env)
                (refute-term names (list 'Iff.mpr (L a) (L b) h (prove-term names b env)) a env)))))))

(defn proof
  "The proof term of tautology formula: case splits on its atoms in order,
   each row closed by the essential-case term. nil when formula is not a
   tautology."
  [formula]
  (when (f/tautology? formula)
    (let [g (binary formula)
          G (->lean g)
          names (atom [])]
      (letfn [(split [env [p & more]]
                (if p
                  (list 'Or.elim (atom->lean p) (list 'Not (atom->lean p)) G
                        (list 'Classical.em (atom->lean p))
                        (list 'fn [(hyp p) :- (atom->lean p)] (split (assoc env p true) more))
                        (list 'fn [(hyp p) :- (list 'Not (atom->lean p))] (split (assoc env p false) more)))
                  (prove-term names g env)))]
        (split {} (f/atoms g))))))

(defn- class-letters [formula]
  (distinct (for [a (f/atoms formula) :when (and (seq? a) (= 'in (first a)))] (nth a 2))))

(defn- elements [formula]
  (distinct (for [a (f/atoms formula) :when (and (seq? a) (= 'in (first a)))] (nth a 1))))

(defn binders
  "The theorem's parameters: a type alpha with its elements and classes
   (predicates) when formula speaks of membership, then the sentence letters."
  [formula]
  (let [letters (remove #(and (seq? %) (= 'in (first %))) (f/atoms formula))
        classes (class-letters formula)
        els (elements formula)]
    (vec (concat (when (seq classes) '[α :- Type])
                 (mapcat (fn [x] [x :- 'α]) els)
                 (mapcat (fn [c] [c :- '(arrow α Prop)]) classes)
                 (mapcat (fn [p] [p :- 'Prop]) letters)))))

(defn theorem
  "The theorem proving formula, as data for ansatz.core/prove-theorem, or nil
   when formula is not a tautology:
     {:name :params :statement :tactics [(exact term)]}"
  [theorem-name formula]
  (when-let [term (proof formula)]
    {:name theorem-name
     :params (binders formula)
     :statement (->lean (binary formula))
     :tactics [(list 'exact term)]}))
