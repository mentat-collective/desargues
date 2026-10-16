(ns desargues.logic.term
  "Terms with binders: alpha-equivalence, substitution, free occurrence and
   one-step rewriting. Binders are (forall [x ...] body), (exists ...),
   (exists! ...) and (class [x ...] body)."
  (:require [desargues.logic.formula :as f]))

(def binder-ops '#{forall exists exists! class})

(defn binder? [form]
  (and (seq? form) (binder-ops (first form)) (vector? (second form))))

(defn alpha
  "form with every bound variable renamed v0 v1 ... in binding order;
   alpha-equivalent forms become equal."
  [form]
  (let [n (atom -1)]
    (letfn [(go [form env]
              (cond
                (symbol? form) (get env form form)
                (binder? form) (let [[op bs body] form
                                     bs' (mapv (fn [_] (symbol (str "v" (swap! n inc)))) bs)]
                                 (list op bs' (go body (merge env (zipmap bs bs')))))
                (seq? form) (apply list (map #(go % env) form))
                (vector? form) (mapv #(go % env) form)
                :else form))]
      (go form {}))))

(defn alpha=
  "True when a and b are alpha-equivalent."
  [a b]
  (= (alpha a) (alpha b)))

(defn alpha-atoms
  "formula with each propositional atom alpha-normalised on its own, so the
   same quantified statement is one atom wherever it occurs."
  [formula]
  (if (f/compound? formula)
    (apply list (first formula) (map alpha-atoms (rest formula)))
    (alpha formula)))

(defn substitute
  "form with free occurrences of the symbols in smap replaced."
  [form smap]
  (cond
    (symbol? form) (get smap form form)
    (binder? form) (let [[op bs body] form]
                     (list op bs (substitute body (apply dissoc smap bs))))
    (seq? form) (apply list (map #(substitute % smap) form))
    (vector? form) (mapv #(substitute % smap) form)
    :else form))

(defn free-in?
  "True when symbol x occurs free in form."
  [x form]
  (not= form (substitute form {x ::probe})))

(defn rewrites
  "Every form obtained from form by replacing ONE subterm s with (rewrite s),
   wherever that is non-nil."
  [form rewrite]
  (concat (when-some [r (rewrite form)] [r])
          (when (seq? form)
            (for [i (range 1 (count form))
                  s' (rewrites (nth form i) rewrite)]
              (apply list (assoc (vec form) i s'))))))
