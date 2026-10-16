(ns desargues.steps.expr
  "Small calculations over FROZEN Emmy forms that the rules share: simplify,
   free-of, derivative, a form as a function, substitution, and the split
   of a product into its constant and variable parts.

   Every form is Emmy's frozen s-expression ('(* x (exp x))), the
   representation emmy.pattern matches against. Host-free .cljc."
  (:require [emmy.env :as e]
            [emmy.expression :as x]))

(defn ->list
  "Any nested seq (lazy, cons) as nested lists, the shape Emmy reads, with
   integral BigInts as longs (so 1N and 1 are one constant to the matcher)."
  [form]
  (cond (seq? form) (apply list (map ->list form))
        #?@(:clj [(instance? clojure.lang.BigInt form) (if (< (abs form) Long/MAX_VALUE) (long form) form)])
        :else form))

(defn simp
  "Emmy's simplification of a frozen form, frozen again."
  [form]
  (if (number? form)
    (->list form)
    (->list (e/freeze (e/simplify (e/literal-number (->list form)))))))

(defn free-of?
  "True when `form` does not contain the variable `v`."
  [v form]
  (not (contains? (x/variables-in form) v)))

(defn depends-on? [v form] (not (free-of? v form)))

(def ^:private ops
  {'+ e/+ '- e/- '* e/* '/ e// 'expt e/expt 'sqrt e/sqrt 'exp e/exp 'log e/log
   'sin e/sin 'cos e/cos 'tan e/tan 'cot e/cot 'sec e/sec 'csc e/csc
   'atan e/atan 'asin e/asin 'acos e/acos 'abs e/abs 'sinh e/sinh 'cosh e/cosh
   'square e/square 'cube e/cube 'complex e/complex 'real-part e/real-part
   'imag-part e/imag-part})

(defn ->fn
  "`form` as a one-argument function of `v`, built from Emmy's generic
   operators (so Emmy's D differentiates it and a number evaluates it)."
  [form v]
  (let [form (->list form)]
    (fn [t] (x/evaluate form {v t} ops))))

(defn evaluate
  "The value of `form` with `v` bound to `t` (a number or a form)."
  [form v t]
  (let [t (if (seq? t) (e/literal-number (->list t)) t)
        r ((->fn form v) t)]
    (if (number? r) r (e/freeze r))))

(defn- drop-log-abs
  "(log (abs u)) -> (log u): the same derivative, u'/u, on every branch.
   Emmy's autodiff abs needs a numeric sign, so ln|u| has no symbolic
   derivative otherwise (doc/emmy-upstream.md)."
  [form]
  (cond (and (seq? form) (= 'log (first form)) (seq? (second form)) (= 'abs (first (second form))))
        (list 'log (drop-log-abs (second (second form))))
        (seq? form) (apply list (map drop-log-abs form))
        :else form))

(defn d
  "d/dv of `form`, simplified."
  [form v]
  (simp (e/freeze ((e/D (->fn (drop-log-abs form) v)) v))))

(defn subst
  "`form` with every occurrence of the subform `old` replaced by `new`."
  [form old new]
  (x/substitute form old new))

(defn ratio
  "a / b simplified."
  [a b]
  (simp (list '/ a b)))

(defn factors
  "The factors of a product form, or [form]."
  [form]
  (if (and (seq? form) (= '* (first form))) (vec (rest form)) [form]))

(defn product
  "The product form of `fs` (1 when empty, the factor itself when one)."
  [fs]
  (case (count fs) 0 1 1 (first fs) (cons '* fs)))

(defn const-split
  "[c rest]: `form` = c * rest with c free of v (c = 1 when there is none).
   Reads the factors of a product, and of the numerator and denominator of
   a quotient."
  [v form]
  (let [[num den] (if (and (seq? form) (= '/ (first form)) (= 3 (count form)))
                    [(nth form 1) (nth form 2)]
                    [form 1])
        nf (factors num) df (factors den)
        c  (simp (list '/ (product (filter #(free-of? v %) nf))
                       (product (filter #(free-of? v %) df))))]
    (if (= 1 c)
      [1 form]
      [c (simp (list '/ form c))])))

(defn sum-terms
  "The terms of a sum form, or nil when it is not a sum."
  [form]
  (when (and (seq? form) (= '+ (first form))) (vec (rest form))))

(defn linear-in
  "[a b] when form = a v + b with a, b free of v and a ≠ 0, else nil."
  [v form]
  (let [a (d form v)]
    (when (and (not= 0 a) (free-of? v a))
      (let [b (simp (evaluate form v 0))]
        (when (free-of? v b) [a b])))))

(defn subforms
  "Every subform of `form` (itself included), depth first."
  [form]
  (tree-seq seq? rest form))
