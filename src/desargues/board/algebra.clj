(ns desargues.board.algebra
  "Pure layer of boards: the algebra, by Emmy; its realisations, by target.

   Emmy is the computer algebra system. An author's function arrives as an
   S-expression over the board's variable and its params; Emmy turns it into
   a symbolic function, differentiates it with D and simplifies. Nothing here
   evaluates a number.

   What Emmy returns is one symbolic expression; `realize` dispatches it by
   TARGET (:sexp, :tex, :raster, ...), and the set of targets is open: it is
   desargues.kernel/realize, where a new target is a defmethod."
  (:require [clojure.walk :as walk]
            [desargues.kernel :as kernel]
            [emmy.env :as e]))

;; ---------------------------------------------------------------------------
;; S-expression -> Emmy

(def ^:private emmy-ops
  "Operators an author may write, read as Emmy's generic functions."
  '#{+ - * / expt square cube sqrt exp log sin cos tan asin acos atan
     sinh cosh tanh abs})

(defn- ->emmy-form [expr]
  (walk/postwalk (fn [x] (if (and (symbol? x) (emmy-ops x)) (symbol "emmy.env" (name x)) x))
                 expr))

(defn- param-syms [{:keys [params]}] (mapv (comp symbol name :id) params))

(defn emmy-fn
  "The author's f as a Clojure fn of params then var, over Emmy's generics."
  [{:keys [f var] :or {var 'x} :as spec}]
  (eval `(fn [~@(param-syms spec) ~var] ~(->emmy-form f))))

(defn symbolic
  "An author's S-expression as an Emmy expression, with `syms` (its params)
   held symbolic. A literal stays a number."
  [expr syms]
  (apply (eval `(fn [~@syms] ~(->emmy-form expr))) syms))

(defn- symbolic-args [{:keys [var] :or {var 'x} :as spec}]
  (conj (param-syms spec) var))

(defn expression
  "f, symbolic in its params and its variable."
  [spec]
  (e/simplify (apply (emmy-fn spec) (symbolic-args spec))))

(defn derivative
  "f', by Emmy's D in the variable, params held symbolic, simplified."
  [{:keys [var] :or {var 'x} :as spec}]
  (let [g (emmy-fn spec)
        ps (param-syms spec)]
    (e/simplify ((e/D (fn [v] (apply g (conj ps v)))) var))))

;; ---------------------------------------------------------------------------
;; Realisations, open by target

(def realize
  "target, emmy-expression -> that expression for the target. This is
   desargues.kernel/realize, the one multimethod: a method added under either
   name is the same registration."
  kernel/realize)
