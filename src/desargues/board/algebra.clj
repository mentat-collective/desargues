(ns desargues.board.algebra
  "Pure layer of boards: the algebra, by Emmy; its realisations, by target.

   Emmy is the computer algebra system. An author's function arrives as an
   S-expression over the board's variable and its params; Emmy turns it into
   a symbolic function, differentiates it with D and simplifies. Nothing here
   evaluates a number.

   What Emmy returns is one symbolic expression; `realize` dispatches it by
   TARGET, and the set of targets is open:
     :sexp    the simplified expression as data (Emmy's freeze)
     :tex     LaTeX, for the slide's labels (Emmy's ->TeX)
     :raster  the vocabulary raster's deftm compiles, for the kernel
   A new target (a shader, another compiler) is a defmethod, never an edit.

   Every number is raster's: the :raster realisation feeds the kernel that
   runs natively on the JVM for the static frame and as WebAssembly in the
   page."
  (:require [clojure.walk :as walk]
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

(defmulti realize
  "target, emmy-expression -> that expression for the target."
  (fn [target _expr] target))

(defmethod realize :sexp [_ expr] (e/freeze expr))

(defmethod realize :tex [_ expr] (e/->TeX expr))

(def ^:private raster-fns
  '{sin raster.math/sin cos raster.math/cos tan raster.math/tan
    exp raster.math/exp log raster.math/log sqrt raster.math/sqrt
    sinh raster.math/sinh cosh raster.math/cosh tanh raster.math/tanh
    asin raster.math/asin acos raster.math/acos atan raster.math/atan
    abs raster.math/abs})

(defn- binary
  "A variadic application as nested binary ones: raster's typed dispatch
   resolves +, * one pair at a time."
  [op args]
  (reduce (fn [acc a] (list op acc a)) args))

(defn- power
  "x^k: integer k becomes multiplication (raster's wasm pow is exp(k log x),
   undefined for x < 0), any other k goes to raster.math/pow."
  [base k]
  (cond
    (and (integer? k) (zero? k)) 1.0
    (and (integer? k) (pos? k)) (binary '* (repeat k base))
    (integer? k) (list '/ 1.0 (power base (- k)))
    :else (list 'raster.math/pow base (double k))))

(defn- lower-raster [x]
  (cond
    ;; Every literal a double: raster's wasm backend has no Long*Double.
    (and (number? x) (not (double? x))) (double x)
    (not (seq? x)) x
    :else
    (let [[op & args] x]
      (case op
        (+ *) (if (next args) (binary op args) (first args))
        - (if (next args) (binary '- args) (list '- 0.0 (first args)))
        / (if (next args) (binary '/ args) (list '/ 1.0 (first args)))
        expt (power (first args) (let [k (second args)] (if (double? k) (if (== k (Math/rint k)) (long k) k) k)))
        square (power (first args) 2)
        cube (power (first args) 3)
        (if-let [f (raster-fns op)] (cons f args) x)))))

(defmethod realize :raster [_ expr]
  ;; postwalk lowers bottom-up, so every argument is already raster's
  ;; vocabulary when its operator is rewritten; `power` sees the base lowered
  ;; and the exponent still a literal.
  (walk/postwalk lower-raster (e/freeze expr)))
