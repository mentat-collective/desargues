(ns desargues.kernel
  "Kernels as data, before any backend runs them. Pure: no engine is required
   here, and nothing is evaluated except by `compile-jvm`.

   Two things live here.

   `realize` turns one Emmy expression into the form a TARGET wants, and the
   set of targets is open:
     :sexp    the simplified expression as data (Emmy's freeze)
     :tex     LaTeX (Emmy's ->TeX)
     :raster  the vocabulary raster's deftm compiles
   A new target (a shader, another compiler) is a defmethod, never an edit.

   The rest is the intermediate form of an Emmy :primitive kernel, the source
   Emmy's compiler emits for a state derivative:
     (fn [in out ps] (let [bindings] (doto out (aset k expr) ..)))
   `source->ir` reads it into {:in :out :params :bindings :body}, `stores`
   lists what it writes, and `chunk` splits its bindings into runs small
   enough to JIT. Every backend that compiles such a kernel starts from these
   (the JVM here, raster in desargues.infrastructure.raster-kernels), so they
   evaluate the same expressions on the same values."
  (:refer-clojure :exclude [chunk])
  (:require [clojure.walk :as walk]
            [emmy.env :as e]))

;; ---------------------------------------------------------------------------
;; Realisations, open by target

(defmulti realize
  "target, emmy-expression -> that expression for the target."
  (fn [target _expr] target))

(defmethod realize :default [target _]
  (throw (ex-info (str "No realisation for target " (pr-str target)
                       "; register one with (defmethod desargues.kernel/realize " (pr-str target) " ...)")
                  {:target target :known (disj (set (keys (methods realize))) :default)})))

(defmethod realize :sexp [_ expr] (e/freeze expr))

(defmethod realize :tex [_ expr] (e/->TeX expr))

(def ^:private raster-fns
  ;; raster.math has no sqrt or abs: those are JVM intrinsics raster lowers
  ;; to f64.sqrt / f64.abs, and pow lives in raster.numeric.
  '{sin raster.math/sin cos raster.math/cos tan raster.math/tan
    exp raster.math/exp log raster.math/log sqrt Math/sqrt
    sinh raster.math/sinh cosh raster.math/cosh tanh raster.math/tanh
    asin raster.math/asin acos raster.math/acos atan raster.math/atan
    abs Math/abs})

(defn- binary
  "A variadic application as nested binary ones: raster's typed dispatch
   resolves +, * one pair at a time."
  [op args]
  (reduce (fn [acc a] (list op acc a)) args))

(defn- power
  "x^k: integer k becomes multiplication (raster's wasm pow is exp(k log x),
   undefined for x < 0), any other k goes to raster.numeric/pow."
  [base k]
  (cond
    (and (integer? k) (zero? k)) 1.0
    (and (integer? k) (pos? k)) (binary '* (repeat k base))
    (integer? k) (list '/ 1.0 (power base (- k)))
    :else (list 'raster.numeric/pow base (double k))))

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

;; ---------------------------------------------------------------------------
;; Emmy :primitive kernels as data

(defn source->ir
  "Emmy's :primitive kernel source -> {:in :out :params :bindings :body}:
   the three array symbols, the let bindings as [sym expr] pairs in order,
   and the (doto out (aset k expr) ..) body."
  [[_ [in out params] [_ bindings body]]]
  {:in in :out out :params params
   :bindings (mapv vec (partition 2 bindings))
   :body body})

(defn stores
  "[[k expr] ..]: the slot of the out array each (aset k expr) of the ir's
   body writes, and the expression written."
  [{[_ _out & sets] :body}]
  (mapv (fn [[_ k expr]] [(long k) expr]) sets))

(def jit-chunk-size
  "Let bindings per generated JVM method; keeps each well under HotSpot's
   8000-byte huge-method limit (beyond it a method is never JIT-compiled)."
  120)

(defn chunk
  "Split `bindings` ([[sym expr] ..]) into runs of at most `size`, to be
   compiled as separate methods that hand their locals over through one
   scratch array, a slot per binding. Returns
     {:slots  {sym slot}
      :chunks [{:needs [sym ..] :bindings [[sym expr] ..]} ..]
      :tail-needs [sym ..]}
   :needs are the earlier bindings a chunk reads back from scratch,
   :tail-needs those `tail` (the forms evaluated after every chunk) reads;
   both in slot order."
  [bindings tail size]
  (let [slots (into {} (map-indexed (fn [i [sym _]] [sym i]) bindings))
        refs (fn [form] (sort-by slots (into #{} (filter slots) (tree-seq coll? seq form))))]
    {:slots slots
     :chunks (mapv (fn [run]
                     (let [own (set (map first run))]
                       {:needs (vec (remove own (refs (map second run))))
                        :bindings (vec run)}))
                   (partition-all size bindings))
     :tail-needs (vec (refs tail))}))

;; ---------------------------------------------------------------------------
;; The JVM backend

(defn jvm-form
  "The ir as Clojure source for (fn [^doubles in ^doubles out ^doubles ps]).
   Beyond `size` bindings (default jit-chunk-size) the let becomes chunk fns
   (see chunk); every binding is evaluated by the same expression on the same
   values, so the result is bit-identical to the unsplit source."
  ([ir] (jvm-form ir jit-chunk-size))
  ([{:keys [in out params bindings body]} size]
   (if (<= (count bindings) size)
     (list 'fn [in out params] (list 'let (vec (mapcat identity bindings)) body))
     (let [{:keys [slots chunks tail-needs]} (chunk bindings body size)
           scratch (with-meta (gensym "scratch") {:tag 'doubles})
           load (fn [syms] (mapcat (fn [s] [s (list 'clojure.core/aget scratch (slots s))]) syms))
           names (vec (repeatedly (count chunks) #(gensym "chunk")))
           chunk-fns (map (fn [{:keys [needs bindings]}]
                            (list 'fn [in params scratch]
                                  (list 'let (vec (concat (load needs) (mapcat identity bindings)))
                                        (cons 'do (concat (map (fn [[s _]] (list 'clojure.core/aset scratch (slots s) s)) bindings)
                                                          [nil])))))
                          chunks)]
       (list 'let (vec (interleave names chunk-fns))
             (list 'fn [in out params]
                   (list 'let [scratch (list 'clojure.core/double-array (count bindings))]
                         (cons 'do (concat (map (fn [nm] (list nm in params scratch)) names)
                                           [(list 'let (vec (load tail-needs)) body)])))))))))

(defn compile-jvm
  "Emmy :primitive kernel source -> a (fn [^doubles in ^doubles out ^doubles
   ps]) every method of which is small enough for HotSpot to JIT."
  [source]
  (binding [*ns* (the-ns 'desargues.kernel)]
    (eval (jvm-form (source->ir source)))))
