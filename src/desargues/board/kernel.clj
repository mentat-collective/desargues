(ns desargues.board.kernel
  "Pure layer of boards: from a BoardSpec to a kernel PLAN. No eval, no I/O.

   A plan is everything a board kind decides:
     :outputs  the arrays the kernel fills, in ABI order (keywords)
     :form     (fn [kname] deftm-source), the kernel as data
     :layers   what plato draws by default, as data (plato.board.layer)
     :probes   where the draggable probes start
     :labels   strings derived from the math, for the page

   `plan` is OPEN: a new board kind is a new `defmethod`, never an edit here.
   Every kind shares one kernel ABI, which plato's island calls without
   knowing the kind:
     (export out1 .. outk n x0 h p1 .. pm) -> n
   out_i are byte offsets of f64 arrays of length n in the module's memory,
   x runs from x0 in steps of h, and p_j are the params in :params order.

   raster's wasm backend (v0.2.258) shapes every kernel body two ways:
   - no Long*Double arithmetic (`(* i h)` emits f64.convert_i32_s on an f64
     operand, an invalid module), so the variable is carried through the loop
     as a Double and h is a parameter; the Long index is only compared and
     incremented;
   - no type for a bare nil, so a kernel returns the sample count, not Void."
  (:require [clojure.walk :as walk]
            [raster.sym.diff :as diff]))

;; ---------------------------------------------------------------------------
;; Shared vocabulary for kernel bodies

(def ^:private math-ops
  "Bare transcendentals, qualified so deftm dispatches them through
   raster.math, which the wasm backend lowers to inline polynomials."
  {'sin 'raster.math/sin 'cos 'raster.math/cos 'tan 'raster.math/tan
   'exp 'raster.math/exp 'log 'raster.math/log 'pow 'raster.math/pow
   'sqrt 'raster.math/sqrt 'sinh 'raster.math/sinh 'cosh 'raster.math/cosh
   'tanh 'raster.math/tanh})

(defn lower
  "An author's S-expression in the vocabulary deftm compiles."
  [expr]
  (walk/postwalk #(get math-ops % %) expr))

(defn param-syms [{:keys [params]}]
  (mapv (comp symbol name :id) params))

(defn sweep-form
  "The deftm source every kind shares: sweep `var` over n samples from x0 in
   steps of h, binding `bindings` (a vector of [sym expr]) at each sample,
   then storing each output's expression. `carry` threads extra loop state:
   a vector of [sym init next-expr]."
  [kname {:keys [var] :or {var 'x} :as spec} outputs bindings stores carry]
  (let [ps (param-syms spec)
        arrays (mapv (comp symbol name) outputs)]
    `(raster.core/deftm ~kname
       (~'All [~'T]
        [~@(mapcat (fn [a] [a :- (list 'Array 'T)]) arrays)
         ~'n :- ~'Long ~'x0 :- ~'Double ~'h :- ~'Double
         ~@(mapcat (fn [p] [p :- 'Double]) ps)] :- ~'Long
        (loop [~'i 0 ~var ~'x0 ~@(mapcat (fn [[sym init]] [sym init]) carry)]
          (if (< ~'i ~'n)
            (let [~@(mapcat identity bindings)]
              ~@(map (fn [a e] (list 'aset a 'i e)) arrays stores)
              (recur (+ ~'i 1) (+ ~var ~'h) ~@(map (fn [[_ _ nxt]] nxt) carry)))
            ~'i))))))

;; ---------------------------------------------------------------------------
;; The open set of board kinds

(defmulti plan
  "BoardSpec -> kernel plan (see ns doc). Dispatches on :kind."
  :kind)

(defmethod plan :default [spec]
  (throw (ex-info (str "No board kind " (pr-str (:kind spec))
                       "; register one with (defmethod desargues.board.kernel/plan " (:kind spec) " ...)")
                  {:kind (:kind spec) :known (keys (methods plan))})))

;; ---- :calculus: f, f' (raster's symbolic derivative), running integral ----

(defn derivative
  "f' as raster's simplified S-expression."
  [{:keys [f var] :or {var 'x}}]
  (diff/differentiate f var))

(defmethod plan :calculus [{:keys [f] :as spec}]
  (let [df (derivative spec)]
    {:outputs [:xs :ys :dys :iys]
     :form (fn [kname]
             (sweep-form kname spec [:xs :ys :dys :iys]
                         [['y (lower f)]
                          ['acc '(if (> i 0) (+ acc (* 0.5 h (+ prev y))) 0.0)]]
                         [(or (:var spec) 'x) 'y (lower df) 'acc]
                         [['acc 0.0 'acc] ['prev 0.0 'y]]))
     :layers [{:layer :area :of :ys :probe :area}
              {:layer :curve :of :dys :style :dashed :color :pink :name "f′"}
              {:layer :curve :of :ys :color :blue :name "f"}
              {:layer :tangent :of :ys :slope :dys :probe :tangent}
              {:layer :integral :of :iys :probe :area}]
     :probes {:tangent 0.8 :area [-1.5 1.5]}
     :labels {:f (pr-str f) :df (pr-str df)}}))
