(ns desargues.board.jvm
  "Boundary adapter: the JVM as a KernelCompiler of desargues.board.compiler.

   A board kernel is deftm source (desargues.board.kernel/sweep-form) whose
   body is plain Clojure (loop, let, aset, arithmetic) over raster's math
   vocabulary. This compiler rewrites that vocabulary to java.lang.Math and
   evaluates the body as an ordinary Clojure fn with the kernel's ABI:

     (kernel out1 .. outk n x0 h p1 .. pm) -> n

   so desargues.board/sample-frame runs it unchanged and a board is sampled
   on any JVM with NO raster on the classpath (no :dynamics alias). It emits
   no WebAssembly: `wasm-bytes` throws {:reason :unsupported}. Use it to read
   a board's numbers (desargues.board/sample-board, desargues.board.picture);
   use the :raster compiler to ship a board to a page."
  (:require [clojure.walk :as walk]
            [desargues.board.compiler :as compiler]))

(def ^:private jvm-vocabulary
  "raster's math vocabulary (desargues.kernel/realize :raster), on the JVM."
  '{raster.math/sin Math/sin raster.math/cos Math/cos raster.math/tan Math/tan
    raster.math/exp Math/exp raster.math/log Math/log
    raster.math/sinh Math/sinh raster.math/cosh Math/cosh raster.math/tanh Math/tanh
    raster.math/asin Math/asin raster.math/acos Math/acos raster.math/atan Math/atan
    raster.numeric/pow Math/pow
    aset clojure.core/aset-double})

(defn- lower
  "A kernel body in raster's vocabulary as JVM Clojure."
  [body]
  (walk/postwalk (fn [x] (if (symbol? x) (get jvm-vocabulary x x) x)) body))

(defn jvm-form
  "The deftm `form` (desargues.board.kernel/sweep-form) as the source of a
   Clojure fn taking the same arguments in the same order. They arrive as a
   rest seq: a kernel has one argument per output and per param, often more
   than a Clojure fn's 20 positional parameters."
  [[_deftm _kname [_all _tvars args _ _ret body]]]
  (let [syms (vec (take-nth 3 args))
        argv (gensym "args")]
    (list 'fn ['& argv]
          (list 'let [syms (list 'clojure.core/vec argv)]
                (lower body)))))

(defrecord JvmCompiler [ns-sym]
  compiler/KernelCompiler
  (define-kernel [_ kname form]
    (let [f (binding [*ns* (the-ns ns-sym)] (eval (jvm-form form)))]
      (intern ns-sym kname f)))
  (wasm-bytes [_ _kernel export]
    (throw (ex-info (str "The :jvm kernel compiler emits no WebAssembly (kernel " export
                         "); compile with the :raster backend")
                    {:reason :unsupported :export export}))))

(defn jvm-compiler
  "A JvmCompiler interning kernels into `ns-sym` (default
   desargues.board.jvm-kernels, created on demand)."
  ([] (jvm-compiler 'desargues.board.jvm-kernels))
  ([ns-sym]
   (create-ns ns-sym)
   (binding [*ns* (the-ns ns-sym)] (refer-clojure))
   (->JvmCompiler ns-sym)))

(defmethod compiler/kernel-compiler :jvm
  ([_] (jvm-compiler))
  ([_ {:keys [ns]}] (if ns (jvm-compiler ns) (jvm-compiler))))
