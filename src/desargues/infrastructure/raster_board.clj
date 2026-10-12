(ns desargues.infrastructure.raster-board
  "Boundary adapter: raster as the :raster KernelCompiler of
   desargues.board.compiler. Kernels (deftm source) are evaluated into a
   namespace that refers raster's vocabulary, and compiled to f64 WebAssembly
   by raster's pipeline. Only loaded when the :raster backend is asked for;
   raster is on the classpath under the :dynamics alias."
  (:require [desargues.board.compiler :as compiler]
            [raster.compiler.pipeline :as pl]))

(defrecord RasterCompiler [ns-sym]
  compiler/KernelCompiler
  (define-kernel [_ kname form]
    (binding [*ns* (the-ns ns-sym)]
      (eval form))
    (ns-resolve ns-sym kname))
  (wasm-bytes [_ kernel export]
    (byte-array (:bytes (pl/compile-wasm kernel :name export :dtype :double)))))

(defn raster-compiler
  "A RasterCompiler evaluating kernels into `ns-sym` (default
   desargues.board.kernels, created on demand)."
  ([] (raster-compiler 'desargues.board.kernels))
  ([ns-sym]
   (create-ns ns-sym)
   (binding [*ns* (the-ns ns-sym)]
     (refer-clojure)
     (require '[raster.core] '[raster.math] '[raster.numeric]))
   (->RasterCompiler ns-sym)))

(defmethod compiler/kernel-compiler :raster
  ([_] (raster-compiler))
  ([_ {:keys [ns]}] (if ns (raster-compiler ns) (raster-compiler))))
