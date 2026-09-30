(ns desargues.board.compiler
  "Protocol layer of boards: the port through which a kernel plan becomes
   something that runs.

   KernelCompiler is deliberately small (ISP): turn a plan's form into a
   callable kernel on this host, and turn that kernel into WebAssembly bytes.
   Everything upstream (desargues.board.kernel) is pure data and knows no
   compiler; everything downstream (desargues.board) holds a KernelCompiler
   and never learns which one (LSP). RasterCompiler is the one backend today;
   a second is a new record, not an edit."
  (:require [raster.compiler.pipeline :as pl]))

(defprotocol KernelCompiler
  (define-kernel [compiler kname form]
    "Make `form` (a kernel's deftm source) callable on this host under the
     name `kname`; return the callable (a var).")
  (wasm-bytes [compiler kernel export]
    "The WebAssembly module of `kernel` with its entry point exported as
     `export`, as a byte array."))

(defrecord RasterCompiler [ns-sym]
  KernelCompiler
  (define-kernel [_ kname form]
    (binding [*ns* (the-ns ns-sym)]
      (eval form))
    (ns-resolve ns-sym kname))
  (wasm-bytes [_ kernel export]
    (byte-array (:bytes (pl/compile-wasm kernel :name export :dtype :double)))))

(defn raster-compiler
  "The raster backend: kernels are evaluated into `ns-sym` (default
   desargues.board.kernels, created on demand) and compiled to f64 wasm."
  ([] (raster-compiler 'desargues.board.kernels))
  ([ns-sym]
   (create-ns ns-sym)
   (binding [*ns* (the-ns ns-sym)]
     (refer-clojure)
     (require '[raster.core] '[raster.math]))
   (->RasterCompiler ns-sym)))
