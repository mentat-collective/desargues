(ns desargues.board.compiler
  "Protocol layer of boards: the port through which a kernel plan becomes
   something that runs.

   KernelCompiler is deliberately small (ISP): turn a plan's form into a
   callable kernel on this host, and turn that kernel into WebAssembly bytes.
   Everything upstream (desargues.board.kernel) is pure data and knows no
   compiler; everything downstream (desargues.board) holds a KernelCompiler
   and never learns which one (LSP).

   `kernel-compiler` answers a KernelCompiler by backend key (see
   desargues.backend). No engine is required here: the :raster compiler is
   desargues.infrastructure.raster-board, loaded when :raster is asked for. A
   second backend is a record and a defmethod in its own namespace."
  (:require [desargues.backend :as backend]))

(defprotocol KernelCompiler
  (define-kernel [compiler kname form]
    "Make `form` (a kernel's deftm source) callable on this host under the
     name `kname`; return the callable (a var).")
  (wasm-bytes [compiler kernel export]
    "The WebAssembly module of `kernel` with its entry point exported as
     `export`, as a byte array."))

(defmulti kernel-compiler
  "backend key, opts -> a KernelCompiler. opts are the backend's own (the
   :raster compiler takes {:ns sym}, the namespace kernels are evaluated
   into)."
  (fn [backend & _] backend))

(defmethod kernel-compiler :default [backend & args]
  (backend/dispatch-default kernel-compiler "board kernel compiler" backend args))

(defn raster-compiler
  "The raster backend's KernelCompiler: (kernel-compiler :raster {:ns ns-sym}).
   Kernels are evaluated into `ns-sym` (default desargues.board.kernels) and
   compiled to f64 wasm."
  ([] (kernel-compiler :raster))
  ([ns-sym] (kernel-compiler :raster {:ns ns-sym})))
