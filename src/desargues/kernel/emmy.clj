(ns desargues.kernel.emmy
  "desargues kernels as Emmy compilation modes.

   Needs an Emmy whose compiler is open: emmy.expression.compile/compile-code,
   the multimethod of mentat-collective/emmy#199. There a mode is a method,
   so
     (compile/compile-state-fn f params state
                               {:mode :desargues/jvm :calling-convention :primitive})
   answers the kernel desargues.kernel/compile-jvm builds: Emmy's :primitive
   function split into methods small enough for HotSpot to JIT.

   On an Emmy without the multimethod (v0.32.0 and earlier) `open-compiler?`
   is false and `register-mode!` registers nothing; callers there ask Emmy
   for :mode :source and hand the source to the kernel compiler themselves,
   as desargues.physics.lagrangian does."
  (:require [desargues.kernel :as kernel]
            [emmy.expression.compile]))

(defn- compile-code
  "Emmy's compile-code multimethod, or nil on an Emmy that predates it."
  ^clojure.lang.MultiFn []
  (some-> (resolve 'emmy.expression.compile/compile-code) deref))

(defn open-compiler?
  "True when this Emmy takes compilation modes from other libraries."
  []
  (some? (compile-code)))

(defn register-mode!
  "Register `mode` with Emmy: its code objects are rendered to Clojure source
   by Emmy's own :clj method and handed to `compile-source` (source ->
   compiled kernel). Returns true when registered, false on an Emmy whose
   compiler is closed."
  [mode compile-source]
  (if-let [mm (compile-code)]
    (do (.addMethod mm mode (fn [_ code] (compile-source ((get-method mm :clj) :clj code))))
        true)
    false))

(defn install!
  "Register the modes desargues ships: :desargues/jvm (desargues.kernel/
   compile-jvm). Engine backends add their own with register-mode! when they
   load (:desargues/raster, from desargues.infrastructure.raster-adapter).
   Returns true when Emmy's compiler is open."
  []
  (register-mode! :desargues/jvm kernel/compile-jvm))

(install!)
