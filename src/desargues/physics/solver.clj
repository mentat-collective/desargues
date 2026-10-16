(ns desargues.physics.solver
  "OPEN stratum of the physics bounded context: the `solver` port,
   backend key -> TrajectorySolver.

   A new engine is ONE defmethod in its own namespace, registered with
   desargues.backend/register-backend! (raster, wasm and gpu live in
   desargues.infrastructure.*); nothing here is edited (OCP). :jvm is the
   built-in stepper.

   Requires desargues.physics.stepper and desargues.backend (JVM registry,
   hence .clj)."
  (:require [desargues.backend :as backend]
            [desargues.physics.stepper :as stepper]))

(defmulti solver
  "backend key, opts -> a TrajectorySolver (see desargues.backend).

     (solver :jvm)                      the built-in stepper, rk4
     (solver :jvm {:integrator euler})  the stepper on another Integrator
     (solver :raster {:alg :tsit5})     raster's ODE solvers
     (solver :raster {:kernel? true})   the whole RK4 loop in one raster kernel

   opts are the backend's own. A new engine is a defmethod in its own
   namespace, registered with desargues.backend/register-backend!."
  (fn [backend & _] backend))

(defmethod solver :jvm
  ([_] (stepper/stepper-solver))
  ([_ {:keys [integrator]}] (if integrator (stepper/stepper-solver integrator) (stepper/stepper-solver))))

(defmethod solver :default [backend & args]
  (backend/dispatch-default solver "trajectory solver" backend args))

(defn ->solver
  "A TrajectorySolver from what a caller may pass as :solver: a
   TrajectorySolver, a backend key, or [backend-key opts]."
  [x]
  (cond
    (keyword? x) (solver x)
    (and (vector? x) (keyword? (first x))) (apply solver x)
    :else x))
