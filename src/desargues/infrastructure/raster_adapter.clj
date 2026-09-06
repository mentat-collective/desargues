(ns desargues.infrastructure.raster-adapter
  "Boundary adapter: the raster ODE engine (io.github.replikativ/raster) as a
   TrajectorySolver provider for desargues.videos.physics.

   The PhysicalSystem's `derivatives` is the pure kernel raster integrates;
   this adapter projects raster's solution back into the domain Trajectory
   shape ({:time t :state {label value ...}}). Nothing above this namespace
   sees a raster type. Only this file requires raster, and raster is only on
   the classpath under the :dynamics alias."
  (:require [desargues.videos.physics :as phys]
            [raster.core :refer [ftm]]
            [raster.ode :as ode]
            [raster.ode.core :as ode-core]))

(defn- state->array
  ^doubles [labels state]
  (double-array (map #(double (get state %)) labels)))

(defn- array->state
  [labels ^doubles u]
  (zipmap labels (seq u)))

(defn rhs
  "raster right-hand side (du u t) for `system`. The state vector is ordered
   by (state-labels system); every derivative the system does not report is
   taken as 0."
  [system]
  (let [labels (vec (phys/state-labels system))
        n      (count labels)
        fill!  (fn [^doubles du ^doubles u _t]
                 (let [derivs (phys/derivatives system (array->state labels u))]
                   (dotimes [i n]
                     (aset du i (double (get derivs (labels i) 0.0))))))]
    (ftm [du :- (Array double), u :- (Array double), t :- Double]
      (fill! du u t))))

(defrecord RasterSolver [alg]
  phys/TrajectorySolver
  (solve-trajectory [_ system {:keys [dt duration] :or {dt 0.01 duration 10.0}}]
    (let [labels (vec (phys/state-labels system))
          u0     (state->array labels (phys/get-state system))
          prob   (ode/ode-problem (rhs system) u0 0.0 (double duration))
          {:keys [ts us]} (ode/solve alg prob (double dt))]
      (mapv (fn [t u] {:time t :state (array->state labels u)}) ts us))))

(defn raster-solver
  "A TrajectorySolver backed by raster.
   alg: :rk4 (default; fixed step at :dt, points on the dt grid),
        :euler (fixed step),
        :tsit5 / :dp5 (adaptive; :dt is the initial step and points land on
        the adaptive grid, so sample with physics/sample-trajectory),
        or any raster ODE algorithm value."
  ([] (raster-solver :rk4))
  ([alg]
   (->RasterSolver
    (case alg
      :rk4   (ode-core/->RK4)
      :euler (ode-core/->Euler)
      :tsit5 (ode/tsit5)
      :dp5   (ode/dp5)
      alg))))
