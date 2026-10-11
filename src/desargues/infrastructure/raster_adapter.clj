(ns desargues.infrastructure.raster-adapter
  "Boundary adapter: the raster engine (io.github.replikativ/raster) as
   - a TrajectorySolver provider for desargues.videos.physics, and
   - a GeometryKernel provider for desargues.geometry.kernel (batched
     homography-over-cloud and leaf-space lerp grids, as deftm loops).

   The PhysicalSystem's `derivatives` is the pure kernel raster integrates;
   this adapter projects raster's solution back into the domain Trajectory
   shape ({:time t :state {label value ...}}). Nothing above this namespace
   sees a raster type. Only this file requires raster, and raster is only on
   the classpath under the :dynamics alias."
  (:require [desargues.geometry.kernel :as kernel]
            [desargues.videos.physics :as phys]
            [raster.arrays :as ra]
            [raster.core :refer [deftm ftm]]
            [raster.ode :as ode]
            [raster.ode.core :as ode-core]))

(defn- state->array
  ^doubles [labels state]
  (double-array (map #(double (get state %)) labels)))

(defn- array->state
  [labels ^doubles u]
  (zipmap labels (seq u)))

(defn grid
  "Sample times 0, dt, 2dt, ... up to and including `duration`."
  [dt duration]
  (let [dt       (double dt)
        duration (double duration)
        n        (long (Math/floor (+ (/ duration dt) 1e-6)))
        ts       (mapv #(* % dt) (range (inc n)))]
    (if (< (- duration (peek ts)) (* 1e-9 dt)) ts (conj ts duration))))

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

(defrecord RasterSolver [alg resample?]
  phys/TrajectorySolver
  (solve-trajectory [_ system {:keys [dt duration] :or {dt 0.01 duration 10.0}}]
    (let [labels (vec (phys/state-labels system))
          f      (rhs system)
          u0     (state->array labels (phys/get-state system))
          prob   (ode/ode-problem f u0 0.0 (double duration))
          sol    (ode/solve alg prob (double dt))
          {:keys [ts us]} (if resample?
                            (ode/saveat (ode/dense-output f sol) (grid dt duration))
                            sol)]
      (mapv (fn [t u] {:time (double t) :state (array->state labels u)}) ts us))))

(defn raster-solver
  "A TrajectorySolver backed by raster.
   alg: :rk4 (default) / :euler, fixed step on the :dt grid;
        :tsit5 / :dp5, adaptive with :dt as the initial step;
        or any raster ODE algorithm value.
   opts {:resample? bool}: when true (the default for :tsit5 / :dp5) the
   solution is interpolated (raster dense-output) onto the 0, dt, 2dt, ...
   duration grid, so every algorithm yields the same time points; false
   returns the solver's own accepted steps."
  ([] (raster-solver :rk4))
  ([alg] (raster-solver alg {}))
  ([alg {:keys [resample?] :or {resample? (contains? #{:tsit5 :dp5} alg)}}]
   (->RasterSolver
    (case alg
      :rk4   (ode-core/->RK4)
      :euler (ode-core/->Euler)
      :tsit5 (ode/tsit5)
      :dp5   (ode/dp5)
      alg)
    (boolean resample?))))

(deftm h-cloud!
  [h :- (Array double) xs :- (Array double) ys :- (Array double)
   ox :- (Array double) oy :- (Array double) ow :- (Array double)] :- (Array double)
  (let [n (ra/alength xs)]
    (loop [i (int 0)]
      (if (>= i n)
        ow
        (let [x (ra/aget xs i) y (ra/aget ys i)
              w (+ (+ (* (ra/aget h 6) x) (* (ra/aget h 7) y)) (ra/aget h 8))]
          (ra/aset ow i w)
          (ra/aset ox i (/ (+ (+ (* (ra/aget h 0) x) (* (ra/aget h 1) y)) (ra/aget h 2)) w))
          (ra/aset oy i (/ (+ (+ (* (ra/aget h 3) x) (* (ra/aget h 4) y)) (ra/aget h 5)) w))
          (recur (unchecked-inc-int i)))))))

(deftm lerp-grid-into!
  [a :- (Array double) b :- (Array double) out :- (Array double) samples :- Long] :- (Array double)
  (let [l (ra/alength a)
        denom (double (+ samples 1))]
    (loop [s (long 0)]
      (if (>= s samples)
        out
        (let [t (/ (double (+ s 1)) denom)
              base (* s l)]
          (loop [j (int 0)]
            (when (< j l)
              (ra/aset out (+ base j) (+ (ra/aget a j) (* t (- (ra/aget b j) (ra/aget a j)))))
              (recur (unchecked-inc-int j))))
          (recur (+ s 1)))))))

(defrecord RasterKernel []
  kernel/GeometryKernel
  (transform-cloud! [_ h xs ys ox oy ow] (h-cloud! h xs ys ox oy ow))
  (lerp-grid! [_ a b out samples] (lerp-grid-into! a b out (long samples))))

(defn geometry-kernel
  "A desargues.geometry.kernel/GeometryKernel whose batched loops are raster
   deftm functions."
  []
  (->RasterKernel))
