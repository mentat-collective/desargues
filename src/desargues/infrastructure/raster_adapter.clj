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
            [desargues.kernel.emmy :as emmy-modes]
            [desargues.videos.physics :as phys]
            [raster.arrays :as ra]
            [raster.core :refer [deftm ftm]]
            [raster.ode :as ode]
            [raster.ode.core :as ode-core]
            [desargues.infrastructure.raster-kernels :as rk]))

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

(defn double-pendulum?
  "A physics/Pendulum whose state and parameters are all Doubles: the shape
   the stepper's double fast path takes, so the kernel can match it bitwise.
   The engines that run the hand-written pendulum kernels (this adapter's
   kernel solver, the GPU solver) recognise it by this predicate."
  [system]
  (and (instance? desargues.videos.physics.Pendulum system)
       (every? #(instance? Double %) ((juxt :theta :omega :gravity :length :damping) system))))

(defn- kernel-source
  "The Emmy kernel source a compiled Lagrangian system carries, or nil."
  [system]
  (:kernel-source system))

(defn- step-times
  "The times the stepper visits for dt over duration: 0, then t+dt until
   t >= duration, accumulated exactly as StepperSolver does."
  [^double dt ^double duration]
  (loop [t 0.0 ts (transient [0.0])]
    (if (>= t duration)
      (persistent! ts)
      (let [t' (+ t dt)] (recur t' (conj! ts t'))))))

(defn- kernel-plan
  "How the kernel solver runs `system`: {:key :n :u0 :labels :params :run}
   where (run u0s nb out steps dt params) integrates nb stacked states, or nil
   when the system has no raster kernel. :key groups systems one call can
   batch; :labels name the state slots (state-labels order)."
  [system]
  (cond
    (kernel-source system)
    (let [src (kernel-source system)
          {:keys [rk4 n]} (rk/kernels src)]
      {:key [:emmy src] :n n :u0 (:state (phys/array-kernel system))
       :labels (vec (phys/state-labels system))
       :run (fn [u0s nb out steps dt _] (rk4 u0s (long nb) out (long steps) (double dt)))})

    (double-pendulum? system)
    {:key [:pendulum] :n 2
     :u0 (double-array [(:theta system) (:omega system)])
     :params [(/ (double (:gravity system)) (double (:length system))) (double (:damping system))]
     :labels [:theta :omega]
     :run (fn [u0s nb out steps dt params]
            (rk/pendulum-rk4 u0s (long nb) (double-array (map first params)) (double-array (map second params))
                             out (long steps) (double dt)))}))

(defn run-group
  "Integrate systems sharing one kernel (their plans) in one kernel call. Each
   row of the kernel's output becomes {label value} over (:labels plan), built
   straight from the output array. A plan is {:n :u0 :labels :params :run},
   (run u0s nb out steps dt params) filling out with the batched-RK4 layout
   (see desargues.infrastructure.raster-kernels); any engine that fills that
   layout (a JVM kernel, a wasm module) shares this projection and the
   stepper's time grid."
  [plans dt duration]
  (let [{:keys [n run]} (first plans)
        n (long n)
        ts (step-times dt duration)
        steps (dec (count ts))
        rows (inc steps)
        nb (count plans)
        u0s (double-array (* nb n))
        out (double-array (* nb rows n))]
    (dotimes [b nb] (System/arraycopy ^doubles (:u0 (nth plans b)) 0 u0s (* b n) n))
    (run u0s nb out steps dt (mapv :params plans))
    (mapv (fn [b {:keys [labels]}]
            (let [labels (object-array labels)]
              (loop [i 0 ts (seq ts) acc (transient [])]
                (if (< i rows)
                  (let [off (* (+ (* b rows) i) n)
                        state (loop [j 0 m (transient {})]
                                (if (< j n)
                                  (recur (inc j) (assoc! m (aget labels j) (aget out (+ off j))))
                                  (persistent! m)))]
                    (recur (inc i) (next ts) (conj! acc {:time (first ts) :state state})))
                  (persistent! acc)))))
          (range nb) plans)))

(defrecord RasterKernelSolver []
  phys/TrajectorySolver
  (solve-trajectory [this system opts]
    (first (phys/solve-batch this [system] opts)))
  phys/BatchTrajectorySolver
  (solve-batch [_ systems {:keys [dt duration] :or {dt 0.01 duration 10.0}}]
    (let [dt (double dt) duration (double duration)
          plans (mapv kernel-plan systems)
          fallback (phys/stepper-solver phys/rk4)
          groups (group-by (fn [i] (some-> (plans i) :key)) (range (count systems)))
          results (reduce-kv
                   (fn [acc k idxs]
                     (if k
                       (into acc (map vector idxs (run-group (mapv plans idxs) dt duration)))
                       (into acc (map (fn [i] [i (phys/solve-trajectory fallback (systems i) {:dt dt :duration duration})]) idxs))))
                   {} groups)]
      (mapv results (range (count systems))))))

(defn kernel-solver
  "A TrajectorySolver (and BatchTrajectorySolver) whose fixed-step RK4 runs
   entirely inside a raster deftm kernel: compiled Lagrangian systems (from
   their Emmy kernel source) and all-Double pendulums. Answers the same
   trajectory as phys/stepper-solver rk4, bit for bit; systems without a
   kernel go to that stepper. solve-batch integrates every system that shares
   a kernel in ONE kernel call (N initial conditions, pendulum parameter
   sweeps)."
  []
  (->RasterKernelSolver))

(defn kernel-rhs
  "A raster RHS that calls a compiled deftm derivative directly (no state map,
   no Clojure fn per evaluation), or nil when `system` has none: a compiled
   Lagrangian system or an all-Double pendulum."
  [system]
  (cond
    (kernel-source system) ((:rhs-fn (rk/kernels (kernel-source system))))
    (double-pendulum? system)
    (let [gl (/ (double (:gravity system)) (double (:length system)))
          mu (double (:damping system))]
      (ftm [du :- (Array double), u :- (Array double), t :- Double]
        (rk/pendulum-deriv du u gl mu)))))

(defn rhs
  "raster right-hand side (du u t) for `system`. The state vector is ordered
   by (state-labels system); every derivative the system does not report is
   taken as 0. Systems with a compiled kernel (see kernel-rhs) get it; the
   rest go through `derivatives` on a state map per evaluation."
  [system]
  (or (kernel-rhs system)
      (let [labels (vec (phys/state-labels system))
            n      (count labels)
            fill!  (fn [^doubles du ^doubles u _t]
                     (let [derivs (phys/derivatives system (array->state labels u))]
                       (dotimes [i n]
                         (aset du i (double (get derivs (labels i) 0.0))))))]
        (ftm [du :- (Array double), u :- (Array double), t :- Double]
          (fill! du u t)))))

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

;; ── the :raster backend's port methods (desargues.backend) ───────────────────

(defmethod phys/solver :raster
  ([_] (raster-solver))
  ([_ {:keys [alg kernel?] :as opts}]
   (if kernel?
     (kernel-solver)
     (raster-solver (or alg :rk4) (select-keys opts [:resample?])))))

(defmethod kernel/provider :raster [_] (geometry-kernel))

;; On an Emmy with an open compiler, (compile-state-fn .. {:mode
;; :desargues/raster :calling-convention :primitive}) answers the raster
;; kernels of the function: {:n :rk4 :deriv :nscratch :rhs-fn}.
(emmy-modes/register-mode! :desargues/raster rk/kernels)
