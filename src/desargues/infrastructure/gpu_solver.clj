(ns desargues.infrastructure.gpu-solver
  "Boundary adapter: batched RK4 on a GPU, as the :gpu backend.

   raster compiles a raster.par/map-void! deftm to an OpenCL (or Level Zero)
   kernel through Panama, no JNI: one work item per trajectory. Two kernels:
   a compiled Lagrangian system's Emmy derivative inside the batched RK4
   (desargues.infrastructure.raster-kernels/gpu-kernel), and the damped
   pendulum with per-trajectory g/L and damping (pendulum-par-rk4). Systems
   with neither go to the JVM stepper.

   A solver owns one raster.gpu session, opened on first use: kernels compile
   once per solver (~50 ms each), buffers live only for the call that made
   them. Close the solver (it is java.io.Closeable) to release the device.
   Calls on one solver are serialised; the session is not thread-safe.

   The GPU pays where batches are wide: pendulums x 800 steps on an RTX 4070
   Laptop, CPU kernel vs GPU, 64: 3.5 vs 3.6 ms; 1024: 67 vs 18 ms; 8192:
   456 vs 79 ms. Values track the CPU kernel to ~1e-14 (the device's sin and
   its fused multiply-adds round differently), not bit for bit.

   raster is on the classpath under the :dynamics alias; a device needs an
   OpenCL ICD (or Level Zero) driver. raster opens a session lazily, so a
   missing driver surfaces at the first kernel build of the first call;
   `device-available?` asks beforehand. A device of a backend raster does
   not know fails at once, as ex-info {:reason :no-device :device ..}."
  (:require [desargues.infrastructure.raster-adapter :as ra]
            [desargues.infrastructure.raster-kernels :as rk]
            [desargues.physics :as phys]
            [raster.gpu :as gpu]))

(def default-device
  "The device a solver opens when none is named: the first OpenCL device."
  :ocl:0)

(defn- open-session
  "A raster.gpu session on `device`, or an ex-info naming the device and why
   it could not open."
  [device]
  (try
    (gpu/make-session device)
    (catch Throwable t
      (throw (ex-info (str "GPU device " (pr-str device) " is not available (" (.getMessage t) ")")
                      {:device device :reason :no-device} t)))))

(defn- launch!
  "Compile kernel `kvar` under `k` in `sess` unless it is there, upload
   `arrays` ({param-name double[]}), allocate `outputs` ({param-name length}),
   run it over nb work items with `scalars`, and return {param-name double[]}
   of the outputs. Every buffer this call made is freed before it returns."
  [sess k kvar arrays outputs scalars nb]
  (when-not (get-in @sess [:kernels k])
    (gpu/compile! sess k kvar {:dtype :double}))
  (let [bufs (merge (into {} (map (fn [[p ^doubles a]] [p [:double (alength a) a]])) arrays)
                    (into {} (map (fn [[p len]] [p [:double len nil]])) outputs))
        keys-of (into {} (map (fn [p] [p (keyword (str "desargues-" p))])) (keys bufs))]
    (try
      (gpu/alloc! sess (into {} (map (fn [[p spec]] [(keys-of p) spec])) bufs))
      (gpu/invoke! sess k keys-of scalars nb)
      (into {} (map (fn [p] [p (double-array (gpu/download sess (keys-of p)))])) (keys outputs))
      (finally
        (doseq [bk (vals keys-of)]
          (when (get-in @sess [:buffers bk]) (gpu/free-buffer! sess bk)))))))

(defn- emmy-run
  "The plan :run for Emmy kernel source `src` on the solver's session."
  [session-of src]
  (let [{:keys [var n]} (rk/gpu-kernel src)
        n (long n)]
    (fn [^doubles u0s nb ^doubles out steps dt _]
      (let [nb (long nb)
            {o "out"} (launch! (session-of) [:rk4 src] var
                               {"u0s" u0s}
                               {"out" (alength out) "st" (* nb n) "s" (* nb n) "ks" (* nb 4 n)}
                               [{:type :double :value (double dt)} {:type :int :value (int steps)}]
                               nb)]
        (System/arraycopy o 0 out 0 (alength out))))))

(defn- pendulum-run [session-of]
  (fn [^doubles u0s nb ^doubles out steps dt params]
    (let [{o "out"} (launch! (session-of) [:pendulum] #'rk/pendulum-par-rk4
                             {"u0s" u0s
                              "gl" (double-array (map first params))
                              "mu" (double-array (map second params))}
                             {"out" (alength out)}
                             [{:type :double :value (double dt)} {:type :int :value (int steps)}]
                             nb)]
      (System/arraycopy o 0 out 0 (alength ^doubles out)))))

(defn- plan
  "How the GPU solver runs `system`, in raster-adapter/run-group's plan
   shape, or nil when it has no GPU kernel."
  [session-of system]
  (cond
    (:kernel-source system)
    (let [src (:kernel-source system)]
      {:key [:emmy src] :n (:n (rk/gpu-kernel src))
       :u0 (:state (phys/array-kernel system))
       :labels (vec (phys/state-labels system))
       :run (emmy-run session-of src)})

    (ra/double-pendulum? system)
    {:key [:pendulum] :n 2
     :u0 (double-array [(:theta system) (:omega system)])
     :params [(/ (double (:gravity system)) (double (:length system))) (double (:damping system))]
     :labels [:theta :omega]
     :run (pendulum-run session-of)}))

(defrecord GpuSolver [device session lock]
  phys/TrajectorySolver
  (solve-trajectory [this system opts]
    (first (phys/solve-batch this [system] opts)))

  phys/BatchTrajectorySolver
  (solve-batch [_ systems {:keys [dt duration] :or {dt 0.01 duration 10.0}}]
    (locking lock
      (let [dt (double dt) duration (double duration)
            session-of (fn [] (or @session (reset! session (open-session device))))
            plans (mapv #(plan session-of %) systems)
            fallback (phys/stepper-solver phys/rk4)
            groups (group-by (fn [i] (some-> (plans i) :key)) (range (count systems)))
            results (reduce-kv
                     (fn [acc k idxs]
                       (if k
                         (into acc (map vector idxs (ra/run-group (mapv plans idxs) dt duration)))
                         (into acc (map (fn [i] [i (phys/solve-trajectory fallback (systems i) {:dt dt :duration duration})]) idxs))))
                     {} groups)]
        (mapv results (range (count systems))))))

  java.io.Closeable
  (close [_]
    (locking lock
      (when-let [s @session]
        (gpu/close-session! s)
        (reset! session nil)))))

(defn gpu-solver
  "A TrajectorySolver (and BatchTrajectorySolver, and java.io.Closeable)
   integrating on GPU `device` (default :ocl:0; :ze:0 for Level Zero):
   compiled Lagrangian systems and all-Double pendulums, each kind sharing
   one kernel launch, the stepper's time grid exactly. The device opens on
   the first call; close the solver to release it."
  ([] (gpu-solver default-device))
  ([device] (->GpuSolver device (atom nil) (Object.))))

(defn device-available?
  "True when `device` runs a kernel end to end: a solver on it integrates one
   pendulum for two steps (session, kernel build, buffer upload, launch,
   download). raster opens sessions lazily and its kernel build does not
   reach the driver either, so only a real launch proves an OpenCL ICD and a
   platform are present. raster does not check a device's index: an :ocl:N
   past the last device still binds the runtime it initialised."
  ([] (device-available? default-device))
  ([device]
   (try
     (with-open [gs ^java.io.Closeable (gpu-solver device)]
       (boolean (seq (phys/solve-trajectory gs (phys/make-pendulum {}) {:dt 0.01 :duration 0.02}))))
     (catch Throwable _no-device false))))

;; ── the :gpu backend's port methods (desargues.backend) ──────────────────────

(defmethod phys/solver :gpu
  ([_] (gpu-solver))
  ([_ {:keys [device]}] (gpu-solver (or device default-device))))
