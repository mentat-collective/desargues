(ns desargues.infrastructure.wasm-runtime
  "Boundary adapter: Emmy-derived trajectories as WebAssembly, and the :wasm
   backend that runs them on the JVM.

   raster compiles a compiled Lagrangian system's batched RK4 to a wasm module
   (desargues.infrastructure.raster-kernels/wasm-kernel). `trajectory-module`
   hands that module to whoever hosts it (a browser, node, any wasm engine);
   `wasm-solver` runs it here through Chicory, a pure-JVM wasm engine, so the
   module the browser gets is the module the tests integrate with.

   Chicory's AOT compiler (com.dylibso.chicory/compiler, wasm -> JVM bytecode)
   is used when it is on the classpath, its interpreter otherwise; both answer
   the same values. This engine exists for portability and proof, not speed:
   on the JVM the raster kernel solver is ~100x faster, and V8 runs the same
   module at about the kernel's speed.

   Only this namespace requires Chicory; it is on the classpath under the
   :dynamics alias, with raster."
  (:require [desargues.infrastructure.raster-adapter :as ra]
            [desargues.infrastructure.raster-kernels :as rk]
            [desargues.physics :as phys])
  (:import (com.dylibso.chicory.runtime ExportFunction Instance Memory)
           (com.dylibso.chicory.wasm Parser WasmModule)))

(defn trajectory-module
  "The wasm module integrating `system` (a compiled Lagrangian system, which
   carries its Emmy :kernel-source), or nil when the system has none:
     {:bytes :export :n :work :labels :u0}
   :labels name the state slots in order and :u0 is the initial state; the
   export's ABI is raster-kernels/wasm-kernel's."
  [system]
  (when-let [src (:kernel-source system)]
    (assoc (rk/wasm-kernel src)
           :labels (vec (phys/state-labels system))
           :u0 (:state (phys/array-kernel system)))))

(def ^:private aot-compile
  "Chicory's MachineFactoryCompiler/compile, or nil when the compiler is not
   on the classpath (the interpreter then runs every module)."
  (delay
    (try
      (let [c (Class/forName "com.dylibso.chicory.compiler.MachineFactoryCompiler")
            m (.getMethod c "compile" (into-array Class [WasmModule]))]
        (fn [module] (.invoke m nil (object-array [module]))))
      (catch ClassNotFoundException _compiler-not-on-classpath nil))))

(def ^:private parsed
  "module bytes -> {:module WasmModule :machine factory-or-nil}: parsing and
   AOT compilation happen once per module; instances are made per call."
  (atom {}))

(defn- prepared [^bytes bs]
  (let [k (vec bs)]
    (or (get @parsed k)
        (let [module (Parser/parse bs)
              p {:module module :machine (when-let [compile @aot-compile] (compile module))}]
          (get (swap! parsed (fn [m] (if (contains? m k) m (assoc m k p)))) k)))))

(defn- instantiate ^Instance [^bytes bs]
  (let [{:keys [module machine]} (prepared bs)
        b (Instance/builder ^WasmModule module)]
    (.build (if machine (.withMachineFactory b machine) b))))

(defn run-rk4!
  "Run wasm-kernel `k`'s export on a fresh instance: nb initial states
   back to back in u0s, steps fixed RK4 steps of dt, into `out` (nb x
   (steps+1) x n doubles, the raster-kernels layout). Returns out."
  ^doubles [{:keys [bytes export n]} ^doubles u0s nb ^doubles out steps dt]
  (let [inst (instantiate bytes)
        ^Memory mem (.memory inst)
        n (long n) nb (long nb)
        p-out (* 8 (alength u0s))
        p-st (+ p-out (* 8 (alength out)))
        p-s (+ p-st (* 8 n))
        p-ks (+ p-s (* 8 n))
        need (+ p-ks (* 8 4 n))
        have (* 65536 (long (.pages mem)))]
    (when (< have need)
      (.grow mem (int (quot (+ (- need have) 65535) 65536))))
    (dotimes [i (alength u0s)] (.writeF64 mem (* 8 i) (aget u0s i)))
    (.apply ^ExportFunction (.export inst ^String export)
            (long-array [0 nb p-out steps (Double/doubleToRawLongBits (double dt)) p-st p-s p-ks]))
    (dotimes [i (alength out)] (aset out i (.readDouble mem (+ p-out (* 8 i)))))
    out))

(defn- wasm-plan
  "How the wasm solver runs `system`, in raster-adapter/run-group's plan
   shape, or nil when it has no Emmy kernel source."
  [system]
  (when-let [{:keys [n labels u0] :as k} (trajectory-module system)]
    {:key [:wasm (:kernel-source system)] :n n :u0 u0 :labels labels
     :run (fn [u0s nb out steps dt _] (run-rk4! k u0s nb out steps dt))}))

(defrecord WasmSolver []
  phys/TrajectorySolver
  (solve-trajectory [this system opts]
    (first (phys/solve-batch this [system] opts)))
  phys/BatchTrajectorySolver
  (solve-batch [_ systems {:keys [dt duration] :or {dt 0.01 duration 10.0}}]
    (let [dt (double dt) duration (double duration)
          plans (mapv wasm-plan systems)
          fallback (phys/stepper-solver phys/rk4)
          groups (group-by (fn [i] (some-> (plans i) :key)) (range (count systems)))
          results (reduce-kv
                   (fn [acc k idxs]
                     (if k
                       (into acc (map vector idxs (ra/run-group (mapv plans idxs) dt duration)))
                       (into acc (map (fn [i] [i (phys/solve-trajectory fallback (systems i) {:dt dt :duration duration})]) idxs))))
                   {} groups)]
      (mapv results (range (count systems))))))

(defn wasm-solver
  "A TrajectorySolver (and BatchTrajectorySolver) that integrates compiled
   Lagrangian systems in their wasm module on Chicory: the stepper's RK4 time
   grid exactly, values within rounding of it (sin/cos come from
   desargues.kernel.libm, within 2^-53 of Math; ~1e-13 over 3 s of a chaotic
   double pendulum). Systems sharing a kernel run in one module call; systems
   without one go to the JVM stepper."
  []
  (->WasmSolver))

;; ── the :wasm backend's port methods (desargues.backend) ─────────────────────

(defmethod phys/solver :wasm
  ([_] (wasm-solver))
  ([_ _opts] (wasm-solver)))
