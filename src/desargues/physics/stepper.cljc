(ns desargues.physics.stepper
  "PURE stratum of the physics bounded context: the built-in TrajectorySolver.

   StepperSolver drives any step-based Integrator over a time span. On the
   JVM an RK4 stepper over an ArraySystem runs the whole loop on reused
   double[]s (desargues.physics.arrays): the same IEEE operations, so the
   same Trajectory, bit for bit.

   Requires desargues.physics.protocols and desargues.physics.integrators
   (and, on the JVM only, desargues.physics.arrays). Host-free .cljc."
  (:require #?(:clj [desargues.physics.arrays :as arrays])
            [desargues.physics.integrators :as integrators]
            [desargues.physics.protocols :as p]))

(defn- step-loop
  "The generic fixed-step loop: a Trajectory of (get-state) after each step."
  [integrator system dt duration]
  (loop [sys system
         t 0.0
         trajectory [{:time 0.0 :state (p/get-state system)}]]
    (if (>= t duration)
      trajectory
      (let [new-sys (p/step integrator sys dt)
            new-t (+ t dt)]
        (recur new-sys new-t (conj trajectory {:time new-t :state (p/get-state new-sys)}))))))

(defrecord StepperSolver [integrator]
  p/TrajectorySolver
  (solve-trajectory [_ system {:keys [dt duration] :or {dt 0.01 duration 10.0}}]
    (if-let [kernel #?(:clj (and (integrators/rk4? integrator)
                                 (arrays/array-kernel-for system dt))
                       :default nil)]
      #?(:clj (arrays/rk4-trajectory-arrays kernel (double dt) (double duration))
         :default nil)
      (step-loop integrator system dt duration))))

(defn stepper-solver
  "A TrajectorySolver that drives a step-based Integrator (default rk4)."
  ([] (stepper-solver integrators/rk4))
  ([integrator] (->StepperSolver integrator)))
