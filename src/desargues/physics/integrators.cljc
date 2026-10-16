(ns desargues.physics.integrators
  "PURE stratum of the physics bounded context: fixed-step ODE integrators.

   EulerIntegrator and RK4Integrator implement the Integrator port over any
   PhysicalSystem. On the JVM, RK4 short-circuits all-Double states through
   desargues.physics.arrays (same IEEE operations, same order, bit-identical);
   elsewhere it always takes the generic map-merging step. A new integrator
   is a record in its own namespace (OCP).

   Requires desargues.physics.protocols (and, on the JVM only,
   desargues.physics.arrays). Host-free .cljc."
  (:require #?(:clj [desargues.physics.arrays :as arrays])
            [desargues.physics.protocols :as p]))

(defrecord EulerIntegrator []
  p/Integrator
  (step [_ system dt]
    (let [state (p/get-state system)
          derivs (p/derivatives system state)
          new-state (merge-with + state (into {} (map (fn [[k v]] [k (* v dt)]) derivs)))]
      (p/set-state system new-state)))
  (integrator-name [_] "Euler"))

(defn- rk4-generic-step
  "new = old + dt/6 * (k1 + 2*k2 + 2*k3 + k4), on state maps."
  [system state dt]
  (let [k1 (p/derivatives system state)
        state2 (merge-with + state (into {} (map (fn [[k v]] [k (* v (/ dt 2))]) k1)))
        k2 (p/derivatives system state2)
        state3 (merge-with + state (into {} (map (fn [[k v]] [k (* v (/ dt 2))]) k2)))
        k3 (p/derivatives system state3)
        state4 (merge-with + state (into {} (map (fn [[k v]] [k (* v dt)]) k3)))
        k4 (p/derivatives system state4)
        combined (merge-with
                  (fn [& vs] (reduce + vs))
                  k1
                  (into {} (map (fn [[k v]] [k (* 2 v)]) k2))
                  (into {} (map (fn [[k v]] [k (* 2 v)]) k3))
                  k4)]
    (merge-with + state (into {} (map (fn [[k v]] [k (* v (/ dt 6))]) combined)))))

(defrecord RK4Integrator []
  p/Integrator
  (step [_ system dt]
    (let [state (p/get-state system)
          fast #?(:clj (arrays/rk4-fast-step system state dt) :default nil)]
      (p/set-state system (or fast (rk4-generic-step system state dt)))))
  (integrator-name [_] "Runge-Kutta 4"))

(def euler (->EulerIntegrator))
(def rk4 (->RK4Integrator))

(defn rk4?
  "True when `integrator` is the RK4 integrator (the stepper's array loop
   reproduces exactly its steps)."
  [integrator]
  (instance? RK4Integrator integrator))

(defn step-euler
  "Single Euler integration step for state evolution.
   state: current state map
   deriv-fn: function that computes derivatives from state
   dt: time step"
  [state deriv-fn dt]
  (let [derivs (deriv-fn state)]
    (reduce-kv (fn [s k v]
                 (if-let [dv (get derivs k)]
                   (update s k + (* dv dt))
                   s))
               state
               state)))
