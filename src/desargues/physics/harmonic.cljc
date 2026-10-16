(ns desargues.physics.harmonic
  "PURE stratum of the physics bounded context: harmonic analysis and the
   analytic solutions used to validate the integrators.

   Speaks only the Oscillator port, so any system implementing it gets a
   period and a damping classification (OCP: no type switch here).

   Requires only desargues.physics.protocols. Host-free .cljc."
  (:require [desargues.physics.protocols :as p]))

(defn natural-frequency
  "Compute natural frequency of a system.

   For pendulum: ω = sqrt(g/L)
   For spring-mass: ω = sqrt(k/m)"
  [system]
  (p/natural-frequency system))

(defn damping-ratio
  "Compute damping ratio ζ = μ / (2 * sqrt(k*m)) for spring-mass
   or ζ = μ / (2 * sqrt(g*L)) for pendulum."
  [system]
  (p/damping-ratio system))

(defn period
  "Compute period of oscillation."
  [system]
  (/ (* 2 Math/PI) (natural-frequency system)))

(defn damping-type
  "Classify damping: :underdamped, :critically-damped, :overdamped"
  [system]
  (let [zeta (damping-ratio system)]
    (cond
      (< zeta 1) :underdamped
      (= zeta 1) :critically-damped
      :else :overdamped)))

(defn harmonic-solution
  "Analytical solution for undamped harmonic motion.
   x(t) = A*cos(ωt) + B*sin(ωt)
   where A = x0, B = v0/ω"
  [{:keys [x0 v0 omega]} t]
  (+ (* x0 (Math/cos (* omega t)))
     (* (/ v0 omega) (Math/sin (* omega t)))))

(defn damped-harmonic-solution
  "Analytical solution for damped harmonic motion (underdamped case).
   x(t) = e^(-γt) * (A*cos(ω't) + B*sin(ω't))
   where γ = μ/2m, ω' = sqrt(ω² - γ²)"
  [{:keys [x0 v0 omega gamma]} t]
  (let [omega-prime (Math/sqrt (- (* omega omega) (* gamma gamma)))
        A x0
        B (/ (+ v0 (* gamma x0)) omega-prime)]
    (* (Math/exp (- (* gamma t)))
       (+ (* A (Math/cos (* omega-prime t)))
          (* B (Math/sin (* omega-prime t)))))))
