(ns desargues.physics.forces
  "CORE stratum of the physics bounded context: forces as value objects.

   One record per force law, each implementing the Force port; a composite
   sums any forces additively. A new law is one record in its own namespace
   (OCP).

   Requires only desargues.physics.protocols. Host-free .cljc."
  (:require [clojure.string :as str]
            [desargues.physics.protocols :as p]))

(defrecord SpringForce [k rest-length anchor]
  p/Force
  (force-value [_ _ {:keys [x]}]
    (- (* k (- x rest-length))))
  (force-name [_] (str "Spring(k=" k ")")))

(defrecord DampingForce [mu]
  p/Force
  (force-value [_ _ {:keys [v]}]
    (- (* mu v)))
  (force-name [_] (str "Damping(μ=" mu ")")))

(defrecord GravityForce [g]
  p/Force
  (force-value [_ _ _]
    (- g))
  (force-name [_] (str "Gravity(g=" g ")")))

(defrecord CompositeForce [forces]
  p/Force
  (force-value [_ system state]
    (reduce + (map #(p/force-value % system state) forces)))
  (force-name [_]
    (str "Composite[" (str/join " + " (map p/force-name forces)) "]")))

(defn spring-force
  "Create a spring force F = -k(x - rest-length)"
  [k & {:keys [rest-length anchor] :or {rest-length 0 anchor [0 0 0]}}]
  (->SpringForce k rest-length anchor))

(defn damping-force
  "Create a damping force F = -μv"
  [mu]
  (->DampingForce mu))

(defn gravity-force
  "Create a gravity force F = -g"
  [g]
  (->GravityForce g))

(defn combine-forces
  "Combine multiple forces into a composite force."
  [& forces]
  (->CompositeForce (vec forces)))
