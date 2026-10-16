(ns desargues.physics.specs
  "CORE stratum of the physics bounded context: the clojure.spec shapes of
   the constructor and evolve options.

   Requires only desargues.physics.protocols. Host-free .cljc."
  (:require [clojure.spec.alpha :as s]
            [desargues.physics.protocols :as p]))

(s/def ::length (s/and number? pos?))
(s/def ::gravity (s/and number? pos?))
(s/def ::damping (s/and number? #(>= % 0)))
(s/def ::initial-theta number?)
(s/def ::initial-omega number?)
(s/def ::top-point (s/coll-of number? :count 3))

(s/def ::pendulum-opts
  (s/keys :opt-un [::length ::gravity ::damping ::initial-theta ::initial-omega ::top-point]))

(s/def ::k (s/and number? pos?))
(s/def ::mu (s/and number? #(>= % 0)))
(s/def ::mass (s/and number? pos?))
(s/def ::initial-x number?)
(s/def ::initial-v number?)

(s/def ::spring-mass-opts
  (s/keys :opt-un [::k ::mu ::mass ::initial-x ::initial-v]))

(s/def ::dt (s/and number? pos?))
(s/def ::duration (s/and number? pos?))
(s/def ::integrator #(satisfies? p/Integrator %))

(s/def ::evolve-opts
  (s/keys :opt-un [::dt ::duration ::integrator]))
