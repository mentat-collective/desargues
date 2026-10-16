(ns desargues.player.calculus-surfaces
  "The surfaces z = f(x, y) of the partial-derivatives demo, with their
   partial derivatives computed symbolically by Emmy.

   `catalog` is built by desargues.player.calculus-symbolic/surface-catalog:
   Emmy differentiates every form once, at compile time on the JVM, so this
   namespace compiles to ClojureScript with no Emmy in the page. Each entry
   is {:label :body :infix {:f :fx :fy} :tex {:f :fx :fy} :f :fx :fy}, the
   last three numeric (fn [x y] z).

   Pure data plus `mix`, the surface (1 - m) f + m g, which a morph needs:
   differentiation is linear, so its partials are the same mix of the two
   surfaces' partials. Host-free .cljc."
  #?(:clj  (:require [desargues.player.calculus-symbolic :refer [surface-catalog]])
     :cljs (:require-macros [desargues.player.calculus-symbolic :refer [surface-catalog]])))

(def catalog
  "Surface key -> surface. Heights are scaled to stay near [-2.5 2.5] over
   the demo's domain [-2 2] x [-2 2]."
  (surface-catalog
   {:paraboloid {:label "paraboloid"    :body (/ (+ (square x) (square y)) 3)}
    :saddle     {:label "saddle"        :body (/ (- (square x) (square y)) 3)}
    :waves      {:label "sin x cos y"   :body (* (sin x) (cos y))}
    :bump       {:label "gaussian bump" :body (* 2 (exp (- (+ (square x) (square y)))))}
    :monkey     {:label "monkey saddle" :body (/ (- (cube x) (* 3 x (square y))) 6)}
    :dipole     {:label "dipole"        :body (* 2 x (exp (- (/ (+ (square x) (square y)) 2))))}
    :ripple     {:label "ripple"        :body (* (/ 3 5) (cos (* (/ 3 2) x)) (cos y))}}))

(def order
  "Catalog keys in menu order."
  [:paraboloid :saddle :waves :bump :monkey :dipole :ripple])

(defn surface
  "The catalog entry for `k` (the paraboloid for an unknown key)."
  [k]
  (get catalog k (:paraboloid catalog)))

(defn mix
  "Numeric {:f :fx :fy} of (1 - m) f + m g for catalog entries `f` and `g`;
   `f`'s own when m is 0 or there is no `g`."
  [f g m]
  (let [m (double m)]
    (if (or (zero? m) (nil? g))
      (select-keys f [:f :fx :fy])
      (let [w  (- 1.0 m)
            at (fn [k] (fn [x y] (+ (* w ((k f) x y)) (* m ((k g) x y)))))]
        {:f (at :f) :fx (at :fx) :fy (at :fy)}))))
