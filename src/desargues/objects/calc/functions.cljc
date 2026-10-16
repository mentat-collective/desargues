(ns desargues.objects.calc.functions
  "The one-variable functions the calculus objects are built on, each
   differentiated by Emmy at compile time (desargues.objects.calc.symbolic/
   fn-catalog), so this namespace compiles to ClojureScript with no Emmy in
   the page. An entry is {:label :body :infix {:f :df} :tex {:f :df} :poly
   :f :df}: :f and :df numeric (fn [x]), :poly the exact coefficients of a
   polynomial body (nil otherwise), which the exact readout integrates.

   `as-fn` turns what an object holds in a function slot (a catalog key, an
   entry, or a bare (fn [x])) into an entry. Host-free .cljc."
  #?(:clj  (:require [desargues.objects.calc.symbolic :refer [fn-catalog]])
     :cljs (:require-macros [desargues.objects.calc.symbolic :refer [fn-catalog]])))

(def catalog
  "Function key -> entry. Scaled to sit in [0 4] x [-1 5]."
  (fn-catalog
   {:parabola  {:label "4 - x^2 (on [0 2])" :body (- 4 (square x))}
    :square    {:label "x^2"                :body (square x)}
    :cubic     {:label "x^3/3 - x + 2"      :body (+ (- (/ (cube x) 3) x) 2)}
    :root-like {:label "x/2 + 1"            :body (+ (/ x 2) 1)}
    :wave      {:label "1.2 + sin 2x"       :body (+ (/ 6 5) (sin (* 2 x)))}
    :bell      {:label "3 e^(-x^2/2)"       :body (* 3 (exp (- (/ (square x) 2))))}
    :sqrt      {:label "sqrt x"             :body (sqrt x)}
    :xsinx     {:label "x sin x + 2"        :body (+ (* x (sin x)) 2)}}))

(def order
  "Catalog keys in menu order."
  [:parabola :square :cubic :root-like :wave :bell :sqrt :xsinx])

(defn as-fn
  "The function entry for `v`: a catalog key, an entry map, or a bare
   (fn [x]) (wrapped, no :df: derivatives then fall back to numeric)."
  [v]
  (cond (keyword? v) (get catalog v)
        (map? v)     v
        (fn? v)      {:f v :label "f"}
        :else        nil))

(defn options
  "Select options {:value :label} over `ks` (default every catalog key)."
  ([] (options order))
  ([ks] (mapv (fn [k] {:value k :label (:label (catalog k))}) ks)))
