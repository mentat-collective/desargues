(ns desargues.objects.calc.numeric
  "PROTOCOL + CORE stratum: the numeric port the calculus objects compute
   their values through (integrals, double integrals), and its host adapter.

   Values never call a quadrature directly: they ask a port (INumeric), so a
   faster or exact backend substitutes without touching any object (DIP,
   LSP). `port` is an open multimethod keyed by backend name:

     :jvm     composite 5-point Gauss-Legendre on the host (the name holds in
              ClojureScript too: it is the host's own arithmetic)
     :raster  raster deftm kernels (desargues.objects.calc.raster-port),
              registered when that namespace is loaded under the :dynamics
              alias; `resolve-port` falls back to :jvm when it is absent

   Host-free .cljc.")

(defprotocol INumeric
  "A numeric backend for integrals."
  (-integrate [port f a b]
    "The integral of (f x) over [a b] (signed; b < a negates).")
  (-integrate2 [port f x-range y-range]
    "The double integral of (f x y) over the rectangle x-range by y-range.")
  (-port-name [port] "The backend's keyword."))

(def gauss-nodes
  "5-point Gauss-Legendre nodes and weights on [-1 1]."
  [[0.0 0.5688888888888889]
   [-0.5384693101056831 0.4786286704993665] [0.5384693101056831 0.4786286704993665]
   [-0.9061798459386640 0.2369268850561891] [0.9061798459386640 0.2369268850561891]])

(defn panel-points
  "[[x w] ...] quadrature points and weights of [a b] split into `panels`
   equal panels, 5 Gauss-Legendre points each."
  [a b panels]
  (let [a (double a) b (double b) n (max 1 (long panels))
        h (/ (- b a) n)]
    (for [i (range n)
          :let [m (+ a (* h (+ i 0.5)))]
          [t w] gauss-nodes]
      [(+ m (* 0.5 h t)) (* 0.5 h w)])))

(defn- weighted-sum [f pts]
  (reduce (fn [acc [x w]] (+ acc (* (double w) (double (f x))))) 0.0 pts))

(defrecord HostNumeric [panels]
  INumeric
  (-integrate [_ f a b] (weighted-sum f (panel-points a b panels)))
  (-integrate2 [_ f [x0 x1] [y0 y1]]
    (let [ys (panel-points y0 y1 (max 8 (quot panels 4)))]
      (weighted-sum (fn [x] (weighted-sum #(f x %) ys))
                    (panel-points x0 x1 (max 8 (quot panels 4))))))
  (-port-name [_] :jvm))

(defmulti port
  "The INumeric registered under backend keyword `k`."
  identity)

(defmethod port :jvm [_] (->HostNumeric 64))

(defmethod port :default [k]
  (throw (ex-info (str "no numeric port " (pr-str k)) {:port k})))

(defn available
  "The backend keywords with a registered port."
  []
  (set (remove #{:default} (keys (methods port)))))

(defn resolve-port
  "The port for `k` (an INumeric is returned as is). :raster loads its
   adapter on the JVM when it is on the classpath; a backend that is not
   available falls back to :jvm."
  [k]
  (cond
    (satisfies? INumeric k) k
    (contains? (methods port) k) (port k)
    :else
    #?(:clj (if (and (= :raster k)
                     (try (require 'desargues.objects.calc.raster-port) true
                          (catch Throwable _ false))
                     (contains? (methods port) :raster))
              (port :raster)
              (port :jvm))
       :cljs (port :jvm))))

(defn integrate
  "The integral of `f` over [a b] through port `p` (a keyword or INumeric)."
  [p f a b]
  (if (== (double a) (double b)) 0.0 (-integrate (resolve-port p) f a b)))

(defn integrate2
  "The double integral of (f x y) over x-range by y-range through port `p`."
  [p f x-range y-range]
  (-integrate2 (resolve-port p) f x-range y-range))
