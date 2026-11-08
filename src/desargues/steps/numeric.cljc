(ns desargues.steps.numeric
  "NUMERIC backends of the step solver, chosen by key behind the INumeric
   port (desargues.steps.protocols).

     :jvm     built in: composite Simpson quadrature, Riemann sums and
              sampling in plain Clojure (a reify of INumeric).
     :raster  the raster backend desargues already uses for trajectories
              (desargues.backend, the :dynamics alias). Resolved lazily, so
              this namespace loads without raster on the classpath.

   `backend` is a multimethod on the key (OCP): a new engine is one
   defmethod returning an INumeric. Callers write (integrate (backend k) f a
   b) and never know which engine ran."
  (:require [desargues.steps.protocols]))

(defmulti backend
  "The INumeric for backend key `k`."
  identity)

(defn- simpson [f a b n]
  (let [n (if (odd? n) (inc n) n)
        h (/ (- (double b) (double a)) n)
        x #(+ (double a) (* % h))]
    (* (/ h 3.0)
       (reduce + (map (fn [i] (* (cond (or (zero? i) (= i n)) 1.0 (odd? i) 4.0 :else 2.0)
                                 (double (f (x i)))))
                      (range (inc n)))))))

(defn- riemann-sum [f a b n method]
  (let [h (/ (- (double b) (double a)) n)
        x #(+ (double a) (* % h))
        cell (case method
               :left      (fn [i] (f (x i)))
               :right     (fn [i] (f (x (inc i))))
               :mid       (fn [i] (f (x (+ i 0.5))))
               :trapezoid (fn [i] (/ (+ (f (x i)) (f (x (inc i)))) 2.0)))]
    (* h (reduce + (map (comp double cell) (range n))))))

(defmethod backend :jvm [_]
  (reify desargues.steps.protocols/INumeric
    (integrate [_ f a b] (simpson f a b 512))
    (riemann [_ f a b n method] (riemann-sum f a b n method))
    (sample [_ f a b n]
      (let [h (/ (- (double b) (double a)) n)]
        (mapv (fn [i] (let [x (+ (double a) (* i h))] [x (double (f x))])) (range (inc n)))))))

(defmethod backend :raster [_]
  ;; raster compiles numeric kernels; desargues.backend owns the registration.
  ;; Until the quadrature kernel is registered there, fall back to :jvm loudly.
  #?(:clj  (if-let [f (try (requiring-resolve 'desargues.steps.numeric-raster/backend)
                           (catch Throwable _ nil))]
             (f)
             (do (binding [*out* *err*] (println "[steps.numeric] :raster not available; using :jvm"))
                 (backend :jvm)))
     :cljs (backend :jvm)))

(def ^:dynamic *backend*
  "The backend key used when a caller does not name one."
  :jvm)

(defn current [] (backend *backend*))
