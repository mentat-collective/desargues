(ns desargues.objects.calc.readout
  "BOUNDARY stratum (JVM, Emmy): exact readouts of calculus objects, the
   methods of desargues.objects.calc.figure/exact.

   For a catalog function with a polynomial body, areas, Riemann sums, FTC
   accumulations and volumes of revolution are computed EXACTLY in rationals
   (desargues.objects.calc.symbolic/poly) and printed by Emmy (pi kept
   symbolic: 256/15 pi, not 53.6165). Tangent slopes print Emmy's
   derivative f'(x) with its value at a rational x0. A non-polynomial body
   gives nil (the readout then shows the numeric value only).

   `exact-value` returns the number itself (a rational, or a multiple of pi
   as {:pi-times r}) for tests."
  (:require [desargues.objects.calc.core :as calc]
            [desargues.objects.calc.figure :as figure]
            [desargues.objects.calc.symbolic :as sym]
            [clojure.string :as str]))

(defn- poly-of [o k] (:poly (calc/fn-entry o k)))
(defn- rat [x] (rationalize (double x)))
(defn- upto-x [{:keys [a b upto]}] (+ (rat a) (* (rat (or upto 1)) (- (rat b) (rat a)))))
(defn- p* [a b] (apply merge-with + {} (for [[i x] a [j y] b] {(+ i j) (* x y)})))
(defn- neg [p] (update-vals p -))

(defmulti exact-value
  "The exact value of resolved object `o`: a rational, {:pi-times r}, or nil."
  :kind)

(defmethod exact-value :default [_] nil)

(defmethod exact-value :area-under [o]
  (when-let [q (poly-of o :f)] (sym/definite q (rat (:a o)) (upto-x o))))

(defmethod exact-value :accumulation [o]
  (when-let [q (poly-of o :f)] (sym/definite q (rat (:a o)) (rat (:x o)))))

(defmethod exact-value :area-between [o]
  (let [f (poly-of o :f) g (poly-of o :g)]
    (when (and f g) (sym/definite (merge-with + f (neg g)) (rat (:a o)) (upto-x o)))))

(defmethod exact-value :riemann [{:keys [a b n rule] :as o}]
  (when-let [q (poly-of o :f)]
    (let [n (long n) a (rat a) dx (/ (- (rat b) a) n)
          at (fn [x] (sym/eval-poly q x))]
      (reduce + (for [i (range n) :let [l (+ a (* i dx)) r (+ l dx)]]
                  (* dx (case rule :right (at r) :mid (at (/ (+ l r) 2))
                              :trapezoid (/ (+ (at l) (at r)) 2) (at l))))))))

(defmethod exact-value :revolution [{:keys [method a b] :as o}]
  (let [f (poly-of o :f) g (poly-of o :g) a (rat a) b (rat b)]
    (case method
      :disk   (when f {:pi-times (sym/definite (p* f f) a b)})
      :washer (when (and f g) {:pi-times (sym/definite (merge-with + (p* f f) (neg (p* g g))) a b)})
      :shell  (when f {:pi-times (* 2 (sym/definite (p* {1 1} f) a b))})
      nil)))

(defmethod exact-value :tangent-line [o]
  (when-let [q (poly-of o :f)]
    (sym/eval-poly (into {} (keep (fn [[i c]] (when (pos? i) [(dec i) (* i c)]))) q) (rat (:x0 o)))))

(defn- show [v]
  (cond (map? v)   (let [r (:pi-times v)]
                     (cond (= 1 r) "π" (integer? r) (str r " π") :else (str (show r) " π")))
        (ratio? v) (str (numerator v) "/" (denominator v))
        :else      (str v)))

(defn exact-text
  "Emmy's exact text for `o`, or nil."
  [o]
  (when-let [v (exact-value o)]
    (case (:kind o)
      :tangent-line (str "f'(x) = " (str/replace (str (get-in (calc/fn-entry o :f) [:infix :df]))
                                                 "+ -" "- ")
                         " -> " (show v))
      (show v))))

(defmethod figure/exact :default [o]
  (try (exact-text o) (catch Exception _ nil)))
