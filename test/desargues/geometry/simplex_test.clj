(ns desargues.geometry.simplex-test
  "desargues.geometry.simplex must reproduce emmy's Nelder-Mead bit for bit:
   same result vector, value, iteration and function-call counts."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.geometry.simplex :as sx]
            [emmy.numerical.multimin.nelder-mead :as nm]))

(defn- bits [{:keys [result value] :as r}]
  (assoc (select-keys r [:converged? :iterations :fncalls])
         :result (mapv #(Double/doubleToRawLongBits (double %)) result)
         :value (Double/doubleToRawLongBits (double value))))

(def ^:private rosen
  (fn [xs] (let [x (nth xs 0) y (nth xs 1)]
             (+ (* 100.0 (Math/pow (- y (* x x)) 2)) (Math/pow (- 1.0 x) 2)))))

(def ^:private quad5
  (fn [xs] (reduce + (map-indexed (fn [i x] (* (inc i) (- x i) (- x i))) xs))))

(def ^:private plateau
  (fn [xs] (Math/floor (* 3.0 (reduce + (map #(Math/abs (double %)) xs))))))

(deftest bit-identical-to-emmy
  (doseq [[label f starts] [["rosenbrock" rosen [[-1.2 1.0] [0.0 0.0] [2.0 -3.0]]]
                            ["quad5" quad5 [[0.0 1.0 0.5 -2.0 3.0] [1 2 3 4 5] [0.0 0.0 0.0 0.0 0.0]]]
                            ["plateau (ties)" plateau [[0.3 -0.2 1.0] [0.0 0.0 0.0]]]]
          x0 starts
          opts [{} {:maxiter 50} {:maxfun 30}
                {:adaptive? false :simplex-tolerance 1e-9 :fn-tolerance 1e-12}
                {:simplex-tolerance 1e-7 :fn-tolerance 1e-9 :maxiter 800}]]
    (testing (str label " " x0 " " opts)
      (is (= (bits (nm/nelder-mead f x0 opts))
             (bits (sx/nelder-mead (fn [^doubles a] (f (vec a))) x0 opts)))))))
