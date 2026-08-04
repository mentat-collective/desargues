(ns desargues.domain.pure-test
  "Pure-domain invariants (property-based, hive-test paradigm via test.check).
   Covers: value-object equality, protocol dispatch (SOLID), and the
   correctness fixes (single-application transform, sound evaluate/derivative)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.domain.math-expression :as expr]
            [desargues.domain.services :as svc]
            [desargues.domain.protocols :as p]
            [desargues.domain.number-theory :as nt]
            [desargues.domain.number-theory-services :as nts]
            [emmy.env :as e]))

;; ---------------------------------------------------------------------------
;; Value-object equality  (gensym removed -> content-determined identity)
;; ---------------------------------------------------------------------------

(deftest expression-value-equality
  (testing "structurally-equal expressions are = with equal ids"
    (is (= (expr/create-expression (e/sin 'x))
           (expr/create-expression (e/sin 'x))))
    (is (= (:id (expr/create-expression (e/sin 'x)))
           (:id (expr/create-expression (e/sin 'x)))))
    (is (= (expr/point 'x 0) (expr/point 'x 0)))))

(deftest expression-id-is-pure
  (let [property (prop/for-all [n gen/small-integer]
                   (= (:id (expr/create-expression (e/+ 'x n)))
                      (:id (expr/create-expression (e/+ 'x n)))))]
    (is (:pass? (tc/quick-check 100 property)))))

;; ---------------------------------------------------------------------------
;; IFactorizable  (protocol dispatch is consistent with the service of truth)
;; ---------------------------------------------------------------------------

(deftest factorizable-matches-service
  (let [property (prop/for-all [n (gen/choose 1 500)]
                   (= (p/prime-factors (nt/create-factorization n))
                      (:factors (nts/prime-factorize n))))]
    (is (:pass? (tc/quick-check 200 property)))))

(deftest factorization-reconstructs-n
  (let [property (prop/for-all [n (gen/choose 2 1000)]
                   (= n (reduce-kv (fn [acc p e] (* acc (long (Math/pow p e))))
                                   1 (p/prime-factors (nt/create-factorization n)))))]
    (is (:pass? (tc/quick-check 200 property)))))

;; ---------------------------------------------------------------------------
;; IEvaluable / IDifferentiable  (verified engine semantics)
;; ---------------------------------------------------------------------------

(deftest evaluate-substitutes-and-simplifies
  (is (= 0 (p/evaluate (expr/create-expression (e/sin 'x)) (expr/point 'x 0))))
  (is (= 0 (:output (svc/evaluate-expression
                     (expr/create-expression (e/sin 'x))
                     (expr/point 'x 0))))))

(deftest function-derivative-is-sound
  (let [f  (expr/create-function 'f (fn [x] (e/sin x)))
        df (p/derivative f)]
    (is (= "\\cos\\left(x\\right)" (e/->TeX ((:f df) 'x))))
    (is (= "- \\sin\\left(x\\right)"
           (e/->TeX ((:f (p/derivative df)) 'x))))))

;; ---------------------------------------------------------------------------
;; transform-expression applies the transformation EXACTLY once (bug fix)
;; ---------------------------------------------------------------------------

(deftest transform-applies-once
  (let [ex    (expr/create-expression (e/sin 'x))
        calls (atom 0)
        out   (svc/transform-expression ex (fn [x] (swap! calls inc) (e/+ x 1)))]
    (is (= 1 @calls))
    (is (= (e/+ (e/sin 'x) 1) (:expr out)))))

;; ---------------------------------------------------------------------------
;; IDotArrangement
;; ---------------------------------------------------------------------------

(deftest dot-grid-arrangement
  (is (= :grid-2d (p/arrangement-type (nt/create-dot-grid-2d 2 3))))
  (is (= [2 3]    (p/dimensions (nt/create-dot-grid-2d 2 3))))
  (is (= 6        (count (p/to-dots (nt/create-dot-grid-2d 2 3))))))
