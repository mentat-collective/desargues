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

;; ---------------------------------------------------------------------------
;; hasheq consistent with =  (kanban 20260704003158-70df3903)
;; ---------------------------------------------------------------------------

(deftest expression-hash-consistent-with-equality
  (testing "value-equal expressions hash equal, so set/map membership works"
    (let [a (expr/create-expression (e/sin 'x))
          b (expr/create-expression (e/sin 'x))]
      (is (= a b))
      (is (= (hash a) (hash b)))
      (is (= 1 (count (hash-set a b))))
      (is (contains? {a :found} b))))
  (testing "different metadata -> different value"
    (is (not= (expr/create-expression (e/sin 'x) {:k 1})
              (expr/create-expression (e/sin 'x) {:k 2}))))
  (testing "keyword lookup and pr round-trip survive the deftype"
    (let [a (expr/create-expression (e/sin 'x) {:k 1})]
      (is (= {:k 1} (:metadata a)))
      (is (= :dflt (get a :nope :dflt)))
      (is (re-find #"#desargues/MathExpression" (pr-str a))))))

(deftest expression-hash-is-pure
  (let [property (prop/for-all [n gen/small-integer]
                   (= 1 (count (hash-set (expr/create-expression (e/+ 'x n))
                                         (expr/create-expression (e/+ 'x n))))))]
    (is (:pass? (tc/quick-check 100 property)))))

;; ---------------------------------------------------------------------------
;; Hardening  (kanban 20260704003158-4ed5c668)
;; ---------------------------------------------------------------------------

(deftest nth-derivative-guards-n
  (let [f (expr/create-function 'f (fn [x] (e/sin x)))]
    (is (= "\\sin\\left(x\\right)" (e/->TeX ((:f (p/nth-derivative f 0)) 'x))))
    (is (= "- \\sin\\left(x\\right)" (e/->TeX ((:f (p/nth-derivative f 2)) 'x))))
    (is (thrown? clojure.lang.ExceptionInfo (p/nth-derivative f -1)))
    (is (thrown? clojure.lang.ExceptionInfo (p/nth-derivative f 2.5)))))

(deftest ids-are-injective-over-content
  (testing "a bare s-expression and the engine expression it prints as differ"
    (is (not= (:id (expr/create-expression '(sin x)))
              (:id (expr/create-expression (e/sin 'x))))))
  (testing "same-named functions with different bodies differ"
    (is (not= (:id (expr/create-function 'f (fn [x] (e/sin x))))
              (:id (expr/create-function 'f (fn [x] (e/cos x)))))))
  (testing "same-named, same-body functions agree"
    (is (= (:id (expr/create-function 'f (fn [x] (e/sin x))))
           (:id (expr/create-function 'f (fn [x] (e/sin x))))))))

(deftest evaluate-coerces-every-numeric-kind
  (let [ev (fn [ex v] (p/evaluate (expr/create-expression ex) (expr/point 'x v)))]
    (is (= 0 (ev (e/sin 'x) 0)))
    (is (= 1/3 (ev (e// 'x 3) 1)))
    (is (= (e/complex 0 1) (ev (e/sqrt 'x) -1)))
    (is (= -1 (ev (e/expt 'x 2) (e/complex 0 1))))
    (testing "irreducible symbolic results stay symbolic"
      (is (= "\\sqrt {2}" (e/->TeX (ev (e/sqrt 'x) 2)))))))

;; ---------------------------------------------------------------------------
;; NestedFactorization is the single source of arrangement + positions
;; (kanban 20260703234315-6282bbfe, CPPB audit)
;; ---------------------------------------------------------------------------

(deftest nested-factorization-single-source
  (let [property (prop/for-all [n (gen/choose 1 300)]
                   (let [nf (nt/create-nested-factorization n)]
                     (and (keyword? (get-in nf [:arrangement :type]))
                          (= n (count (:positions nf)))
                          (every? #(= 3 (count %)) (:positions nf)))))]
    (is (:pass? (tc/quick-check 200 property))))
  (testing "prefer-3d forces a keyword 3d arrangement with n positions"
    (let [nf (nt/create-nested-factorization 12 :prefer-3d true)]
      (is (nt/is-3d? nf))
      (is (= 12 (count (:positions nf)))))))
