(ns desargues.pipeline.emmy-test
  "The Emmy conveyor is pure: every stage runs here with NO Python. The
   boundary conversion (LaTeX -> Python) is a stub, which proves the
   injection seam and pins the CPPB invariants: promoters are independent,
   composition equals the sum of its promoters, effects never leak up."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.pipeline.emmy :as conv]
            [desargues.domain.math-expression :as expr]
            [desargues.domain.services :as svc]
            [emmy.env :as e]))

(def sin-f (fn [x] (e/sin x)))

(def stub-converter
  "A boundary stand-in: records that it was called and tags the input."
  (let [calls (atom [])]
    (with-meta (fn [s] (swap! calls conj s) (str "PY<" s ">")) {:calls calls})))

(deftest collect-fixes-symbolic-facts
  (let [{:keys [expr derivative-expr var]} (conv/collect-function sin-f)]
    (is (= 'x var))
    (is (= (e/sin 'x) expr))
    (is (= (e/cos 'x) derivative-expr))))

(deftest promoters-are-independent
  (testing "promote-latex never touches the converter"
    (let [before (count @(:calls (meta stub-converter)))]
      (is (= "\\sin\\left(x\\right)" (:content (conv/promote-latex (e/sin 'x)))))
      (is (= before (count @(:calls (meta stub-converter)))))))
  (testing "promote-python calls exactly the injected converter, once"
    (let [before (count @(:calls (meta stub-converter)))
          out (conv/promote-python (expr/->LaTeX "\\sin(x)") stub-converter)]
      (is (= "PY<\\sin(x)>" (:content out)))
      (is (= (inc before) (count @(:calls (meta stub-converter))))))))

(deftest pipeline-composes-promoters
  (let [spec (conv/derivative-spec sin-f)]
    (is (= "\\sin\\left(x\\right)" (:func-latex spec)))
    (is (= "\\cos\\left(x\\right)" (:deriv-latex spec)))
    (testing "the composed spec equals the promoters applied separately"
      (is (= (:func-latex spec) (:latex (conv/expression-spec (sin-f 'x)))))
      (is (= (:deriv-latex spec) (:content (conv/promote-latex ((e/D sin-f) 'x))))))))

(deftest python-code-threads-the-injected-boundary
  (is (= "PY<\\sin\\left(x\\right)>" (conv/python-code (e/sin 'x) stub-converter))))

(deftest conveyor-agrees-with-domain-services
  (testing "one definition of LaTeX rendering: the conveyor and the entity
            services project the same value"
    (let [property (prop/for-all [n gen/small-integer]
                     (let [x (e/+ (e/sin 'x) n)]
                       (= (:content (conv/promote-latex x))
                          (:content (svc/expression-to-latex (expr/create-expression x))))))]
      (is (:pass? (tc/quick-check 100 property))))))
