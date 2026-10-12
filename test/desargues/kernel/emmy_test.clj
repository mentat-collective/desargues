(ns desargues.kernel.emmy-test
  "desargues kernels as Emmy compilation modes. The modes exist only on an
   Emmy whose compiler is open (mentat-collective/emmy#199); on the pinned
   v0.32.0 the bridge must register nothing and say so."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.kernel.emmy :as ke]
            [emmy.env :as e]
            [emmy.expression.compile :as compile]))

(defn- pendulum-derivative []
  (fn [[_t theta omega]]
    (e/up 1 omega (e/- (e/* 9.8 (e/sin theta)) (e/* 0.1 omega)))))

(defn- derivative-at [kernel]
  (let [out (double-array 3)]
    (kernel (double-array [0.0 0.3 -0.2]) out (double-array 0))
    (vec out)))

(defn- compile-as [mode]
  (compile/compile-state-fn pendulum-derivative [] (e/up 0.0 1.0 1.0)
                            {:mode mode :calling-convention :primitive :cache? false}))

(deftest jvm-mode
  (if (ke/open-compiler?)
    (testing "on an open Emmy, :desargues/jvm is a mode and answers Emmy's own numbers"
      (is (true? (ke/install!)))
      (is (contains? ((resolve 'emmy.expression.compile/registered-modes)) :desargues/jvm))
      (is (= (derivative-at (compile-as :native))
             (derivative-at (compile-as :desargues/jvm)))))
    (testing "on a closed Emmy the bridge registers nothing"
      (is (false? (ke/install!)))
      (is (false? (ke/register-mode! :desargues/anything identity)))
      (is (thrown? clojure.lang.ExceptionInfo (compile-as :desargues/jvm))))))
