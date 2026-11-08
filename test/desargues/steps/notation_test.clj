(ns desargues.steps.notation-test
  "The solution notation is Emmy expressions, typeset by Emmy's renderer."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.steps.model :as m]
            [desargues.steps.notation :as n]
            [desargues.steps.render :as render]
            [emmy.env :as e]))

(deftest forms-are-emmy-expressions
  (testing "an integral composes with Emmy arithmetic"
    (is (= '(- (* u v) (integral v u))
           (e/freeze (e/- (e/* 'u 'v) (m/integral 'v 'u))))))
  (testing "form-kind names the operator at the root"
    (is (= :integral (m/form-kind (m/integral 'x 'x))))
    (is (= :plus-C (m/form-kind (m/plus-c 'x))))
    (is (m/form? (m/eval-at 'x 'x 0 1)))
    (is (not (m/form? (m/eq 'a 'b)))))
  (testing "integral-parts reads an integral back"
    (is (= '[(sin x) x 0 pi] (m/integral-parts (m/integral (e/sin 'x) 'x [0 'pi]))))
    (is (= '[x x nil nil] (m/integral-parts (m/integral 'x 'x))))))

(deftest tex-of-the-notation
  (is (= "\\int x\\,e^{x} \\, dx = u\\,v - \\int v \\, du"
         (n/->TeX (m/eq (m/integral (e/* 'x (e/exp 'x)) 'x)
                        (e/- (e/* 'u 'v) (m/integral 'v 'u))))))
  (is (= "\\int_{0}^{\\pi} \\left(x + 1\\right) \\, dx"
         (n/->TeX (m/integral (e/+ 'x 1) 'x [0 'pi]))))
  (is (= "\\frac{{u}^{3}}{3} + C" (n/->TeX (m/plus-c (e// (e/cube 'u) 3)))))
  (is (= "\\frac{1}{2}\\,\\int \\frac{1}{x - 1} \\, dx"
         (n/->TeX (e/* (e// 1 2) (m/integral (e// 1 (e/- 'x 1)) 'x)))))
  (testing "fractions in sums and equations carry no parentheses"
    (is (= "\\frac{1}{{x}^{2} - 1} = \\frac{A}{x - 1} + \\frac{B}{x + 1}"
           (n/->TeX (m/eq (e// 1 (e/- (e/square 'x) 1))
                          (e/+ (e// 'A (e/- 'x 1)) (e// 'B (e/+ 'x 1))))))))
  (testing "a power of a fraction keeps them"
    (is (= "{\\left(\\frac{x}{2}\\right)}^{3}" (n/->TeX (e/expt (e// 'x 2) 3)))))
  (testing "ln|u|, sin^n x, the bracket and the substitution bar"
    (is (= "\\ln\\left|x - 1\\right|" (n/->TeX (e/log (e/abs (e/- 'x 1))))))
    (is (= "\\frac{\\sin^{3}\\left(x\\right)}{3}" (n/->TeX (e// (e/cube (e/sin 'x)) 3))))
    (is (= "\\Big[ - \\cos\\left(x\\right) \\Big]_{0}^{\\pi}"
           (n/->TeX (m/eval-at (e/- (e/cos 'x)) 'x 0 'pi))))
    (is (= "\\frac{{u}^{3}}{3} \\,\\Big|_{u = \\sin\\left(x\\right)}"
           (n/->TeX (m/subst (e// (e/cube 'u) 3) 'u (e/sin 'x)))))))

(deftest infix-of-the-notation
  (is (= "∫ x exp(x) dx = u v - ∫ v du"
         (n/->infix (m/eq (m/integral (e/* 'x (e/exp 'x)) 'x)
                          (e/- (e/* 'u 'v) (m/integral 'v 'u))))))
  (is (= "(x - 1) exp(x) + C" (n/->infix (m/plus-c (e/* (e/- 'x 1) (e/exp 'x))))))
  (is (= "∫ sin²(x) dx" (n/->infix (m/integral (e/square (e/sin 'x)) 'x)))))

(deftest render-is-a-thin-call-into-emmy
  (is (= (n/->TeX (m/plus-c 'x)) (render/tex (m/plus-c 'x))))
  (is (= "\\text{Let }u\\text{ = }\\sin\\left(x\\right)"
         (render/parts->tex [[:text "Let "] [:math 'u] [:text " = "] [:math (e/sin 'x)]])))
  (is (= "Hence x + C" (render/parts->text [[:text "Hence "] [:math (m/plus-c 'x)]]))))
