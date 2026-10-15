(ns desargues.emmy-python.equations
  (:require [emmy.env :as e :refer :all]))

(defn expt1 [x]
  (e/sin (+ x pi)))

(defn expt2 [x]
  "Quadratic function"
  (+ (* 2 (square x)) (* 3 x) 1))

(defn expt3 [x]
  "Exponential decay"
  (* (exp (- x)) (cos x)))

(defn expt4 [x]
  "Gaussian"
  (exp (- (square x))))

(comment
  (->TeX 'pi)
  ;; => "\\pi"

  (simplify (expt1 0.01))
  ;; => -0.00999983333416633

  (expt1 'x)
  ;; => (sin (+ x 3.141592653589793))

  (expt1 (literal-number 'x))
  ;; => (sin (+ x 3.141592653589793))

  (->TeX (expt1 'x))
  ;; => "\\sin\\left(x + 3.141592653589793\\right)"
  ;; Better: "\\sin\\left(x + \\pi\\right)"

  (->TeX ((D expt1) 'x))
  ;; => "\\cos\\left(x + 3.141592653589793\\right)"
  ;; Better: "\\cos\\left(x + \\pi\\right)"

  ;; Replace numeric pi with symbolic pi
  (->TeX (e/sin (+ 'x 'pi)))
  ;; => "\\sin\\left(x + \\pi\\right)"

  (->TeX ((D #(e/sin (+ % 'pi))) 'x))
  ;; => "\\cos\\left(x + \\pi\\right)"

  ;; Quadratic
  (->TeX (expt2 'x))
  ;; => "1 + 3\\,x + 2\\,{x}^{2}"

  ;; Its derivative
  (->TeX ((D expt2) 'x))
  ;; => "3 + 4\\,x"

  ;; Exponential decay with oscillation
  (->TeX (expt3 'x))

  ;; Gaussian
  (->TeX (expt4 'x))
  ;; => "e^{- {x}^{2}}"

  ;; Second derivative of Gaussian
  (->TeX ((D (D expt4)) 'x)))
