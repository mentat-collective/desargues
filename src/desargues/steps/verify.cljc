(ns desargues.steps.verify
  "VERIFY stratum: an answer is shown as correct only when Emmy confirms it.

   For an indefinite integral, F is checked by differentiating it back:
   symbolically (simplify (F' - f) to 0) and numerically at sample points
   in the problem's check interval (simplify is not complete, so the
   numeric check is what makes a verdict trustworthy). A definite integral
   is checked against quadrature on the INumeric backend in use
   (desargues.steps.numeric), so the same check runs on :jvm or :raster."
  (:require [desargues.steps.numeric :as numeric]
            [desargues.steps.protocols :as p]
            [emmy.env :as e]))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) (* 1e-6 (max 1.0 (Math/abs (double b))))))

(defn- symbolic-residual
  "simplify (F' - f) at the symbol x, or :undecided when Emmy cannot
   differentiate F symbolically: its autodiff abs needs the sign of the
   argument, so ln|u| has no symbolic derivative at a bare x."
  [dF f]
  (try (e/simplify (e/- (dF 'x) (f 'x)))
       (catch #?(:clj Exception :cljs :default) _ :undecided)))

(defn antiderivative
  "Verdict for F as an antiderivative of f (both one-argument functions),
   checked at `points`. With logs of absolute values, put points on every
   interval of the domain: the check then covers each branch."
  [f F points]
  (let [dF       (e/D F)
        residual (symbolic-residual dF f)
        numeric  (every? (fn [x] (close? (dF x) (f x))) points)]
    {:symbolic? (and (number? residual) (zero? residual))
     :numeric?  numeric
     :residual  residual
     :ok?       numeric}))

(defn definite
  "Verdict for `value` as the integral of f over [a b], on the current
   numeric backend."
  [f a b value]
  (let [q (p/integrate (numeric/current) f a b)]
    {:quadrature q :value (double value) :ok? (close? value q)}))
