(ns desargues.steps.corpus
  "An integration corpus in the order of the indefinite-integral chapter of
   a classic problem book (Demidovich's analysis problems): direct
   integration from the table, the constant factor and sums, substitution,
   by parts, rational functions, trigonometric integrals, and definite
   integrals. Expressions only (original exercises of the same kinds, no
   text from the book), each with the points at which its answer is
   checked (one per interval of the domain when a log of |u| appears).

   Host-free .cljc."
  )

(defn- p [id section f & {:keys [points bounds] :or {points [0.3 0.7 1.4]}}]
  {:id id :section section :f f :var 'x :points points :bounds bounds})

(def sections
  ["Direct integration" "Substitution" "By parts" "Rational functions"
   "Trigonometric integrals" "Definite integrals"])

(def problems
  [;; 1. Direct integration: the table, constants and sums
   (p :d1 "Direct integration" '(expt x 5))
   (p :d2 "Direct integration" '(+ (* 3 (expt x 2)) (* -4 x) 7))
   (p :d3 "Direct integration" '(/ 1 (expt x 3)))
   (p :d4 "Direct integration" '(sqrt x))
   (p :d5 "Direct integration" '(/ 5 x) :points [0.5 2.0 -1.5])
   (p :d6 "Direct integration" '(+ (exp x) (cos x)))
   (p :d7 "Direct integration" '(expt 3 x))
   (p :d8 "Direct integration" '(/ 2 (+ (expt x 2) 1)))
   (p :d9 "Direct integration" '(/ 1 (+ (expt x 2) 9)))
   (p :d10 "Direct integration" '(/ 1 (sqrt (- 4 (expt x 2)))))
   (p :d11 "Direct integration" '(/ 1 (expt (cos x) 2)))
   (p :d12 "Direct integration" '(+ (sin x) (* 2 (sqrt x))))
   ;; 2. Substitution
   (p :s1 "Substitution" '(* (expt (sin x) 2) (cos x)))
   (p :s2 "Substitution" '(expt (+ (* 2 x) 1) 5))
   (p :s3 "Substitution" '(/ 1 (- (* 3 x) 2)) :points [1.0 2.0 0.2])
   (p :s4 "Substitution" '(exp (* 3 x)))
   (p :s5 "Substitution" '(* x (cos (expt x 2))))
   (p :s6 "Substitution" '(* x (exp (expt x 2))))
   (p :s7 "Substitution" '(/ x (+ (expt x 2) 1)))
   (p :s8 "Substitution" '(/ (log x) x) :points [0.5 2.0 3.0])
   (p :s9 "Substitution" '(sin (* 5 x)))
   (p :s10 "Substitution" '(* (cos x) (exp (sin x))))
   (p :s11 "Substitution" '(tan x) :points [0.3 0.7 1.2])
   ;; 3. By parts
   (p :b1 "By parts" '(* x (exp x)))
   (p :b2 "By parts" '(* x (sin x)))
   (p :b3 "By parts" '(* x (cos x)))
   (p :b4 "By parts" '(log x) :points [0.5 1.5 3.0])
   (p :b5 "By parts" '(* x (log x)) :points [0.5 1.5 3.0])
   (p :b6 "By parts" '(atan x))
   (p :b7 "By parts" '(* (exp x) (sin x)))
   (p :b8 "By parts" '(* (exp x) (cos x)))
   ;; 4. Rational functions
   (p :r1 "Rational functions" '(/ 1 (- (expt x 2) 1)) :points [1.5 3.0 0.3 -0.5 -2.5])
   (p :r2 "Rational functions" '(/ 1 (* x (+ x 1))) :points [0.5 2.0 -0.5 -2.0])
   (p :r3 "Rational functions" '(/ (+ (* 2 x) 3) (+ (expt x 2) (* 3 x) 2)) :points [0.5 2.0 -1.5 -3.0])
   (p :r4 "Rational functions" '(/ 1 (+ (expt x 2) (* 2 x) 5)))
   (p :r5 "Rational functions" '(/ (expt x 2) (+ x 1)) :points [0.5 2.0 -2.0])
   (p :r6 "Rational functions" '(/ 1 (- (expt x 2) 4)) :points [3.0 0.5 -3.0])
   ;; 5. Trigonometric integrals
   (p :t1 "Trigonometric integrals" '(expt (sin x) 2))
   (p :t2 "Trigonometric integrals" '(expt (cos x) 2))
   (p :t3 "Trigonometric integrals" '(* (sin x) (cos x)))
   (p :t4 "Trigonometric integrals" '(expt (tan x) 2) :points [0.3 0.7 1.2])
   (p :t5 "Trigonometric integrals" '(* (expt (cos x) 3) (sin x)))
   ;; 6. Definite integrals
   (p :i1 "Definite integrals" '(sin x) :bounds [0 'pi])
   (p :i2 "Definite integrals" '(expt x 2) :bounds [0 3])
   (p :i3 "Definite integrals" '(* x (exp x)) :bounds [0 1])
   (p :i4 "Definite integrals" '(/ 1 (+ (expt x 2) 1)) :bounds [0 1])])
