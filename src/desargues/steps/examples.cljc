(ns desargues.steps.examples
  "Five worked integrals, written as derivations on the step model.

   Hand-built for now, so the notebook and the blackboard have something to
   show; the tactic search (desargues.steps.tactics) will produce the same
   shapes. Each example carries its integrand f and answer F as functions,
   so desargues.steps.verify checks the answer with Emmy, and the points or
   bounds the check uses. Original exercises in the style of a classic
   problem book's integration chapter."
  (:require [desargues.steps.model :as m]
            [emmy.env :as e]))

(def x 'x) (def u 'u) (def du 'du) (def dx 'dx) (def v 'v) (def dv 'dv)

(defn- step [rule construction from to & {:as more}]
  (m/step (merge {:rule rule :construction construction :from from :to to} more)))

;; 1. Substitution: ∫ sin²x cos x dx ---------------------------------------
(def substitution
  (let [f (fn [x] (e/* (e/square (e/sin x)) (e/cos x)))
        F (fn [x] (e// (e/cube (e/sin x)) 3))]
    {:id :sin2cos :title "Substitution" :section "Integration by substitution"
     :f f :F F :points [0.3 1.1 2.0 -0.7]
     :figure {:a 0 :b (/ Math/PI 2)}
     :derivation
     (m/derivation
      (m/problem {:id :sin2cos :kind :integral :integrand f})
      [(step :integral/substitution :let (m/integral (f x) x) (m/integral (e/square u) u)
             :bindings (array-map u (e/sin x) du (e/* (e/cos x) dx)))
       (step :integral/table :table (m/integral (e/square u) u) (m/plus-c (e// (e/cube u) 3)))
       (step :integral/back :substitute-back (m/plus-c (e// (e/cube u) 3)) (m/plus-c (F x))
             :bindings {u (e/sin x)})]
      (m/plus-c (F x)))}))

;; 2. By parts: ∫ x eˣ dx ----------------------------------------------------
(def by-parts
  (let [f (fn [x] (e/* x (e/exp x)))
        F (fn [x] (e/* (e/- x 1) (e/exp x)))
        parts-formula (m/eq (m/integral u v) (m/op :- (e/* u v) (m/integral v u)))]
    {:id :xexp :title "Integration by parts" :section "Integration by parts"
     :f f :F F :points [0.0 0.5 1.5 -1.0]
     :figure {:a 0 :b 1.5}
     :derivation
     (m/derivation
      (m/problem {:id :xexp :kind :integral :integrand f})
      [(step :integral/by-parts :choose (m/integral (f x) x) nil
             :bindings (array-map u x dv (e/* (e/exp x) dx)
                                  :reason "LIATE: an algebraic factor comes before an exponential"))
       (step :integral/by-parts :let nil (m/integral (e/exp x) x)
             :bindings (array-map du dx v (e/exp x))
             :text nil)
       (step :integral/by-parts :apply-formula (m/integral (f x) x)
             (m/eq (m/integral (f x) x) (m/op :- (e/* x (e/exp x)) (m/integral (e/exp x) x)))
             :bindings {:formula parts-formula})
       (step :integral/table :table (m/integral (e/exp x) x) (m/plus-c (e/exp x)))
       (step :integral/simplify :hence nil (m/plus-c (F x)))]
      (m/plus-c (F x)))}))

;; 3. By parts twice, the integral comes back: ∫ eˣ sin x dx -----------------
(def cyclic
  (let [f (fn [x] (e/* (e/exp x) (e/sin x)))
        F (fn [x] (e// (e/* (e/exp x) (e/- (e/sin x) (e/cos x))) 2))
        parts-formula (m/eq (m/integral u v) (m/op :- (e/* u v) (m/integral v u)))]
    {:id :expsin :title "By parts twice (the integral returns)" :section "Integration by parts"
     :f f :F F :points [0.2 1.0 2.5 -1.3]
     :figure {:a 0 :b Math/PI}
     :derivation
     (m/derivation
      (m/problem {:id :expsin :kind :integral :integrand f})
      [(step :integral/by-parts :choose (m/integral (f x) x) nil
             :bindings (array-map u (e/sin x) dv (e/* (e/exp x) dx)
                                  :reason "either choice works; keep it consistent on the second pass"))
       (step :integral/by-parts :apply-formula nil
             (m/eq 'I (m/op :- (e/* (e/exp x) (e/sin x)) (m/integral (e/* (e/exp x) (e/cos x)) x)))
             :bindings {:formula parts-formula})
       (step :integral/by-parts :choose nil nil
             :bindings (array-map u (e/cos x) dv (e/* (e/exp x) dx) :reason "the same pattern again"))
       (step :integral/by-parts :then nil
             (m/eq 'I (m/op :- (e/* (e/exp x) (e/sin x)) (e/* (e/exp x) (e/cos x)) 'I)))
       (step :integral/cycle :solve-for nil (m/eq 'I (m/plus-c (F x)))
             :bindings {:unknown 'I})]
      (m/plus-c (F x)))}))

;; Table rule with a log: ln|u| ----------------------------------------------
(defn ln-abs
  "ln|u|: the antiderivative of 1/u on every interval of its domain, not
   only where u > 0. The table rules build it, so the absolute value is in
   the answer itself (verify differentiates it on each branch), not added
   when the answer is rendered."
  [u]
  (e/log (e/abs u)))

(def reciprocal-rule
  "Table entry: ∫ du/u = ln|u| + C."
  (m/eq (m/integral (e// 1 u) u) (m/plus-c (ln-abs u))))

;; 4. Partial fractions: ∫ dx / (x² − 1) ------------------------------------
(def partial-fractions
  (let [f     (fn [x] (e// 1 (e/- (e/square x) 1)))
        F     (fn [x] (e/* (e// 1 2) (e/- (ln-abs (e/- x 1)) (ln-abs (e/+ x 1)))))
        prob  (m/problem {:id :recip :kind :integral :integrand f})
        split (m/op :- (m/op :* (e// 1 2) (m/integral (e// 1 (e/- x 1)) x))
                    (m/op :* (e// 1 2) (m/integral (e// 1 (e/+ x 1)) x)))
        finish [(step :integral/linearity :split (m/integral (f x) x) split)
                (step :integral/table :apply-formula nil (m/plus-c (F x))
                      :bindings {:formula reciprocal-rule})]]
    {:id :recip :title "Partial fractions" :section "Integration of rational functions"
     ;; one point on each interval of the domain: x < −1, −1 < x < 1, x > 1
     :f f :F F :points [1.5 2.0 3.7 0.3 -0.5 -2.5]
     :figure {:a 1.5 :b 4}
     :derivation
     (m/derivation
      prob
      (into [(step :integral/partial-fractions :rewrite (m/integral (f x) x)
                   (m/eq (f x) (e/+ (e// 'A (e/- x 1)) (e// 'B (e/+ x 1))))
                   :bindings {:identity (m/eq (e/- (e/square x) 1) (e/* (e/- x 1) (e/+ x 1)))})
             (step :integral/partial-fractions :then nil (m/eq 'A (e// 1 2))
                   :text "Clear denominators: 1 = A(x + 1) + B(x − 1). At x = 1, A = 1/2; at x = −1, B = −1/2.")]
            finish)
      (m/plus-c (F x)))
     :alternatives
     [{:title "No unknowns: write the numerator as a difference of the factors"
       :derivation
       (m/derivation
        prob
        (into [(step :integral/numerator-split :rewrite (m/integral (f x) x)
                     (m/eq (f x)
                           (e/* (e// 1 2) (e// (e/- (e/+ x 1) (e/- x 1)) (e/* (e/- x 1) (e/+ x 1))))
                           (e/* (e// 1 2) (e/- (e// 1 (e/- x 1)) (e// 1 (e/+ x 1)))))
                     :bindings {:identity (m/eq 1 (e/* (e// 1 2) (e/- (e/+ x 1) (e/- x 1))))})]
              finish)
        (m/plus-c (F x)))}
      {:title "Heaviside's cover-up: each coefficient is one evaluation"
       :derivation
       (m/derivation
        prob
        (into [(step :integral/cover-up :cover-up (f x) (m/eq 'A (e// 1 2))
                     :bindings {:factor (e/- x 1) :root 1 :rest (e// 1 (e/+ x 1)) :coef 'A})
               (step :integral/cover-up :cover-up (f x) (m/eq 'B (e// -1 2))
                     :bindings {:factor (e/+ x 1) :root -1 :rest (e// 1 (e/- x 1)) :coef 'B})]
              finish)
        (m/plus-c (F x)))}
      {:title "Straight from the table: ∫ dx/(x² − a²) with a = 1"
       :derivation
       (m/derivation
        prob
        [(step :integral/table :apply-formula (m/integral (f x) x)
               (m/plus-c (e/* (e// 1 2) (ln-abs (e// (e/- x 1) (e/+ x 1)))))
               :bindings {:formula (m/eq (m/integral (e// 1 (e/- (e/square x) (e/square 'a))) x)
                                         (m/plus-c (e/* (e// 1 (e/* 2 'a))
                                                        (ln-abs (e// (e/- x 'a) (e/+ x 'a))))))})
         (step :integral/note :hence nil (m/plus-c (F x))
               :text "The same answer: ln|(x − 1)/(x + 1)| = ln|x − 1| − ln|x + 1|.")]
        (m/plus-c (F x)))}]}))

;; 5. A definite integral and its area: ∫₀^π sin x dx ------------------------
(def area
  (let [f (fn [x] (e/sin x))
        F (fn [x] (e/- (e/cos x)))]
    {:id :sin-area :title "Area under one arch of the sine" :section "Definite integrals"
     :f f :F F :points [0.4 1.2 2.9] :bounds [0 Math/PI] :value 2.0
     :figure {:a 0 :b Math/PI}
     :derivation
     (m/derivation
      (m/problem {:id :sin-area :kind :definite :integrand f :bounds [0 'π]})
      [(step :integral/table :table (m/integral (f x) x [0 'π]) (m/plus-c (F x)))
       (step :integral/ftc :evaluate nil
             (m/eq (e/- (e/- (e/cos 'π)) (e/- (e/cos 0))) (e/+ 1 1) 2))]
      2)}))

(def all
  "Every worked example, in reading order."
  [substitution by-parts cyclic partial-fractions area])
