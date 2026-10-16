(ns desargues.steps.table
  "The TABLE OF INTEGRALS as emmy.pattern rules over the integration
   variable, and its rows as Steps.

   Each row is an Emmy pattern in the variable `v` (patterns are built per
   variable so ?u-free constants can be tested with predicates) and a
   consequence giving the antiderivative from the bindings. `lookup` tries
   the rows in order (emmy.pattern.rule/choice semantics) and answers the
   first {:row id :bindings b :F form}. The table rule of the solver
   (desargues.steps.rules) records the row and its bindings in its Step, so
   a reader sees which entry of the table closed the goal.

   Host-free .cljc."
  (:require [desargues.steps.expr :as x]
            [emmy.pattern.match :as m]))

(defn- const? [v] #(and (some? %) (x/free-of? v %)))

(defn rows
  "The table rows for variable v: [id pattern (fn [bindings] F) formula-form]."
  [v]
  (let [c?  (const? v)
        n?  #(and (number? %) (not= -1 %))
        pos #(and (number? %) (pos? %))]
    [[:const    (list '? '?c c?)                          (fn [b] (list '* (b '?c) v))]
     [:var      v                                         (fn [_] (list '/ (list 'expt v 2) 2))]
     [:power    (list 'expt v (list '? '?n n?))           (fn [b] (let [k (x/simp (list '+ (b '?n) 1))]
                                                                     (list '/ (list 'expt v k) k)))]
     [:recip    (list '/ 1 v)                             (fn [_] (list 'log (list 'abs v)))]
     [:recip    (list 'expt v -1)                         (fn [_] (list 'log (list 'abs v)))]
     [:sqrt     (list 'sqrt v)                            (fn [_] (list '* (/ 2 3) (list 'expt v (/ 3 2))))]
     [:rsqrt    (list '/ 1 (list 'sqrt v))                (fn [_] (list '* 2 (list 'sqrt v)))]
     [:exp      (list 'exp v)                             (fn [_] (list 'exp v))]
     [:exp-a    (list 'expt (list '? '?a pos) v)          (fn [b] (list '/ (list 'expt (b '?a) v) (list 'log (b '?a))))]
     [:sin      (list 'sin v)                             (fn [_] (list '- (list 'cos v)))]
     [:cos      (list 'cos v)                             (fn [_] (list 'sin v))]
     [:sec2     (list '/ 1 (list 'expt (list 'cos v) 2))  (fn [_] (list 'tan v))]
     [:csc2     (list '/ 1 (list 'expt (list 'sin v) 2))  (fn [_] (list '- (list 'cot v)))]
     [:tan      (list 'tan v)                             (fn [_] (list '- (list 'log (list 'abs (list 'cos v)))))]
     [:tan      (list '/ (list 'sin v) (list 'cos v))     (fn [_] (list '- (list 'log (list 'abs (list 'cos v)))))]
     [:cot      (list '/ (list 'cos v) (list 'sin v))     (fn [_] (list 'log (list 'abs (list 'sin v))))]
     [:atan     (list '/ 1 (list '+ (list 'expt v 2) 1))  (fn [_] (list 'atan v))]
     [:atan-a   (list '/ 1 (list '+ (list 'expt v 2) (list '? '?a2 pos)))
      (fn [b] (let [a (list 'sqrt (b '?a2))] (list '* (list '/ 1 a) (list 'atan (list '/ v a)))))]
     [:asin     (list '/ 1 (list 'sqrt (list '+ (list '* -1 (list 'expt v 2)) 1))) (fn [_] (list 'asin v))]
     [:asin-a   (list '/ 1 (list 'sqrt (list '+ (list '* -1 (list 'expt v 2)) (list '? '?a2 pos))))
      (fn [b] (list 'asin (list '/ v (list 'sqrt (b '?a2)))))]
     [:log      (list 'log v)                             (fn [_] (list '- (list '* v (list 'log v)) v))]
     [:sinh     (list 'sinh v)                            (fn [_] (list 'cosh v))]
     [:cosh     (list 'cosh v)                            (fn [_] (list 'sinh v))]]))

(defn lookup
  "The first table row whose pattern matches `f` in variable `v`:
   {:row id :bindings b :F antiderivative}, or nil."
  [f v]
  (some (fn [[id pat F]]
          (let [b (m/match pat f)]
            (when-not (m/failed? b)
              (let [b (into {} (filter (comp symbol? key)) b)]
                {:row id :bindings b :F (x/simp (F b))}))))
        (rows v)))

(defn formula
  "The table entry as an equation ∫ f dv = F, for a :apply-formula step."
  [{:keys [F]} f v integral eq plus-c]
  (eq (integral f v) (plus-c F)))
