(ns desargues.studio.blackboard-lessons-test
  "The blackboard's hand-written worked examples, checked by Emmy: each
   answer differentiated back gives the integrand (symbolically, and at
   sample points), the definite integral is F(b) - F(a), and the figure
   pane's plain fns agree with the Emmy ones. Plus the step-solver adapter."
  (:require [clojure.test :refer [deftest is testing]]
            [emmy.env :as e]
            [desargues.studio.blackboard-lessons :as bl]))

(def emmy
  "Integrand f and antiderivative F of each lesson, as Emmy fns: the answer
   the board writes, transcribed from its TeX."
  {:u-sub    {:f (fn [x] (e/* (e/expt (e/sin x) 2) (e/cos x)))
              :F (fn [x] (e// (e/expt (e/sin x) 3) 3))}
   :by-parts {:f (fn [x] (e/* x (e/exp x)))
              :F (fn [x] (e/* (e/- x 1) (e/exp x)))}
   :cyclic   {:f (fn [x] (e/* (e/exp x) (e/sin x)))
              :F (fn [x] (e// (e/* (e/exp x) (e/- (e/sin x) (e/cos x))) 2))}
   ;; x > 1, where |(x-1)/(x+1)| = (x-1)/(x+1)
   :partial  {:f (fn [x] (e// 1 (e/- (e/expt x 2) 1)))
              :F (fn [x] (e/* (e// 1 2) (e/log (e// (e/- x 1) (e/+ x 1)))))}
   :area     {:f (fn [x] (e/sin x))
              :F (fn [x] (e/- (e/cos x)))
              :value 2}})

(def sample-xs [1.3 1.7 2.2 2.9 3.6])

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(deftest every-lesson-has-an-emmy-check
  (is (= (set (map :id bl/lessons)) (set (keys emmy)))))

(deftest answers-differentiate-back-to-the-integrand
  (doseq [[k {:keys [f F]}] emmy]
    (testing (str (name k) ": symbolically, D F - f simplifies to 0")
      (is (= 0 (e/freeze (e/simplify (e/- ((e/D F) 'x) (f 'x)))))))
    (testing (str (name k) ": numerically at sample points")
      (doseq [x sample-xs]
        (is (close? ((e/D F) x) (f x)) (str "x = " x))))))

(deftest the-definite-integral-is-two
  (let [{:keys [F value]} (emmy :area)]
    (is (close? value (e/- (F Math/PI) (F 0.0))))
    (testing "and the answer the board boxes says so"
      (is (= "2" (:answer (bl/lesson :area))))
      (is (every? #(= "= 2" (:tex (peek (:steps %))))
                  (:pathways (bl/lesson :area)))))))

(deftest figure-fns-agree-with-emmy
  (doseq [{:keys [id figure]} bl/lessons
          :let [{:keys [f F]} (emmy id)]
          x sample-xs]
    (is (close? ((:f figure) x) (f x)) (str (name id) " f at " x))
    (when (:F figure)
      (is (close? ((:F figure) x) (F x)) (str (name id) " F at " x)))))

(deftest every-pathway-ends-in-a-boxed-answer
  (doseq [l bl/lessons p (:pathways l)]
    (testing (str (name (:id l)) " / " (:label p))
      (is (>= (count (:pathways l)) 2))
      (is (seq (:steps p)))
      (is (every? #(and (string? (:tex %)) (string? (:explain %))) (:steps p)))
      (is (:answer? (peek (:steps p))))
      (is (= 1 (count (filter :answer? (:steps p))))))))

(deftest adapter-from-the-step-solver-display-shape
  (let [display [{:tex "\\int 2x\\,dx" :explain "Power rule."}
                 {:tex "= x^2 + C" :explain "Done."}]
        p (bl/from-display display "power rule")]
    (is (= "power rule" (:label p)))
    (is (:answer? (peek (:steps p))))
    (is (not (:answer? (first (:steps p))))))
  (testing "a solved problem becomes a lesson: alternatives are extra pathways"
    (let [solution {:answer "x^2 + C" :verified? true
                    :derivation [:d1] :alternatives [[:d2] [:d3]]}
          display  (fn [d] [{:tex (str "\\text{" (name (first d)) "}") :explain "step"}])
          l (bl/from-solution display solution {:id :poly :title "power rule"
                                                :problem "\\int 2x\\,dx"})]
      (is (= 3 (count (:pathways l))))
      (is (= :poly (:id l)))
      (is (re-find #"verified" (:check l)))
      (is (every? #(:answer? (peek (:steps %))) (:pathways l))))))

(deftest adapter-from-a-step-model-derivation
  ;; the shape of desargues.steps.model (branch solution-notebooks): Steps
  ;; with :construction :from :to :bindings, words from phrase/parts
  (let [tex   (fn [e] (cond (string? e) e (symbol? e) (name e) :else (pr-str e)))
        parts (fn [s] (case (:construction s)
                        :let [[:text "Let "] [:math 'u] [:text " = "] [:math "\\sin x"]]
                        [[:text "Then "] [:math (:to s)]]))
        d {:steps [{:construction :let :to "\\int u^2\\,du" :bindings (array-map 'u "\\sin x")}
                   {:construction :then :to "\\frac{u^3}{3} + C" :bindings {}}]
           :answer "\\frac{\\sin^3 x}{3} + C"}
        p (bl/from-derivation d parts tex "substitution")]
    (is (= "substitution" (:label p)))
    (is (= ["\\int u^2\\,du" "\\frac{u^3}{3} + C" "= \\frac{\\sin^3 x}{3} + C"] (mapv :tex (:steps p))))
    (is (= "Let $u$ = $\\sin x$" (:explain (first (:steps p)))) "words from the construction")
    (is (= "u = \\sin x" (:side (first (:steps p)))) "bindings go to the margin")
    (is (:answer? (peek (:steps p))))))
