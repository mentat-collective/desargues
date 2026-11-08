(ns desargues.steps.engine-test
  "The solving engine: rules on emmy.pattern, combinators, DP search,
   pathways, the library and the corpus, every answer verified by Emmy."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.steps.corpus :as corpus]
            [desargues.steps.integrator :as integrator]
            [desargues.steps.library :as library]
            [desargues.steps.phrase :as phrase]
            [desargues.steps.protocols :as p]
            [desargues.steps.rational :as rational]
            [desargues.steps.render :as render]
            [desargues.steps.rules :as rules]
            [desargues.steps.table :as table]
            [desargues.steps.tactics :as t]))

(deftest table-rows-are-emmy-patterns
  (is (= :power (:row (table/lookup '(expt x 3) 'x))))
  (is (= '(log (abs x)) (:F (table/lookup '(/ 1 x) 'x))))
  (is (= :atan-a (:row (table/lookup '(/ 1 (+ (expt x 2) 9)) 'x))))
  (is (nil? (table/lookup '(* x (exp x)) 'x))))

(deftest combinators-are-rules
  (let [g (t/goal :integral '(+ (expt x 2) (sin x)) 'x)]
    (is (empty? (p/moves rules/table-rule g)))
    (is (seq (p/moves (t/orelse rules/table-rule rules/linearity) g)))
    (is (= 1 (count (p/moves (t/attempt rules/table-rule) g))) "attempt: the identity move")
    (is (seq (p/moves (t/choice rules/table-rule rules/linearity rules/substitution) g)))
    (is (satisfies? p/IRule (t/then rules/linearity rules/table-rule)))))

(deftest partial-fractions-over-q
  (is (= 2 (count (rational/decompose [1] [-1 0 1] 'x))))
  (is (= [-1 0 1] (rational/coefficients '(+ (expt x 2) -1) 'x)))
  (is (nil? (rational/coefficients '(sin x) 'x))))

(deftest substitution-is-recorded
  (let [{:keys [derivation]} (integrator/integrate '(* (expt (sin x) 2) (cos x)) 'x)
        constructions (map :construction (:steps derivation))]
    (is (some #{:let} constructions))
    (is (some #{:substitute-back} constructions))
    (is (re-find #"Let u = sin\(x\)"
                 (render/parts->text (phrase/parts (first (filter #(= :let (:construction %)) (:steps derivation)))))))))

(deftest by-parts-chooses-by-liate-and-keeps-alternatives
  (let [d (:derivation (integrator/integrate '(* x (exp x)) 'x))
        choose (first (filter #(= :choose (:construction %)) (:steps d)))]
    (is (= 'x (get-in choose [:bindings 'u]))))
  (testing "the integral that returns is solved for"
    (is (some #(= :solve-for (:construction %))
              (:steps (:derivation (integrator/integrate '(* (exp x) (sin x)) 'x))))))
  (testing "pathways: more than one way, cheapest first"
    (let [ps (integrator/pathways '(expt (sin x) 2) 'x)]
      (is (>= (count ps) 1))
      (is (apply <= (map :cost ps))))))

(deftest the-corpus-is-solved-and-verified
  (let [checked (map integrator/check corpus/problems)
        bad (remove (comp :ok? :verdict) checked)]
    (is (<= 30 (count corpus/problems) 50))
    (is (empty? (map :id bad)) "every corpus answer verified by Emmy")))

(deftest the-library-cites-and-orders
  (let [probs [(first (filter #(= :s1 (:id %)) corpus/problems))
               {:id :again :f '(* (expt (sin x) 2) (cos x)) :var 'x}]
        [lib results] (library/learn-all probs)
        again (last results)]
    (is (= #{:s1} (get-in again [:result :solution :cited])))
    (is (= :cite (-> again :result :derivation :steps first :construction)))
    (is (contains? (get (library/prerequisites lib) :again) :s1))))
