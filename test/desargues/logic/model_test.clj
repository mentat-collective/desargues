(ns desargues.logic.model-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.logic.model :as m]))

(def u #{0 1 2})

(defn- env [& kvs] (assoc (apply hash-map kvs) ::m/universe u))

(defn- den [form & kvs] (m/denote form (apply env kvs)))

(deftest class-operators
  (is (= #{0 1 2} (den '(union A B) 'A #{0} 'B #{1 2})))
  (is (= #{1} (den '(inter A B) 'A #{0 1} 'B #{1 2})))
  (is (= #{0} (den '(diff A B) 'A #{0 1} 'B #{1 2})))
  (is (= #{0 2} (den '(sym-diff A B) 'A #{0 1} 'B #{1 2})))
  (is (= #{2} (den '(compl A) 'A #{0 1})))
  (is (= #{#{} #{0}} (den '(power A) 'A #{0})))
  (is (= #{0 1} (den '(big-union cA) 'cA #{#{0} #{1}})))
  (is (= #{0 1} (den '(set-of a b) 'a 0 'b 1)))
  (is (true? (den '(is-set A) 'A #{0})))
  (testing "a replacement class {t : ...} collects terms outside the universe"
    (is (= #{[0 [0 0]] [1 [1 1]]}
           (den '(class [p] (exists [x] (and (in x A) (= p (pair x (pair x x)))))) 'A #{0 1})))))

(deftest graphs-and-functions
  (let [g #{[0 1] [1 2]} h #{[1 0] [2 2]}]
    (testing "Pinter's composition: (compose G H) applies H first"
      (is (= #{[0 0] [1 2]} (den '(compose G H) 'G h 'H g))))
    (is (= #{[1 0] [2 1]} (den '(inverse G) 'G g)))
    (is (= #{1} (den '(image G A) 'G g 'A #{0})))
    (is (= 2 (den '(apply G x) 'G g 'x 1)))
    (is (true? (den '(maps G A B) 'G g 'A #{0 1} 'B u)))
    (is (true? (den '(injective G) 'G g)))
    (is (false? (den '(surjective G A B) 'G g 'A #{0 1} 'B u)))
    (is (true? (den '(bijective G A B) 'G g 'A #{0 1} 'B #{1 2})))
    (is (= #{[0 0] [1 1] [0 1] [1 0] [2 2]} (den '(kernel f) 'f #{[0 5] [1 5] [2 6]})))
    (is (= #{[#{} #{}] [#{0} #{1}]} (den '(image-map f) 'f #{[0 1]})))
    (is (= 4 (count (den '(exp B A) 'A #{0 1} 'B #{0 1}))))))

(deftest relations
  (let [r #{[0 0] [1 1] [2 2] [0 1] [1 0]}]
    (is (true? (den '(equivalence G A) 'G r 'A u)))
    (testing "an equivalence relation in A is a relation in A"
      (is (false? (den '(equivalence G A) 'G r 'A #{0 1}))))
    (is (= #{0 1} (den '(class-of x G) 'G r 'x 0)))
    (is (= #{#{0 1} #{2}} (den '(quotient A G) 'G r 'A u)))
    (is (= #{[#{0 1} #{0 1}] [#{2} #{2}]} (den '(quotient-relation G G) 'G r)))
    (is (= #{[0 0] [2 2]} (den '(relation-restrict G B) 'G r 'B #{0 2})))
    (testing "an indexed partition {A_i} of A: members nonempty, pairwise disjoint, covering"
      (is (true? (den '(partition (family [i I] A_i) A) 'I #{0 1} 'A u 'A_ {0 #{0 1} 1 #{2} 2 #{}})))
      (is (false? (den '(partition (family [i I] A_i) A) 'I #{0 1} 'A u 'A_ {0 #{0 1} 1 #{1 2} 2 #{}}))))))

(deftest indexed-families
  (let [fam {0 #{0 1} 1 #{1 2} 2 #{}}]
    (testing "A_i is the member at i of the family held by the letter A_"
      (is (= #{0 1 2} (den '(indexed-union [i I] A_i) 'I #{0 1} 'A_ fam)))
      (is (= #{1} (den '(indexed-inter [i I] A_i) 'I #{0 1} 'A_ fam)))
      (is (= #{1} (den '(indexed-inter [[i j] (product I J)] (inter A_i A_j)) 'I #{0} 'J #{1} 'A_ fam))))
    (testing "(index A i) reads the family held by A"
      (is (= #{1 2} (den '(index A i) 'A fam 'i 1)))
      (is (= 4 (count (den '(family-product A I) 'I #{0 1} 'A fam)))))))

(deftest orders
  (let [chain3 #{[0 0] [1 1] [2 2] [0 1] [1 2] [0 2]}
        e (fn [& kvs] (apply env '<= chain3 kvs))]
    (is (true? (m/denote '(leq x y) (e 'x 0 'y 2))))
    (is (= 1 (m/denote '(sup B) (e 'B #{0 1}))))
    (testing "sup-in computes in A; sup computes in the ambient chain"
      (is (= 1 (m/denote '(sup-in A B) (e 'A #{1 2} 'B #{}))))
      (is (= 0 (m/denote '(sup B) (e 'B #{})))))
    (is (= #{1 2} (m/denote '(upper-bounds B) (e 'B #{0 1}))))
    (is (true? (m/denote '(chain C A) (e 'C #{0 2} 'A u))))
    (is (true? (m/denote '(cut L U A) (e 'L #{0} 'U #{1 2} 'A u))))
    (is (true? (m/denote '(lattice A) (e 'A u))))
    (is (true? (m/denote '(isomorphic A B) (e 'A #{0 1} 'B #{1 2}))))
    (is (true? (m/denote '(maximal (pair a b) (lex-product A B)) (e 'a 1 'b 1 'A #{0 1} 'B #{0 1}))))
    (is (true? (m/denote '(complete-lattice (cuts A)) (e 'A u))))
    (is (false? (m/denote '(complete-lattice (cuts A)) (env '<= #{[0 0] [1 1]} 'A #{0 1})))))
  (testing "order-union keeps A and C incomparable, where the model's order need not"
    (let [v (env '<= #{[0 0] [1 1] [2 2] [2 0] [2 1]} 'A #{1} 'B #{2} 'C #{0})]
      (is (false? (m/denote '(isomorphic (union A C) (union B C)) v)))
      (is (true? (m/denote '(isomorphic (order-union A C) (order-union B C)) v))))))

(deftest sorts-are-inferred
  (is (= {'P :prop 'Q :prop} (m/sorts '(iff (or P Q) (or Q P)))))
  (is (= :classes (get (m/sorts '(implies (in A cB) (subset A (big-union cB)))) 'cB)))
  (is (= [:family :relation]
         (get (m/sorts '(= (ran (indexed-union [i I] G_i)) (indexed-union [i I] (ran G_i)))) 'G_)))
  (is (= {'A :class 'A_ [:family :class] 'I :class}
         (m/sorts '(partition (family [i I] A_i) A))))
  (is (= :function (get (m/sorts '(maps f A B)) 'f)))
  (is (= :order (get (m/sorts '(leq x y)) '<=))))

(deftest definitions-unfold
  (is (= ['(implies (and (maps f A B) (defined (kernel f))) (equivalence (kernel f) A)) '[[G (kernel f)]]]
         (m/unfold-definitions '(implies (and (maps f A B) (= G (kernel f))) (equivalence G A))))))

(deftest model-search
  (testing "A - B = B - A is refuted"
    (let [r (m/check '(= (diff A B) (diff B A)))]
      (is (= :counterexample (:verdict r)))
      (is (false? (m/denote '(= (diff A B) (diff B A)) (:counterexample r))))))
  (testing "De Morgan holds in every model"
    (let [r (m/check '(= (compl (union A B)) (inter (compl A) (compl B))) :universe u)]
      (is (= :holds (:verdict r)))
      (is (:exhaustive? r))))
  (testing "quantifiers range over the sort their body gives the variable"
    (is (= :holds (:verdict (m/check '(forall [C] (subset (inter A C) A)))))))
  (testing "a statement whose hypothesis no model meets is vacuous"
    (is (= :vacuous (:verdict (m/check '(implies (and (in x A) (notin x A)) (= A B))))))))
