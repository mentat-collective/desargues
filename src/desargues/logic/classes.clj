(ns desargues.logic.classes
  "Statements about classes as statements about one element.

   `unfold` is OPEN on the class operator: it rewrites membership in
   (op & args) by op's definition, ONE step, e.g.

     (in x (union A B))  ->  (or (in x A) (in x B))
     (in x (sym-diff A B)) -> (in x (union (diff A B) (diff B A)))

   `statement` reads a class statement by the axiom of extent: A = B becomes
   x in A <=> x in B and A subset B becomes x in A => x in B, for an arbitrary
   element x. `membership` unfolds every membership down to atoms (in x A) with
   A a letter, so a Boolean class identity holds exactly when the resulting
   propositional formula is a tautology. The steps are kept for display."
  (:require [desargues.logic.formula :as f]))

(def element
  "The arbitrary element membership statements are read at."
  'x)

(defmulti unfold
  "(in x (op & args)) -> the formula op's definition gives, one step."
  (fn [_x [op]] op))

(defmethod unfold :default [_ _] nil)

(defmethod unfold 'union [x [_ & as]] (cons 'or (map #(list 'in x %) as)))
(defmethod unfold 'inter [x [_ & as]] (cons 'and (map #(list 'in x %) as)))
(defmethod unfold 'compl [x [_ a]] (list 'not (list 'in x a)))
(defmethod unfold 'diff [x [_ a b]] (list 'and (list 'in x a) (list 'not (list 'in x b))))
(defmethod unfold 'sym-diff [x [_ a b]] (list 'in x (list 'union (list 'diff a b) (list 'diff b a))))
(defmethod unfold 'empty [_ _] false)
(defmethod unfold 'universe [_ _] true)

(defn- class-term [t] (if (#{'empty 'universe} t) (list t) t))

(defn- unfoldable
  "The membership (in x C) where C has an unfold method, or nil."
  [g]
  (when (and (seq? g) (= 'in (first g)))
    (let [[_ x c] g
          c (class-term c)]
      (when (seq? c)
        (let [r (unfold x c)]
          (when (some? r) r))))))

(defn statement
  "Class statement -> the formula about the element x it means:
     (= A B)      -> (iff (in x A) (in x B))
     (subset A B) -> (implies (in x A) (in x B))
   connectives applied over statements are kept; anything else is returned
   unchanged."
  [s]
  (cond
    (and (seq? s) (= '= (first s))) (list 'iff (list 'in element (nth s 1)) (list 'in element (nth s 2)))
    (and (seq? s) (= 'subset (first s))) (list 'implies (list 'in element (nth s 1)) (list 'in element (nth s 2)))
    (f/compound? s) (cons (first s) (map statement (rest s)))
    :else s))

(defn- step-once
  "Unfold the first (leftmost, outermost) unfoldable membership in g:
   [g' path] or nil."
  [g]
  (if-some [r (unfoldable g)] [r []] (when (f/compound? g)
      (some (fn [[i a]]
              (when-let [[a' p] (step-once a)]
                [(apply list (first g) (assoc (vec (rest g)) i a')) (into [i] p)]))
            (map-indexed vector (rest g))))))

(defn unfold-steps
  "Every one-step unfolding of formula until none applies:
   [{:formula :path :rule}] where :path locates the rewritten subterm in the
   formula BEFORE the step and :rule is the operator unfolded."
  [formula]
  (loop [g formula steps []]
    (if-let [[g' p] (step-once g)]
      (recur g' (conj steps {:formula g :path p
                             :rule (let [c (class-term (nth (f/subterm g p) 2))] (first c))}))
      (conj steps {:formula g :path nil :rule nil}))))

(defn membership
  "The propositional formula a class statement means: atoms (in x A) with A a
   class letter."
  [s]
  (:formula (peek (unfold-steps (statement s)))))
