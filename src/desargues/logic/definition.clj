(ns desargues.logic.definition
  "Definitions of the set-theory vocabulary, as statements.

   `defines` is OPEN on the predicate symbol: (defines form) -> the formula
   form abbreviates, one level, or nil. Class statements (= and subset
   between classes) are read by the axiom of extent; membership in a class
   term unfolds by desargues.logic.classes/unfold. Bound variables are the
   symbols x y z (fresh ones are chosen when those occur in form)."
  (:require [desargues.logic.classes :as cl]
            [desargues.logic.term :as t]))

(defmulti defines
  "form -> the formula it abbreviates, one level, or nil."
  (fn [form] (when (seq? form) (first form))))

(defmethod defines :default [_] nil)

(defn- fresh
  "n bound-variable symbols not occurring in form."
  [form n]
  (let [used (set (filter symbol? (tree-seq coll? seq form)))]
    (vec (take n (remove used (concat '[x y z u v w] (map #(symbol (str "x" %)) (range))))))))

(defmethod defines 'subset [[_ a b :as form]]
  (let [[x] (fresh form 1)]
    (list 'forall [x] (list 'implies (list 'in x a) (list 'in x b)))))

(defmethod defines '= [[_ a b :as form]]
  (let [[x] (fresh form 1)]
    (list 'forall [x] (list 'iff (list 'in x a) (list 'in x b)))))

(defmethod defines 'proper-subset [[_ a b]]
  (list 'and (list 'subset a b) (list 'not (list '= a b))))

(defmethod defines 'function [[_ f :as form]]
  (let [[x y z] (fresh form 3)]
    (list 'forall [x y z]
          (list 'implies (list 'and (list 'in (list 'pair x y) f) (list 'in (list 'pair x z) f))
                (list '= y z)))))

(defmethod defines 'injective [[_ f :as form]]
  (let [[x y] (fresh form 2)]
    (list 'forall [x y]
          (list 'implies (list '= (list 'apply f x) (list 'apply f y)) (list '= x y)))))

(defmethod defines 'surjective [[_ f a b :as form]]
  (let [[x y] (fresh form 2)]
    (list 'forall [y] (list 'implies (list 'in y b)
                            (list 'exists [x] (list 'and (list 'in x a) (list '= (list 'apply f x) y)))))))

(defmethod defines 'reflexive [[_ r a :as form]]
  (let [[x] (fresh form 1)]
    (list 'forall [x] (list 'implies (list 'in x a) (list 'in (list 'pair x x) r)))))

(defmethod defines 'symmetric [[_ r :as form]]
  (let [[x y] (fresh form 2)]
    (list 'forall [x y] (list 'implies (list 'in (list 'pair x y) r) (list 'in (list 'pair y x) r)))))

(defmethod defines 'transitive [[_ r :as form]]
  (let [[x y z] (fresh form 3)]
    (list 'forall [x y z]
          (list 'implies (list 'and (list 'in (list 'pair x y) r) (list 'in (list 'pair y z) r))
                (list 'in (list 'pair x z) r)))))

(defmethod defines 'antisymmetric [[_ r :as form]]
  (let [[x y] (fresh form 2)]
    (list 'forall [x y]
          (list 'implies (list 'and (list 'in (list 'pair x y) r) (list 'in (list 'pair y x) r))
                (list '= x y)))))

(defmethod defines 'equivalence [[_ r a]]
  (list 'and (list 'reflexive r a) (list 'symmetric r) (list 'transitive r)))

(defmethod defines 'partial-order [[_ r a]]
  (list 'and (list 'reflexive r a) (list 'antisymmetric r) (list 'transitive r)))

(defmethod defines 'element [[_ a :as form]]
  (let [[c] (fresh form 1)]
    (list 'exists [c] (list 'in a c))))

(defmethod defines 'disjoint [[_ a b]]
  (list '= (list 'inter a b) 'empty))

;; ---------------------------------------------------------------------------
;; Membership in class terms (Pinter 1.2 and 1.4), on classes/unfold

(defmethod cl/unfold 'class [z [_ [v] body]]
  (list 'and (list 'element z) (t/substitute body {v z})))

(defmethod cl/unfold 'set-of [z [_ & as]]
  (list 'and (list 'element z)
        (if (= 1 (count as))
          (list '= z (first as))
          (cons 'or (map #(list '= z %) as)))))

(defmethod cl/unfold 'pair [z [_ a b]]
  (list 'in z (list 'set-of (list 'set-of a) (list 'set-of a b))))

(defmethod cl/unfold 'pair-alt [z [_ a b]]
  (list 'in z (list 'set-of (list 'set-of a 'empty) (list 'set-of b (list 'set-of 'empty)))))

(defmethod cl/unfold 'product [z [_ a b :as c]]
  (let [[x y] (fresh (list z c) 2)]
    (list 'exists [x]
          (list 'exists [y]
                (list 'and (list '= z (list 'pair x y)) (list 'in x a) (list 'in y b))))))
