(ns desargues.logic.definition.order
  "Definitions of the order and lattice vocabulary (Pinter, chapter 4),
   registered on desargues.logic.definition/defines, and membership in the
   order's class terms (down-sets, intervals, bound classes) registered on
   desargues.logic.classes/unfold.

   The order is the ambient letter leq; an element's membership in the
   ambient ordered class is left implicit, as in the book."
  (:require [desargues.logic.classes :as cl]
            [desargues.logic.definition :as d]))

(defn- fresh
  "n bound-variable symbols not occurring in form."
  [form n]
  (let [used (set (filter symbol? (tree-seq coll? seq form)))]
    (vec (take n (remove used (concat '[x y z u v w] (map #(symbol (str "x" %)) (range))))))))

;; ---------------------------------------------------------------------------
;; Comparisons (4.0, 4.3, 4.4)

(defmethod d/defines 'lt [[_ a b]]
  (list 'and (list 'leq a b) (list 'not (list '= a b))))

(defmethod d/defines 'comparable [[_ a b]]
  (list 'or (list 'leq a b) (list 'leq b a)))

(defmethod d/defines 'chain [[_ c a :as form]]
  (let [[x y] (fresh form 2)]
    (list 'and (list 'subset c a)
          (list 'forall [x y] (list 'implies (list 'and (list 'in x c) (list 'in y c)) (list 'comparable x y))))))

(defmethod d/defines 'fully-ordered [[_ a :as form]]
  (let [[x y] (fresh form 2)]
    (list 'forall [x y] (list 'implies (list 'and (list 'in x a) (list 'in y a)) (list 'comparable x y)))))

;; ---------------------------------------------------------------------------
;; Order-preserving functions (4.10, 4.11, 4.15)

(defn- preserves [rel [_ f a _ :as form]]
  (let [[x y] (fresh form 2)]
    (list 'forall [x y]
          (list 'implies (list 'and (list 'in x a) (list 'in y a) (list rel x y))
                (list rel (list 'apply f x) (list 'apply f y))))))

(defmethod d/defines 'increasing [form] (preserves 'leq form))

(defmethod d/defines 'strictly-increasing [form] (preserves 'lt form))

(defmethod d/defines 'isomorphism [[_ f a b :as form]]
  (let [[x y] (fresh form 2)]
    (list 'and (list 'bijective f a b)
          (list 'forall [x y]
                (list 'implies (list 'and (list 'in x a) (list 'in y a))
                      (list 'iff (list 'leq x y) (list 'leq (list 'apply f x) (list 'apply f y))))))))

(defmethod d/defines 'isomorphic [[_ a b :as form]]
  (let [[f] (fresh (list form 'x 'y 'z) 1)]
    (list 'exists [f] (list 'isomorphism f a b))))

;; ---------------------------------------------------------------------------
;; Distinguished elements and bounds (4.18, 4.19, 4.20)

(defn- extreme [rel [_ m a :as form]]
  (let [[x] (fresh form 1)]
    (list 'and (list 'in m a)
          (list 'forall [x] (list 'implies (list 'and (list 'in x a) (rel x m)) (list '= x m))))))

(defmethod d/defines 'maximal [form] (extreme (fn [x m] (list 'leq m x)) form))

(defmethod d/defines 'minimal [form] (extreme (fn [x m] (list 'leq x m)) form))

(defn- bound-of [above? [_ m b :as form]]
  (let [[x] (fresh form 1)]
    (list 'forall [x] (list 'implies (list 'in x b) (if above? (list 'leq x m) (list 'leq m x))))))

(defmethod d/defines 'greatest [[_ m a :as form]]
  (list 'and (list 'in m a) (bound-of true form)))

(defmethod d/defines 'least [[_ m a :as form]]
  (list 'and (list 'in m a) (bound-of false form)))

(defmethod d/defines 'upper-bound [form] (bound-of true form))

(defmethod d/defines 'lower-bound [form] (bound-of false form))

(defmethod d/defines 'bounded-above [[_ b a :as form]]
  (let [[x] (fresh form 1)]
    (list 'exists [x] (list 'and (list 'in x a) (list 'upper-bound x b)))))

(defmethod d/defines 'bounded-below [[_ b a :as form]]
  (let [[x] (fresh form 1)]
    (list 'exists [x] (list 'and (list 'in x a) (list 'lower-bound x b)))))

(defn- has-bound [bound rel [_ b a :as form]]
  (let [[s x] (fresh form 2)]
    (list 'exists [s]
          (list 'and (list 'in s a) (list bound s b)
                (list 'forall [x] (list 'implies (list 'and (list 'in x a) (list bound x b)) (rel s x)))))))

(defmethod d/defines 'has-sup [form] (has-bound 'upper-bound (fn [s x] (list 'leq s x)) form))

(defmethod d/defines 'has-inf [form] (has-bound 'lower-bound (fn [s x] (list 'leq x s)) form))

;; ---------------------------------------------------------------------------
;; Convex classes and cuts (Ex. 4.2.4, 4.7)

(defmethod d/defines 'convex [[_ c a :as form]]
  (let [[x y z] (fresh form 3)]
    (list 'and (list 'subset c a)
          (list 'forall [x y z]
                (list 'implies (list 'and (list 'in x c) (list 'in y c) (list 'in z a) (list 'leq x z) (list 'leq z y))
                      (list 'in z c))))))

(defmethod d/defines 'cut [[_ l u a :as form]]
  (let [[x y] (fresh form 2)]
    (list 'and (list 'not (list '= l 'empty)) (list 'not (list '= u 'empty))
          (list '= (list 'inter l u) 'empty) (list '= (list 'union l u) a)
          (list 'forall [x y] (list 'implies (list 'and (list 'in x l) (list 'in y a) (list 'leq y x)) (list 'in y l)))
          (list 'forall [x y] (list 'implies (list 'and (list 'in x u) (list 'in y a) (list 'leq x y)) (list 'in y u))))))

;; ---------------------------------------------------------------------------
;; Completeness and lattices (4.33, 4.35, 4.43, 4.45)

(defmethod d/defines 'conditionally-complete [[_ a :as form]]
  (let [[b] (fresh (list form 'x 'y 'z) 1)]
    (list 'forall [b] (list 'implies (list 'and (list 'subset b a) (list 'not (list '= b 'empty)) (list 'bounded-above b a))
                            (list 'has-sup b a)))))

(defmethod d/defines 'complete-lattice [[_ a :as form]]
  (let [[b] (fresh (list form 'x 'y 'z) 1)]
    (list 'forall [b] (list 'implies (list 'subset b a) (list 'has-sup b a)))))

(defmethod d/defines 'lattice [[_ a :as form]]
  (let [[x y] (fresh form 2)]
    (list 'forall [x y] (list 'implies (list 'and (list 'in x a) (list 'in y a))
                              (list 'and (list 'has-sup (list 'set x y) a) (list 'has-inf (list 'set x y) a))))))

(defmethod d/defines 'sublattice [[_ b a :as form]]
  (let [[x y] (fresh form 2)]
    (list 'and (list 'subset b a)
          (list 'forall [x y] (list 'implies (list 'and (list 'in x b) (list 'in y b))
                                    (list 'and (list 'in (list 'join x y) b) (list 'in (list 'meet x y) b)))))))

;; ---------------------------------------------------------------------------
;; Membership in the order's class terms (4.5, 4.20, Ex. 4.2.8, Ex. 4.2.10)

(defmethod cl/unfold 'down-set [x [_ a s]] (list 'and (list 'in x s) (list 'leq x a)))

(defmethod cl/unfold 'initial-segment [x [_ a s]] (list 'and (list 'in x s) (list 'lt x a)))

(defmethod cl/unfold 'closed-interval [x [_ a b s :as c]]
  (if (= 4 (count c))
    (list 'and (list 'in x s) (list 'leq a x) (list 'leq x b))
    (list 'and (list 'leq a x) (list 'leq x b))))

(defmethod cl/unfold 'upper-bounds [x [_ b]] (list 'upper-bound x b))

(defmethod cl/unfold 'lower-bounds [x [_ b]] (list 'lower-bound x b))
