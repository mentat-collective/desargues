(ns desargues.logic.definition.graph
  "Definitions of ordered pairs, products, graphs, families and sets
   (Pinter, chapter 1, sections 4 to 7).

   Predicates (graph, element, is-set, proper-class) register on
   desargues.logic.definition/defines. Class terms register on
   desargues.logic.classes/unfold, one membership step each:

     (in (pair a b) (inverse G))       -> (in (pair b a) G)
     (in (pair a b) (compose G H))     -> (exists [z] (and (in (pair a z) H) (in (pair z b) G)))
     (in (pair a b) (product A B))     -> (and (in a A) (in b B))
     (in (pair a b) (restrict G B))    -> (and (in (pair a b) G) (in a B))
     (in w C), C one of those, w not written as a pair
                                       -> (exists [x] (exists [y] (and (= w (pair x y)) (in (pair x y) C))))
     (in a (dom G))                    -> (exists [y] (in (pair a y) G))
     (in b (ran G))                    -> (exists [x] (in (pair x b) G))
     (in b (image G B))                -> (exists [x] (and (in x B) (in (pair x b) G)))
     (in x (indexed-union [i I] A_i))  -> (exists [i] (and (in i I) (in x A_i)))
     (in x (indexed-inter [i I] A_i))  -> (forall [i] (implies (in i I) (in x A_i)))
     (in x (big-union cA))             -> (exists [A] (and (in A cA) (in x A)))
     (in x (big-inter cA))             -> (and (element x) (forall [A] (implies (in A cA) (in x A))))
     (in B (power A))                  -> (and (element B) (subset B A))
     (in x (set-of a b))               -> (and (element x) (or (= x a) (= x b)))
     (in x (pair a b))                 -> (in x (set-of (set-of a) (set-of a b)))

   An index pattern keeps its own variables, because a subscripted letter
   A_i names its index inside the symbol; [i j] over I x J reads as the pair
   (pair i j). Bound variables chosen here are fresh for the statement."
  (:require [desargues.logic.classes :as cl]
            [desargues.logic.definition :as d]))

(defn- fresh
  "n symbols from candidates that do not occur in form."
  [form candidates n]
  (let [used (set (filter symbol? (tree-seq coll? seq form)))]
    (vec (take n (remove used (concat candidates (map #(symbol (str "x" %)) (range))))))))

(defn- pair-term? [t] (and (seq? t) (= 'pair (first t)) (= 3 (count t))))

(defn- nest-exists
  "(exists [v1] (exists [v2] ... body)): one variable per quantifier, so a
   witness is chosen one name at a time."
  [vs body]
  (reduce (fn [b v] (list 'exists [v] b)) body (reverse vs)))

;; ---------------------------------------------------------------------------
;; Predicates

(defn- inhabits
  "(exists [Y] (in X Y)): X belongs to some class (1.48)."
  [form x]
  (let [[y] (fresh form '[Y Z W] 1)]
    (list 'exists [y] (list 'in x y))))

(defmethod d/defines 'element [[_ x :as form]] (inhabits form x))

(defmethod d/defines 'is-set [[_ x :as form]] (inhabits form x))

(defmethod d/defines 'proper-class [[_ x :as form]]
  (let [[y] (fresh form '[Y Z W] 1)]
    (list 'forall [y] (list 'not (list 'in x y)))))

(defmethod d/defines 'graph [[_ g]]
  (list 'subset g '(product universe universe)))

;; ---------------------------------------------------------------------------
;; Graphs: a pair is unfolded by the operator's definition, any other element
;; first by being a pair of the graph

(defmulti ^:private pair-member
  "(in (pair a b) (op & args)) -> its definition, for a graph operator op."
  (fn [_a _b [op]] op))

(defn- as-pair
  "w in C read as: w is some pair (x, y) that belongs to C."
  [w c]
  (let [[x y] (fresh (list w c) '[x y u v] 2)]
    (nest-exists [x y] (list 'and (list '= w (list 'pair x y)) (list 'in (list 'pair x y) c)))))

(defn- graph-member [w c]
  (if (pair-term? w)
    (pair-member (nth w 1) (nth w 2) c)
    (as-pair w c)))

(defmethod pair-member 'inverse [a b [_ g]]
  (list 'in (list 'pair b a) g))

(defmethod pair-member 'compose [a b [_ g h :as c]]
  (let [[z] (fresh (list a b c) '[z w u] 1)]
    (list 'exists [z] (list 'and (list 'in (list 'pair a z) h) (list 'in (list 'pair z b) g)))))

(defmethod pair-member 'product [a b [_ p q]]
  (list 'and (list 'in a p) (list 'in b q)))

(defmethod pair-member 'restrict [a b [_ g s]]
  (list 'and (list 'in (list 'pair a b) g) (list 'in a s)))

(doseq [op '[inverse compose product restrict]]
  (defmethod cl/unfold op [w c] (graph-member w c)))

(defmethod cl/unfold 'dom [a [_ g :as c]]
  (let [[y] (fresh (list a c) '[y v w] 1)]
    (list 'exists [y] (list 'in (list 'pair a y) g))))

(defmethod cl/unfold 'ran [b [_ g :as c]]
  (let [[x] (fresh (list b c) '[x u w] 1)]
    (list 'exists [x] (list 'in (list 'pair x b) g))))

(defmethod cl/unfold 'image [b [_ g s :as c]]
  (let [[x] (fresh (list b c) '[x u w] 1)]
    (list 'exists [x] (list 'and (list 'in x s) (list 'in (list 'pair x b) g)))))

;; ---------------------------------------------------------------------------
;; Families

(defn- index-vars [pat] (if (vector? pat) pat [pat]))

(defn- index-term [pat] (if (vector? pat) (cons 'pair pat) pat))

(defmethod cl/unfold 'indexed-union [x [_ [pat dom] body]]
  (nest-exists (index-vars pat) (list 'and (list 'in (index-term pat) dom) (list 'in x body))))

(defmethod cl/unfold 'indexed-inter [x [_ [pat dom] body]]
  (list 'forall (index-vars pat) (list 'implies (list 'in (index-term pat) dom) (list 'in x body))))

(defmethod cl/unfold 'big-union [x [_ ca :as c]]
  (let [[a] (fresh (list x c) '[A B C] 1)]
    (list 'exists [a] (list 'and (list 'in a ca) (list 'in x a)))))

(defmethod cl/unfold 'big-inter [x [_ ca :as c]]
  (let [[a] (fresh (list x c) '[A B C] 1)]
    (list 'and (list 'element x) (list 'forall [a] (list 'implies (list 'in a ca) (list 'in x a))))))

;; ---------------------------------------------------------------------------
;; Sets

(defmethod cl/unfold 'power [b [_ a]]
  (list 'and (list 'element b) (list 'subset b a)))

(defmethod cl/unfold 'set-of [x [_ & as]]
  (list 'and (list 'element x)
        (if (= 1 (count as)) (list '= x (first as)) (cons 'or (map #(list '= x %) as)))))

(defmethod cl/unfold 'pair [x [_ a b]]
  (list 'in x (list 'set-of (list 'set-of a) (list 'set-of a b))))
