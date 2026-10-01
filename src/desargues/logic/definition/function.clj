(ns desargues.logic.definition.function
  "Definitions of the function vocabulary (Pinter, chapter 2), registered
   on desargues.logic.definition/defines, and membership in the chapter's
   function-valued class terms, registered on desargues.logic.classes/unfold.

     (maps f A B)            -> f subset A x B, F1 and F2 (2.1)
     (functional-graph G)    -> F2 for G (Ex. 2.2.10)
     (bijective f A B)       -> f : A -> B, injective and surjective (2.8)
     (equipotent A B)        -> some bijective f : A -> B exists (2.9)
     (invertible f A B)      -> f : A -> B and f^-1 : B -> A (2.19)

     (in x (preimage f D))               -> (exists [y] (and (in y D) (in (pair x y) f)))
     (in (pair a b) (identity A))        -> (and (in a A) (= a b))
     (in (pair a b) (inclusion-fn B A))  -> (and (in a B) (= a b))
     (in (pair a b) (constant-fn c A B)) -> (and (in a A) (= b c))
     (in f (exp B A))                    -> (and (element f) (maps f A B))
     (in f (family-product A I))         -> (and (element f) (maps f I (indexed-union [i I] (index A i)))
                                                 (forall [i] (implies (in i I) (in (apply f i) (index A i)))))

   A graph term met at an element p not written as a pair is read as some
   pair of the term, one witness name at a time. The inverse image is read
   through the graph, as the direct image is, so element chases stay at the
   level of pairs."
  (:require [desargues.logic.classes :as cl]
            [desargues.logic.definition :as d]))

(defn- fresh
  "n symbols from candidates (then x0 x1 ...) occurring in none of forms."
  [candidates n & forms]
  (let [used (set (filter symbol? (tree-seq coll? seq forms)))]
    (vec (take n (remove used (concat candidates (map #(symbol (str "x" %)) (range))))))))

(defn- pair-parts
  "[a b] when t is (pair a b), else nil."
  [t]
  (when (and (seq? t) (= 'pair (first t)) (= 3 (count t))) (vec (rest t))))

(defn- at-pair
  "Membership of p in a graph term: (on-pair a b) when p is (pair a b), else
   (exists [u] (exists [v] (and (= p (pair u v)) (in (pair u v) term))))."
  [p term on-pair]
  (if-let [[a b] (pair-parts p)]
    (on-pair a b)
    (let [[u v] (fresh '[u v w] 2 p term)]
      (list 'exists [u]
            (list 'exists [v] (list 'and (list '= p (list 'pair u v)) (list 'in (list 'pair u v) term)))))))

(defn- f2
  "Condition F2 for the graph g: one image per argument."
  [g & forms]
  (let [[x y1 y2] (apply fresh '[x y1 y2] 3 g forms)]
    (list 'forall [x y1 y2]
          (list 'implies (list 'and (list 'in (list 'pair x y1) g) (list 'in (list 'pair x y2) g))
                (list '= y1 y2)))))

;; ---------------------------------------------------------------------------
;; Predicates (2.1, 2.8, 2.9, 2.19, Ex. 2.2.10)

(defmethod d/defines 'maps [[_ f a b :as form]]
  (let [[x y] (fresh '[x y] 2 form)]
    (list 'and
          (list 'subset f (list 'product a b))
          (list 'forall [x] (list 'implies (list 'in x a)
                                  (list 'exists [y] (list 'and (list 'in y b) (list 'in (list 'pair x y) f)))))
          (f2 f form))))

(defmethod d/defines 'functional-graph [[_ g :as form]] (f2 g form))

(defmethod d/defines 'bijective [[_ f a b]]
  (list 'and (list 'maps f a b) (list 'injective f) (list 'surjective f a b)))

(defmethod d/defines 'equipotent [[_ a b :as form]]
  (let [[f] (fresh '[f g h] 1 form)]
    (list 'exists [f] (list 'bijective f a b))))

(defmethod d/defines 'invertible [[_ f a b]]
  (list 'and (list 'maps f a b) (list 'maps (list 'inverse f) b a)))

;; ---------------------------------------------------------------------------
;; Membership in function-valued class terms (2.10-2.12, 2.28, 2.32, 2.34)

(defmethod cl/unfold 'preimage [x [_ f s :as c]]
  (let [[y] (fresh '[y v w] 1 x c)]
    (list 'exists [y] (list 'and (list 'in y s) (list 'in (list 'pair x y) f)))))

(defmethod cl/unfold 'identity [p [_ a :as c]]
  (at-pair p c (fn [x y] (list 'and (list 'in x a) (list '= x y)))))

(defmethod cl/unfold 'inclusion-fn [p [_ b _ :as c]]
  (at-pair p c (fn [x y] (list 'and (list 'in x b) (list '= x y)))))

(defmethod cl/unfold 'constant-fn [p [_ k a _ :as c]]
  (at-pair p c (fn [x y] (list 'and (list 'in x a) (list '= y k)))))

(defmethod cl/unfold 'exp [f [_ b a]]
  (list 'and (list 'element f) (list 'maps f a b)))

(defmethod cl/unfold 'family-product [f [_ a is :as c]]
  (let [[i] (fresh '[i j k] 1 f c)]
    (list 'and (list 'element f)
          (list 'maps f is (list 'indexed-union [i is] (list 'index a i)))
          (list 'forall [i] (list 'implies (list 'in i is) (list 'in (list 'apply f i) (list 'index a i)))))))
