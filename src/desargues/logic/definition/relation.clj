(ns desargues.logic.definition.relation
  "Definitions of the relation vocabulary (Pinter, chapter 3), registered
   on desargues.logic.definition/defines, and membership in the chapter's
   relation-valued class terms, registered on desargues.logic.classes/unfold.

   A relation term is unfolded at a pair, (in (pair a b) R), by the book's
   own reading of R; at any other element p it is unfolded to the class
   comprehension it abbreviates, (exists [u v] (and (= p (pair u v)) ...)).
   The equivalence class G_x reads (in z (class-of x G)) as (z, x) in G
   (Def 3.8; the y in A conjunct is implied when G is a relation in A)."
  (:require [desargues.logic.classes :as cl]
            [desargues.logic.definition :as d]))

(defn- fresh
  "n bound-variable symbols not occurring in forms."
  [n & forms]
  (let [used (set (filter symbol? (tree-seq coll? seq forms)))]
    (vec (take n (remove used (concat '[x y z u v w] (map #(symbol (str "x" %)) (range))))))))

(defn- pair-parts
  "[a b] when t is (pair a b), else nil."
  [t]
  (when (and (seq? t) (= 'pair (first t)) (= 3 (count t))) (vec (rest t))))

(defn- at-pair
  "Membership of element p in a relation term: (on-pair a b) at a pair,
   else p read as some pair of the term, one witness name at a time:
   (exists [u] (exists [v] (and (= p (pair u v)) (in (pair u v) term))))."
  [p term on-pair]
  (if-let [[a b] (pair-parts p)]
    (on-pair a b)
    (let [[u v] (fresh 2 p term)]
      (list 'exists [u]
            (list 'exists [v] (list 'and (list '= p (list 'pair u v)) (list 'in (list 'pair u v) term)))))))

;; ---------------------------------------------------------------------------
;; Properties of a relation (Def 3.1, Def 3.6)

(defmethod d/defines 'relation-in [[_ g a]]
  (list 'subset g (list 'product a a)))

(defmethod d/defines 'irreflexive [[_ r a :as form]]
  (let [[x] (fresh 1 form)]
    (list 'forall [x] (list 'implies (list 'in x a) (list 'not (list 'in (list 'pair x x) r))))))

(defmethod d/defines 'asymmetric [[_ r :as form]]
  (let [[x y] (fresh 2 form)]
    (list 'forall [x y] (list 'implies (list 'in (list 'pair x y) r) (list 'not (list 'in (list 'pair y x) r))))))

(defmethod d/defines 'intransitive [[_ r :as form]]
  (let [[x y z] (fresh 3 form)]
    (list 'forall [x y z]
          (list 'implies (list 'and (list 'in (list 'pair x y) r) (list 'in (list 'pair y z) r))
                (list 'not (list 'in (list 'pair x z) r))))))

;; ---------------------------------------------------------------------------
;; Relation-valued class terms

(defmethod cl/unfold 'class-of [z [_ x g]]
  (list 'in (list 'pair z x) g))

(defmethod cl/unfold 'quotient [c [_ a g :as term]]
  (let [[x] (fresh 1 c term)]
    (list 'exists [x] (list 'and (list 'in x a) (list '= c (list 'class-of x g))))))

(defmethod cl/unfold 'identity [p [_ a :as term]]
  (at-pair p term (fn [x y] (list 'and (list 'in x a) (list '= x y)))))

(defmethod cl/unfold 'kernel [p [_ f :as term]]
  (at-pair p term (fn [a b] (list '= (list 'apply f a) (list 'apply f b)))))

(defmethod cl/unfold 'relation-preimage [p [_ f g :as term]]
  (at-pair p term (fn [a b] (list 'in (list 'pair (list 'apply f a) (list 'apply f b)) g))))

(defmethod cl/unfold 'relation-image [p [_ f h :as term]]
  (at-pair p term
           (fn [a b]
             (let [[x y] (fresh 2 p term a b)]
               (list 'exists [x y]
                     (list 'and (list 'in (list 'pair x y) h)
                           (list '= a (list 'apply f x)) (list '= b (list 'apply f y))))))))

(defmethod cl/unfold 'relation-restrict [p [_ g b :as term]]
  (at-pair p term (fn [x y] (list 'and (list 'in x b) (list 'in y b) (list 'in (list 'pair x y) g)))))

(defmethod cl/unfold 'quotient-relation [p [_ h g :as term]]
  (at-pair p term
           (fn [a b]
             (let [[x y] (fresh 2 p term a b)]
               (list 'exists [x y]
                     (list 'and (list 'in (list 'pair x y) h)
                           (list '= a (list 'class-of x g)) (list '= b (list 'class-of y g))))))))

(defmethod cl/unfold 'relation-product [p [_ g h :as term]]
  (at-pair p term
           (fn [s t]
             (if (and (pair-parts s) (pair-parts t))
               (let [[x w] (pair-parts s) [y z] (pair-parts t)]
                 (list 'and (list 'in (list 'pair x y) g) (list 'in (list 'pair w z) h)))
               (let [[x w y z] (fresh 4 p term s t)
                     s' (list 'pair x w)
                     t' (list 'pair y z)]
                 (reduce (fn [body v] (list 'exists [v] body))
                         (list 'and (list '= s s') (list '= t t') (list 'in (list 'pair s' t') term))
                         [z y w x]))))))
