(ns desargues.logic.model
  "Set-theory statements evaluated in FINITE models.

   A model is an env: a map from the statement's free letters to values, plus
   ::universe, the finite set elements range over. Values are plain data:
   elements are any values, a class is a set, an ordered pair (a b) is the
   vector [a b], a relation or graph is a set of pairs, and a function is a
   graph that is functional.

   `denote` is OPEN on the operator: (denote form env) -> value. `signature`
   is OPEN too: the sorts of an operator's arguments, which `sorts` uses to
   infer what each free letter is (:element :class :relation :function), so
   `models` can enumerate every model of a small universe and
   `counterexample` can search them."
  (:require [clojure.set :as set]
            [desargues.logic.connective :as c]))

;; ---------------------------------------------------------------------------
;; Denotation

(defn subsets
  "Every subset of the finite collection xs, as vectors."
  [xs]
  (reduce (fn [acc x] (concat acc (map #(conj % x) acc))) [[]] xs))

(defn cartesian
  "Every tuple taking one element from each collection of colls."
  [colls]
  (reduce (fn [acc c] (for [t acc x c] (conj t x))) [[]] colls))

(defn- op-of [form]
  (cond (seq? form) (first form)
        (symbol? form) ::letter
        :else ::literal))

(defmulti denote
  "The value of form in env."
  (fn [form _env] (op-of form)))

(defn universe [env] (::universe env))

(defmethod denote ::literal [form _] form)

(defmethod denote ::letter [form env]
  (case form
    empty #{}
    universe (universe env)
    (let [v (get env form ::unbound)]
      (if (= ::unbound v)
        (throw (ex-info (str "Unbound letter " form) {:letter form :env (keys env)}))
        v))))

(defmethod denote :default [form env]
  (let [op (first form)]
    (if (c/connective? op)
      (c/value op (mapv #(boolean (denote % env)) (rest form)))
      (throw (ex-info (str "No denotation for " op) {:form form})))))

(defn- args [form env] (mapv #(denote % env) (rest form)))

(defn- bind
  "env with each binder of a [x y] vector bound, for every assignment over
   the universe: a seq of envs."
  [binders env]
  (reduce (fn [envs x] (for [e envs a (universe env)] (assoc e x a)))
          [env] binders))

(defmethod denote 'forall [[_ bs body] env] (every? #(denote body %) (bind bs env)))
(defmethod denote 'exists [[_ bs body] env] (boolean (some #(denote body %) (bind bs env))))
(defmethod denote 'exists! [[_ bs body] env] (= 1 (count (filter #(denote body %) (bind bs env)))))
(defmethod denote 'class [[_ bs body] env]
  (set (for [e (bind bs env) :when (denote body e)]
         (if (= 1 (count bs)) (get e (first bs)) (mapv e bs)))))

(defmethod denote 'in [form env] (let [[x a] (args form env)] (contains? a x)))
(defmethod denote 'notin [form env] (let [[x a] (args form env)] (not (contains? a x))))
(defmethod denote '= [form env] (apply = (args form env)))
(defmethod denote 'subset [form env] (let [[a b] (args form env)] (set/subset? a b)))
(defmethod denote 'proper-subset [form env] (let [[a b] (args form env)] (and (set/subset? a b) (not= a b))))

(defmethod denote 'union [form env] (apply set/union (args form env)))
(defmethod denote 'inter [form env] (apply set/intersection (args form env)))
(defmethod denote 'diff [form env] (let [[a b] (args form env)] (set/difference a b)))
(defmethod denote 'sym-diff [form env]
  (let [[a b] (args form env)] (set/union (set/difference a b) (set/difference b a))))
(defmethod denote 'compl [form env] (set/difference (universe env) (first (args form env))))
(defmethod denote 'power [form env] (set (map set (subsets (vec (first (args form env)))))))

(defmethod denote 'pair [form env] (vec (args form env)))
(defmethod denote 'product [form env]
  (let [[a b] (args form env)] (set (for [x a y b] [x y]))))
(defmethod denote 'dom [form env] (set (map first (first (args form env)))))
(defmethod denote 'ran [form env] (set (map second (first (args form env)))))
(defmethod denote 'inverse [form env] (set (map (fn [[x y]] [y x]) (first (args form env)))))
(defmethod denote 'compose [form env]
  (let [[g h] (args form env)]
    (set (for [[x z] h [z' y] g :when (= z z')] [x y]))))
(defmethod denote 'image [form env]
  (let [[g a] (args form env)] (set (for [[x y] g :when (contains? a x)] y))))
(defmethod denote 'preimage [form env]
  (let [[g b] (args form env)] (set (for [[x y] g :when (contains? b y)] x))))
(defmethod denote 'restrict [form env]
  (let [[g a] (args form env)] (set (filter #(contains? a (first %)) g))))
(defmethod denote 'identity [form env] (set (for [x (first (args form env))] [x x])))
(defmethod denote 'apply [form env]
  (let [[g x] (args form env)
        ys (for [[a b] g :when (= a x)] b)]
    (if (= 1 (count ys)) (first ys) ::undefined)))
(defmethod denote 'big-union [form env] (apply set/union #{} (first (args form env))))
(defmethod denote 'big-inter [form env]
  (let [fam (first (args form env))]
    (if (empty? fam) (universe env) (apply set/intersection fam))))

(defn functional? [g] (every? #(= 1 (count %)) (vals (group-by first g))))

(defmethod denote 'function [form env] (functional? (first (args form env))))
(defmethod denote 'maps [form env]
  (let [[g a b] (args form env)]
    (and (functional? g) (= (set (map first g)) a) (set/subset? (set (map second g)) b))))
(defmethod denote 'injective [form env]
  (let [[g] (args form env)] (and (functional? g) (functional? (set (map (fn [[x y]] [y x]) g))))))
(defmethod denote 'surjective [form env]
  (let [[g _a b] (args form env)] (= (set (map second g)) b)))
(defmethod denote 'bijective [form env]
  (let [[g a b] (args form env)]
    (and (denote (list 'maps g a b) env) (denote (list 'injective g) env)
         (= (set (map second (denote g env))) (denote b env)))))

(defn- field-of [env a] (if a a (universe env)))

(defmethod denote 'reflexive [form env]
  (let [[r a] (args form env)] (every? #(contains? r [% %]) (field-of env a))))
(defmethod denote 'symmetric [form env]
  (let [[r] (args form env)] (every? (fn [[x y]] (contains? r [y x])) r)))
(defmethod denote 'antisymmetric [form env]
  (let [[r] (args form env)] (every? (fn [[x y]] (or (= x y) (not (contains? r [y x])))) r)))
(defmethod denote 'transitive [form env]
  (let [[r] (args form env)]
    (every? (fn [[x y]] (every? (fn [[y' z]] (or (not= y y') (contains? r [x z]))) r)) r)))
(defmethod denote 'equivalence [form env]
  (let [[r a] (rest form)]
    (and (denote (list 'reflexive r a) env) (denote (list 'symmetric r) env) (denote (list 'transitive r) env))))
(defmethod denote 'partial-order [form env]
  (let [[r a] (rest form)]
    (and (denote (list 'reflexive r a) env) (denote (list 'antisymmetric r) env) (denote (list 'transitive r) env))))
(defmethod denote 'class-of [form env]
  (let [[x r] (args form env)] (set (for [[a b] r :when (= a x)] b))))
(defmethod denote 'quotient [form env]
  (let [[a r] (args form env)] (set (for [x a] (set (for [[p q] r :when (= p x)] q))))))
(defmethod denote 'partition [form env]
  (let [[p a] (args form env)]
    (and (every? seq p) (= (apply set/union #{} p) a)
         (= (reduce + (map count p)) (count (apply set/union #{} p))))))

;; ---------------------------------------------------------------------------
;; Sorts of free letters

(defmulti signature
  "The sorts of an operator's argument positions, a vector (the last sort
   repeats for n-ary operators), or nil when unknown."
  identity)

(defmethod signature :default [_] nil)

(doseq [op '[union inter diff sym-diff subset proper-subset]]
  (defmethod signature op [_] [:class :class]))
(defmethod signature 'compl [_] [:class])
(defmethod signature 'power [_] [:class])
(defmethod signature 'in [_] [:element :class])
(defmethod signature 'notin [_] [:element :class])
(defmethod signature 'product [_] [:class :class])
(doseq [op '[dom ran inverse function injective symmetric antisymmetric transitive]]
  (defmethod signature op [_] [:relation]))
(defmethod signature 'compose [_] [:relation :relation])
(doseq [op '[image preimage restrict]]
  (defmethod signature op [_] [:relation :class]))
(defmethod signature 'apply [_] [:relation :element])
(defmethod signature 'maps [_] [:relation :class :class])
(defmethod signature 'surjective [_] [:relation :class :class])
(defmethod signature 'bijective [_] [:relation :class :class])
(doseq [op '[reflexive equivalence partial-order]]
  (defmethod signature op [_] [:relation :class]))
(defmethod signature 'class-of [_] [:element :relation])
(defmethod signature 'quotient [_] [:class :relation])
(defmethod signature 'pair [_] [:element :element])

(def ^:private binders-of '#{forall exists exists! class})

(defn sorts
  "{letter sort} for the free letters of statement whose sort its operators
   determine. Bound variables are elements."
  [statement]
  (letfn [(walk [acc form bound]
            (cond
              (symbol? form) acc
              (not (seq? form)) acc
              (binders-of (first form))
              (let [[_ bs body] form] (walk acc body (into bound bs)))
              :else
              (let [[op & as] form
                    sig (signature op)]
                (reduce (fn [acc [i a]]
                          (let [s (when sig (get sig i (peek sig)))
                                acc (if (and s (symbol? a) (not (bound a))
                                             (not (#{'empty 'universe} a)))
                                      (update acc a #(or % s))
                                      acc)]
                            (walk acc a bound)))
                        acc
                        (map-indexed vector as)))))]
    (walk {} statement #{})))

;; ---------------------------------------------------------------------------
;; Models

(defn- all-subsets [xs] (map set (subsets (vec xs))))

(defn values-of
  "Every value of a sort over the universe u."
  [sort u]
  (case sort
    :element (seq u)
    :class (all-subsets u)
    :relation (all-subsets (for [x u y u] [x y]))
    :function (map (fn [ys] (set (map vector u ys)))
                   (cartesian (repeat (count u) (seq u))))))

(defn models
  "Every env over universe u for the given {letter sort}, lazily."
  [letter-sorts u]
  (let [ls (vec (keys letter-sorts))]
    (map (fn [vs] (assoc (zipmap ls vs) ::universe (set u)))
         (cartesian (map #(values-of (letter-sorts %) u) ls)))))

(defn counterexample
  "The first model over universe u (default #{0 1}) in which statement is
   false, or nil; stops after :limit models (default 200000)."
  [statement & {:keys [universe limit] :or {universe #{0 1} limit 200000}}]
  (first (filter #(not (denote statement %))
                 (take limit (models (sorts statement) universe)))))
