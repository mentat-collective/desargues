(ns desargues.logic.model
  "Set-theory statements evaluated in FINITE models.

   A model is an env: a map from the statement's free letters to values, plus
   ::universe, the finite set elements range over. Values are plain data:
   elements are any values, a class is a set, an ordered pair (a b) is the
   vector [a b], a relation or graph is a set of pairs, a function is a
   functional graph, an indexed family {A_i} is a map from index to member,
   and an ordered class is either a plain class (ordered by the model's order
   letter `<=`, a partial order on the universe) or an Ordered record that
   carries its own order (lexicographic products, classes of cuts, ...).
   A sentence letter is a :prop, true or false. Every finite class is a set.

   `denote` is OPEN on the operator: (denote form env) -> value. `signature`
   is OPEN too: the sorts of an operator's argument positions, which `sorts`
   uses to infer what each letter is (bound variables included, so quantifiers
   range over classes or relations when the body says so). `implicit-letters`
   names letters an operator reads without mentioning (the order `<=`).
   `check` enumerates every model of a small universe, or a seeded sample of
   them when there are too many, and reports :holds, :vacuous (no model met
   the hypothesis) or :counterexample."
  (:require [clojure.set :as set]
            [desargues.logic.connective :as c]))

;; ---------------------------------------------------------------------------
;; Finite combinatorics

(defn subsets
  "Every subset of the finite collection xs, as vectors."
  [xs]
  (reduce (fn [acc x] (concat acc (map #(conj % x) acc))) [[]] xs))

(defn cartesian
  "Every tuple taking one element from each collection of colls."
  [colls]
  (reduce (fn [acc c] (for [t acc x c] (conj t x))) [[]] colls))

(defn all-subsets [xs] (map set (subsets (vec xs))))

(defn- permutations [xs]
  (if (empty? xs)
    [[]]
    (for [x xs p (permutations (remove #{x} xs))] (into [x] p))))

;; ---------------------------------------------------------------------------
;; Values

(defrecord Ordered [carrier order])

(defn ordered? [v] (instance? Ordered v))

(defn defined? [v] (not= ::undefined v))

(defn elems
  "The members of a class value; anything that is not a class has none."
  [v]
  (cond (ordered? v) (:carrier v)
        (set? v) v
        :else #{}))

(defn pair? [v] (and (vector? v) (= 2 (count v))))

(defn- pairs [g] (filter pair? (elems g)))

(defn functional? [g] (every? #(= 1 (count %)) (vals (group-by first (pairs g)))))

(defn- graph? [g] (and (set? g) (every? pair? g)))

(defn- ap
  "g(x): the unique y with (x, y) in g, else ::undefined."
  [g x]
  (let [ys (for [[a b] (pairs g) :when (= a x)] b)]
    (if (= 1 (count ys)) (first ys) ::undefined)))

(defn- dom* [g] (set (map first (pairs g))))
(defn- ran* [g] (set (map second (pairs g))))
(defn- image* [g a] (let [a (elems a)] (set (for [[x y] (pairs g) :when (contains? a x)] y))))
(defn- preimage* [g b] (let [b (elems b)] (set (for [[x y] (pairs g) :when (contains? b y)] x))))
(defn- inverse* [g] (set (map (fn [[x y]] [y x]) (pairs g))))
(defn- class-of* [x r] (set (for [[a b] (pairs r) :when (= a x)] b)))

(defn- maps? [g a b]
  (and (graph? g) (functional? g) (= (dom* g) (elems a)) (set/subset? (ran* g) (elems b))))

;; ---------------------------------------------------------------------------
;; Denotation

(defn universe [env] (::universe env))

(defn subscript
  "[family i] for a subscripted letter X_i, else nil. The family is the
   letter X_ (so X itself stays free to name a class, as in a partition
   {A_i} of A)."
  [sym]
  (when-let [[_ x i] (re-matches #"(.+)_([^_]+)" (name sym))]
    [(symbol (str x "_")) (symbol i)]))

(defn- op-of [form]
  (cond (seq? form) (first form)
        (symbol? form) ::letter
        :else ::literal))

(defmulti denote
  "The value of form in env."
  (fn [form _env] (op-of form)))

(defmethod denote ::literal [form _] form)

(defn- unbound [form env]
  (throw (ex-info (str "Unbound letter " form) {:letter form :env (keys env)})))

(defmethod denote ::letter [form env]
  (case form
    empty #{}
    universe (universe env)
    two #{0 1}
    (if (contains? env form)
      (get env form)
      (if-let [[x i] (subscript form)]
        (if (contains? env x) (get (env x) (denote i env) ::undefined) (unbound form env))
        (unbound form env)))))

(defmethod denote :default [form env]
  (let [op (first form)]
    (if (c/connective? op)
      (c/value op (mapv #(boolean (denote % env)) (rest form)))
      (throw (ex-info (str "No denotation for " op) {:form form})))))

(defn- args [form env] (mapv #(denote % env) (rest form)))

;; -- binders --------------------------------------------------------------

(declare binder-sorts values space-of)

(def ^:dynamic *budget*
  "A volatile holding how many binder assignments quantifier expansion may
   still enumerate, or nil for no bound."
  nil)

(defn- bind
  "Every env extending env by an assignment to the binders, each ranging over
   the values of the sort the body gives it. Spends the expansion's size from
   *budget* and throws ::exhausted once it is spent."
  [binders body env]
  (let [ss (binder-sorts binders body)
        u (universe env)]
    (when-let [b *budget*]
      (when (neg? (vswap! b - (reduce *' 1 (map #(:count (space-of (ss %) u)) binders))))
        (throw (ex-info "model search budget spent" {::exhausted true}))))
    (reduce (fn [envs x] (for [e envs a (values (ss x) u)] (assoc e x a)))
            [env] binders)))

(defn- occurs? [x form] (boolean (some #{x} (tree-seq coll? seq form))))

(defn- conjuncts [f] (if (and (seq? f) (= 'and (first f))) (rest f) [f]))

(defn- replacement
  "A class {v : exists xs (... and v = t)} read as the replacement {t : ...}:
   [xs t other-conjuncts], or nil."
  [vs body]
  (when (and (= 1 (count vs)) (seq? body) (= 'exists (first body)))
    (let [v (first vs)
          [_ xs inner] body
          cs (conjuncts inner)
          e (first (filter #(and (seq? %) (= '= (first %)) (= 3 (count %)) (some #{v} (rest %))) cs))
          t (when e (first (remove #{v} (rest e))))]
      (when (and e (some? t) (not (occurs? v t)))
        [xs t (remove #{e} cs)]))))

(defmethod denote 'forall [[_ bs body] env] (every? #(denote body %) (bind bs body env)))
(defmethod denote 'exists [[_ bs body] env] (boolean (some #(denote body %) (bind bs body env))))
(defmethod denote 'exists! [[_ bs body] env] (= 1 (count (filter #(denote body %) (bind bs body env)))))
(defmethod denote 'class [[_ bs body] env]
  (if-let [[xs t cs] (replacement bs body)]
    (set (for [e (bind xs (list ::with t (cons 'and cs)) env)
               :when (every? #(denote % e) cs)
               :let [v (denote t e)] :when (defined? v)]
           v))
    (set (for [e (bind bs body env) :when (denote body e)]
           (if (= 1 (count bs)) (get e (first bs)) (mapv e bs))))))

(defn pattern-vars
  "The variables of an index pattern: i, [i j] or (pair i j)."
  [pat]
  (cond (symbol? pat) [pat]
        (vector? pat) (mapcat pattern-vars pat)
        (seq? pat) (mapcat pattern-vars (rest pat))
        :else []))

(defn- match [pat v env]
  (if (symbol? pat)
    (assoc env pat v)
    (let [ps (if (vector? pat) pat (rest pat))]
      (when (and (vector? v) (= (count v) (count ps)))
        (reduce (fn [e [p x]] (and e (match p x e))) env (map vector ps v))))))

(defn- indexed
  "[[index env] ...] for an index binder [pattern domain]."
  [[pat dom] env]
  (for [i (elems (denote dom env)) :let [e (match pat i env)] :when e] [i e]))

(defn- family* [b body env] (into {} (for [[i e] (indexed b env)] [i (denote body e)])))

(defn- product-of
  "The product of a family {i -> class}: every choice function, as a graph."
  [fam]
  (let [ks (vec (keys fam))]
    (set (for [t (cartesian (map #(seq (elems (fam %))) ks))] (set (map vector ks t))))))

(defmethod denote 'family [[_ b body] env] (family* b body env))
(defmethod denote 'indexed-union [[_ b body] env]
  (apply set/union #{} (map elems (vals (family* b body env)))))
(defmethod denote 'indexed-inter [[_ b body] env]
  (let [ms (map elems (vals (family* b body env)))]
    (if (empty? ms) (universe env) (apply set/intersection ms))))
(defmethod denote 'indexed-product [[_ b body] env] (product-of (family* b body env)))
(defmethod denote 'index [form env] (let [[a i] (args form env)] (get a i ::undefined)))
(defmethod denote 'family-product [form env]
  (let [[a is] (args form env)] (product-of (into {} (for [i (elems is)] [i (get a i #{})])))))
(defmethod denote 'defined [form env] (defined? (denote (second form) env)))

;; -- classes ----------------------------------------------------------------

(defmethod denote 'in [form env] (let [[x a] (args form env)] (contains? (elems a) x)))
(defmethod denote 'notin [form env] (let [[x a] (args form env)] (not (contains? (elems a) x))))
(defmethod denote '= [form env]
  (let [vs (args form env)] (and (every? defined? vs) (apply = vs))))
(defmethod denote 'subset [form env] (let [[a b] (args form env)] (set/subset? (elems a) (elems b))))
(defmethod denote 'proper-subset [form env]
  (let [[a b] (map elems (args form env))] (and (set/subset? a b) (not= a b))))
(defmethod denote 'disjoint [form env]
  (let [[a b] (map elems (args form env))] (empty? (set/intersection a b))))

(defmethod denote 'union [form env] (apply set/union (map elems (args form env))))
(defmethod denote 'inter [form env] (apply set/intersection (map elems (args form env))))
(defmethod denote 'diff [form env] (let [[a b] (map elems (args form env))] (set/difference a b)))
(defmethod denote 'sym-diff [form env]
  (let [[a b] (map elems (args form env))] (set/union (set/difference a b) (set/difference b a))))
(defmethod denote 'compl [form env] (set/difference (universe env) (elems (first (args form env)))))
(defmethod denote 'power [form env] (set (all-subsets (elems (first (args form env))))))
(defmethod denote 'big-union [form env] (apply set/union #{} (map elems (elems (first (args form env))))))
(defmethod denote 'big-inter [form env]
  (let [fam (elems (first (args form env)))]
    (if (empty? fam) (universe env) (apply set/intersection (map elems fam)))))

(defmethod denote 'element [_ _] true)
(defmethod denote 'is-set [_ _] true)
(defmethod denote 'proper-class [_ _] false)
(defmethod denote 'set-of [form env] (set (args form env)))
(defmethod denote 'set [form env] (set (args form env)))
(defmethod denote 'equipotent [form env]
  (let [[a b] (args form env)] (= (count (elems a)) (count (elems b)))))

;; -- pairs, graphs, functions ----------------------------------------------

(defmethod denote 'pair [form env] (vec (args form env)))
(defmethod denote 'pair-alt [form env]
  (let [[x y] (args form env)] (hash-set (hash-set x #{}) (hash-set y #{#{}}))))
(defmethod denote 'product [form env]
  (let [[a b] (map elems (args form env))] (set (for [x a y b] [x y]))))
(defmethod denote 'graph [form env] (graph? (first (args form env))))
(defmethod denote 'relation [form env] (graph? (first (args form env))))
(defmethod denote 'relation-in [form env]
  (let [[g a] (args form env) a (elems a)]
    (and (graph? g) (every? (fn [[x y]] (and (a x) (a y))) g))))
(defmethod denote 'functional-graph [form env]
  (let [[g] (args form env)] (and (graph? g) (functional? g))))
(defmethod denote 'dom [form env] (dom* (first (args form env))))
(defmethod denote 'ran [form env] (ran* (first (args form env))))
(defmethod denote 'inverse [form env] (inverse* (first (args form env))))
(defmethod denote 'compose [form env]
  (let [[g h] (args form env)]
    (set (for [[x z] (pairs h) [z' y] (pairs g) :when (= z z')] [x y]))))
(defmethod denote 'image [form env] (let [[g a] (args form env)] (image* g a)))
(defmethod denote 'preimage [form env] (let [[g b] (args form env)] (preimage* g b)))
(defmethod denote 'restrict [form env]
  (let [[g a] (args form env) a (elems a)] (set (filter #(contains? a (first %)) (pairs g)))))
(defmethod denote 'identity [form env] (set (for [x (elems (first (args form env)))] [x x])))
(defmethod denote 'apply [form env] (let [[g x] (args form env)] (ap g x)))

(defmethod denote 'function [form env] (functional? (first (args form env))))
(defmethod denote 'maps [form env] (let [[g a b] (args form env)] (maps? g a b)))
(defmethod denote 'injective [form env]
  (let [[g] (args form env)] (and (functional? g) (functional? (inverse* g)))))
(defmethod denote 'surjective [form env]
  (let [[g a b] (args form env)] (and (maps? g a b) (= (ran* g) (elems b)))))
(defmethod denote 'bijective [form env]
  (let [[g a b] (args form env)]
    (and (maps? g a b) (functional? (inverse* g)) (= (ran* g) (elems b)))))
(defmethod denote 'invertible [form env]
  (let [[g a b] (args form env)] (and (maps? g a b) (maps? (inverse* g) b a))))
(defmethod denote 'func-product [form env]
  (let [[f g] (args form env)]
    (set (for [[x fx] (pairs f) [y gy] (pairs g)] [[x y] [fx gy]]))))
(defmethod denote 'constant-fn [form env]
  (let [[b a _] (args form env)] (set (for [x (elems a)] [x b]))))
(defmethod denote 'inclusion-fn [form env]
  (let [[b _] (args form env)] (set (for [x (elems b)] [x x]))))
(defmethod denote 'characteristic-fn [form env]
  (let [[b a] (args form env) b (elems b)] (set (for [x (elems a)] [x (if (b x) 0 1)]))))
(defmethod denote 'image-map [form env]
  (let [[f] (args form env)] (set (for [c (all-subsets (dom* f))] [c (image* f c)]))))
(defmethod denote 'preimage-map [form env]
  (let [[f b] (args form env)
        b (if (= 3 (count form)) (elems b) (ran* f))]
    (set (for [d (all-subsets b)] [d (preimage* f d)]))))
(defmethod denote 'exp [form env]
  (let [[b a] (map elems (args form env)) as (vec a)]
    (set (for [t (cartesian (repeat (count as) (seq b)))] (set (map vector as t))))))
(defmethod denote 'projection [form env]
  (let [[a b c] (rest form)]
    (if (integer? a)
      (let [[l r] (map #(elems (denote % env)) [b c])]
        (set (for [x l y r] [[x y] (if (= 1 a) x y)])))
      (let [fam (denote a env) i (denote c env)
            prod (product-of (into {} (for [j (elems (denote b env))] [j (get fam j #{})])))]
        (set (for [x prod] [x (ap x i)]))))))

;; -- relations ------------------------------------------------------------

(defn- field-of [env a] (if (some? a) (elems a) (universe env)))

(defmethod denote 'reflexive [form env]
  (let [[r a] (args form env) r (set (pairs r))] (every? #(contains? r [% %]) (field-of env a))))
(defmethod denote 'irreflexive [form env]
  (let [[r a] (args form env) r (set (pairs r))] (not-any? #(contains? r [% %]) (field-of env a))))
(defmethod denote 'symmetric [form env]
  (let [[r] (args form env) r (set (pairs r))] (every? (fn [[x y]] (contains? r [y x])) r)))
(defmethod denote 'asymmetric [form env]
  (let [[r] (args form env) r (set (pairs r))] (not-any? (fn [[x y]] (contains? r [y x])) r)))
(defmethod denote 'antisymmetric [form env]
  (let [[r] (args form env) r (set (pairs r))]
    (every? (fn [[x y]] (or (= x y) (not (contains? r [y x])))) r)))
(defn- transitive? [r]
  (every? (fn [[x y]] (every? (fn [[y' z]] (or (not= y y') (contains? r [x z]))) r)) r))
(defmethod denote 'transitive [form env] (transitive? (set (pairs (first (args form env))))))
(defmethod denote 'intransitive [form env]
  (let [r (set (pairs (first (args form env))))]
    (every? (fn [[x y]] (every? (fn [[y' z]] (or (not= y y') (not (contains? r [x z])))) r)) r)))
(defn- relation-in? [r a env] (or (nil? a) (denote (list 'relation-in r a) env)))
(defmethod denote 'equivalence [form env]
  (let [[r a] (rest form)]
    (and (relation-in? r a env) (denote (list 'reflexive r a) env)
         (denote (list 'symmetric r) env) (denote (list 'transitive r) env))))
(defmethod denote 'partial-order [form env]
  (let [[r a] (rest form)]
    (and (relation-in? r a env) (denote (list 'reflexive r a) env)
         (denote (list 'antisymmetric r) env) (denote (list 'transitive r) env))))
(defmethod denote 'class-of [form env] (let [[x r] (args form env)] (class-of* x r)))
(defmethod denote 'quotient [form env]
  (let [[a r] (args form env)] (set (for [x (elems a)] (class-of* x r)))))
(defmethod denote 'partition [form env]
  (let [[p a] (args form env)
        indexed? (and (map? p) (not (ordered? p)))
        ms (map elems (if indexed? (vals p) (elems p)))]
    (and (every? seq ms)
         (= (apply set/union #{} ms) (elems a))
         (if indexed?
           (every? (fn [[i j]] (or (= i j) (empty? (set/intersection (elems (p i)) (elems (p j))))))
                   (for [i (keys p) j (keys p)] [i j]))
           (= (reduce + (map count ms)) (count (apply set/union #{} ms)))))))
(defmethod denote 'kernel [form env]
  (let [[f] (args form env) d (dom* f)]
    (set (for [x d y d :when (= (ap f x) (ap f y))] [x y]))))
(defmethod denote 'relation-preimage [form env]
  (let [[f g] (args form env) d (dom* f) g (set (pairs g))]
    (set (for [x d y d :when (contains? g [(ap f x) (ap f y)])] [x y]))))
(defmethod denote 'relation-image [form env]
  (let [[f h] (args form env)]
    (set (for [[x y] (pairs h) :let [u (ap f x) v (ap f y)] :when (and (defined? u) (defined? v))] [u v]))))
(defmethod denote 'relation-restrict [form env]
  (let [[g b] (args form env) b (elems b)] (set (filter (fn [[x y]] (and (b x) (b y))) (pairs g)))))
(defmethod denote 'quotient-relation [form env]
  (let [[h g] (args form env)] (set (for [[x y] (pairs h)] [(class-of* x g) (class-of* y g)]))))
(defmethod denote 'quotient-function [form env]
  (let [[f g] (args form env)] (set (for [x (dom* f)] [(class-of* x g) (ap f x)]))))
(defmethod denote 'canonical-map [form env]
  (let [[a g] (args form env)] (set (for [x (elems a)] [x (class-of* x g)]))))
(defmethod denote 'relation-product [form env]
  (let [[g h] (args form env)] (set (for [[x y] (pairs g) [w z] (pairs h)] [[x w] [y z]]))))

;; -- orders -----------------------------------------------------------------

(def order-letter
  "The env letter holding the model's partial order on the universe."
  '<=)

(defn- order-of [env] (get env order-letter #{}))

(defn ord
  "[carrier order] of an ordered value: an Ordered keeps its own order, a
   plain class is ordered by the model's order restricted to it."
  [env v]
  (if (ordered? v)
    [(:carrier v) (:order v)]
    (let [s (elems v)]
      [s (set (filter (fn [[x y]] (and (s x) (s y))) (order-of env)))])))

(defn- ambient [env] [(universe env) (order-of env)])
(defn- le [r x y] (contains? r [x y]))
(defn- lt [r x y] (and (not= x y) (le r x y)))

(defn- upper-bounds* [[s r] b] (set (filter (fn [u] (every? #(le r % u) b)) s)))
(defn- lower-bounds* [[s r] b] (set (filter (fn [l] (every? #(le r l %) b)) s)))
(defn- least* [[_ r] xs] (let [ls (filter (fn [l] (every? #(le r l %) xs)) xs)] (if (= 1 (count ls)) (first ls) ::undefined)))
(defn- greatest* [[_ r] xs] (let [gs (filter (fn [g] (every? #(le r % g) xs)) xs)] (if (= 1 (count gs)) (first gs) ::undefined)))
(defn- sup* [sr b] (least* sr (upper-bounds* sr (elems b))))
(defn- inf* [sr b] (greatest* sr (lower-bounds* sr (elems b))))
(defn- join* [env x y] (sup* (ambient env) (hash-set x y)))
(defn- meet* [env x y] (inf* (ambient env) (hash-set x y)))

(defn- lattice? [[s _ :as sr]]
  (every? (fn [[x y]] (and (contains? s (sup* sr (hash-set x y))) (contains? s (inf* sr (hash-set x y)))))
          (for [x s y s] [x y])))

(defn- convex? [[s r] c]
  (let [c (elems c)]
    (and (set/subset? c s)
         (every? (fn [[a b x]] (or (not (le r a x)) (not (le r x b)) (c x)))
                 (for [a c b c x s] [a b x])))))

(defn- cut? [[s r] l u]
  (let [l (elems l) u (elems u)]
    (and (seq l) (seq u) (empty? (set/intersection l u)) (= (set/union l u) s)
         (every? (fn [x] (every? (fn [y] (or (not (le r y x)) (l y))) s)) l)
         (every? (fn [x] (every? (fn [y] (or (not (le r x y)) (u y))) s)) u))))

(defn- inclusion-order [carrier]
  (->Ordered (set carrier) (set (for [a carrier b carrier :when (set/subset? (elems a) (elems b))] [a b]))))

(defn- increasing? [g [sa ra] [sb rb] rel]
  (and (maps? g sa sb)
       (every? (fn [[x y]] (or (not (rel ra x y)) (rel rb (ap g x) (ap g y))))
               (for [x sa y sa] [x y]))))

(defn- isomorphism? [g [sa ra] [sb rb]]
  (and (maps? g sa sb) (functional? (inverse* g)) (= (ran* g) sb)
       (every? (fn [[x y]] (= (le ra x y) (le rb (ap g x) (ap g y))))
               (for [x sa y sa] [x y]))))

(defmethod denote 'leq [form env] (let [[x y] (args form env)] (le (order-of env) x y)))
(defmethod denote 'lt [form env] (let [[x y] (args form env)] (lt (order-of env) x y)))
(defmethod denote 'comparable [form env]
  (let [[x y] (args form env) r (order-of env)] (or (le r x y) (le r y x))))
(defmethod denote 'chain [form env]
  (let [[c a] (args form env) [s r] (ord env a) c (elems c)]
    (and (set/subset? c s) (every? (fn [[x y]] (or (le r x y) (le r y x))) (for [x c y c] [x y])))))
(defmethod denote 'fully-ordered [form env]
  (let [[s r] (ord env (first (args form env)))]
    (every? (fn [[x y]] (or (le r x y) (le r y x))) (for [x s y s] [x y]))))
(defmethod denote 'initial-segment [form env]
  (let [[a x] (args form env) [s r] (ord env x)] (set (filter #(lt r % a) s))))
(defmethod denote 'down-set [form env]
  (let [[a x] (args form env) [s r] (ord env x)] (set (filter #(le r % a) s))))
(defmethod denote 'closed-interval [form env]
  (let [[a b x] (args form env)
        [s r] (if (= 4 (count form)) (ord env x) (ambient env))]
    (set (filter #(and (le r a %) (le r % b)) s))))
(defmethod denote 'cut [form env] (let [[l u a] (args form env)] (boolean (cut? (ord env a) l u))))
(defmethod denote 'cuts [form env]
  (let [[s _ :as sr] (ord env (first (args form env)))
        cs (for [l (all-subsets s) :let [u (set/difference s l)] :when (cut? sr l u)] [l u])]
    (->Ordered (set cs) (set (for [p cs q cs :when (set/subset? (first p) (first q))] [p q])))))
(defmethod denote 'convex [form env] (let [[c a] (args form env)] (convex? (ord env a) c)))
(defmethod denote 'convex-subsets [form env]
  (let [[s _ :as sr] (ord env (first (args form env)))]
    (inclusion-order (filter #(convex? sr %) (all-subsets s)))))
(defmethod denote 'inclusion-order [form env] (inclusion-order (elems (first (args form env)))))
(defn- lex [env a b first?]
  (let [[sa ra] (ord env a) [sb rb] (ord env b)
        car (set (for [x sa y sb] [x y]))
        le? (fn [[a1 b1] [a2 b2]]
              (if first?
                (or (lt ra a1 a2) (and (= a1 a2) (le rb b1 b2)))
                (or (lt rb b1 b2) (and (= b1 b2) (le ra a1 a2)))))]
    (->Ordered car (set (for [p car q car :when (le? p q)] [p q])))))

(defmethod denote 'order-union [form env]
  (let [os (map #(ord env %) (args form env))]
    (->Ordered (apply set/union #{} (map first os)) (apply set/union #{} (map second os)))))
(defmethod denote 'lex-product [form env] (let [[a b] (args form env)] (lex env a b true)))
(defmethod denote 'antilex-product [form env] (let [[a b] (args form env)] (lex env a b false)))
(defmethod denote 'increasing [form env]
  (let [[g a b] (args form env)] (increasing? g (ord env a) (ord env b) le)))
(defmethod denote 'strictly-increasing [form env]
  (let [[g a b] (args form env)] (increasing? g (ord env a) (ord env b) lt)))
(defmethod denote 'isomorphism [form env]
  (let [[g a b] (args form env)] (isomorphism? g (ord env a) (ord env b))))
(defmethod denote 'isomorphic [form env]
  (let [[a b] (args form env) [sa :as oa] (ord env a) [sb :as ob] (ord env b) xs (vec sa)]
    (and (= (count sa) (count sb))
         (boolean (some #(isomorphism? (set (map vector xs %)) oa ob) (permutations (vec sb)))))))
(defmethod denote 'maximal [form env]
  (let [[m a] (args form env) [s r] (ord env a)] (and (contains? s m) (not-any? #(lt r m %) s))))
(defmethod denote 'minimal [form env]
  (let [[m a] (args form env) [s r] (ord env a)] (and (contains? s m) (not-any? #(lt r % m) s))))
(defmethod denote 'greatest [form env]
  (let [[m a] (args form env) [s r] (ord env a)] (and (contains? s m) (every? #(le r % m) s))))
(defmethod denote 'least [form env]
  (let [[m a] (args form env) [s r] (ord env a)] (and (contains? s m) (every? #(le r m %) s))))
(defmethod denote 'upper-bound [form env]
  (let [[x b] (args form env) r (order-of env)] (every? #(le r % x) (elems b))))
(defmethod denote 'lower-bound [form env]
  (let [[x b] (args form env) r (order-of env)] (every? #(le r x %) (elems b))))
(defmethod denote 'upper-bounds [form env] (upper-bounds* (ambient env) (elems (first (args form env)))))
(defmethod denote 'lower-bounds [form env] (lower-bounds* (ambient env) (elems (first (args form env)))))
(defmethod denote 'sup [form env] (sup* (ambient env) (first (args form env))))
(defmethod denote 'inf [form env] (inf* (ambient env) (first (args form env))))
(defmethod denote 'sup-in [form env] (let [[a b] (args form env)] (sup* (ord env a) b)))
(defmethod denote 'inf-in [form env] (let [[a b] (args form env)] (inf* (ord env a) b)))
(defmethod denote 'has-sup [form env] (let [[b a] (args form env)] (defined? (sup* (ord env a) b))))
(defmethod denote 'has-inf [form env] (let [[b a] (args form env)] (defined? (inf* (ord env a) b))))
(defmethod denote 'bounded-above [form env]
  (let [[b a] (args form env)] (boolean (seq (upper-bounds* (ord env a) (elems b))))))
(defmethod denote 'bounded-below [form env]
  (let [[b a] (args form env)] (boolean (seq (lower-bounds* (ord env a) (elems b))))))
(defmethod denote 'join [form env] (let [[x y] (args form env)] (join* env x y)))
(defmethod denote 'meet [form env] (let [[x y] (args form env)] (meet* env x y)))
(defmethod denote 'lattice [form env]
  (let [[a] (args form env)] (and (= (elems a) (universe env)) (lattice? (ambient env)))))
(defmethod denote 'lattice-ops [form env] (denote (cons 'lattice (rest form)) env))
(defmethod denote 'distributive-lattice [form env]
  (let [u (universe env)]
    (and (denote (cons 'lattice (rest form)) env)
         (every? (fn [[x y z]] (= (join* env x (meet* env y z)) (meet* env (join* env x y) (join* env x z))))
                 (for [x u y u z u] [x y z])))))
(defmethod denote 'complete-lattice [form env]
  (let [[s _ :as sr] (ord env (first (args form env)))]
    (every? #(contains? s (sup* sr %)) (all-subsets s))))
(defmethod denote 'sublattice [form env]
  (let [[b a] (map elems (args form env))]
    (and (set/subset? b a)
         (every? (fn [[x y]] (and (contains? b (join* env x y)) (contains? b (meet* env x y))))
                 (for [x b y b] [x y])))))

;; ---------------------------------------------------------------------------
;; Sorts of letters

(defmulti signature
  "The sort hints of an operator's argument positions, a vector (the last
   hint repeats for n-ary operators), or nil when unknown. Called with the
   form and the hints known so far, for operators whose hints depend on them.
   Hints: :prop :element :class :graph (any relation, not widening a function)
   :function :relation :classes (a class of classes) [:family s] :order."
  (fn [op & _] op))

(defmethod signature :default [& _] nil)

(defmulti implicit-letters
  "{letter sort} an operator reads from the model without naming it."
  identity)

(defmethod implicit-letters :default [_] nil)

(defn- sig [op hints] (defmethod signature op [& _] hints))

(def ^:private class-sorts #{:class :graph :function :relation :classes})

(doseq [op '[in notin]]
  (defmethod signature op [_ form known]
    (let [x (second form)]
      [:element (cond (and (seq? x) (= 'pair (first x))) :graph
                      (class-sorts (get known x)) :classes
                      :else :class)])))

(doseq [op '[union inter diff sym-diff subset proper-subset disjoint equipotent]] (sig op [:class]))
(doseq [op '[compl power is-set element proper-class]] (sig op [:class]))
(doseq [op '[product exp]] (sig op [:class :class]))
(doseq [op '[pair pair-alt set-of set]] (sig op [:element]))
(doseq [op '[dom ran inverse]] (sig op [:graph]))
(sig 'compose [:graph :graph])
(doseq [op '[image preimage restrict]] (sig op [:graph :class]))
(sig 'apply [:function :element])
(sig 'identity [:class])
(doseq [op '[function functional-graph relation graph symmetric antisymmetric transitive asymmetric intransitive]]
  (sig op [:relation]))
(doseq [op '[maps surjective bijective invertible]] (sig op [:function :class :class]))
(doseq [op '[injective image-map kernel]] (sig op [:function]))
(sig 'preimage-map [:function :class])
(sig 'func-product [:function :function])
(sig 'constant-fn [:element :class :class])
(doseq [op '[inclusion-fn characteristic-fn]] (sig op [:class :class]))
(doseq [op '[big-union big-inter]] (sig op [:classes]))
(doseq [op '[reflexive irreflexive equivalence partial-order relation-in relation-restrict]]
  (sig op [:relation :class]))
(sig 'class-of [:element :relation])
(doseq [op '[quotient canonical-map]] (sig op [:class :relation]))
(sig 'partition [:classes :class])
(doseq [op '[relation-preimage relation-image quotient-function]] (sig op [:function :relation]))
(doseq [op '[quotient-relation relation-product]] (sig op [:relation :relation]))
(sig 'index [[:family :class] :element])
(sig 'family-product [[:family :class] :class])
(defmethod signature 'projection [_ form _]
  (if (integer? (second form)) [nil :class :class] [[:family :class] :class :element]))

(def ^:private order-ops
  {'leq [:element :element] 'lt [:element :element] 'comparable [:element :element]
   'join [:element :element] 'meet [:element :element]
   'chain [:class :class] 'fully-ordered [:class]
   'initial-segment [:element :class] 'down-set [:element :class]
   'closed-interval [:element :element :class]
   'cut [:class :class :class] 'cuts [:class] 'convex [:class :class] 'convex-subsets [:class]
   'lex-product [:class :class] 'antilex-product [:class :class] 'order-union [:class]
   'increasing [:function :class :class] 'strictly-increasing [:function :class :class]
   'isomorphism [:function :class :class] 'isomorphic [:class :class]
   'maximal [:element :class] 'minimal [:element :class] 'greatest [:element :class] 'least [:element :class]
   'upper-bound [:element :class] 'lower-bound [:element :class]
   'upper-bounds [:class] 'lower-bounds [:class] 'sup [:class] 'inf [:class]
   'sup-in [:class :class] 'inf-in [:class :class] 'has-sup [:class :class] 'has-inf [:class :class]
   'bounded-above [:class :class] 'bounded-below [:class :class]
   'lattice [:class] 'lattice-ops [:class] 'distributive-lattice [:class] 'complete-lattice [:class]
   'sublattice [:class :class]})

(doseq [[op hints] order-ops]
  (sig op hints)
  (defmethod implicit-letters op [_] {order-letter :order}))
(sig 'inclusion-order [:classes])

(def ^:private rank {:element 0 :class 1 :graph 2 :function 3 :relation 4 :classes 5})

(defn merge-sort
  "The sort meeting both hints: the more general one."
  [a b]
  (cond (nil? a) b
        (nil? b) a
        (= a b) a
        (and (vector? a) (vector? b)) [:family (merge-sort (second a) (second b))]
        (vector? a) a
        (vector? b) b
        (= :prop a) b
        (= :prop b) a
        (= :order a) a
        (= :order b) b
        :else (max-key rank a b)))

(def ^:private quantifiers '#{forall exists exists! class})
(def ^:private indexers '#{indexed-union indexed-inter indexed-product family})
(def ^:private constants '#{empty universe two})

(defn- hint [acc sym s]
  (cond (or (nil? s) (constants sym)) acc
        (subscript sym) (let [[x i] (subscript sym)]
                          (-> acc (update x merge-sort [:family s]) (update i merge-sort :element)))
        :else (update acc sym merge-sort s)))

(defn- walk-hints [acc form known]
  (if-not (seq? form)
    acc
    (let [op (first form)]
      (cond
        (quantifiers op) (walk-hints acc (nth form 2 nil) known)
        (indexers op)
        (let [[_ [pat dom] body] form
              acc (reduce #(hint %1 %2 :element) acc (pattern-vars pat))
              acc (if (symbol? dom) (hint acc dom :class) (walk-hints acc dom known))]
          (if (symbol? body) (hint acc body :class) (walk-hints acc body known)))
        (c/connective? op)
        (reduce (fn [acc a] (if (symbol? a) (hint acc a :prop) (walk-hints acc a known))) acc (rest form))
        :else
        (let [sg (signature op form known)]
          (reduce (fn [acc [i a]]
                    (if (symbol? a)
                      (hint acc a (when sg (get sg i (peek sg))))
                      (walk-hints acc a known)))
                  acc (map-indexed vector (rest form))))))))

(defn hints
  "{symbol hint} from every occurrence in form, to a fixpoint."
  [form]
  (loop [known {} n 0]
    (let [h (walk-hints {} form known)]
      (if (or (= h known) (> n 4)) h (recur h (inc n))))))

(defn- final-sort [s]
  (cond (nil? s) :element
        (vector? s) [:family (if (nil? (second s)) :class (final-sort (second s)))]
        (= :graph s) :relation
        :else s))

(def ^:private binder-sorts
  (memoize (fn [bs body] (let [h (hints body)] (zipmap bs (map #(final-sort (get h %)) bs))))))

(defn free-letters
  "The letters of form not bound by a quantifier or index binder."
  ([form] (free-letters form #{}))
  ([form bound]
   (cond
     (symbol? form) (cond (or (bound form) (constants form)) #{}
                          (subscript form) (let [[x i] (subscript form)]
                                             (into #{} (remove bound) [x i]))
                          :else #{form})
     (seq? form) (let [op (first form)]
                   (cond (quantifiers op) (let [[_ bs body] form] (free-letters body (into bound bs)))
                         (indexers op) (let [[_ [pat dom] body] form]
                                         (set/union (free-letters dom bound)
                                                    (free-letters body (into bound (pattern-vars pat)))))
                         :else (apply set/union #{} (map #(free-letters % bound) (rest form)))))
     :else #{})))

(defn- ops-in [form]
  (if (seq? form) (cons (first form) (mapcat ops-in (rest form))) (when (vector? form) (mapcat ops-in form))))

(defn sorts
  "{letter sort} for every free letter of statement, plus the letters its
   operators read implicitly. Letters no operator constrains are elements."
  [statement]
  (let [h (hints statement)]
    (merge (into {} (map (fn [l] [l (final-sort (get h l))])) (free-letters statement))
           (apply merge (map implicit-letters (distinct (ops-in statement)))))))

;; ---------------------------------------------------------------------------
;; Value spaces and models

(defn- partial-order? [r u]
  (and (every? #(contains? r [% %]) u)
       (every? (fn [[x y]] (or (= x y) (not (contains? r [y x])))) r)
       (transitive? r)))

(def ^:private space-of
  (memoize
   (fn [sort u]
     (let [u (vec (clojure.core/sort u))
           v (fn [xs] (let [xs (vec xs)] {:count (count xs) :nth xs}))]
       (if (vector? sort)
         (let [{n :count f :nth} (space-of (second sort) u) k (count u)]
           {:count (reduce *' 1 (repeat k n))
            :nth (fn [i] (loop [i i j 0 m {}]
                           (if (= j k) m (recur (quot i n) (inc j) (assoc m (u j) (f (rem i n)))))))})
         (case sort
           :prop (v [false true])
           :element (v u)
           :class (v (all-subsets u))
           :relation (v (all-subsets (for [x u y u] [x y])))
           :classes (v (all-subsets (all-subsets u)))
           :order (v (filter #(partial-order? % u) (all-subsets (for [x u y u] [x y]))))
           :function (v (for [t (cartesian (repeat (count u) (cons ::none u)))]
                          (set (keep (fn [[x y]] (when (not= ::none y) [x y])) (map vector u t)))))))))))

(defn values
  "Every value of a sort over the universe u."
  [sort u]
  (let [{n :count f :nth} (space-of sort u)] (map f (range n))))

(defn model-count [letter-sorts u] (reduce *' 1 (map #(:count (space-of % u)) (vals letter-sorts))))

(defn- ordered-letters [letter-sorts] (sort-by str (keys letter-sorts)))

(defn models
  "Every env over universe u for the given {letter sort}, lazily."
  [letter-sorts u]
  (let [ls (ordered-letters letter-sorts)]
    (map (fn [vs] (assoc (zipmap ls vs) ::universe (set u)))
         (cartesian (map #(values (letter-sorts %) u) ls)))))

(defn sample-models
  "n envs over universe u drawn uniformly with a seeded generator."
  [letter-sorts u n seed]
  (let [ls (ordered-letters letter-sorts)
        sps (mapv #(space-of (letter-sorts %) u) ls)
        rnd (java.util.Random. (long seed))
        draw (fn [] (assoc (zipmap ls (map (fn [{c :count f :nth}] (f (long (* (.nextDouble rnd) c)))) sps))
                           ::universe (set u)))]
    (loop [acc (transient []) k 0] (if (= k n) (persistent! acc) (recur (conj! acc (draw)) (inc k))))))

;; ---------------------------------------------------------------------------
;; Checking

(defn- substitute [form x t]
  (cond (= form x) t
        (seq? form) (apply list (map #(substitute % x t) form))
        (vector? form) (mapv #(substitute % x t) form)
        :else form))

(defn- bound-letters [form]
  (set (mapcat (fn [f] (when (seq? f)
                         (cond (quantifiers (first f)) (second f)
                               (indexers (first f)) (pattern-vars (first (second f))))))
               (tree-seq coll? seq form))))

(defn unfold-definitions
  "A hypothesis conjunct x = t (x a free letter not in t) defines x: x is
   replaced by t everywhere and the conjunct by (defined t). Returns
   [statement' [[x t] ...]]."
  [statement]
  (if-not (and (seq? statement) (= 'implies (first statement)))
    [statement []]
    (let [[_ h concl] statement
          banned (bound-letters statement)]
      (loop [cs (vec (conjuncts h)) concl concl defs []]
        (let [free (free-letters (list* 'and concl cs))
              d (first (for [[k cj] (map-indexed vector cs)
                             :when (and (seq? cj) (= '= (first cj)) (= 3 (count cj)))
                             [x t] [[(nth cj 1) (nth cj 2)] [(nth cj 2) (nth cj 1)]]
                             :when (and (symbol? x) (free x) (not (banned x)) (not (constants x))
                                        (not (subscript x)) (not (occurs? x t)))]
                         [k x t]))]
          (if-let [[k x t] d]
            (recur (mapv #(substitute % x t) (assoc cs k (list 'defined t)))
                   (substitute concl x t)
                   (conj defs [x t]))
            [(list 'implies (if (= 1 (count cs)) (first cs) (cons 'and cs)) concl) defs]))))))

(defn- split-implication [s]
  (if (and (seq? s) (= 'implies (first s)) (= 3 (count s))) [(nth s 1) (nth s 2)] [nil s]))

(defn check
  "Search the models of statement over universe (default #{0 1}): all of them
   when there are at most :limit (default 20000), else :limit seeded samples.
   The search stops early once :budget binder assignments (default 2e7) are
   spent, so a statement whose quantifiers range over large sorts still ends.
   {:verdict :holds|:vacuous|:counterexample, :models n, :witnesses k (models
   meeting the hypothesis), :exhaustive? b, :total N, :counterexample env}."
  [statement & {:keys [universe limit seed budget]
                :or {universe #{0 1} limit 20000 seed 1 budget 20000000}}]
  (let [[stmt defs] (unfold-definitions statement)
        [hyp concl] (split-implication stmt)
        ls (sorts stmt)
        total (model-count ls universe)
        exhaustive? (<= total limit)
        envs (if exhaustive? (models ls universe) (sample-models ls universe limit seed))
        base {:universe universe :total total :sorts ls}
        spent? (fn [ex] (::exhausted (ex-data ex)))
        b (volatile! budget)]
    (binding [*budget* b]
      (loop [envs (seq envs) n 0 w 0]
        (let [outcome (when envs
                        (try (let [e (first envs)
                                   h? (or (nil? hyp) (boolean (denote hyp e)))]
                               {:h? h? :refuted? (and h? (not (denote concl e))) :env e})
                             (catch clojure.lang.ExceptionInfo ex
                               (if (spent? ex) ::spent (throw ex)))))]
          (cond
            (or (nil? envs) (= ::spent outcome))
            (assoc base :verdict (if (pos? w) :holds :vacuous) :models n :witnesses w
                   :exhaustive? (and exhaustive? (nil? envs)))

            (:refuted? outcome)
            (assoc base :verdict :counterexample :models (inc n) :witnesses (inc w)
                   :counterexample (reduce (fn [e [x t]] (assoc e x (denote t e))) (:env outcome) defs))

            :else (recur (next envs) (inc n) (if (:h? outcome) (inc w) w))))))))

(defn counterexample
  "A model over universe u (default #{0 1}) in which statement is false, or
   nil; searches at most :limit models (default 200000)."
  [statement & {:keys [universe limit] :or {universe #{0 1} limit 200000}}]
  (:counterexample (check statement :universe universe :limit limit)))
