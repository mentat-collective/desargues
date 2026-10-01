(ns desargues.logic.refute
  "The truth table run backwards: hunt for a counterexample.

   Assume the formula is FALSE and propagate. At a node whose value is known,
   the connective's rows that give that value and agree with what is already
   known are its candidates. One candidate (the essential case) pins every
   argument; several candidates still pin any position on which they all
   agree. A node with no candidate is a clash: the branch closes. When nothing
   more is forced the search branches on the candidates of one node.

   Every branch closing proves the formula a tautology; an open branch is a
   counterexample. The search tree is data:

     {:assign {subformula bool}
      :steps  [{:kind :assume|:force|:clash :node :value :rows :forced}]
      :status :closed | :open | :branch
      :split  {:node :rows}          ; when :branch
      :children [tree ...]           ; when :branch
      :counterexample env}           ; when :open"
  (:require [desargues.logic.connective :as c]
            [desargues.logic.formula :as f]))

(defn- known [assign a] (if (f/constant? a) a (get assign a)))

(defn- candidates
  "The rows of node g giving value v that agree with assign."
  [assign g v]
  (let [as (f/args g)]
    (filterv (fn [row] (every? (fn [[a x]] (let [k (known assign a)] (or (nil? k) (= k x))))
                               (map vector as row)))
             (c/rows-giving (f/op g) (count as) v))))

(defn- agreed
  "Positions on which every row agrees: {index value}."
  [rows]
  (into {} (for [i (range (count (first rows)))
                 :let [vs (set (map #(nth % i) rows))]
                 :when (= 1 (count vs))]
             [i (first vs)])))

(defn- force-step
  "The next forcing at a valued compound node, or a clash, or nil."
  [assign]
  (some (fn [[g v]]
          (when (f/compound? g)
            (let [rows (candidates assign g v)]
              (if (empty? rows)
                {:kind :clash :node g :value v :rows []}
                (let [as (f/args g)
                      new (into {} (for [[i x] (agreed rows)
                                         :let [a (nth as i)]
                                         :when (and (not (f/constant? a)) (nil? (get assign a)))]
                                     [a x]))]
                  (when (seq new)
                    {:kind :force :node g :value v :rows rows :forced new}))))))
        assign))

(defn- propagate
  "Force until nothing changes: [assign steps clash?]."
  [assign]
  (loop [assign assign steps []]
    (if-let [{:keys [kind forced] :as s} (force-step assign)]
      (if (= :clash kind)
        [assign (conj steps s) true]
        (recur (merge assign forced) (conj steps s)))
      [assign steps false])))

(defn- open-split
  "A valued compound node with more than one candidate row, or nil."
  [assign]
  (some (fn [[g v]]
          (when (f/compound? g)
            (let [rows (candidates assign g v)]
              (when (< 1 (count rows)) {:node g :value v :rows rows}))))
        (sort-by (comp - count f/subformulas key) assign)))

(defn- search [formula assign steps]
  (let [[assign more clash?] (propagate assign)
        steps (into steps more)]
    (cond
      clash? {:assign assign :steps steps :status :closed}
      :else
      (if-let [{:keys [node rows] :as split} (open-split assign)]
        {:assign assign :steps steps :status :branch :split split
         :children (mapv (fn [row]
                           (let [forced (into {} (remove (comp f/constant? first))
                                              (map vector (f/args node) row))]
                             (search formula (merge assign forced)
                                     [{:kind :force :node node :value (get assign node)
                                       :rows [row] :forced forced}])))
                         rows)}
        {:assign assign :steps steps :status :open
         :counterexample (into {} (for [a (f/atoms formula)] [a (get assign a true)]))}))))

(defn refute
  "The counterexample search tree for formula, starting from formula = false."
  [formula]
  (search formula {formula false} [{:kind :assume :node formula :value false}]))

(defn leaves [tree]
  (if (= :branch (:status tree)) (mapcat leaves (:children tree)) [tree]))

(defn tautology?
  "True when every branch of formula's refutation closes."
  [formula]
  (every? #(= :closed (:status %)) (leaves (refute formula))))

(defn counterexample
  "An env falsifying formula, or nil when it is a tautology."
  [formula]
  (some :counterexample (leaves (refute formula))))
