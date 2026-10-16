(ns desargues.steps.search
  "SEARCH: dynamic programming over the moves a rule licenses.

     cost(goal) = min over moves m of  cost(m) + Σ cost(subgoal)

   memoised on the goal (desargues.steps.tactics/goal-key) in a table that
   lives for one `solve` call (or longer, when a caller passes its own), with
   a guard against cycles (a goal met again while it is still being worked
   costs infinity on that path) and a depth bound.

   A Solution is the best way to close a goal: its answer, the Steps that
   write it out (the move's own steps, each subgoal's solution in order,
   then the move's finishing steps), its cost and the rules and lemmas it
   used. `pathways` ranks every first move of the top goal by the cost of
   its best completion: the alternative ways a student could go.

   Host-free .cljc."
  (:require [desargues.steps.protocols :as p]
            [desargues.steps.tactics :as t]))

(def ^:dynamic *max-depth*
  "How deep the search goes below the top goal. Each level multiplies the
   moves tried; 5 covers the textbook chapter (substitution inside parts
   inside linearity) in well under a second per problem."
  5)

(defrecord Solution [goal answer steps cost rules cited move])

(def ^:private inf #?(:clj Double/POSITIVE_INFINITY :cljs js/Infinity))

(declare best)

(defn- complete
  "The Solution `move` gives `g` when each subgoal is solved at its best,
   or nil when a subgoal cannot be solved."
  [ctx g m depth]
  (let [subs (map #(best ctx % (inc depth)) (:subgoals m))]
    (when (every? some? subs)
      (let [{:keys [answer steps]} ((:finish m) (mapv :answer subs))]
        (->Solution g answer
                    (-> (:steps m) (into (mapcat :steps subs)) (into steps))
                    (+ (:cost m) (reduce + 0 (map :cost subs)))
                    (into #{(:rule m)} (mapcat :rules subs))
                    (into (set (:cited m)) (mapcat :cited subs))
                    m)))))

(defn- with-solver
  "g carrying the port back into this search, for rules that need an
   auxiliary result (v = ∫ dv)."
  [ctx g depth]
  (assoc g :solve (fn [g'] (best ctx (with-solver ctx g' depth) (inc depth)))))

(defn- safe-complete
  "complete, or nil when Emmy's algebra fails while finishing the move."
  [ctx g m depth]
  (try (complete ctx g m depth)
       (catch #?(:clj ArithmeticException :cljs :default) _ nil)
       (catch #?(:clj IllegalArgumentException :cljs :default) _ nil)))

(defn- candidates
  "Every complete Solution of g, cheapest first."
  [ctx g depth]
  (->> (p/moves (:rule ctx) (with-solver ctx g depth))
       (keep #(safe-complete ctx g % depth))
       (sort-by :cost)))

(defn best
  "The cheapest Solution of goal g, or nil. ctx: {:rule IRule :table atom
   :open atom}."
  ([ctx g] (best ctx g 0))
  ([ctx g depth]
   (let [k (t/goal-key g)
         table (:table ctx)]
     (cond
       (contains? @table k)                (get @table k)
       (or (> depth *max-depth*)
           (contains? @(:open ctx) k))     nil
       :else
       (do (swap! (:open ctx) conj k)
           (let [s (first (candidates ctx g depth))]
             (swap! (:open ctx) disj k)
             (swap! table assoc k s)
             s))))))

(defn context
  "A search context over `rule`, with a fresh (or the given) memo table."
  ([rule] (context rule (atom {})))
  ([rule table] {:rule rule :table table :open (atom #{})}))

(defn solve
  "The best Solution of goal g under `rule`."
  [rule g]
  (best (context rule) g))

(defn pathways
  "Every way to start on g, each completed at its best, cheapest first:
   [{:rule id :cost c :solution Solution} ...]. The first is `solve`'s."
  [rule g]
  (let [ctx (context rule)]
    (swap! (:open ctx) conj (t/goal-key g))
    (->> (candidates ctx g 0)
         (map (fn [s] {:rule (get-in s [:move :rule]) :cost (:cost s) :solution s})))))

(defn table
  "The DP table of a context as rows {:goal key :cost c :rule id}."
  [ctx]
  (for [[k s] @(:table ctx)]
    {:goal k :cost (if s (:cost s) inf) :rule (get-in s [:move :rule])}))
