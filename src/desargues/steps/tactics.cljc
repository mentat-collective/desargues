(ns desargues.steps.tactics
  "TACTICS: goals, moves, and the combinators that build rules out of rules.

   A Goal is what is left to do: integrate `f` (a frozen Emmy form) with
   respect to `var` (:integral), take its derivative (:derivative), or
   integrate between bounds (:definite). A goal carries `solve`, the port
   back into the search (desargues.steps.search), for the rules that need
   an auxiliary result before they can move (by parts needs v = ∫ dv).

   A Move is what one rule application does to a goal:
     :rule      the rule id that licensed it
     :steps     the Steps written before the subgoals are worked
     :subgoals  the goals it leaves (empty when it closes the goal)
     :finish    (fn [answers]) -> {:answer F :steps [...]} combining the
                subgoals' answers into this goal's answer, with the Steps
                written after them (substitute back, hence, solve for ...)
     :cost      what the move costs the reader (the search minimises it)

   IRule (desargues.steps.protocols) is the port: (moves rule goal) is
   every Move a rule licenses. `rule` builds one from a function; the
   combinators then / orelse / choice / repeat / attempt are IRules made of
   IRules, so tactics compose as values.

   Host-free .cljc."
  (:require [desargues.steps.expr :as x]
            [desargues.steps.protocols :as p]))

(defrecord Goal [kind f var bounds solve])

(defn goal
  "A goal: (goal :integral f var) or (goal :definite f var [a b])."
  ([kind f var] (->Goal kind (x/simp f) var nil nil))
  ([kind f var bounds] (->Goal kind (x/simp f) var bounds nil)))

(defn sub-goal
  "A goal derived from `parent`: same kind and solver unless given."
  ([parent f] (sub-goal parent f (:var parent)))
  ([parent f var] (->Goal (if (= :definite (:kind parent)) :integral (:kind parent))
                          (x/simp f) var nil (:solve parent))))

(defn goal-key
  "What the search memoises on: the goal up to its solver."
  [g]
  [(:kind g) (:f g) (:var g) (:bounds g)])

(defrecord Move [rule steps subgoals finish cost])

(defn move
  "A Move. `finish` defaults to passing the single subgoal's answer through."
  [{:keys [rule steps subgoals finish cost]
    :or {steps [] subgoals [] cost 1}}]
  (->Move rule (vec steps) (vec subgoals)
          (or finish (fn [[a]] {:answer a :steps []}))
          cost))

(defn closing
  "A move that closes its goal with `answer`."
  [rule steps answer cost]
  (move {:rule rule :steps steps :cost cost
         :finish (fn [_] {:answer answer :steps []})}))

;; ## Rules as values

(defn rule
  "An IRule named `id` whose moves are (f goal), a seq of Moves. A rule
   whose algebra Emmy cannot carry out on this goal (a division by zero
   while simplifying, say) licenses no move there; the search goes on."
  [id f]
  (reify p/IRule
    (rule-id [_] id)
    (moves [_ g]
      (try (doall (or (seq (f g)) ()))
           (catch #?(:clj ArithmeticException :cljs :default) _ ())
           (catch #?(:clj IllegalArgumentException :cljs :default) _ ())))))

(defn- compose
  "m1 then m2, where m2 works on m1's only subgoal."
  [m1 m2]
  (move {:rule     (:rule m1)
         :steps    (into (:steps m1) (:steps m2))
         :subgoals (:subgoals m2)
         :cost     (+ (:cost m1) (:cost m2))
         :finish   (fn [answers]
                     (let [r2 ((:finish m2) answers)
                           r1 ((:finish m1) [(:answer r2)])]
                       {:answer (:answer r1) :steps (into (:steps r2) (:steps r1))}))}))

(defn then
  "Every r1 move followed by every r2 move on its single subgoal. A move
   of r1 that leaves zero or several subgoals is kept as it is."
  [& rs]
  (reduce
   (fn [r1 r2]
     (rule [:then (p/rule-id r1) (p/rule-id r2)]
           (fn [g]
             (mapcat (fn [m1]
                       (if (= 1 (count (:subgoals m1)))
                         (map #(compose m1 %) (p/moves r2 (first (:subgoals m1))))
                         [m1]))
                     (p/moves r1 g)))))
   rs))

(defn choice
  "Every move of every rule: the alternatives the search ranks."
  [& rs]
  (rule (into [:choice] (map p/rule-id rs))
        (fn [g] (mapcat #(p/moves % g) rs))))

(defn orelse
  "The moves of the first rule that has any."
  [& rs]
  (rule (into [:orelse] (map p/rule-id rs))
        (fn [g] (some #(seq (p/moves % g)) rs))))

(def ^:private identity-move
  (move {:rule :identity :cost 0}))

(defn attempt
  "The moves of r, or the identity move (no step, the goal itself) when it
   has none: r made optional inside a `then`."
  [r]
  (rule [:try (p/rule-id r)]
        (fn [g] (or (seq (p/moves r g))
                    [(assoc identity-move :subgoals [g])]))))

(defn repeated
  "r applied while it applies (at most `limit` times): each maximal chain."
  ([r] (repeated r 8))
  ([r limit]
   (rule [:repeat (p/rule-id r)]
         (fn self [g]
           (mapcat (fn [m]
                     (let [more (when (and (pos? limit) (= 1 (count (:subgoals m))))
                                  (seq (p/moves (repeated r (dec limit)) (first (:subgoals m)))))]
                       (if more (map #(compose m %) more) [m])))
                   (p/moves r g))))))
