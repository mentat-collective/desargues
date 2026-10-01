(ns desargues.logic.proof.scope
  "What is in force at each line of a proof.

   A Scope holds the givens in force (line index -> claim) and the subproof
   depth. `enter` is OPEN on the line's :by: how a line changes the scope
   for the lines after it. By default it changes nothing.")

(defrecord Scope [in-force depth])

(def initial (->Scope (sorted-map) 0))

(defmulti enter
  "The scope after line i, given the scope in force at it."
  (fn [_scope _i line] (:by line)))

(defmethod enter :default [scope _ _] scope)

(defmethod enter :hyp [scope i {:keys [claim]}]
  (update scope :in-force assoc i claim))

(defmethod enter :assume [scope i {:keys [claim]}]
  (-> scope (update :in-force assoc i claim) (update :depth inc)))

(defmethod enter :discharge [scope _ {:keys [from]}]
  (-> scope (update :in-force dissoc (first from)) (update :depth dec)))

(defn scopes
  "The Scope in force at each line: [scope-at-0 scope-at-1 ...]."
  [lines]
  (vec (butlast (reductions (fn [s [i line]] (enter s i line))
                            initial
                            (map-indexed vector lines)))))

(defn givens
  "The claims in force in scope."
  [scope]
  (vals (:in-force scope)))
