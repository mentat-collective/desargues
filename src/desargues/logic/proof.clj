(ns desargues.logic.proof
  "Step-by-step proofs as data, checked line by line.

   A proof is {:goal formula :lines [{:claim formula :by rule ...}]}, shaped
   by desargues.logic.proof.schema/Proof. Checking runs, in order:

     shape     the proof against its schema
     scopes    what is in force at each line   (desargues.logic.proof.scope)
     justify   every line by its rule          (desargues.logic.proof.rules)
     report    the lines' results, and whether the last line is the goal

   (check proof)          against an empty library
   (check proof library)  a desargues.logic.proof.library/ResultLibrary of the
                          results proved before this one"
  (:require [desargues.logic.proof.evidence :as ev]
            [desargues.logic.proof.library :as lib]
            [desargues.logic.proof.rules :as rules]
            [desargues.logic.proof.schema :as schema]
            [desargues.logic.proof.scope :as sc]
            [desargues.logic.term :as t]
            [hive-dsl.result :as r]))

(defn- line-result
  "One line's report row."
  [line i result]
  (merge {:index i :claim (:claim line) :by (:by line) :ok? (r/ok? result)}
         (if (r/ok? result) (:ok result) (:error result))))

(defn- justify-all
  "Each line justified in its scope."
  [{:keys [goal lines]} library]
  (mapv (fn [i line scope]
          (line-result line i (rules/justify line {:lines lines :index i :scope scope
                                                   :goal goal :library library})))
        (range) lines (sc/scopes lines)))

(defn report
  "The report of already-justified rows: {:ok? :concludes? :weakest :lines}."
  [goal rows]
  (let [concludes? (t/alpha= goal (:claim (peek rows)))]
    {:ok? (and concludes? (every? :ok? rows))
     :concludes? concludes?
     :weakest (ev/weakest (map :evidence rows))
     :lines rows}))

(defn check
  "(ok report) for a well-shaped proof, (err {:proof/shape explanation})
   otherwise."
  ([proof] (check proof lib/empty-library))
  ([proof library]
   (if-let [problem (schema/explain proof)]
     (r/err {:proof/shape problem})
     (r/ok (report (:goal proof) (justify-all proof library))))))
