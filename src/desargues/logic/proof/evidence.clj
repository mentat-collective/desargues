(ns desargues.logic.proof.evidence
  "The evidence a proof line rests on: a closed set, ordered from strongest
   to weakest."
  (:require [hive-dsl.adt :refer [defadt]]))

(defadt Evidence
  "What justified a proof line."
  :evidence/given          ; a hypothesis or an assumption
  :evidence/definitional   ; one definition unfolded or folded
  :evidence/used           ; an instance of a result already proved
  :evidence/tautology      ; propositional consequence of the cited lines
  :evidence/structural     ; a quantifier or subproof rule
  :evidence/model          ; no counterexample in small finite models
  :evidence/cited          ; a book result taken as given
  :evidence/unknown)       ; no rule recognised the line

(def strength
  "Evidence variants from strongest to weakest."
  [:evidence/given :evidence/definitional :evidence/used :evidence/tautology
   :evidence/structural :evidence/model :evidence/cited :evidence/unknown])

(defn weakest
  "The weakest of the given evidence variants, or nil for none."
  [variants]
  (when (seq variants)
    (apply max-key #(.indexOf ^java.util.List strength %) variants)))
