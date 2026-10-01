(ns desargues.logic.proof.schema
  "The shapes of authored proofs, as malli schemas."
  (:require [malli.core :as m]
            [malli.error :as me]))

(def Formula
  "A formula or term: symbols, constants, and nested lists or binder vectors."
  [:schema {:registry {::form [:or symbol? boolean? number? string? keyword?
                               [:and vector? [:sequential [:ref ::form]]]
                               [:and seq? [:sequential [:ref ::form]]]]}}
   ::form])

(def LineRef
  "One line index or several."
  [:or nat-int? [:vector nat-int?]])

(def Line
  [:map
   [:claim Formula]
   [:by keyword?]
   [:from {:optional true} LineRef]
   [:with {:optional true} [:map-of symbol? Formula]]
   [:vars {:optional true} [:vector symbol?]]
   [:result {:optional true} string?]])

(def Proof
  [:map
   [:goal Formula]
   [:lines [:vector {:min 1} Line]]])

(defn explain
  "nil when proof has the Proof shape, else the humanised explanation."
  [proof]
  (some-> (m/explain Proof proof) me/humanize))
