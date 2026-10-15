(ns desargues.devx.specs
  "Specification Pattern for Segment Queries.
   
   This module provides composable specifications for querying
   segments in a scene graph. Specifications can be combined
   using logical operators (and, or, not).
   
   ## Usage
   
   ```clojure
   ;; Find dirty segments
   (find-segments graph (dirty?))
   
   ;; Find segments with specific dependency
   (find-segments graph (depends-on? :intro))
   
   ;; Combine specs
   (find-segments graph 
     (and-spec (dirty?)
               (has-metadata? :source-ns)))
   
   ;; Custom predicate
   (find-segments graph 
     (where #(> (:render-time (:metadata %)) 5.0)))
   ```")

;; =============================================================================
;; Specification Protocol
;; =============================================================================

(defprotocol ISpecification
  "Protocol for segment specifications."
  (satisfied? [this segment]
    "Check if the segment satisfies this specification.")
  (spec-description [this]
    "Return a human-readable description of the specification."))

;; =============================================================================
;; Base Specifications
;; =============================================================================

(defrecord StateSpec [state]
  ISpecification
  (satisfied? [_ segment]
    (= state (:state segment)))
  (spec-description [_]
    (str "state=" (name state))))

(defrecord DirtySpec []
  ISpecification
  (satisfied? [_ segment]
    (= :dirty (:state segment)))
  (spec-description [_]
    "dirty"))

(defrecord CachedSpec []
  ISpecification
  (satisfied? [_ segment]
    (= :cached (:state segment)))
  (spec-description [_]
    "cached"))

(defrecord PendingSpec []
  ISpecification
  (satisfied? [_ segment]
    (= :pending (:state segment)))
  (spec-description [_]
    "pending"))

(defrecord ErrorSpec []
  ISpecification
  (satisfied? [_ segment]
    (= :error (:state segment)))
  (spec-description [_]
    "error"))

(defrecord DependsOnSpec [dep-id]
  ISpecification
  (satisfied? [_ segment]
    (contains? (or (:deps segment) #{}) dep-id))
  (spec-description [_]
    (str "depends-on:" (name dep-id))))

(defrecord HasDependentsSpec []
  ISpecification
  (satisfied? [_ segment]
    (seq (:deps segment)))
  (spec-description [_]
    "has-dependents"))

(defrecord IndependentSpec []
  ISpecification
  (satisfied? [_ segment]
    (empty? (:deps segment)))
  (spec-description [_]
    "independent"))

(defrecord HasMetadataSpec [key]
  ISpecification
  (satisfied? [_ segment]
    (contains? (:metadata segment) key))
  (spec-description [_]
    (str "has-metadata:" (name key))))

(defrecord MetadataEqualsSpec [key value]
  ISpecification
  (satisfied? [_ segment]
    (= value (get-in segment [:metadata key])))
  (spec-description [_]
    (str "metadata:" (name key) "=" value)))

(defrecord IdMatchesSpec [pattern]
  ISpecification
  (satisfied? [_ segment]
    (re-matches pattern (name (:id segment))))
  (spec-description [_]
    (str "id-matches:" pattern)))

(defrecord PredicateSpec [pred-fn description]
  ISpecification
  (satisfied? [_ segment]
    (pred-fn segment))
  (spec-description [_]
    (or description "custom-predicate")))

;; =============================================================================
;; Composite Specifications
;; =============================================================================

(defrecord AndSpec [specs]
  ISpecification
  (satisfied? [_ segment]
    (every? #(satisfied? % segment) specs))
  (spec-description [_]
    (str "(" (clojure.string/join " AND " (map spec-description specs)) ")")))

(defrecord OrSpec [specs]
  ISpecification
  (satisfied? [_ segment]
    (some #(satisfied? % segment) specs))
  (spec-description [_]
    (str "(" (clojure.string/join " OR " (map spec-description specs)) ")")))

(defrecord NotSpec [spec]
  ISpecification
  (satisfied? [_ segment]
    (not (satisfied? spec segment)))
  (spec-description [_]
    (str "NOT(" (spec-description spec) ")")))

;; =============================================================================
;; Factory Functions
;; =============================================================================

(defn state?
  "Specification: segment has specific state."
  [state]
  (->StateSpec state))

(defn dirty?
  "Specification: segment is dirty (needs re-render)."
  []
  (->DirtySpec))

(defn cached?
  "Specification: segment is cached."
  []
  (->CachedSpec))

(defn pending?
  "Specification: segment is pending."
  []
  (->PendingSpec))

(defn error?
  "Specification: segment has error."
  []
  (->ErrorSpec))

(defn depends-on?
  "Specification: segment depends on given segment ID."
  [dep-id]
  (->DependsOnSpec dep-id))

(defn has-dependents?
  "Specification: segment has dependencies."
  []
  (->HasDependentsSpec))

(defn independent?
  "Specification: segment has no dependencies."
  []
  (->IndependentSpec))

(defn has-metadata?
  "Specification: segment has metadata key."
  [key]
  (->HasMetadataSpec key))

(defn metadata-equals?
  "Specification: segment metadata key equals value."
  [key value]
  (->MetadataEqualsSpec key value))

(defn id-matches?
  "Specification: segment ID matches regex pattern."
  [pattern]
  (->IdMatchesSpec (if (string? pattern) (re-pattern pattern) pattern)))

(defn where
  "Specification: custom predicate function.
   
   Usage:
   (where #(> (:render-time (:metadata %)) 5.0) \"slow-render\")"
  ([pred-fn]
   (->PredicateSpec pred-fn nil))
  ([pred-fn description]
   (->PredicateSpec pred-fn description)))

;; =============================================================================
;; Combinators
;; =============================================================================

(defn and-spec
  "Combine specifications with AND."
  [& specs]
  (->AndSpec (vec specs)))

(defn or-spec
  "Combine specifications with OR."
  [& specs]
  (->OrSpec (vec specs)))

(defn not-spec
  "Negate a specification."
  [spec]
  (->NotSpec spec))

;; =============================================================================
;; Query Functions
;; =============================================================================

(defn find-segments
  "Find all segments in graph matching specification.
   Returns sequence of segments."
  [graph spec]
  (let [segments (vals (:segments graph))]
    (filter #(satisfied? spec %) segments)))

(defn find-segment-ids
  "Find all segment IDs in graph matching specification."
  [graph spec]
  (map :id (find-segments graph spec)))

(defn count-segments
  "Count segments matching specification."
  [graph spec]
  (count (find-segments graph spec)))

(defn any-match?
  "Check if any segment matches specification."
  [graph spec]
  (boolean (some #(satisfied? spec %) (vals (:segments graph)))))

(defn all-match?
  "Check if all segments match specification."
  [graph spec]
  (every? #(satisfied? spec %) (vals (:segments graph))))

(defn partition-by-spec
  "Partition segments into [matching, not-matching]."
  [graph spec]
  (let [segments (vals (:segments graph))
        grouped (group-by #(satisfied? spec %) segments)]
    [(get grouped true [])
     (get grouped false [])]))

;; =============================================================================
;; Convenience Queries
;; =============================================================================

(defn needs-render?
  "Specification: segment needs rendering (dirty or pending)."
  []
  (or-spec (dirty?) (pending?)))

(defn renderable?
  "Specification: segment can be rendered (not cached, not rendering)."
  []
  (and-spec (not-spec (cached?))
            (not-spec (state? :rendering))))

(defn from-namespace?
  "Specification: segment defined in given namespace."
  [ns-sym]
  (metadata-equals? :source-ns ns-sym))
