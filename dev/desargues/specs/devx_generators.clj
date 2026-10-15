(ns desargues.specs.devx-generators
  "Custom test.check generators for devx module property-based testing.
   
   Provides generators for:
   - Segment identifiers and metadata
   - Construct functions (simple test versions)
   - Dependency sets
   - Complete Segment records
   - Collections of segments forming valid DAGs"
  (:require [clojure.test.check.generators :as gen]
            [desargues.devx.segment :as seg]))

;; =============================================================================
;; Basic Generators
;; =============================================================================

(def gen-segment-id
  "Generate valid segment identifiers (keywords)."
  (gen/fmap keyword
            (gen/such-that #(and (seq %)
                                 (re-matches #"[a-z][a-z0-9-]*" %))
                           gen/string-alphanumeric
                           100)))

(def gen-simple-segment-id
  "Generate simple segment IDs like :seg-0, :seg-1, etc."
  (gen/fmap #(keyword (str "seg-" %))
            (gen/choose 0 100)))

(def gen-construct-fn
  "Generate simple construct functions for testing.
   These don't actually render anything, just return a value."
  (gen/fmap (fn [n]
              (fn [_scene]
                {:test-value n}))
            gen/nat))

(def gen-distinct-construct-fn
  "Generate construct functions that are distinguishable by their output."
  (gen/fmap (fn [[n s]]
              (fn [_scene]
                {:id n :data s}))
            (gen/tuple gen/nat gen/string-alphanumeric)))

;; =============================================================================
;; Metadata Generators  
;; =============================================================================

(def gen-duration
  "Generate valid duration values (positive numbers)."
  (gen/double* {:min 0.1 :max 60.0 :NaN? false :infinite? false}))

(def gen-segment-metadata
  "Generate valid segment metadata maps."
  (gen/hash-map
   :description gen/string-alphanumeric
   :duration gen-duration
   :source-ns (gen/return 'test.namespace)))

(def gen-minimal-metadata
  "Generate minimal metadata (often empty)."
  (gen/one-of [(gen/return {})
               (gen/hash-map :description gen/string-alphanumeric)]))

;; =============================================================================
;; Dependency Generators
;; =============================================================================

(def gen-empty-deps
  "Generate empty dependency set."
  (gen/return #{}))

(defn gen-dependency-set
  "Generate a set of dependencies from available IDs."
  [available-ids]
  (if (empty? available-ids)
    (gen/return #{})
    (gen/fmap set (gen/vector (gen/elements available-ids) 0 (min 3 (count available-ids))))))

;; =============================================================================
;; Segment Generators
;; =============================================================================

(def gen-independent-segment
  "Generate a segment with no dependencies."
  (gen/fmap
   (fn [[id construct-fn metadata]]
     (seg/create-segment
      :id id
      :construct-fn construct-fn
      :deps #{}
      :metadata metadata))
   (gen/tuple gen-simple-segment-id gen-construct-fn gen-minimal-metadata)))

(defn gen-segment-with-id
  "Generate a segment with a specific ID."
  [id]
  (gen/fmap
   (fn [[construct-fn metadata]]
     (seg/create-segment
      :id id
      :construct-fn construct-fn
      :deps #{}
      :metadata metadata))
   (gen/tuple gen-construct-fn gen-minimal-metadata)))

(defn gen-segment-with-deps
  "Generate a segment with specified dependencies."
  [id deps]
  (gen/fmap
   (fn [[construct-fn metadata]]
     (seg/create-segment
      :id id
      :construct-fn construct-fn
      :deps (set deps)
      :metadata metadata))
   (gen/tuple gen-construct-fn gen-minimal-metadata)))

;; =============================================================================
;; DAG Generators (Acyclic Segment Collections)
;; =============================================================================

(defn gen-linear-chain
  "Generate a linear chain of segments: a -> b -> c -> ..."
  [n]
  (gen/fmap
   (fn [construct-fns]
     (let [ids (mapv #(keyword (str "seg-" %)) (range n))]
       (mapv (fn [idx]
               (let [id (nth ids idx)
                     deps (if (zero? idx) #{} #{(nth ids (dec idx))})]
                 (seg/create-segment
                  :id id
                  :construct-fn (nth construct-fns idx)
                  :deps deps
                  :metadata {})))
             (range n))))
   (gen/vector gen-construct-fn n)))

(defn gen-diamond-dag
  "Generate a diamond-shaped DAG:
        a
       / \\
      b   c
       \\ /
        d"
  []
  (gen/fmap
   (fn [[fn-a fn-b fn-c fn-d]]
     [(seg/create-segment :id :a :construct-fn fn-a :deps #{})
      (seg/create-segment :id :b :construct-fn fn-b :deps #{:a})
      (seg/create-segment :id :c :construct-fn fn-c :deps #{:a})
      (seg/create-segment :id :d :construct-fn fn-d :deps #{:b :c})])
   (gen/tuple gen-construct-fn gen-construct-fn gen-construct-fn gen-construct-fn)))

(def gen-acyclic-segments
  "Generate a collection of 1-5 segments forming a valid DAG.
   Segments are generated in topological order with valid dependencies."
  (gen/bind (gen/choose 1 5)
            gen-linear-chain))

(def gen-small-dag
  "Generate a small DAG with 2-4 segments."
  (gen/one-of
   [(gen-linear-chain 2)
    (gen-linear-chain 3)
    (gen-diamond-dag)]))

;; =============================================================================
;; State Generators
;; =============================================================================

(def gen-render-state
  "Generate a valid render state."
  (gen/elements [:pending :rendering :cached :dirty :error]))

(def gen-renderable-state
  "Generate a state that allows transitioning to :rendering."
  (gen/elements [:pending :dirty :error]))

;; =============================================================================
;; Registration (for spec integration)
;; =============================================================================

(defn register-generators!
  "Register custom generators with spec (call during test setup)."
  []
  ;; Placeholder for future spec integration
  nil)
