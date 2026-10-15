(ns desargues.devx.segment
  "Core Segment abstraction for incremental rendering.
   
   A Segment represents a discrete, cacheable portion of a scene.
   Each segment has:
   - Unique identity (keyword)
   - Content hash for change detection  
   - Dependencies on other segments
   - Render state tracking
   - Associated partial movie file when cached
   
   ## Design Principles
   
   1. Segments are immutable value objects
   2. State changes create new segment versions
   3. Hashing enables content-addressable caching
   4. Dependencies form a DAG (no cycles allowed)"
  (:require [clojure.java.io :as io])
  (:import [java.security MessageDigest]
           [java.util Base64]))

;; =============================================================================
;; Protocols
;; =============================================================================

(defprotocol ISegment
  "Protocol for segment operations."
  (segment-id [this] "Returns the unique identifier for this segment")
  (segment-hash [this] "Returns content hash for change detection")
  (dependencies [this] "Returns set of segment IDs this depends on")
  (render-state [this] "Returns current render state")
  (construct! [this scene] "Execute this segment's animations on scene"))

(defprotocol IHashable
  "Protocol for content-addressable hashing."
  (content-hash [this] "Compute hash of content for caching"))

;; =============================================================================
;; Render States
;; =============================================================================

(def render-states
  "Valid render states for a segment."
  #{:pending ; Not yet rendered
    :rendering ; Currently being rendered
    :cached ; Successfully rendered and cached
    :dirty ; Source changed, needs re-render
    :error}) ; Render failed

(defn valid-state? [state]
  (contains? render-states state))

;; =============================================================================
;; Segment Record
;; =============================================================================

(defrecord Segment
           [id ; keyword - unique identifier like :step-7
            hash ; string - content hash for change detection
            deps ; set of keywords - segment IDs this depends on
            state ; keyword - one of render-states
            partial-file ; string or nil - path to cached partial movie
            construct-fn ; function - (fn [scene] ...) builds animations
            metadata] ; map - {:duration, :description, :source-ns, etc.}

  ISegment
  (segment-id [_] id)
  (segment-hash [_] hash)
  (dependencies [_] (or deps #{}))
  (render-state [_] state)
  (construct! [_ scene]
    (when construct-fn
      (construct-fn scene))))

;; =============================================================================
;; Hashing Utilities
;; =============================================================================

(defn- sha256
  "Compute SHA-256 hash of a string, return base64-encoded."
  [^String s]
  (let [digest (MessageDigest/getInstance "SHA-256")
        hash-bytes (.digest digest (.getBytes s "UTF-8"))]
    (.encodeToString (Base64/getUrlEncoder) hash-bytes)))

(defn- combine-hashes
  "Combine multiple hash values into one."
  [& hashes]
  (sha256 (apply str (sort hashes))))

(defn compute-segment-hash
  "Compute content hash for a segment based on:
   - construct-fn source (if available)
   - dependency hashes
   - metadata that affects rendering"
  [{:keys [construct-fn deps metadata]} dep-hashes]
  (let [fn-hash (sha256 (pr-str construct-fn))
        dep-hash (if (seq dep-hashes)
                   (apply combine-hashes (vals dep-hashes))
                   "no-deps")
        meta-hash (sha256 (pr-str (select-keys metadata
                                               [:duration :quality :run-time])))]
    (combine-hashes fn-hash dep-hash meta-hash)))

;; =============================================================================
;; Segment Construction
;; =============================================================================

(defn create-segment
  "Create a new segment with the given parameters.
   
   Options:
   - :id (required) - unique keyword identifier
   - :construct-fn (required) - function (fn [scene] ...)
   - :deps - set of segment IDs this depends on
   - :metadata - map of additional info
   
   Example:
   (create-segment
     :id :intro
     :construct-fn (fn [scene] (play! scene (create title)))
     :deps #{}
     :metadata {:description \"Introduction sequence\"
                :duration 5.0})"
  [& {:keys [id construct-fn deps metadata]
      :or {deps #{} metadata {}}}]
  {:pre [(keyword? id)
         (or (nil? construct-fn) (fn? construct-fn))
         (set? deps)
         (every? keyword? deps)]}
  (let [seg-data {:construct-fn construct-fn
                  :deps deps
                  :metadata metadata}
        hash (compute-segment-hash seg-data {})]
    (->Segment id hash deps :pending nil construct-fn metadata)))

(defn with-hash
  "Return segment with updated hash based on dependency hashes."
  [segment dep-hashes]
  (let [new-hash (compute-segment-hash segment dep-hashes)]
    (assoc segment :hash new-hash)))

;; =============================================================================
;; State Transitions
;; =============================================================================

(defn mark-rendering
  "Transition segment to :rendering state."
  [segment]
  {:pre [(#{:pending :dirty :error} (:state segment))]}
  (assoc segment :state :rendering))

(defn mark-cached
  "Transition segment to :cached state with partial file path."
  [segment partial-file-path]
  {:pre [(= :rendering (:state segment))
         (string? partial-file-path)]}
  (assoc segment
         :state :cached
         :partial-file partial-file-path))

(defn mark-dirty
  "Transition segment to :dirty state (needs re-render)."
  [segment]
  (assoc segment :state :dirty))

(defn mark-error
  "Transition segment to :error state with error info."
  [segment error-info]
  (assoc segment
         :state :error
         :metadata (assoc (:metadata segment) :error error-info)))

(defn mark-pending
  "Reset segment to :pending state."
  [segment]
  (assoc segment
         :state :pending
         :partial-file nil))

;; =============================================================================
;; Partial File Management
;; =============================================================================

(def ^:dynamic *partial-dir* "media/partial_movies")

(defn partial-file-path
  "Get the path for a segment's partial movie file."
  [segment]
  (str *partial-dir* "/" (name (:id segment)) "_"
       (subs (:hash segment) 0 8) ".mp4"))

(defn partial-file-exists?
  "Check if the partial file for this segment exists."
  [segment]
  (when-let [path (:partial-file segment)]
    (.exists (io/file path))))

(defn ensure-partial-dir!
  "Ensure the partial movies directory exists."
  []
  (let [dir (io/file *partial-dir*)]
    (when-not (.exists dir)
      (.mkdirs dir))))

;; =============================================================================
;; Segment Queries
;; =============================================================================

(defn needs-render?
  "Check if segment needs to be rendered."
  [segment]
  (#{:pending :dirty} (:state segment)))

(defn is-cached?
  "Check if segment is cached and valid."
  [segment]
  (and (= :cached (:state segment))
       (partial-file-exists? segment)))

(defn is-independent?
  "Check if segment has no dependencies."
  [segment]
  (empty? (:deps segment)))

;; =============================================================================
;; Segment DSL Helpers
;; =============================================================================

(defmacro defsegment
  "Define a named segment with a construct function.
   
   Example:
   (defsegment intro
     \"Introduction with title\"
     :deps #{}
     :duration 5.0
     [scene]
     (let [title (create-text \"Hello\")]
       (play! scene (write title))
       (wait! scene 2)))"
  [name docstring & {:keys [deps duration]
                     :or {deps #{}}
                     :as opts}]
  (let [body-start (drop-while #(not (vector? %)) (vals opts))
        args (first body-start)
        body (rest body-start)]
    `(def ~name
       (create-segment
        :id ~(keyword name)
        :construct-fn (fn ~args ~@body)
        :deps ~deps
        :metadata {:description ~docstring
                   :duration ~duration
                   :source-ns '~(ns-name *ns*)}))))

;; =============================================================================
;; Printing
;; =============================================================================

(defmethod print-method Segment [seg ^java.io.Writer w]
  (.write w (str "#Segment{:id " (:id seg)
                 " :state " (:state seg)
                 " :hash " (subs (or (:hash seg) "nil") 0 8) "...}")))
