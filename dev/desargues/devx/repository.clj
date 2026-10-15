(ns desargues.devx.repository
  "Repository Pattern for Segment and Graph Persistence.
   
   This module provides abstractions for storing and retrieving
   scene graphs and segments from various storage backends.
   
   ## Built-in Repositories
   - :memory - In-memory storage (default, for testing)
   - :file - File system storage (EDN format)
   
   ## Custom Repositories
   ```clojure
   (defrecord MyRepository []
     IGraphRepository
     (save-graph [this id graph] ...)
     (load-graph [this id] ...)
     (list-graphs [this] ...)
     (delete-graph [this id] ...))
   
   (set-repository! (->MyRepository))
   ```
   
   ## Usage
   ```clojure
   ;; Save current scene
   (save! :my-project my-scene)
   
   ;; Load scene
   (def restored (load! :my-project))
   
   ;; List saved scenes
   (list-all)
   ```")

;; =============================================================================
;; Repository Protocol
;; =============================================================================

(defprotocol IGraphRepository
  "Protocol for scene graph persistence."

  (repo-name [this]
    "Return the repository name as a keyword.")

  (save-graph [this id graph]
    "Save a graph with the given ID.
     Returns the ID on success.")

  (load-graph [this id]
    "Load a graph by ID.
     Returns the graph or nil if not found.")

  (graph-exists? [this id]
    "Check if a graph with the given ID exists.")

  (list-graphs [this]
    "List all stored graph IDs.")

  (delete-graph [this id]
    "Delete a graph by ID.
     Returns true if deleted, false if not found."))

;; =============================================================================
;; Repository Registry
;; =============================================================================

(defonce ^:private repository-registry
  (atom {}))

(defonce ^:private current-repository
  (atom nil))

(defn register-repository!
  "Register a repository implementation by keyword."
  [repo-key repo-impl]
  {:pre [(keyword? repo-key)
         (satisfies? IGraphRepository repo-impl)]}
  (swap! repository-registry assoc repo-key repo-impl)
  repo-key)

(defn get-repository
  "Get a registered repository by keyword."
  [repo]
  (cond
    (keyword? repo)
    (or (get @repository-registry repo)
        (throw (ex-info (str "Unknown repository: " repo)
                        {:repository repo
                         :available (keys @repository-registry)})))

    (satisfies? IGraphRepository repo)
    repo

    :else
    (throw (ex-info "Invalid repository" {:repository repo}))))

(defn set-repository!
  "Set the current repository."
  [repo]
  (let [impl (get-repository repo)]
    (reset! current-repository impl)
    (repo-name impl)))

(defn current-repo
  "Get the current repository implementation."
  []
  @current-repository)

(defn list-repositories
  "List all registered repository keywords."
  []
  (keys @repository-registry))

;; =============================================================================
;; Memory Repository
;; =============================================================================

(defrecord MemoryRepository [storage]
  IGraphRepository

  (repo-name [_] :memory)

  (save-graph [_ id graph]
    (swap! storage assoc id graph)
    id)

  (load-graph [_ id]
    (get @storage id))

  (graph-exists? [_ id]
    (contains? @storage id))

  (list-graphs [_]
    (keys @storage))

  (delete-graph [_ id]
    (if (contains? @storage id)
      (do (swap! storage dissoc id) true)
      false)))

(defn memory-repository
  "Create an in-memory repository."
  []
  (->MemoryRepository (atom {})))

;; =============================================================================
;; File Repository
;; =============================================================================

(defrecord FileRepository [base-path]
  IGraphRepository

  (repo-name [_] :file)

  (save-graph [_ id graph]
    (let [file-path (str base-path "/" (name id) ".edn")
          ;; Convert graph to serializable form (remove functions)
          serializable (-> graph
                           (update :segments
                                   (fn [segs]
                                     (into {}
                                           (map (fn [[k v]]
                                                  [k (dissoc v :construct-fn)])
                                                segs)))))]
      (clojure.java.io/make-parents file-path)
      (spit file-path (pr-str serializable))
      id))

  (load-graph [_ id]
    (let [file-path (str base-path "/" (name id) ".edn")]
      (when (.exists (clojure.java.io/file file-path))
        (read-string (slurp file-path)))))

  (graph-exists? [_ id]
    (let [file-path (str base-path "/" (name id) ".edn")]
      (.exists (clojure.java.io/file file-path))))

  (list-graphs [_]
    (when (.exists (clojure.java.io/file base-path))
      (->> (file-seq (clojure.java.io/file base-path))
           (filter #(.isFile %))
           (filter #(.endsWith (.getName %) ".edn"))
           (map #(keyword (clojure.string/replace (.getName %) #"\.edn$" ""))))))

  (delete-graph [_ id]
    (let [file-path (str base-path "/" (name id) ".edn")
          file (clojure.java.io/file file-path)]
      (if (.exists file)
        (do (.delete file) true)
        false))))

(defn file-repository
  "Create a file-based repository.
   
   Options:
   - :base-path - Directory to store files (default: \".desargues/graphs\")"
  [& {:keys [base-path] :or {base-path ".desargues/graphs"}}]
  (->FileRepository base-path))

;; =============================================================================
;; High-Level API
;; =============================================================================

(defn save!
  "Save a graph to the current repository.
   
   Usage:
   (save! :my-project my-scene)"
  [id graph]
  (if-let [repo (current-repo)]
    (save-graph repo id graph)
    (throw (ex-info "No repository configured. Use (set-repository! :memory) first."
                    {}))))

(defn load!
  "Load a graph from the current repository.
   
   Usage:
   (load! :my-project)"
  [id]
  (if-let [repo (current-repo)]
    (load-graph repo id)
    (throw (ex-info "No repository configured. Use (set-repository! :memory) first."
                    {}))))

(defn exists?
  "Check if a graph exists in the current repository."
  [id]
  (when-let [repo (current-repo)]
    (graph-exists? repo id)))

(defn list-all
  "List all graphs in the current repository."
  []
  (when-let [repo (current-repo)]
    (list-graphs repo)))

(defn delete!
  "Delete a graph from the current repository."
  [id]
  (if-let [repo (current-repo)]
    (delete-graph repo id)
    (throw (ex-info "No repository configured." {}))))

;; =============================================================================
;; Initialize Default Repository
;; =============================================================================

;; Register built-in repositories
(register-repository! :memory (memory-repository))
(register-repository! :file (file-repository))

;; Set memory as default
(set-repository! :memory)
