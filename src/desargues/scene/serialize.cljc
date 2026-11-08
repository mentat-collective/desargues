(ns desargues.scene.serialize
  "Canonical EDN serialization of recorded scene graphs (ROADMAP 5.4).

   The RecordingBackend (desargues.scene.data, via desargues.scene.record)
   yields a scene graph of plain data: {:scene :nodes :steps :node-count}.
   This namespace writes that graph as CANONICAL EDN text and reads it back:

     (->canonical-edn graph)        ;; => string, byte-identical for equal graphs
     (<-edn s)                      ;; => the graph again (= to the original)
     (save-scene-graph! graph path) ;; JVM: write the canonical text to a file
     (load-scene-graph! path)       ;; JVM: read it back

   Canonical means:
   - map keys and set elements are SORTED at every depth, under one total
     order over EDN values (see `compare-canonical`), so hash-map iteration
     order never leaks into the text;
   - formatting is a pure function of the data: a collection whose compact
     form fits in `line-width` columns stays on one line, otherwise it is
     broken one element (or one map entry) per line, indented two spaces per
     depth; the text ends with a single newline;
   - only values the EDN reader reads back to an EQUAL value are written:
     nil, booleans, finite numbers, strings, characters, keywords, symbols,
     lists/seqs, vectors, sets and maps. No reader tags are emitted. Anything
     else (fns, atoms, records, NaN/Infinity, host objects) is rejected with
     an ex-info naming the offending path, and the finished text is read back
     and compared, so a value that would not round-trip never reaches disk.

   The same scene therefore always yields the same bytes, which is what lets
   a lesson manifest hash it.

   Byte-identical canonical text is guaranteed on the JVM ONLY. Under CLJS
   the reader and printer differ: a 1-character string is also `char?` (so
   it ranks as a char in `compare-canonical`), and a whole double prints
   without its decimal point (1.0 prints as 1). CLJS output reads back to an
   equal value, but its bytes and its order can differ from the JVM's. Every
   hashed artifact (lesson bundles, manifests) is written on the JVM."
  (:require [clojure.string :as str]
            #?(:clj  [clojure.edn :as edn]
               :cljs [cljs.reader :as edn])))

(def line-width
  "Columns a collection's compact form may take before it is broken across
   lines. Part of the format: changing it changes every file's bytes."
  100)

;; ============================================================================
;; Data validation
;; ============================================================================

(defn- finite-number? [x]
  #?(:clj  (and (number? x)
                (not (and (or (instance? Double x) (instance? Float x))
                          (or (Double/isNaN (double x)) (Double/isInfinite (double x))))))
     :cljs (and (number? x) (js/isFinite x))))

(defn- readable-name?
  "Does keyword/symbol `x` print as a token the EDN reader reads back to x?"
  [x]
  (let [s (pr-str x)]
    (and (not (re-find #"[\s,\";\(\)\[\]\{\}\\]" s))
         (= x (try (edn/read-string s) (catch #?(:clj Exception :cljs :default) _ ::unreadable))))))

(defn- reject! [path x why]
  (throw (ex-info (str "desargues.scene.serialize: value at path " (pr-str path)
                       " is not serializable scene data (" why "): "
                       (let [s (pr-str x)] (if (> (count s) 80) (str (subs s 0 77) "...") s)))
                  {:error ::not-data
                   :path  path
                   :type  (str (type x))
                   :reason why})))

(defn check-data!
  "Walk `x` and throw ex-info {:error ::not-data :path [...]} at the first value
   canonical EDN cannot represent. Returns x."
  ([x] (check-data! x []))
  ([x path]
   (cond
     (or (nil? x) (boolean? x) (string? x) (char? x)) x
     (number? x)  (if (finite-number? x) x (reject! path x "non-finite number"))
     (or (keyword? x) (symbol? x))
     (if (readable-name? x) x (reject! path x "name does not read back"))
     (record? x)  (reject! path x "record (would print with a reader tag)")
     (map? x)     (do (doseq [[k v] x]
                        (check-data! k (conj path ::key))
                        (check-data! v (conj path k)))
                      x)
     (or (vector? x) (set? x) (seq? x))
     (do (doall (map-indexed (fn [i v] (check-data! v (conj path i))) x)) x)
     (fn? x)      (reject! path x "function")
     :else        (reject! path x "not a plain EDN value"))))

;; ============================================================================
;; Total order over EDN values
;; ============================================================================

(defn- rank [x]
  (cond (nil? x) 0 (boolean? x) 1 (number? x) 2 (char? x) 3 (string? x) 4
        (keyword? x) 5 (symbol? x) 6 (seq? x) 7 (vector? x) 8 (set? x) 9
        (map? x) 10 :else 11))

(declare compact)

(defn compare-canonical
  "Total order over EDN values: by kind (nil < boolean < number < char <
   string < keyword < symbol < list < vector < set < map), then by value
   within a kind; collections, and numbers that compare equal (1 vs 1.0), by
   their canonical text."
  [a b]
  (let [ra (rank a) rb (rank b)]
    (if (not= ra rb)
      (compare ra rb)
      (let [c (case ra
                (1 2 3 4 5 6) (compare a b)
                0)]
        (if (zero? c)
          (compare (compact a) (compact b))
          c)))))

(defn- sorted-entries [m] (sort-by key compare-canonical (seq m)))

;; ============================================================================
;; Printing
;; ============================================================================

(defn- atom-str
  "Scalar text, independent of the caller's print-control bindings."
  [x]
  (binding [*print-length* nil *print-level* nil *print-meta* false
            #?@(:clj [*print-dup* false])]
    (pr-str x)))

(defn compact
  "Single-line canonical text of (already checked) EDN value `x`."
  [x]
  (cond
    (map? x)    (str "{" (str/join ", " (map (fn [[k v]] (str (compact k) " " (compact v)))
                                             (sorted-entries x)))
                     "}")
    (set? x)    (str "#{" (str/join " " (map compact (sort compare-canonical x))) "}")
    (vector? x) (str "[" (str/join " " (map compact x)) "]")
    (seq? x)    (str "(" (str/join " " (map compact x)) ")")
    :else       (atom-str x)))

(defn- layout
  "Canonical text of `x` starting at column `indent`."
  [x indent]
  (let [flat (compact x)]
    (if (or (not (coll? x))
            (empty? x)
            (<= (+ indent (count flat)) line-width))
      flat
      (let [pad   (apply str (repeat (+ indent 2) \space))
            [open close items]
            (cond
              (map? x)    ["{" "}" (map (fn [[k v]]
                                          (let [ks (compact k)]
                                            (str ks " " (layout v (+ indent 2 (count ks) 1)))))
                                        (sorted-entries x))]
              (set? x)    ["#{" "}" (map #(layout % (+ indent 2)) (sort compare-canonical x))]
              (vector? x) ["[" "]" (map #(layout % (+ indent 2)) x)]
              :else       ["(" ")" (map #(layout % (+ indent 2)) x)])]
        (str open "\n" pad (str/join (str "\n" pad) items) close)))))

;; ============================================================================
;; Public API
;; ============================================================================

(defn <-edn
  "Read canonical (or any tag-free) EDN text `s` back into data. Reader tags
   are refused: no tag readers are installed and #inst/#uuid are not part of
   the format."
  [s]
  (let [refuse (fn [tag]
                 (fn [v]
                   (throw (ex-info (str "desargues.scene.serialize: reader tag #" tag
                                        " is not part of the scene format")
                                   {:error ::reader-tag :tag tag :value v}))))]
    ;; the built-in #inst/#uuid readers are shadowed explicitly; any other
    ;; tag falls to :default
    (edn/read-string {:readers {'inst (refuse 'inst) 'uuid (refuse 'uuid)}
                      :default (fn [tag v] ((refuse tag) v))}
                     s)))

(defn ->canonical-edn
  "The canonical EDN text of scene graph `graph` (any plain EDN value): sorted
   keys at every depth, deterministic line breaks, trailing newline. Throws
   ex-info {:error ::not-data :path ...} on a value that is not plain data, and
   {:error ::round-trip} should the text not read back to an equal value."
  [graph]
  (check-data! graph)
  (let [s (str (layout graph 0) "\n")]
    (when-not (= graph (<-edn s))
      (throw (ex-info "desargues.scene.serialize: canonical text does not read back to the same data"
                      {:error ::round-trip})))
    s))

#?(:clj
   (defn save-scene-graph!
     "Write `graph` to `path` as canonical EDN (UTF-8). Parent directories are
      created. Returns the path. The same graph always writes the same bytes."
     [graph path]
     (let [s (->canonical-edn graph)
           f (java.io.File. (str path))]
       (when-let [d (.getParentFile f)] (.mkdirs d))
       (spit f s :encoding "UTF-8")
       (str path))))

#?(:clj
   (defn load-scene-graph!
     "Read a scene graph written by `save-scene-graph!` from `path`."
     [path]
     (<-edn (slurp (str path) :encoding "UTF-8"))))
