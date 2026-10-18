(ns desargues.lesson.json
  "PURE stratum: lossless JSON for EDN values, in the emmy-viewers fragment JSON
   encoding, v1: the encoding emmy.viewer.export (io.github.buddhilw/emmy-viewers
   0.5.0-fork.1) writes for a fragment's form and metadata. desargues core does
   not depend on emmy-viewers, so the codec is implemented here, tag for tag;
   desargues.lesson.json-test checks it byte for byte against emmy.viewer.export
   when that namespace is on the classpath (the :notebooks alias).

     (->json-data x)  ;; => J(x), JSON-ready data (Clojure maps and vectors)
     (<-json-data j)  ;; => x again
     (->json x) (<-json s)                ;; the same, as JSON text
     (->document fmt x) (<-document fmt s) ;; {\"format\" fmt \"version\" 1 ...}

   One decoder (`<-json-data`) reads both lesson.json and the \"form\"/\"meta\" of
   an emmy-viewers fragment document.

   ## The encoding, J(x)

   | EDN value                   | JSON                                |
   |-----------------------------|-------------------------------------|
   | nil, true, false            | null, true, false                   |
   | string                      | string, never escaped               |
   | integer (fits in 64 bits)   | number without a fraction: 3        |
   | double, finite              | number; integral doubles keep .0    |
   | ##Inf, ##-Inf, ##NaN        | {\"~double\": \"Infinity\"} etc.        |
   | ratio 1/2                   | {\"~ratio\": \"1/2\"}                  |
   | big integer 1N              | {\"~bigint\": \"1\"}                   |
   | big decimal 1.5M            | {\"~bigdec\": \"1.5\"}                 |
   | char \\a                     | {\"~char\": \"a\"}                     |
   | keyword :ns/k               | {\"~kw\": \"ns/k\"}                    |
   | symbol ns/s                 | {\"~sym\": \"ns/s\"}                   |
   | vector [a b]                | [J(a), J(b)]                        |
   | list or seq (a b)           | {\"~list\": [J(a), J(b)]}             |
   | map {k v}                   | {\"~map\": [[J(k), J(v)], ...]}       |
   | set #{a}                    | {\"~set\": [J(a), ...]}               |

   Every JSON object in the encoding has exactly one key, one of the tags
   above, so strings never need escaping and a decoder tells every case apart.
   Map entries and set members are in canonical order: by type rank (nil,
   boolean, number, string, keyword, symbol, other), then by printed form.
   Anything that is not plain data (records, functions, atoms, regexes ...)
   throws. The full table, as published by emmy-viewers:
   https://github.com/BuddhiLW/emmy-viewers/blob/main/DEVELOPING.md"
  (:require [clojure.string :as str]
            #?(:clj [clojure.data.json :as json])))

(def version
  "Version of the emmy-viewers fragment JSON encoding implemented here."
  1)

;; ============================================================================
;; Canonical data (same ordering as emmy.viewer.export)
;; ============================================================================

(defn- type-rank [x]
  (cond (nil? x) 0 (boolean? x) 1 (number? x) 2 (string? x) 3
        (keyword? x) 4 (symbol? x) 5 :else 6))

(defn- printed [x]
  (binding [*print-length*         nil
            *print-level*          nil
            *print-meta*           false
            *print-namespace-maps* false]
    (pr-str x)))

(defn- canonical-compare [a b]
  (let [c (compare (type-rank a) (type-rank b))]
    (if (zero? c) (compare (printed a) (printed b)) c)))

(defn- data-error [x]
  (ex-info (str "lesson JSON: not plain data: " (pr-str x)) {:value x :type (type x)}))

(defn- canonical
  "`x` with every map and set replaced by a sorted equivalent."
  [x]
  (cond (or (nil? x) (boolean? x) (string? x) (keyword? x) (symbol? x)
            (number? x) (char? x))
        x
        (record? x)    (throw (data-error x))
        (map-entry? x) [(canonical (key x)) (canonical (val x))]
        (vector? x)    (mapv canonical x)
        (map? x)       (into (sorted-map-by canonical-compare)
                             (map (fn [[k v]] [(canonical k) (canonical v)])) x)
        (set? x)       (into (sorted-set-by canonical-compare) (map canonical) x)
        (seq? x)       (apply list (map canonical x))
        :else          (throw (data-error x))))

;; ============================================================================
;; Encode
;; ============================================================================

(defn- name-of [x]
  (if-let [ns (namespace x)] (str ns "/" (name x)) (name x)))

(defn- tagged [tag v] (array-map tag v))

(defn- encode-number [x]
  #?(:clj
     (cond (ratio? x)                         (tagged "~ratio" (str x))
           (instance? clojure.lang.BigInt x)  (tagged "~bigint" (str x))
           (instance? java.math.BigInteger x) (tagged "~bigint" (str x))
           (decimal? x)                       (tagged "~bigdec" (str x))
           (and (double? x) (Double/isNaN x)) (tagged "~double" "NaN")
           (and (double? x) (Double/isInfinite x))
           (tagged "~double" (if (pos? x) "Infinity" "-Infinity"))
           (float? x)                         (double x)
           :else                              (long x))
     :cljs
     (cond (js/isNaN x)          (tagged "~double" "NaN")
           (not (js/isFinite x)) (tagged "~double" (if (pos? x) "Infinity" "-Infinity"))
           :else                 x)))

(defn- encode [x]
  (cond (or (nil? x) (boolean? x) (string? x)) x
        (number? x)  (encode-number x)
        (char? x)    (tagged "~char" (str x))
        (keyword? x) (tagged "~kw" (name-of x))
        (symbol? x)  (tagged "~sym" (name-of x))
        (vector? x)  (mapv encode x)
        (seq? x)     (tagged "~list" (mapv encode x))
        (map? x)     (tagged "~map" (mapv (fn [[k v]] [(encode k) (encode v)]) x))
        (set? x)     (tagged "~set" (mapv encode x))
        :else        (throw (data-error x))))

(defn ->json-data
  "J(x): the JSON-ready data of EDN `x` (Clojure maps with one string key, and
   vectors), in canonical order."
  [x]
  (encode (canonical x)))

;; ============================================================================
;; Decode
;; ============================================================================

(defn- parse-double-tag [s]
  (case s
    "NaN"       ##NaN
    "Infinity"  ##Inf
    "-Infinity" ##-Inf
    (throw (ex-info "lesson JSON: bad ~double value" {:value s}))))

(defn- decode-tag [tag v decode]
  (case tag
    "~kw"     (keyword v)
    "~sym"    (symbol v)
    "~char"   (first v)
    "~double" (parse-double-tag v)
    "~list"   (apply list (map decode v))
    "~set"    (into #{} (map decode) v)
    "~map"    (into {} (map (fn [[k x]] [(decode k) (decode x)])) v)
    "~ratio"  #?(:clj  (let [[n d] (str/split v #"/")]
                         (clojure.lang.Numbers/divide (bigint n) (bigint d)))
                 :cljs (let [[n d] (str/split v #"/")]
                         (/ (js/parseFloat n) (js/parseFloat d))))
    "~bigint" #?(:clj (bigint v) :cljs (js/parseInt v 10))
    "~bigdec" #?(:clj (bigdec v) :cljs (js/parseFloat v))
    (throw (ex-info (str "lesson JSON: unknown tag " tag) {:tag tag :value v}))))

(defn <-json-data
  "Inverse of `->json-data`, over JSON-read data (string keys, vectors for
   arrays). Reads lesson.json and emmy-viewers fragment JSON alike."
  [x]
  (cond (vector? x) (mapv <-json-data x)
        (map? x)    (do (when-not (= 1 (count x))
                          (throw (ex-info "lesson JSON: an encoded object has exactly one tag key"
                                          {:value x})))
                        (let [[tag v] (first x)]
                          (decode-tag tag v <-json-data)))
        :else       x))

;; ============================================================================
;; Text and documents
;; ============================================================================

(defn- write-text [data]
  #?(:clj  (str (json/write-str data :indent true :escape-slash false
                                :escape-unicode false :escape-js-separators false)
                "\n")
     :cljs (str (js/JSON.stringify (clj->js data) nil 2) "\n")))

(defn- read-text [s]
  #?(:clj  (json/read-str s)
     :cljs (js->clj (js/JSON.parse s))))

(defn ->json
  "Deterministic JSON text of J(x), indented, with a trailing newline."
  [x]
  (write-text (->json-data x)))

(defn <-json
  "EDN back from `->json` text."
  [s]
  (<-json-data (read-text s)))

(defn ->document
  "JSON text of the document {\"format\": `format-id`, \"version\": 1,
   `body-key`: J(x)}, mirroring the emmy-viewers fragment document
   ({\"format\": \"emmy-viewers/fragment\", \"version\": 1, \"form\": ..})."
  [format-id body-key x]
  (write-text (array-map "format" format-id "version" version body-key (->json-data x))))

(defn <-document
  "The decoded `body-key` of a `->document` text; throws unless its format is
   `format-id` and its version is 1."
  [format-id body-key s]
  (let [doc (read-text s)]
    (when-not (and (map? doc) (= format-id (get doc "format")))
      (throw (ex-info (str "Not a " format-id " JSON document") {:format (get doc "format")})))
    (when-not (= version (get doc "version"))
      (throw (ex-info (str "Unsupported " format-id " JSON version")
                      {:version (get doc "version") :supported version})))
    (<-json-data (get doc body-key))))

(def lesson-format
  "The \"format\" of lesson.json."
  "desargues/lesson")

(defn lesson->json
  "lesson.json text: {\"format\": \"desargues/lesson\", \"version\": 1, \"lesson\": J(lesson)}."
  [lesson]
  (->document lesson-format "lesson" lesson))

(defn json->lesson
  "The lesson back from `lesson->json` text."
  [s]
  (<-document lesson-format "lesson" s))
