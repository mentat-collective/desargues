(ns desargues.lesson.json-test
  "desargues.lesson.json: every EDN kind round-trips (lists stay lists), the
   compact encoding of each kind matches a golden string, and, when
   emmy.viewer.export (emmy-viewers, :notebooks alias) is on the classpath,
   the goldens and a whole fragment document are byte-identical to what
   emmy-viewers writes."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [desargues.lesson.json :as lj]))

(defn- compact
  "Compact JSON text of J(x), written the way emmy.viewer.export writes."
  [x]
  (json/write-str (lj/->json-data x) :escape-slash false))

(def goldens
  "[value golden-compact-json]: one row per kind of the encoding table."
  [[nil "null"]
   [true "true"]
   [false "false"]
   ["s" "\"s\""]
   ["~x" "\"~x\""]
   ["~kw" "\"~kw\""]
   [3 "3"]
   [2.5 "2.5"]
   [1.0 "1.0"]
   [##Inf "{\"~double\":\"Infinity\"}"]
   [##-Inf "{\"~double\":\"-Infinity\"}"]
   [16/3 "{\"~ratio\":\"16/3\"}"]
   [3N "{\"~bigint\":\"3\"}"]
   [1.5M "{\"~bigdec\":\"1.5\"}"]
   [\a "{\"~char\":\"a\"}"]
   [:k "{\"~kw\":\"k\"}"]
   [:ns/k "{\"~kw\":\"ns/k\"}"]
   ['s "{\"~sym\":\"s\"}"]
   ['ns/s "{\"~sym\":\"ns/s\"}"]
   [[1 :a] "[1,{\"~kw\":\"a\"}]"]
   [[] "[]"]
   ['(f x) "{\"~list\":[{\"~sym\":\"f\"},{\"~sym\":\"x\"}]}"]
   [() "{\"~list\":[]}"]
   [{:b 2 :a 1} "{\"~map\":[[{\"~kw\":\"a\"},1],[{\"~kw\":\"b\"},2]]}"]
   [{"k" [1] 1 nil} "{\"~map\":[[1,null],[\"k\",[1]]]}"]
   [#{:a 1 "b"} "{\"~set\":[1,\"b\",{\"~kw\":\"a\"}]}"]
   [{[1] #{}} "{\"~map\":[[[1],{\"~set\":[]}]]}"]])

(deftest every-kind-round-trips
  (doseq [[x _] goldens]
    (is (= x (lj/<-json (lj/->json x))) (pr-str x)))
  (let [nan (lj/<-json (lj/->json ##NaN))]
    (is (and (double? nan) (Double/isNaN nan))))
  (testing "a nested value with every kind; list vs vector kept"
    (let [x    {:k :ns/kw 'sym 'a/b "~s" "~tilde" \c [\d '(f x) [] ()]
                :n [1 2.5 3N 16/3 1.5M nil true] :set #{1 :a "b"} {1 2} {[1] #{}}}
          back (lj/<-json (lj/->json x))]
      (is (= x back))
      (is (seq? (get-in back [\c 1])) "lists stay lists")
      (is (seq? (get-in back [\c 3])))
      (is (vector? (get-in back [\c 2])) "vectors stay vectors")
      (is (= #{} (get-in back [{1 2} [1]])) "maps with non-string keys")
      (is (double? (get-in back [:n 1])))
      (is (instance? clojure.lang.BigInt (get-in back [:n 2])))
      (is (ratio? (get-in back [:n 3])))
      (is (decimal? (get-in back [:n 4])))))
  (testing "1.0 reads back a double, 1 a long"
    (is (double? (lj/<-json (lj/->json 1.0))))
    (is (int? (lj/<-json (lj/->json 1))))))

(deftest golden-strings
  (doseq [[x golden] goldens]
    (is (= golden (compact x)) (pr-str x)))
  (is (= "{\"~double\":\"NaN\"}" (compact ##NaN))))

(deftest canonical-order-is-hash-order-independent
  (let [a (into {} (map (fn [i] [(keyword (str "k" i)) i])) (range 40))
        b (into {} (map (fn [i] [(keyword (str "k" i)) i])) (reverse (range 40)))]
    (is (= (lj/->json a) (lj/->json b)))))

(deftest refuses-non-data
  (doseq [x [(atom 1) inc #"re" (java.util.Date. 0)]]
    (is (thrown? clojure.lang.ExceptionInfo (lj/->json x)) (pr-str x))))

(deftest decoder-rejects-untagged-objects
  (is (thrown? clojure.lang.ExceptionInfo (lj/<-json "{\"a\":1}")))
  (is (thrown? clojure.lang.ExceptionInfo (lj/<-json "{\"~kw\":\"a\",\"~sym\":\"b\"}")))
  (is (thrown? clojure.lang.ExceptionInfo (lj/<-json "{\"~nope\":1}"))))

(deftest lesson-document
  (let [l {:lesson/id :x :lesson/slides [{:slide/title "~t" :slide/blocks ()}]}
        s (lj/lesson->json l)
        doc (json/read-str s)]
    (is (= ["format" "version" "lesson"] (vec (keys (json/read-str s :key-fn identity)))))
    (is (= "desargues/lesson" (get doc "format")))
    (is (= 1 (get doc "version")))
    (is (= l (lj/json->lesson s)))
    (is (seq? (get-in (lj/json->lesson s) [:lesson/slides 0 :slide/blocks])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (lj/json->lesson (lj/->document "emmy-viewers/fragment" "lesson" l))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (lj/json->lesson (str "{\"format\":\"desargues/lesson\",\"version\":2,\"lesson\":null}"))))))

;; ----------------------------------------------------------------------------
;; Cross-check against emmy.viewer.export (only with emmy-viewers on the
;; classpath: clojure -M:test:notebooks)
;; ----------------------------------------------------------------------------

(defn- emmy-export
  "{:fragment f :fragment->json f :json->fragment f} from emmy-viewers, or nil."
  []
  (try
    {:fragment       (requiring-resolve 'emmy.viewer/fragment)
     :fragment->json (requiring-resolve 'emmy.viewer.export/fragment->json)
     :json->fragment (requiring-resolve 'emmy.viewer.export/json->fragment)}
    (catch Throwable _ nil)))

(defn- expected-document
  "The fragment JSON document emmy.viewer.export writes for fragment `frag`,
   built with desargues.lesson.json alone: its metadata (reader :line/:column
   included) with the default viewer named by its registered symbol."
  [frag]
  (json/write-str
   (array-map "format" "emmy-viewers/fragment"
              "version" lj/version
              "form" (lj/->json-data frag)
              "meta" (lj/->json-data (assoc (meta frag) :nextjournal.clerk/viewer
                                            'emmy.viewer/reagent-viewer)))
   :escape-slash false))

(deftest matches-emmy-viewers-export
  (if-let [{:keys [fragment fragment->json json->fragment]} (emmy-export)]
    (let [frag-json (fn [form] (fragment->json (fragment form)))]
      (testing "every golden, as the form of a fragment [x]"
        (doseq [[x golden] (conj goldens [##NaN "{\"~double\":\"NaN\"}"])
                :let [theirs (get (json/read-str (frag-json [x])) "form")]]
          (is (= (str "[" golden "]") (json/write-str theirs :escape-slash false)) (pr-str x))))
      (testing "a whole fragment document is byte-identical"
        (doseq [form ['[:div {:style {:color "red"}} (f x) #{1 :a} 16/3 \c "~s"]
                      '(reagent.core/with-let [a 1] [:p {[1] 2} 3N 1.5M ##Inf])]
                :let [frag (fragment form)]]
          (is (= (fragment->json frag) (expected-document frag)))))
      (testing "each side decodes the other"
        (let [form '[:div (f x) {:a #{1 "b"}} \c 3N 1/2 ()]
              ours (expected-document (fragment form))]
          (is (= form (json->fragment ours)) "emmy-viewers reads ours")
          (is (= form (lj/<-json-data (get (json/read-str (frag-json form)) "form")))
              "we read emmy-viewers'"))))
    (do (println "\n*** SKIPPED desargues.lesson.json-test/matches-emmy-viewers-export:"
                 "emmy.viewer.export is not on the classpath; run clojure -M:test:notebooks ***\n")
        (is true "skipped: emmy-viewers absent"))))
