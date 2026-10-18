(ns desargues.lesson.export-test
  "Lesson export: building a tiny lesson (no media) twice yields identical
   manifests, verify! proves it reproducible and catches tampering (files,
   extra files, manifest entries), generated names are safe and unique,
   titles cannot inject code, media is embedded, and the JSON encoding
   round-trips."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [desargues.lesson.export :as ex]
            [desargues.lesson.schema :as ls]
            [desargues.lesson.tex :as tex]
            [desargues.scene.serialize :as ser]
            [emmy.env :as e])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util Base64)))

(def lesson-path "test/desargues/lesson/tiny_lesson.edn")

(defn- tmp-dir []
  (str (Files/createTempDirectory "lesson-export-test" (make-array FileAttribute 0))))

(defn- read-all [s] (read-string (str "[" s "]")))

(deftest the-tiny-lesson-is-valid
  (is (ls/valid? (ex/read-lesson lesson-path))))

(deftest build-twice-identical
  (let [a (tmp-dir) b (tmp-dir)
        ma (ex/build! lesson-path {:out a})
        mb (ex/build! (ex/read-lesson lesson-path) {:out b :source lesson-path})]
    (is (= ma mb))
    (is (= (slurp (io/file a "manifest.edn")) (slurp (io/file b "manifest.edn")))
        "manifest.edn is byte-identical")
    (is (= #{"lesson.edn" "lesson.json" "notebook.clj"} (set (keys (:files ma)))))
    (is (= [] (:skipped ma)))
    (is (= :tiny-area (:lesson/id ma)))
    (is (= (str "bb lesson:build " lesson-path) (:generated-by ma)))
    (testing "deps are pinned from deps.edn"
      (is (= {:mvn/version "2.5.1"} (get-in ma [:deps 'org.clojure/data.json])))
      (is (string? (get-in ma [:deps 'io.github.mentat-collective/emmy :git/sha]))))
    (testing "no absolute paths or timestamps leak into any file"
      (doseq [f (keys (:files ma)) :let [s (slurp (io/file a f))]]
        (is (not (str/includes? s a)) f)
        (is (not (str/includes? s (System/getProperty "user.dir"))) f)
        (is (not (re-find #"\d{4}-\d{2}-\d{2}T\d{2}:" s)) f)))))

(deftest verify-reproducible-and-tamper
  (let [d (tmp-dir)]
    (ex/build! lesson-path {:out d})
    (is (= {:reproducible? true :mismatches []} (ex/verify! d)))
    (spit (io/file d "notebook.clj") ";; tampered\n" :append true)
    (let [{:keys [reproducible? mismatches]} (ex/verify! d)]
      (is (false? reproducible?))
      (is (= ["notebook.clj"] (map :file mismatches)))
      (is (= (:expected (first mismatches)) (:actual (first mismatches)))
          "the rebuild still matches the manifest; the file on disk does not")
      (is (not= (:expected (first mismatches)) (:on-disk (first mismatches)))))))

(defn- rewrite-manifest! [d f]
  (spit (io/file d "manifest.edn") (ser/->canonical-edn (f (ex/read-manifest d)))))

(deftest verify-compares-manifest-entries
  (doseq [[k f] [[:deps #(assoc-in % [:deps 'org.clojure/data.json] {:mvn/version "0.0.1"})]
                 [:notebook-deps #(update % :notebook-deps dissoc 'org.mentat/clerk-utils)]
                 [:skipped #(assoc % :skipped [{:block :ghost :format :png :from :x :reason :disabled}])]]]
    (testing (str k)
      (let [d (tmp-dir)]
        (ex/build! lesson-path {:out d})
        (rewrite-manifest! d f)
        (let [{:keys [reproducible? mismatches]} (ex/verify! d)]
          (is (false? reproducible?))
          (is (= [k] (map :key mismatches)))
          (is (= (get (ex/read-manifest d) k) (:expected (first mismatches))))
          (is (not= (:expected (first mismatches)) (:actual (first mismatches)))))))))

(deftest verify-reports-extra-files
  (let [d (tmp-dir)]
    (ex/build! lesson-path {:out d})
    (spit (io/file d "stray.txt") "x")
    (io/make-parents (io/file d "media" "old.png"))
    (spit (io/file d "media" "old.png") "y")
    (let [{:keys [reproducible? mismatches]} (ex/verify! d)]
      (is (false? reproducible?))
      (is (= [{:file "media/old.png" :problem :not-in-manifest}
              {:file "stray.txt" :problem :not-in-manifest}]
             (map #(select-keys % [:file :problem]) mismatches))))))

(deftest build-removes-stale-files
  (let [d (tmp-dir)]
    (ex/build! lesson-path {:out d})
    (spit (io/file d "stray.txt") "x")
    (io/make-parents (io/file d "media" "old.png"))
    (spit (io/file d "media" "old.png") "y")
    (ex/build! lesson-path {:out d})
    (is (not (.exists (io/file d "stray.txt"))))
    (is (not (.exists (io/file d "media"))) "an emptied directory is removed too")
    (is (= {:reproducible? true :mismatches []} (ex/verify! d)))))

(deftest build-refuses-unsafe-out-dirs
  (let [cwd    (System/getProperty "user.dir")
        refuse (fn [out]
                 (try (ex/build! lesson-path {:out out}) nil
                      (catch clojure.lang.ExceptionInfo t (:reason (ex-data t)))))]
    (is (= :lesson/unsafe-out-dir (refuse ".")) "the working directory (repo root)")
    (is (= :lesson/unsafe-out-dir (refuse cwd)))
    (is (= :lesson/unsafe-out-dir (refuse "..")) "an ancestor of it")
    (is (= :lesson/unsafe-out-dir (refuse (str (.getParentFile (io/file cwd))))))
    (let [d (tmp-dir)]
      (spit (io/file d "precious.txt") "keep me")
      (is (= :lesson/unsafe-out-dir (refuse d)) "a non-empty dir that is not a bundle")
      (is (= "keep me" (slurp (io/file d "precious.txt")))))
    (let [d (tmp-dir)]
      (.mkdirs (io/file d ".git"))
      (is (= :lesson/unsafe-out-dir (refuse d)) "a git work tree root"))))

(deftest json-round-trips
  (let [d (tmp-dir)
        l (ex/read-lesson lesson-path)]
    (ex/build! l {:out d})
    (is (= l (ex/<-json (slurp (io/file d "lesson.json")))))
    (testing "the convention covers every EDN kind, and list vs vector"
      (let [x {:k :ns/kw 'sym 'a/b "~s" "~tilde" \c [\d '(f x) [] ()]
               :n [1 2.5 3N 16/3 1.5M nil true] :set #{1 :a "b"} {1 2} {[1] #{}}}
            back (ex/<-json (ex/->json x))]
        (is (= x back))
        (is (seq? (get-in back [\c 1])) "lists stay lists")
        (is (seq? (get-in back [\c 3])))
        (is (vector? (get-in back [\c 2])) "vectors stay vectors")))))

(deftest notebook-shape
  (let [s (ex/notebook-source (ex/read-lesson lesson-path))]
    (is (str/includes? s "(ns lesson.tiny-area"))
    (is (= 3 (count (re-seq #"(?m)^;; ## " s))) "one section per slide")
    (is (str/includes? s "(clerk/tex (tex/->TeX '(integrate (expt x 2) x 0 2)))"))
    (is (str/includes? s "[desargues.lesson.tex :as tex]"))
    (is (str/includes? s "(calc-emmy/figure->mafs block-area-fig)"))
    (is (str/includes? s "(scene-image block-dot)"))
    (is (not (str/includes? s "TODO(lesson-export)")) "constructions have a render path")
    (is (str/includes? s "    mafs.core/Mafs\n") "the construction is drawn as a Mafs figure")
    (is (str/includes? s "[mafs.core/Point {:color \"#F0AC5F\", :x 1.0, :y 0.0}]") "M, the midpoint of AB")
    (is (vector? (read-all s)) "the notebook reads as Clojure")))

(deftest integral-tex
  (testing "definite and indefinite integrals typeset with the integral sign"
    (is (= "\\int_{0}^{1} {x}^{2} \\, dx" (tex/->TeX '(integral (expt x 2) x 0 1))))
    (is (= "\\int_{0}^{2} {x}^{2} \\, dx" (tex/->TeX '(integrate (expt x 2) x 0 2))))
    (is (= "\\int {x}^{2} \\, dx" (tex/->TeX '(integral (expt x 2) x))))
    (is (= "\\int_{0}^{1} \\left(x + 1\\right) \\, dx" (tex/->TeX '(integral (+ x 1) x 0 1)))
        "a sum integrand is parenthesized")
    (is (str/includes? (tex/->TeX '(= (integral (expt x 2) x 0 1) (/ 1 3))) "\\int_{0}^{1}"))
    (is (not (str/includes? (tex/->TeX '(integral (expt x 2) x 0 1)) "mathsf"))))
  (testing "everything else is Emmy's ->TeX unchanged"
    (is (= (e/->TeX '(+ (expt x 2) (sin y))) (tex/->TeX '(+ (expt x 2) (sin y)))))
    (is (= "\\begin{equation}\nx\n\\end{equation}" (tex/->TeX 'x :equation true)))))

;; ----------------------------------------------------------------------------
;; Generated names (finding 2)

(def ^:private dot-scene
  {:scene "dot"
   :nodes {1 {:node :dot :id 1 :opts {:color :gold} :at [0 0]}}
   :steps [{:step :hold :seconds 1}]
   :node-count 1})

(defn- lesson-of [blocks]
  {:lesson/id :names :lesson/title "names" :lesson/version "1"
   :lesson/slides [{:slide/id :s :slide/title "s" :slide/blocks blocks}]})

(defn- prose [id] {:block/id id :block/kind :prose :md "x"})

(defn- collision [blocks]
  (try (ex/block-names (lesson-of blocks)) nil
       (catch clojure.lang.ExceptionInfo t (ex-data t))))

(deftest generated-names-are-unique
  (testing "lossy mappings that collide are refused, naming both ids"
    (doseq [[a b] [[:fig/a :fig.a] [:Area :area] [:a-b :a_b] [:x/y-z :x.y/z]]]
      (let [d (collision [(prose a) (prose b)])]
        (is (= :lesson/name-collision (:reason d)) (str a " vs " b))
        (is (= [a b] (:ids d)))))
    (let [msg (try (ex/notebook-source (lesson-of [(prose :fig/a) (prose :fig.a)])) nil
                   (catch clojure.lang.ExceptionInfo t (ex-message t)))]
      (is (str/includes? msg ":fig/a"))
      (is (str/includes? msg ":fig.a")))
    (testing "and build! refuses before writing anything"
      (let [d (tmp-dir)]
        (is (thrown? clojure.lang.ExceptionInfo
                     (ex/build! (lesson-of [(prose :Area) (prose :area)]) {:out d})))
        (is (empty? (.list (io/file d)))))))
  (testing "media file names are unique too (they share the generated name)"
    (let [d (collision [{:block/id :dot :block/kind :scene :scene dot-scene}
                        {:block/id :Pic :block/kind :media :from :dot :format :png}
                        {:block/id :pic :block/kind :media :from :dot :format :gif}])]
      (is (= [:Pic :pic] (:ids d)))))
  (testing "distinct ids give distinct vars and media paths"
    (let [n (ex/block-names (lesson-of [{:block/id :dot :block/kind :scene :scene dot-scene}
                                        {:block/id :dot-png :block/kind :media :from :dot :format :png}
                                        {:block/id :dot-gif :block/kind :media :from :dot :format :gif}]))]
      (is (= 3 (count (set (map :var (vals n))))))
      (is (= #{"media/dot-png.png" "media/dot-gif.gif"} (set (keep :media (vals n))))))))

(deftest generated-names-are-safe
  (let [s (ex/notebook-source
           (lesson-of [{:block/id :scene-image :block/kind :scene :scene dot-scene}
                       {:block/id :1st :block/kind :scene :scene dot-scene}
                       {:block/id :str :block/kind :construction
                        :construction {:id :c :kind :construction :points [{:id :A :at [0 0]}]}}]))
        forms (read-all s)
        defs  (keep #(when (and (seq? %) (= 'def (first %))) (second %)) forms)
        fns   (keep #(when (and (seq? %) (= 'defn (first %))) (second %)) forms)]
    (is (vector? forms) "a block id starting with a digit still reads")
    (is (= '[block-scene-image block-1st block-str] defs))
    (is (= '[scene-image] fns) "the helper is not shadowed by a block named :scene-image")
    (is (str/includes? s "(scene-image block-scene-image)"))
    (is (empty? (filter (set fns) defs)))))

;; ----------------------------------------------------------------------------
;; Comment injection (finding 3)

(deftest titles-cannot-inject-code
  (let [evil  "Area\n(System/exit 0)"
        l     {:lesson/id :inject :lesson/title evil :lesson/version "1\r(System/exit 0)"
               :lesson/slides [{:slide/id :s :slide/title (str "s" "\u2028" "(System/exit 0)")
                                :slide/blocks [(assoc (prose :p) :md "line\n(System/exit 0)")]}]}
        s     (ex/notebook-source l)
        forms (read-all s)]
    (is (not-any? #(and (seq? %) (= 'System/exit (first %))) forms)
        "no (System/exit 0) form reaches the reader")
    (is (not (str/includes? (pr-str forms) "System/exit")))
    (is (str/includes? s ";; # Area (System/exit 0)\n") "the title stays on its comment line")
    (is (str/includes? s ";; Lesson :inject, version 1 (System/exit 0).\n"))
    (is (str/includes? s ";; ## s (System/exit 0)\n"))
    (is (str/includes? s ";; line\n;; (System/exit 0)") "prose: one ;; line per line")
    (doseq [line (str/split-lines s)
            :when (str/includes? line "System/exit")]
      (is (str/starts-with? line ";;") line))))

;; ----------------------------------------------------------------------------
;; Media embedded as data URIs (findings 1 and 6)

(def ^:private media-lesson
  (lesson-of [{:block/id :dot :block/kind :scene :scene dot-scene}
              {:block/id :dot-png :block/kind :media :from :dot :format :png}
              {:block/id :dot-gif :block/kind :media :from :dot :format :gif}]))

(defn- b64 [^bytes bs] (.encodeToString (Base64/getEncoder) bs))

(deftest notebook-embeds-media
  (let [png (byte-array (map unchecked-byte (range 200)))
        gif (.getBytes "GIF89a-fake" "UTF-8")
        s   (ex/notebook-source media-lesson #{} {:dot-png png :dot-gif gif})]
    (testing "media is a data URI in the notebook: no file is read at run time"
      (is (str/includes? s "\"data:image/png;base64,\""))
      (is (str/includes? s (b64 png)))
      (is (str/includes? s "\"data:image/gif;base64,\""))
      (is (str/includes? s (b64 gif)))
      (is (not (str/includes? s "*file*")))
      (is (not (str/includes? s "bundle-dir")))
      (is (not (str/includes? s "io/file"))))
    (testing "the data URI evaluates to the full bytes"
      (let [src (some #(when (and (seq? %) (= 'clerk/html (first %))) (get-in (vec %) [1 1 :src]))
                      (read-all s))]
        (is (= (str "data:image/png;base64," (b64 png)) (eval src)))))
    (testing "different media bytes give a different form, so Clerk's form-hash cache cannot go stale"
      (is (not= s (ex/notebook-source media-lesson #{} {:dot-png (byte-array [1 2 3]) :dot-gif gif}))))
    (testing "deterministic"
      (is (= s (ex/notebook-source media-lesson #{} {:dot-png png :dot-gif gif}))))
    (testing "large media is split into string literals below the class-file constant limit"
      (let [big (byte-array 200000 (byte 7))
            s   (ex/notebook-source media-lesson #{:dot-gif} {:dot-png big})
            lits (filter string? (tree-seq coll? seq (read-all s)))]
        (is (every? #(< (count %) 65535) lits))
        (is (= (str "data:image/png;base64," (b64 big))
               (eval (some #(when (and (seq? %) (= 'clerk/html (first %))) (get-in (vec %) [1 1 :src]))
                           (read-all s)))))))
    (testing "a skipped block says so; a media block with no bytes is an error"
      (is (str/includes? (ex/notebook-source media-lesson #{:dot-png :dot-gif}) "was SKIPPED"))
      (is (thrown? clojure.lang.ExceptionInfo (ex/notebook-source media-lesson #{:dot-gif} {}))))))

(deftest build-embeds-rendered-media
  (let [d (tmp-dir)
        m (ex/build! (lesson-of [{:block/id :dot :block/kind :scene :scene dot-scene}
                                 {:block/id :dot-png :block/kind :media :from :dot :format :png}])
                     {:out d})]
    (if (seq (:skipped m))
      (is (= [:dot-png] (map :block (:skipped m))) "headless rendering unavailable here")
      (let [png (Files/readAllBytes (.toPath (io/file d "media/dot-png.png")))]
        (is (contains? (:files m) "media/dot-png.png"))
        (is (str/includes? (slurp (io/file d "notebook.clj")) (b64 png))
            "the notebook embeds exactly the bytes of the media file")))))

;; ----------------------------------------------------------------------------
;; CLI (finding 10)

(deftest cli-usage
  (doseq [args [[] ["build"] ["verify"] ["bogus"] ["verify" "a" "b"]]]
    (let [err (java.io.StringWriter.)
          code (binding [*err* err] (apply ex/run args))]
      (is (= 2 code) (pr-str args))
      (is (str/includes? (str err) "usage:") (pr-str args)))))
