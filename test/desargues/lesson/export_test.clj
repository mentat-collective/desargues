(ns desargues.lesson.export-test
  "Lesson export: building a tiny lesson (no media) twice yields identical
   manifests, verify! proves it reproducible and catches tampering, and the
   JSON encoding round-trips."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [desargues.lesson.export :as ex]
            [desargues.lesson.schema :as ls]
            [desargues.lesson.tex :as tex]
            [emmy.env :as e])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def lesson-path "test/desargues/lesson/tiny_lesson.edn")

(defn- tmp-dir []
  (str (Files/createTempDirectory "lesson-export-test" (make-array FileAttribute 0))))

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
    (is (str/includes? s "(calc-emmy/figure->mafs area-fig)"))
    (is (str/includes? s "TODO(lesson-export)"))
    (is (vector? (read-string (str "[" s "]"))) "the notebook reads as Clojure")))

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

(def ^:private media-lesson
  {:lesson/id :media-path :lesson/title "media path" :lesson/version "1"
   :lesson/slides
   [{:slide/id :s :slide/title "s"
     :slide/blocks [{:block/id :dot :block/kind :scene
                     :scene {:scene "dot"
                             :nodes {1 {:node :dot :id 1 :opts {:color :gold} :at [0 0]}}
                             :steps [{:step :hold :seconds 1}]
                             :node-count 1}}
                    {:block/id :dot-png :block/kind :media :from :dot :format :png}
                    {:block/id :dot-gif :block/kind :media :from :dot :format :gif}]}]})

(deftest notebook-media-resolves-relative-to-itself
  (let [s (ex/notebook-source media-lesson)]
    (testing "media cells go through the notebook's own directory, not the cwd"
      (is (str/includes? s "(def bundle-dir"))
      (is (str/includes? s "(io/file *file*)"))
      (is (str/includes? s "(clerk/image (media-url \"media/dot-png.png\"))"))
      (is (str/includes? s "(media-data-uri \"media/dot-gif.gif\" \"image/gif\")"))
      (is (not (re-find #"clerk/image \"" s)) "no bare cwd-relative clerk/image path"))
    (testing "no absolute path in the generated source"
      (is (not (str/includes? s (System/getProperty "user.dir"))))
      (is (not (str/includes? s (System/getProperty "user.home"))))
      (is (not (re-find #"\"/" s)) "no string literal starting at the filesystem root"))
    (testing "the bundle-dir form resolves to the directory of the file it is loaded from"
      (let [d    (io/file (tmp-dir) "nested")
            f    (io/file d "nb.clj")
            form (some #(when (and (seq? %) (= 'def (first %)) (= 'bundle-dir (second %))) %)
                       (read-string (str "[" s "]")))]
        (.mkdirs d)
        (spit f (str "(ns lesson-media-path-probe (:require [clojure.java.io :as io]))\n"
                     (pr-str form) "\n"
                     "(def media (io/file bundle-dir \"media/dot-png.png\"))\n"))
        (load-file (str f))
        (is (= (.getCanonicalFile d)
               (.getCanonicalFile ^java.io.File @(resolve 'lesson-media-path-probe/bundle-dir))))
        (is (= (.getCanonicalFile (io/file d "media" "dot-png.png"))
               (.getCanonicalFile ^java.io.File @(resolve 'lesson-media-path-probe/media))))
        (remove-ns 'lesson-media-path-probe)))))
