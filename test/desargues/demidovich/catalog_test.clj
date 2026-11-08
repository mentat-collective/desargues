(ns desargues.demidovich.catalog-test
  "Pure catalog checks over a synthetic book: no OCR archive or output files."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [desargues.demidovich.catalog :as catalog]
            [desargues.demidovich.source :as src]
            [desargues.demidovich.tex :as tex]))

(def ^:private book-lines
  ["Sec. 1. Direct integration"
   "1031. \\(\\int 2x dx\\)"
   "1032*. \\(\\int x dx\\)"
   "1033. \\(\\int 1 dx\\)"
   "1034. \\(\\int \\unknown{x} dx\\)"
   "1035**. a) \\(\\int 1 dx\\); b) \\(\\int 2x dx\\)"
   "Chapter V"
   "1031. \\(x^2 + C\\). Note verification."
   "1032. \\(x\\)."
   "1034. \\(\\unknown{x}\\)."
   "1035. a) \\(x\\); b) \\(x^2\\)."])

(def ^:private layout
  {:problems {:from 1 :to 6} :answers {:from 8 :to 11} :numbers [1031 1035]})

(defn- records []
  (with-redefs [src/ch4 layout] (catalog/catalog book-lines)))

(deftest collect-source-records
  (let [problems (src/collect-problems book-lines layout)
        answers (src/collect-answers book-lines layout)]
    (is (= [1031 1032 1033 1034 1035] (mapv :number problems)))
    (is (= [nil :hint nil nil :solution] (mapv :marker problems)))
    (is (= {:number 1031 :marker nil :section 1 :section-title "Direct integration"
            :line 2 :text "\\(\\int 2x dx\\)"}
           (first problems)))
    (is (= [1031 1032 1034 1035] (vec (keys answers))))
    (is (= 8 (:line (answers 1031))))
    (is (= "\\(x^2 + C\\). Note verification." (:text (answers 1031)))))
  (testing "table headers take their statement from the next cell"
    (is (= "\\(\\int x dx\\)"
           (:text (first (src/collect-problems
                          ["<th>1031.</th>" "<td>\\(\\int x dx\\)</td>"]
                          {:problems {:from 1 :to 2} :numbers [1031 1031]})))))))

(deftest answer-sequence-repairs-ocr-numbers-not-references
  (let [answers (src/collect-answers
                 ["1031. \\(x\\)." "1034*. \\(x^2\\)."
                  "1033. \\(x^3\\). Note compare problem 1032. Later."]
                 {:answers {:from 1 :to 3} :numbers [1031 1034]})]
    (is (= [1031 1032 1033] (vec (keys answers))))
    (is (= {:number 1032 :ocr-number 1034 :marker :hint :line 2 :text "\\(x^2\\)."}
           (answers 1032)))
    (is (str/includes? (:text (answers 1033)) "problem 1032. Later."))))

(deftest split-and-clean-text
  (is (= ["x" "y^2"] (src/math-segments "Inline \\( x \\), display \\[ y^2 \\].")))
  (is (= [["a" "first;"] ["b" "second"]] (src/split-parts "a) first; b) second")))
  (is (nil? (src/split-parts "a) only one part")))
  (is (= {:tex "\\(x^2\\)" :note "Hint take a derivative."}
         (src/split-answer "\\(x^2\\). Hint take a derivative.")))
  (is (= "\\int 2x dx"
         (catalog/statement-integral-tex "Find \\(\\int 2x dx, \\quad x = 1/t\\).")))
  (is (nil? (catalog/statement-integral-tex "No integral: \\(x^2\\).")))
  (is (= "x^2 + C" (catalog/answer-tex "\\(x^2\\) + C, when x > 0."))))

(deftest tex-reader-produces-edn
  (is (= " \\frac{1}{2} " (tex/normalize "\\(\\dfrac{1}{2}\\)")))
  (is (= [[:num 2] [:var 'x] [:op \+] [:pi]] (tex/tokenize "2x+\\pi")))
  (doseq [[input expected]
          [["2x + \\frac{1}{3}" '(+ (* 2 x) (/ 1 3))]
           ["\\sin 2x \\cos x" '(* (sin (* 2 x)) (cos x))]
           ["e^{2x}" '(exp (* 2 x))]
           ["\\ln \\left|x\\right|" '(log (abs x))]
           ["\\sqrt[3]{x^2}" '(expt (expt x 2) (/ 1 3))]]]
    (is (= expected (tex/read-tex input)) input))
  (is (= {:integrand '(/ x (+ 1 (expt x 2))) :var 'x}
         (tex/read-integral "\\int \\frac{x dx}{1+x^2}")))
  (is (= {:integrand 'phi :var 'phi} (tex/read-integral "\\int \\varphi d\\varphi")))
  (is (= '(* x y) (tex/strip-ones '(* 1 x (* 1 y))))))

(deftest tex-failures-carry-reasons
  (doseq [[input reason] [["" :empty] ["\\unknown{x}" :unknown-command]
                          ["\\frac{x}" :unexpected-end]]]
    (is (= reason (try (tex/read-tex input)
                      (catch clojure.lang.ExceptionInfo e (:tex/reason (ex-data e)))))))
  (is (= :no-differential
         (try (tex/read-integral "\\int x^2")
              (catch clojure.lang.ExceptionInfo e (:tex/reason (ex-data e)))))))

(deftest evaluate-expressions-without-eval
  (is (= 25/2 (tex/evaluate '(+ (/ 1 2) (* a (expt x 2))) {'a 3 'x 2})))
  (is (= #{'a 'x} (tex/symbols-of '(+ (* a x) pi))))
  (is (= (+ 6 Math/PI) ((tex/->fn '(+ (* a x) pi) 'x {'a 2}) 3)))
  (is (= :unbound-symbol
         (try (tex/evaluate 'x {})
              (catch clojure.lang.ExceptionInfo e (:tex/reason (ex-data e)))))))

(deftest check-answer-derivatives
  (let [good (catalog/check-answer '(* 2 x) '(expt x 2) 'x)
        bad (catalog/check-answer '(* 2 x) 'x 'x)]
    (is (= {:answer-check :ok :points (count catalog/sample-xs) :agree (count catalog/sample-xs)} good))
    (is (= {:answer-check :mismatch :points (count catalog/sample-xs) :agree 0} bad)))
  (is (= :ok (:answer-check (catalog/check-answer '(* a t) '(/ (* a (expt t 2)) 2) 't)))))

(deftest catalog-keeps-every-problem-and-diagnostic
  (let [[good mismatch missing failed parts :as rs] (records)]
    (is (= [1031 1032 1033 1034 1035] (mapv :number rs)))
    (is (= [:ok :ok :partial :failed :ok] (mapv :parse-status rs)))
    (is (= [:ok :mismatch :unparsed :unparsed :ok] (mapv :answer-check rs)))
    (is (= '(* 2 x) (:integrand good)))
    (is (= '(+ (expt x 2) C) (:answer good)))
    (is (= "Note verification." (:answer-note good)))
    (is (= 'x (:answer mismatch)))
    (is (:answer-missing missing))
    (is (= [{:reason :no-answer}] (:parse-reasons missing)))
    (is (= :unknown-command (-> failed :parse-reasons first :reason)))
    (is (= ["a" "b"] (mapv :part (:parts parts))))
    (is (every? #(= :ok (:answer-check %)) (:parts parts)))))

(deftest promote-retains-the-literal-ocr-answer
  (let [r (catalog/promote (first (src/collect-problems book-lines layout))
                           {:line 8 :text "\\(x \\times x^2\\)." :ocr-number 1034})]
    (is (= :ok (:answer-check r)))
    (is (= 1034 (:answer-ocr-number r)))
    (is (= '(* x (expt x 2)) (:answer-literal r)))
    (is (= '(expt x 2) (:answer r)))
    (is (contains? r :ocr-repair))))

(deftest summarize-and-report
  (let [rs (records)
        summary (catalog/summary rs)
        report (catalog/report-md rs)]
    (is (= 5 (:problems summary)))
    (is (= [{:section 1 :title "Direct integration" :problems 5
             :parsed 3 :partial 1 :failed 1 :checked 2 :mismatches [1032]
             :unparsed 2 :ocr-repaired []}]
           (vec (:sections summary))))
    (is (= [["unknown-command \\unknown" 1]] (vec (:top-failures summary))))
    (is (str/includes? report "| 1 | Direct integration | 5 | 3 | 1 | 1 | 2 | 2 | 1032 |"))
    (is (str/includes? report "Problems without an answer: 1033"))
    (is (str/includes? report "`unknown-command \\unknown`"))))
