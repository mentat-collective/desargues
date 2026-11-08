(ns desargues.demidovich.catalog
  "The Demidovich problem catalog, CPPB:

    Collect   desargues.demidovich.source: book lines -> raw problem/answer text
    Promote   statement and answer TeX -> s-expressions (desargues.demidovich.tex),
              each item stamped with :parse-status
    Check     differentiate the answer with Emmy D and compare with the integrand
              at sample points: :answer-check :ok | :mismatch | :unparsed
    Boundary  `build!` reads the book and writes resources/demidovich/ch4.edn and
              doc/demidovich/ch4-catalog.md

  Every problem in the chapter yields a record; failures are data
  (:parse-status :failed with the reason), never dropped. The records are plain
  EDN: s-expressions over Emmy's generic operator names (see tex/ops)."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [desargues.demidovich.source :as src]
            [desargues.demidovich.tex :as tex]
            [emmy.env :as e]))

(def book-path
  (str (System/getProperty "user.home")
       "/Documents/feed/.ocr/olmocr-20261015/books/math/demidovich-mir-analysis.md"))

;; ---------------------------------------------------------------------------
;; Promote: statements

(defn- reason-of [^Throwable t]
  (let [d (ex-data t)]
    (cond-> {:reason (or (:tex/reason d) :exception)}
      (:command d) (assoc :command (:command d))
      (:tex/token d) (assoc :token (pr-str (:tex/token d)))
      (not (:tex/reason d)) (assoc :message (ex-message t)))))

(defn- attempt [f]
  (try {:value (f)} (catch Throwable t {:error (reason-of t)})))

(defn statement-integral-tex
  "The math segment of a statement that holds the integral, without the
  substitution or condition that may trail it (`, \\quad x = 1/t`)."
  [text]
  (when-let [seg (first (filter #(str/includes? % "\\int") (src/math-segments text)))]
    (-> seg
        (str/replace #"(?s),?\s*\\q?quad.*$" "")
        (str/replace #"(?s)\s*=\s*[^=]*$" (fn [m] (if (str/includes? m "\\int") m "")))
        str/trim)))

(defn- promote-statement [text]
  (if-let [t (statement-integral-tex text)]
    (let [{:keys [value error]} (attempt #(tex/read-integral t))]
      (if value
        {:statement-tex t :integrand (:integrand value) :var (:var value)}
        {:statement-tex t :statement-error error}))
    {:statement-tex (str/join " " (src/math-segments text))
     :statement-error {:reason :no-integral}}))

;; ---------------------------------------------------------------------------
;; Promote: answers

(defn answer-tex
  "Answer text (prose and \\( \\) math) -> one TeX string: delimiters dropped,
  domain remarks (when/if x > 0 ...) and footnote marks cut."
  [s]
  (-> s
      (str/replace #"\\\(|\\\)" " ")
      (str/replace #"\*\)" " ")
      (str/replace #"(?s)[,;]?\s*\b(when|if|for|where|provided)\b.*$" "")
      (str/replace #"\([^()]*[<>][^()]*\)\s*$" "")
      (str/replace #"\s+" " ")
      (str/replace #"[.,;\s]+$" "")
      str/trim))

(defn- times-variants
  "The OCR renders the book's line-break multiplication sign as `x \\times`
  or `\\times x` with a spurious x. Variants with that x removed, to try when
  the literal reading does not check."
  [t]
  (distinct
   (remove #(= % t)
           [(str/replace t #"\bx\s*\\times" " ")
            (str/replace t #"\\times\s*x\b" " ")
            (str/replace t #"\\times" " ")])))

;; ---------------------------------------------------------------------------
;; Check

(def sample-xs
  [0.137 0.291 0.433 0.58 0.719 0.862 1.17 1.43 1.88 2.31 2.76 3.4 4.15
   -0.173 -0.37 -0.61 -0.83 -1.27 -1.9 -2.6 -3.3])

(def param-values
  "Positive, distinct, ordered a > b > c ... so conditions like 0 < b < a hold."
  {'a 2.3 'b 1.4 'c 0.7 'm 1.7 'n 2.6 'p 1.9 'q 3.1 'k 1.3 'A 2.1 'B 0.9
   'alpha 1.15 'beta 0.65 'lambda 0.45 'mu 1.6 'T 2.9 'phi0 0.35 'h 1.25
   'r 1.8 's 1.05 'l 2.45 'g 1.55 'M 1.2 'N 0.8 'P 1.35 'Q 0.95 'R 2.2 'd 0.55
   'u 1.45 'w 0.85 'z 1.75 'y 0.6 'v 0.75 't 0.5 'x 0.4 'omega 1.3 'theta 0.5})

(defn- real-number [v]
  (when (and (number? v) (not (instance? clojure.lang.Ratio v)) )
    (let [d (double v)] (when (Double/isFinite d) d))))

(defn- real-val [v]
  (cond (instance? clojure.lang.Ratio v) (double v)
        (number? v) (real-number v)
        :else nil))

(defn- derivative-at [f x]
  (or (try (real-val ((e/D f) x)) (catch Throwable _ nil))
      ;; central difference where D cannot see through (abs at a kink, etc.)
      (let [h (* 1e-6 (max 1.0 (Math/abs (double x))))
            a (try (real-val (f (+ x h))) (catch Throwable _ nil))
            b (try (real-val (f (- x h))) (catch Throwable _ nil))]
        (when (and a b) (/ (- a b) (* 2 h))))))

(defn check-answer
  "Compare d/dvar answer with integrand at the sample points. Returns
  {:answer-check :ok|:mismatch :points n :agree k}."
  [integrand answer var]
  (let [syms (disj (into (tex/symbols-of integrand) (tex/symbols-of answer)) var)
        env (into {} (map (fn [s] [s (get param-values s 1.1)]) syms))
        f (tex/->fn integrand var env)
        F (tex/->fn answer var env)
        rows (keep (fn [x]
                     (let [fx (try (real-val (f x)) (catch Throwable _ nil))
                           ;; the answer must be real at x as well
                           Fx (try (real-val (F x)) (catch Throwable _ nil))
                           dF (when (and fx Fx) (derivative-at F x))]
                       (when (and fx dF)
                         (<= (Math/abs (- fx dF)) (* 1e-5 (max 1.0 (Math/abs fx)))))))
                   sample-xs)
        n (count rows) k (count (filter true? rows))]
    {:answer-check (if (and (>= k 3) (>= (* 2 k) n)) :ok :mismatch)
     :points n :agree k}))

(defn- promote-answer
  "Answer item text + promoted statement -> answer fields and the check.
  The literal reading comes first; when it does not check, the alternative
  readings are tried (greedy function arguments, `x \\times` OCR repair) and a
  checking one is kept beside the literal one, flagged :ocr-repair or
  :reading."
  [text {:keys [integrand var]}]
  (let [{:keys [tex note]} (src/split-answer text)
        t (answer-tex tex)
        base (cond-> {:answer-tex t} note (assoc :answer-note note))
        read (fn [s greedy?] (attempt #(binding [tex/*greedy-fn-args* greedy?] (tex/read-tex s))))
        {:keys [value error]} (read t false)
        ;; an answer written in x for a statement in another variable (OCR)
        var-for (fn [ans] (if (and var (not (contains? (tex/symbols-of ans) var))
                                   (contains? (tex/symbols-of ans) 'x))
                            'x var))
        candidates (concat [[t true :reading]]
                           (for [t' (times-variants t) g [false true]] [t' g :ocr-repair]))
        checking (fn []
                   (some (fn [[t' g kind]]
                           (let [{v' :value} (read t' g)]
                             (when (and v' (not= v' value))
                               (let [c' (check-answer integrand v' (var-for v'))]
                                 (when (= :ok (:answer-check c'))
                                   [kind {:answer-tex t' :greedy-fn-args g} v' c'])))))
                         candidates))]
    (cond
      (str/blank? t) (assoc base :answer-error {:reason :empty} :answer-check :unparsed)
      (nil? integrand) (if error
                         (assoc base :answer-error error :answer-check :unparsed)
                         (assoc base :answer value :answer-check :unparsed))
      :else
      (let [chk (when value (check-answer integrand value (var-for value)))]
        (if (= :ok (:answer-check chk))
          (merge base {:answer value} chk)
          (if-let [[kind how v' c'] (checking)]
            (merge base {:answer v' kind how} (when value {:answer-literal value}) c')
            (if value
              (merge base {:answer value} chk)
              (assoc base :answer-error error :answer-check :unparsed))))))))

;; ---------------------------------------------------------------------------
;; Problem records

(defn- item-status [{:keys [integrand answer answer-check]}]
  (cond (and integrand answer) :ok
        (or integrand answer) :partial
        :else :failed))

(defn- promote-item [label stmt-text ans-text]
  (let [s (promote-statement stmt-text)
        a (if ans-text
            (promote-answer ans-text s)
            {:answer-check :unparsed :answer-error {:reason :no-answer}})
        it (merge (when label {:part label}) s a)]
    (assoc it :parse-status (item-status it))))

(defn- combine-status [items]
  (let [ss (set (map :parse-status items))]
    (cond (= ss #{:ok}) :ok (= ss #{:failed}) :failed :else :partial)))

(defn- combine-check [items]
  (let [cs (set (map :answer-check items))]
    (cond (contains? cs :mismatch) :mismatch
          (= cs #{:ok}) :ok
          (contains? cs :ok) :ok
          :else :unparsed)))

(defn promote
  "raw problem + raw answer (or nil) -> catalog record."
  [{:keys [number marker section section-title line text]} answer]
  (let [sparts (src/split-parts text)
        aparts (some-> answer :text src/split-parts)
        items (if sparts
                (mapv (fn [[label t]]
                        (promote-item label t (or (some (fn [[l at]] (when (= l label) at)) aparts)
                                                  (when-not aparts (:text answer)))))
                      sparts)
                [(promote-item nil text (:text answer))])
        record (cond-> {:number number
                        :chapter 4
                        :section section
                        :section-title section-title
                        :marker marker
                        :source-line line
                        :statement-text text
                        :answer-text (:text answer)
                        :answer-line (:line answer)}
                 (:ocr-number answer) (assoc :answer-ocr-number (:ocr-number answer))
                 (nil? answer) (assoc :answer-missing true))
        status (combine-status items)
        check (combine-check items)
        errors (vec (distinct (keep #(or (:statement-error %) (:answer-error %)) items)))]
    (cond-> (assoc record :parse-status status :answer-check check)
      (seq errors) (assoc :parse-reasons errors)
      sparts (assoc :parts items)
      (not sparts) (merge (dissoc (first items) :parse-status :answer-check
                                  :statement-error :answer-error)))))

(defn catalog
  "Book lines -> vector of catalog records for Chapter IV, in book order."
  [lines]
  (let [ps (src/collect-problems lines src/ch4)
        as (src/collect-answers lines src/ch4)]
    (mapv #(promote % (get as (:number %))) ps)))

;; ---------------------------------------------------------------------------
;; Report

(defn- failure-forms
  "Every parse error in the catalog as a short form key, for the report."
  [records]
  (for [r records
        err (:parse-reasons r)
        :when (not (#{:no-answer :empty} (:reason err)))]
    (str (name (:reason err))
         (when-let [c (:command err)] (str " \\" c))
         (when (and (not (:command err)) (:token err)) (str " " (:token err))))))

(defn summary [records]
  {:problems (count records)
   :sections
   (for [[sec rs] (sort-by key (group-by :section records))]
     {:section sec
      :title (:section-title (first rs))
      :problems (count rs)
      :parsed (count (filter #(= :ok (:parse-status %)) rs))
      :partial (count (filter #(= :partial (:parse-status %)) rs))
      :failed (count (filter #(= :failed (:parse-status %)) rs))
      :checked (count (filter #(= :ok (:answer-check %)) rs))
      :mismatches (mapv :number (filter #(= :mismatch (:answer-check %)) rs))
      :unparsed (count (filter #(= :unparsed (:answer-check %)) rs))
      :ocr-repaired (mapv :number (filter #(or (:ocr-repair %) (some :ocr-repair (:parts %))) rs))})
   :top-failures (take 10 (sort-by (comp - val) (frequencies (failure-forms records))))})

(defn report-md [records]
  (let [{:keys [sections top-failures problems]} (summary records)
        tot (fn [k] (reduce + (map k sections)))]
    (str
     "# Demidovich, Chapter IV (Indefinite Integrals): problem catalog\n\n"
     "Generated by `desargues.demidovich.catalog/build!` from the olmOCR Markdown of\n"
     "Demidovich (Mir), *Problems in Mathematical Analysis*. Data:\n"
     "`resources/demidovich/ch4.edn`, one record per problem 1031-1500.\n\n"
     "- **parsed**: statement and answer both read into Emmy expressions\n"
     "  (for multi-part problems, every part).\n"
     "- **checked**: d/dx of the answer equals the integrand at sample points\n"
     "  (Emmy `D`, parameters bound to fixed positive values).\n"
     "- **mismatch**: both read, the derivative disagrees. Usually an OCR error in\n"
     "  the statement or the answer; both are kept in the record.\n"
     "- **OCR repaired**: the literal answer failed, a reading without the\n"
     "  spurious `x \\times` (the book's line-break multiplication sign) checks.\n\n"
     "| Sec. | Title | Problems | Parsed | Partial | Failed | Checked | Unchecked | Mismatches |\n"
     "|---:|---|---:|---:|---:|---:|---:|---:|---|\n"
     (str/join
      (for [{:keys [section title problems parsed partial failed checked unparsed mismatches]} sections]
        (format "| %d | %s | %d | %d | %d | %d | %d | %d | %s |\n"
                section title problems parsed partial failed checked unparsed
                (if (seq mismatches) (str/join ", " mismatches) "-"))))
     (format "| | **Total** | **%d** | **%d** | **%d** | **%d** | **%d** | **%d** | **%d** |\n\n"
             problems (tot :parsed) (tot :partial) (tot :failed) (tot :checked) (tot :unparsed)
             (reduce + (map (comp count :mismatches) sections)))
     "OCR-repaired answers: "
     (let [ns (mapcat :ocr-repaired sections)] (if (seq ns) (str/join ", " ns) "none"))
     "\n\nAnswer numbers misread by the OCR and renumbered from the sequence: "
     (str/join ", " (for [r records :when (:answer-ocr-number r)]
                      (str (:number r) " (printed " (:answer-ocr-number r) ")")))
     "\n\nProblems without an answer: "
     (str/join ", " (map :number (filter :answer-missing records)))
     "\n\n## Most common TeX forms that failed to read\n\n"
     "| Count | Form |\n|---:|---|\n"
     (str/join (for [[form n] top-failures] (format "| %d | `%s` |\n" n form))))))

;; ---------------------------------------------------------------------------
;; Boundary

(defn build!
  "Read the book, write the catalog EDN and the Markdown report. Returns the summary."
  ([] (build! {}))
  ([{:keys [book edn md] :or {book book-path
                              edn "resources/demidovich/ch4.edn"
                              md "doc/demidovich/ch4-catalog.md"}}]
   (let [lines (vec (str/split-lines (slurp book)))
         records (catalog lines)]
     (io/make-parents edn)
     (io/make-parents md)
     (spit edn (with-out-str
                 (println ";; Demidovich (Mir), Chapter IV, Indefinite Integrals: problems 1031-1500.")
                 (println ";; Generated by desargues.demidovich.catalog/build!; do not edit by hand.")
                 (binding [*print-length* nil *print-level* nil]
                   (pp/pprint {:book "Demidovich, Problems in Mathematical Analysis (Mir)"
                               :chapter 4 :title "Indefinite Integrals"
                               :expression-heads (vec (sort (map str (keys tex/ops))))
                               :problems records}))))
     (spit md (report-md records))
     (summary records))))
