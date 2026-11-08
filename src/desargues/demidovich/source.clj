(ns desargues.demidovich.source
  "Collect: the OCR'd Markdown of Demidovich (Mir) -> raw problem and answer
  records for one chapter. Pure functions over a vector of lines; the file is
  read once at the boundary (desargues.demidovich.catalog).

  Raw records keep the TeX exactly as the OCR wrote it plus the 1-based
  source line, so every later stage can point back at the book."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Chapter layout

(def ch4
  "Chapter IV (Indefinite Integrals) in the olmOCR run of 2026-10-15.
  Lines are 1-based and inclusive."
  {:chapter 4
   :title "Indefinite Integrals"
   :problems {:from 4597 :to 6545}
   :answers {:from 19593 :to 19879}
   :numbers [1031 1500]})

;; ---------------------------------------------------------------------------
;; Problem starts

(def start-re
  "A problem number at the start of a line or in a table header cell:
  `1051**. ...`, `1063*: ...`, `1222* \\( ...`, `<th>1031.</th>`."
  #"^\s*(?:<th>)?(1[0-9]\d\d)(\*{0,2})(?:[.:]|\s+(?=\\\())\s*(?:</th>)?\s*(.*)$")

(def section-re #"^Sec\.\s*(\d+)\.?\s*(.*?)\s*$")

(defn- marker [stars]
  (case (count stars) 0 nil 1 :hint 2 :solution))

(defn- stop-line?
  "Does this line end the running problem statement?"
  [l]
  (or (re-find start-re l)
      (re-find section-re l)
      (re-find #"^\s*<" l)
      (re-find #"^Chapter\b" l)
      (re-find #"^\s*(Example|Solution|Note|Hint)\b" l)
      (re-find #"^\s*\d°\." l)
      ;; group instructions: "Find the integrals:", "Applying ..., find ...:"
      (re-find #":\s*$" l)))

(defn- table-cell [l]
  (second (re-find #"^\s*<td>(.*)</td>\s*$" l)))

(defn collect-problems
  "lines (vector of the whole book, 0-based) + chapter layout -> raw problem
  records {:number :marker :section :section-title :line :text} in book order."
  [lines {:keys [problems numbers]}]
  (let [{:keys [from to]} problems
        [lo hi] numbers
        idx (range (dec from) to)]
    (loop [[i & more] idx sec nil acc []]
      (if (nil? i)
        acc
        (let [l (nth lines i)]
          (if-let [[_ n t] (re-find section-re l)]
            (recur more {:section (parse-long n) :section-title t} acc)
            (let [[_ n stars tail] (re-find start-re l)
                  n (some-> n parse-long)]
              (if (and n (<= lo n hi))
                (let [;; a table header cell: the statement is in the next <td>
                      tail (if (str/blank? tail) (or (some-> (get lines (inc i)) table-cell) "") tail)
                      cont (when-not (re-find #"<th>" l)
                             (->> (range (inc i) (min (count lines) (+ i 40)))
                                  (map #(nth lines %))
                                  (take-while (complement stop-line?))
                                  (remove str/blank?)))]
                  (recur more sec
                         (conj acc (merge sec {:number n :marker (marker stars) :line (inc i)
                                               :text (str/join "\n" (cons tail cont))}))))
                (recur more sec acc)))))))))

;; ---------------------------------------------------------------------------
;; Answers

(def answer-hit-re
  "A problem number opening an answer: whitespace (or line start) before it,
  a period and whitespace after it."
  #"(?<=^|\s)(1[0-9]\d\d)(\*{0,2})\.(?=\s|$)")

(defn- line-starts [text]
  (vec (cons 0 (keep-indexed (fn [i c] (when (= c \newline) (inc i))) text))))

(defn- offset->line
  "1-based line (within text) holding character offset off."
  [starts off]
  (count (take-while #(<= % off) starts)))

(defn- reference? [text start]
  (re-find #"(?i)problems?\s*$" (subs text (max 0 (- start 12)) start)))

(defn- accept-hits
  "Keep the hits that form the increasing answer sequence. An out-of-order
  number between last and last+2 is an OCR misreading of last+1 (the book
  has 1048 for 1046 and 1477 for 1447); it is renumbered and flagged."
  [hits lo hi]
  (loop [[h & more] hits last (dec lo) acc []]
    (if (nil? h)
      acc
      (let [n (:read h) nxt (:read (first more))]
        (cond
          (not (<= lo n hi)) (recur more last acc)
          (and (= nxt (+ last 2)) (not= n (inc last)))
          (recur more (inc last) (conj acc (assoc h :number (inc last) :ocr-number n)))
          (> n last) (recur more n (conj acc (assoc h :number n)))
          :else (recur more last acc))))))

(defn collect-answers
  "lines + chapter layout -> {number {:number :line :text :marker :ocr-number}}.
  :text is everything up to the next answer: the answer TeX and any
  Hint/Solution/Note that follows it."
  [lines {:keys [answers numbers]}]
  (let [{:keys [from to]} answers
        [lo hi] numbers
        text (str/join "\n" (subvec lines (dec from) to))
        starts (line-starts text)
        m (re-matcher answer-hit-re text)
        hits (loop [acc []]
               (if (.find m)
                 (recur (if (reference? text (.start m))
                          acc
                          (conj acc {:read (parse-long (.group m 1)) :stars (.group m 2)
                                     :start (.start m) :end (.end m)})))
                 acc))
        acc (accept-hits hits lo hi)]
    (into (sorted-map)
          (map (fn [h nxt]
                 [(:number h)
                  (cond-> {:number (:number h)
                           :marker (marker (:stars h))
                           :line (+ from (dec (offset->line starts (:start h))))
                           :text (str/trim (subs text (:end h) (if nxt (:start nxt) (count text))))}
                    (:ocr-number h) (assoc :ocr-number (:ocr-number h)))])
               acc (concat (rest acc) [nil])))))

;; ---------------------------------------------------------------------------
;; Splitting statement and answer text

(def math-re
  "Inline \\( ... \\) and display \\[ ... \\] segments."
  #"(?s)\\\((.*?)\\\)|\\\[(.*?)\\\]")

(defn math-segments [s]
  (mapv (fn [[_ a b]] (str/trim (or a b))) (re-seq math-re s)))

(defn split-parts
  "Text with lettered parts `a) ... b) ...` -> [[\"a\" text] ...], or nil."
  [s]
  (let [m (re-matcher #"(?:^|(?<=[\s;,.(]))([a-e])\)\s" s)
        hs (loop [acc []] (if (.find m) (recur (conj acc [(.group m 1) (.start m) (.end m)])) acc))
        hs (loop [[h & more] hs want \a acc []]
             (cond (nil? h) acc
                   (= (first (first h)) want) (recur more (char (inc (int want))) (conj acc h))
                   :else (recur more want acc)))]
    (when (>= (count hs) 2)
      (mapv (fn [[label _ e] nxt] [label (str/trim (subs s e (if nxt (second nxt) (count s))))])
            hs (concat (rest hs) [nil])))))

(defn split-answer
  "Answer text -> {:tex the answer as written (text and math), :note the
  Hint/Solution/Note that follows, if any}."
  [s]
  (let [[_ head note] (re-find #"(?s)^(.*?)\s*((?:Hint|Solution|Note)\b.*)?$" s)]
    (cond-> {:tex (-> head str/trim (str/replace #"[.;,\s]+$" ""))}
      note (assoc :note (str/trim note)))))
