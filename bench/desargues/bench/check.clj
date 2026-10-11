(ns desargues.bench.check
  "Regression check of a bench report against a baseline report. Pure: two
   report maps in, a verdict map out; desargues.bench/-main does the IO.

   A row regresses when its fresh median is slower than the baseline median
   by more than the relative tolerance AND by more than k times the summed
   median absolute deviations of both runs. Rows present in only one report
   are listed, never failed: the raster rows exist only under :dynamics.")

(def defaults
  "Relative tolerance, spread multiplier and an absolute floor in ms under
   which a difference never counts."
  {:tolerance 0.25 :k 3.0 :floor-ms 0.002})

(defn row-key
  "Identity of a row across runs."
  [{:keys [group id n provider]}]
  [group id n provider])

(defn- median [r] (or (:median-ms r) (:mean-ms r)))

(defn- spread [r] (or (:mad-ms r) (:std-ms r) 0.0))

(defn compare-row
  "Verdict for one row present in both reports."
  [{:keys [tolerance k floor-ms]} base fresh]
  (let [b (median base) f (median fresh)
        delta (- f b)
        ratio (if (pos? b) (/ f b) 1.0)
        noise (* k (+ (spread base) (spread fresh)))
        status (cond
                 (and (> ratio (+ 1 tolerance)) (> delta noise) (> delta floor-ms)) :regression
                 (and (< ratio (/ 1 (+ 1 tolerance))) (> (- delta) noise) (> (- delta) floor-ms)) :improvement
                 :else :ok)]
    {:key (row-key base) :base-ms b :fresh-ms f :ratio ratio :noise-ms noise :status status}))

(defn compare-reports
  "Baseline report vs fresh report -> {:rows [...] :regressions [...]
   :improvements [...] :missing [keys] :new [keys] :ok? bool}."
  ([baseline fresh] (compare-reports baseline fresh {}))
  ([baseline fresh opts]
   (let [opts  (merge defaults opts)
         index (fn [rep] (into {} (map (juxt row-key identity)) (:results rep)))
         bi (index baseline) fi (index fresh)
         rows (vec (for [[k b] bi :let [f (fi k)] :when f] (compare-row opts b f)))
         by (group-by :status rows)]
     {:rows rows
      :regressions (vec (:regression by))
      :improvements (vec (:improvement by))
      :missing (vec (remove fi (keys bi)))
      :new (vec (remove bi (keys fi)))
      :opts opts
      :ok? (empty? (:regression by))})))

(defn- label [[group id n provider]]
  (apply str (name group) " " (interpose " " (remove nil? [id (some->> n (str "n=")) provider]))))

(defn summary-lines
  "Human-readable lines for a verdict."
  [{:keys [rows regressions improvements missing new opts ok?]}]
  (let [line (fn [{:keys [key base-ms fresh-ms ratio]}]
               (format "  %-55s %10.4f -> %10.4f ms  x%.2f" (label key) (double base-ms) (double fresh-ms) (double ratio)))]
    (concat
     [(format "compared %d rows (tolerance %.0f%%, k %.1f x MAD)"
              (count rows) (* 100 (:tolerance opts)) (double (:k opts)))]
     (when (seq regressions) (cons "REGRESSIONS" (map line regressions)))
     (when (seq improvements) (cons "improvements" (map line improvements)))
     (when (seq missing) [(str "not in this run (" (count missing) "): " (vec (map label missing)))])
     (when (seq new) [(str "not in the baseline (" (count new) "): " (vec (map label new)))])
     [(if ok? "bench:check OK" "bench:check FAILED")])))
