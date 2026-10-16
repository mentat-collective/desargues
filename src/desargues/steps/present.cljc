(ns desargues.steps.present
  "PRESENT stratum: the pipeline between a step's phrase parts and what a
   surface displays, as CPPB:

     Collect   the step's phrase parts (desargues.steps.phrase/parts)
     Promote   each math part's expression into house notation
               (`notation` middleware, e.g. (exp u) -> e^u)
     Pipeline  middleware over segments: parts become [:text s] / [:tex s],
               then adjacent pieces are joined (`join-bindings` makes
               u = sin x one formula instead of three)
     Boundary  a surface turns the final segments into its own output
               (`->md` for Clerk, the blackboard draws them as chalk)

   A middleware is a plain function segments -> segments (or parts -> parts
   for the Promote stage), so a surface composes its own chain: `present`
   takes the chains as data. Host-free .cljc, no Clerk here: the Clerk
   viewer (notebooks/desargues/steps/viewers.clj) only calls `present` in
   its :transform-fn, on the JVM, before anything is pushed to the browser."
  (:require [clojure.string :as str]
            [desargues.steps.phrase :as phrase]
            [desargues.steps.render :as render]
            [emmy.env :as e]))

;; ── Promote: expression notation ──────────────────────────────────────────

(defn exp->e
  "(exp u) written e^u, inside a frozen expression."
  [form]
  (cond (and (seq? form) (= 'exp (first form)) (= 2 (count form)))
        (list 'expt 'e (exp->e (second form)))
        (seq? form) (apply list (map exp->e form))
        :else form))

(defn log->ln
  "(log u) written ln u: Emmy's log is the natural logarithm, and a reader
   may take a bare log for base 10."
  [form]
  (cond (and (seq? form) (= 'log (first form)) (= 2 (count form)))
        (list 'ln (log->ln (second form)))
        (seq? form) (apply list (map log->ln form))
        :else form))

(def default-notation
  "Rewrites applied to every frozen math expression, in order."
  [exp->e log->ln])

(def default-tex-fixes
  "Rewrites of the TeX string Emmy produces: operators Emmy does not know
   come out sans-serif (\\mathsf{ln}); textbooks set them upright (\\ln).
   Candidate for Emmy's renderer (doc/emmy-upstream.md)."
  [#(clojure.string/replace % "\\mathsf{ln}" "\\ln")])

(defn- tex-of [form]
  (reduce (fn [s f] (f s)) (render/tex form) default-tex-fixes))

(defn- promote-value
  "Rewrite every Emmy expression inside `v`: a bare expression, or the
   fields of a math-form record (Integral, Eq, Op ...) and their vectors."
  [rewrites v]
  (cond (record? v) (reduce-kv (fn [r k x] (assoc r k (promote-value rewrites x))) v v)
        (vector? v) (mapv #(promote-value rewrites %) v)
        (or (keyword? v) (string? v) (nil? v) (map? v)) v
        :else       (reduce (fn [f r] (r f)) (e/freeze v) rewrites)))

(defn notation
  "Promote middleware: rewrite each math part of `parts` through `rewrites`
   (functions over frozen Emmy forms), records included."
  [rewrites]
  (fn [parts]
    (mapv (fn [[kind v :as part]]
            (if (= kind :math) [:math (promote-value rewrites v)] part))
          parts)))

;; ── Pipeline: segments ────────────────────────────────────────────────────

(defn ->segments
  "Phrase parts to segments: text stays text, math becomes TeX."
  [parts]
  (mapv (fn [[kind v]] (if (= kind :math) [:tex (tex-of v)] [:text v])) parts))

(def ^:private joiners
  "Short connectives that belong inside a formula between two math pieces."
  #{" = " "=" " + " " - " ", "})

(defn join-bindings
  "Pipeline middleware: [:tex a] [:text \" = \"] [:tex b] becomes one [:tex
   \"a = b\"], so u = sin x reads as a single formula."
  [segments]
  (reduce (fn [acc [kind v :as seg]]
            (if (>= (count acc) 2)
              (let [[k1 v1] (peek acc) [k2 v2] (peek (pop acc))]
                (if (and (= kind :tex) (= k1 :text) (joiners v1) (= k2 :tex))
                  (let [j (str/trim v1)]
                    (conj (pop (pop acc)) [:tex (str v2 (if (= j ",") ",\\ " (str " " j " ")) v)]))
                  (conj acc seg)))
              (conj acc seg)))
          [] segments))

(def default-pipeline
  "Segment middleware applied in order."
  [join-bindings])

;; ── Boundary helpers ──────────────────────────────────────────────────────

(defn ->md
  "Markdown for segments: prose with inline $math$."
  [segments]
  (apply str (map (fn [[kind v]]
                    (if (= kind :tex)
                      (str "$" v "$")
                      (str/replace v #"([*_`\[\]$])" "\\\\$1")))
                  segments)))

(defn present
  "Collect -> Promote -> Pipeline for step `s`: its final segments.
   opts: :notation (rewrites) and :pipeline (segment middleware)."
  ([s] (present s {}))
  ([s {:keys [notation-rewrites pipeline]
       :or {notation-rewrites default-notation pipeline default-pipeline}}]
   (let [parts ((notation notation-rewrites) (phrase/parts s))]
     (reduce (fn [segs mw] (mw segs)) (->segments parts) pipeline))))

(defn present-form
  "A lone math form (a problem statement, an answer) through the same
   Promote stage, as TeX."
  ([form] (present-form form default-notation))
  ([form rewrites]
   (let [[[_ v]] ((notation rewrites) [[:math form]])]
     (tex-of v))))
