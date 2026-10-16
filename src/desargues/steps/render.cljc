(ns desargues.steps.render
  "RENDER stratum: a thin call into Emmy.

   Every math form a step holds is an Emmy expression (integrals, + C and
   equation chains included, see desargues.steps.model), so `tex` is Emmy's
   ->TeX extended with the solution notation (desargues.steps.notation) and
   `infix` is Emmy's ->infix extended the same way. `parts->tex` lays a
   step's phrase parts (desargues.steps.phrase) out as one TeX line, text
   runs in \\text{...}; `parts->md` as prose with inline math. Renderers for
   other surfaces (the blackboard, plain text) take the same parts."
  (:require [clojure.string :as str]
            [desargues.steps.notation :as notation]))

(defn tex
  "TeX of a math form: Emmy's ->TeX with the solution notation."
  [form]
  (cond (string? form)  (notation/tex-text form)
        (keyword? form) (notation/tex-text (name form))
        :else           (notation/->TeX form)))

(defn infix
  "Infix text of a math form: Emmy's ->infix with the solution notation."
  [form]
  (cond (string? form)  form
        (keyword? form) (name form)
        :else           (notation/->infix form)))

(defn parts->tex
  "One TeX line for phrase parts [[:text s] [:math e] ...]."
  [parts]
  (apply str (map (fn [[kind v]] (case kind :text (notation/tex-text v) :math (tex v))) parts)))

(defn parts->md
  "Markdown for phrase parts: text as prose, math inline ($...$), so a long
   step wraps like a sentence instead of overflowing a display block."
  [parts]
  (apply str (map (fn [[kind v]]
                    (case kind
                      :text (str/replace v #"([*_`\[\]$])" "\\\\$1")
                      :math (str "$" (tex v) "$")))
                  parts)))

(defn parts->text
  "Plain text for phrase parts, math as Emmy's infix."
  [parts]
  (apply str (map (fn [[kind v]] (case kind :text v :math (infix v))) parts)))
