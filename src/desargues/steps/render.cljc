(ns desargues.steps.render
  "RENDER stratum: math forms and phrase parts to TeX and plain text.

   `tex` is one method per math-form type (OCP): Emmy expressions go through
   emmy's ->TeX, the model's Integral / Eq / Plus-C records compose their
   pieces. `parts->tex` lays a step's phrase parts (desargues.steps.phrase)
   out as one TeX line, text in \\text{...}. Renderers for other surfaces
   (the blackboard, plain text) take the same parts."
  (:require [clojure.string :as str]
            [desargues.steps.model :as model]
            [emmy.env :as e]))

(defn textbook
  "A frozen Emmy expression in textbook notation before typesetting:
   (exp u) is written e^u. Candidate for Emmy's own TeX renderer
   (doc/emmy-upstream.md)."
  [form]
  (cond (and (seq? form) (= 'exp (first form)) (= 2 (count form)))
        (list 'expt 'e (textbook (second form)))
        (seq? form) (apply list (map textbook form))
        :else form))

(defmulti tex
  "TeX of a math form."
  (fn [form] (cond (model/form? form)              (model/form-kind form)
                   (keyword? form)                 :keyword
                   (string? form)                  :string
                   :else                           :expr)))

(defmethod tex :expr [x] (e/->TeX (textbook (e/freeze x))))

(defmethod tex :string [s] (str "\\text{" s "}"))

(defmethod tex :keyword [k] (str "\\text{" (name k) "}"))

(defmethod tex :integral [{:keys [integrand var bounds]}]
  (str "\\int" (when-let [[a b] bounds] (str "_{" (tex a) "}^{" (tex b) "}"))
       " " (tex integrand) " \\, d" (tex var)))

(defmethod tex :eq [{:keys [sides]}]
  (apply str (interpose " = " (map tex sides))))

(defmethod tex :plus-c [{:keys [expr]}]
  (str (tex expr) " + C"))

(defmethod tex :op [{:keys [op args]}]
  (let [sep  ({:+ " + " :- " - " :* " \\cdot "} op)
        wrap (fn [a] (if (and (model/form? a) (= :op (model/form-kind a))) (str "\\left(" (tex a) "\\right)") (tex a)))]
    (apply str (interpose sep (map wrap args)))))

(defn- escape-text [s]
  (-> s (str/replace "\\" "\\backslash ") (str/replace "{" "\\{")
      (str/replace "}" "\\}") (str/replace "%" "\\%")))

(defn parts->tex
  "One TeX line for phrase parts [[:text s] [:math e] ...]."
  [parts]
  (apply str (map (fn [[kind v]]
                    (case kind
                      :text (str "\\text{" (escape-text v) "}")
                      :math (tex v)))
                  parts)))

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
  (apply str (map (fn [[kind v]]
                    (case kind
                      :text v
                      :math (if (record? v) (pr-str v) (str (e/->infix v)))))
                  parts)))
