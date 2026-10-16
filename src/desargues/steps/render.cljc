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

(defmulti tex
  "TeX of a math form."
  (fn [form] (cond (instance? model/Integral form) :integral
                   (instance? model/Eq form)       :eq
                   (instance? model/Plus-C form)   :plus-c
                   (instance? model/Op form)       :op
                   (keyword? form)                 :keyword
                   (string? form)                  :string
                   :else                           :expr)))

(defmethod tex :expr [x] (e/->TeX x))

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
        wrap (fn [a] (if (instance? model/Op a) (str "\\left(" (tex a) "\\right)") (tex a)))]
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

(defn parts->text
  "Plain text for phrase parts, math as Emmy's infix."
  [parts]
  (apply str (map (fn [[kind v]]
                    (case kind
                      :text v
                      :math (if (record? v) (pr-str v) (str (e/->infix v)))))
                  parts)))
