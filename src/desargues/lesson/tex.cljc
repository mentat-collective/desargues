(ns desargues.lesson.tex
  "TeX for lesson :tex blocks: Emmy's ->TeX extended with integral rules, built
   through the extensible renderer (emmy.expression.render/TeX-renderer) rather
   than by patching Emmy.

     (->TeX '(integral (expt x 2) x 0 1))  ;; => \"\\\\int_{0}^{1} {x}^{2} \\\\, dx\"
     (->TeX '(integral (expt x 2) x))      ;; => \"\\\\int {x}^{2} \\\\, dx\"

   `integral` and its synonym `integrate` are frozen-expression heads; Emmy has
   no TeX rule for them and would otherwise set them as a function name,
   integral(x^2, x, 0, 1). The generated lesson notebook (desargues.lesson.export)
   typesets every :tex block through this namespace."
  (:require [clojure.string :as str]
            [emmy.expression.render :as render]))

(defn- integral-tex
  "Special handler: the rendered arguments [f x] or [f x a b]. An integrand
   that is a sum or difference is parenthesized, so \\int a + b \\, dx cannot
   be misread as a + \\int b."
  [[f x a b :as args]]
  (let [f (if (re-find #" [+-] " (str f)) (str "\\left(" f "\\right)") f)]
    (case (count args)
      2 (str "\\int " f " \\, d" x)
      4 (str "\\int_{" a "}^{" b "} " f " \\, d" x)
      nil)))

(def integral-handlers
  "Operator symbol -> special handler, merged over Emmy's TeX defaults."
  {'integral  integral-tex
   'integrate integral-tex})

(def ^:private render*
  (render/TeX-renderer :special-handlers integral-handlers))

(defn ->TeX
  "TeX string of (frozen) expression `expr`, Emmy's ->TeX plus the rules in
   `integral-handlers`. `:equation` behaves as in emmy.env/->TeX."
  [expr & {:keys [equation]}]
  (let [s (render* expr)]
    (if equation
      (str "\\begin{equation}\n"
           (when (and (string? equation) (not (str/blank? equation)))
             (str "\\label{" equation "}\n"))
           s "\n\\end{equation}")
      s)))
