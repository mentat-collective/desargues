(ns desargues.logic.tex
  "Formulas to TeX through Emmy's renderer, with the operators of
   desargues.logic.notation and a colour highlight.

     (->TeX '(iff (not (or P Q)) (and (not P) (not Q))))
     (->TeX (highlight '(or (and P Q) R) [1] :gold))   ; colour one subterm

   A path is a vector of argument indices from the root, as in get-in over
   (rest form)."
  (:require [clojure.walk :as walk]
            [desargues.logic.notation :as n]
            [emmy.env]
            [emmy.expression.render :as render]))

(defn- renderer-options
  [notations]
  {:infix? (set (for [[op {:keys [infix?]}] notations :when infix?] op))
   :precedence-map (into {} (for [[op {:keys [precedence]}] notations :when precedence]
                              [op precedence]))
   :special-handlers (into {} (for [[op {:keys [render]}] notations] [op render]))
   :decorators {'color (fn [[c x]] (str "\\textcolor{" (name c) "}{" x "}"))}})

(defn- notations []
  (into {} (for [op (keys (methods n/notation)) :when (symbol? op)] [op (n/notation op)])))

(def ^:private renderer
  "The renderer for the current notation methods, rebuilt when they change."
  (let [cache (atom [nil nil])]
    (fn []
      (let [ms (methods n/notation)
            [k r] @cache]
        (if (identical? k ms)
          r
          (let [r (apply render/TeX-renderer (mapcat identity (renderer-options (notations))))]
            (reset! cache [ms r])
            r))))))

(def ^:private constants
  "Constant symbols -> symbols whose name is their TeX."
  {'empty (symbol "\\varnothing")
   'universe (symbol "\\mathcal{U}")})

(defn- prepare
  "Formula -> the expression Emmy renders: binder vectors spliced into the
   form, constants and truth values as symbols named by their TeX."
  [form]
  (walk/postwalk
   (fn [x]
     (cond (and (seq? x) (vector? (second x)) (#{'forall 'exists 'exists! 'class
                                                 'indexed-union 'indexed-inter}
                                               (first x)))
           (concat [(first x)] (second x) (drop 2 x))
           (and (seq? x) (= 'not (first x)) (seq? (second x)) (= 'in (first (second x))))
           (cons 'notin (rest (second x)))
           (contains? constants x) (constants x)
           (true? x) 'T
           (false? x) 'F
           :else x))
   form))

(defn ->TeX
  "The TeX of formula."
  [formula]
  (binding [render/*TeX-sans-serif-symbols* false]
    ((renderer) (prepare formula))))

(defn highlight
  "formula with the subterm at path wrapped in (color c ...). c is a colour
   name, keyword or symbol."
  [formula path c]
  (if (empty? path)
    (list 'color (symbol (name c)) formula)
    (let [[i & more] path
          [op & args] formula]
      (apply list op (assoc (vec args) i (highlight (nth args i) (vec more) c))))))
