(ns desargues.steps.notation
  "Emmy's renderers extended with the notation of a worked solution.

   A solution writes things Emmy's algebra has no operator for: the integral
   sign (with and without bounds), the constant of integration, an
   evaluation bracket [F]_a^b, a substitution F |_{u = g}, the derivative
   operator d/dx. desargues.steps.model builds each one as a symbolic
   operator in Emmy's expression tree (an Emmy literal, so it composes with
   Emmy arithmetic):

     (e/- (e/* 'u 'v) (m/integral 'v 'u))   ;=> (- (* u v) (integral v u))

   and `->TeX` / `->infix` here are Emmy's own renderers
   (emmy.expression.render TeX-renderer and make-infix-renderer) extended
   with one special handler per operator. Nothing glues TeX around a
   rendered expression: the whole formula, operators included, goes through
   Emmy's precedence and parenthesization in one pass.

   Written Emmy-shaped (operator symbols, handler maps, a pre-render
   ruleset like Emmy's rewrite-negation) so it can move upstream into
   emmy.expression.render; see doc/emmy-upstream.md.

   Host-free .cljc."
  (:require [clojure.string :as s]
            [emmy.env :as e]
            [emmy.expression.render :as render]
            [emmy.pattern.rule :as R :refer [=>]]))

(def operators
  "The notation operators rendered here (paren is render-internal grouping)."
  '#{integral plus-C eval-at subst derivative paren})

;; ## Pre-render rewrites, Emmy-shaped (a ruleset run before rendering, as
;; ## Emmy's own rewrite-negation and rewrite-trig-powers are):
;;  - an integrand, derivand or bracketed function that is a sum is grouped;
;;  - ln of an absolute value is one operator, written ln|u|;
;;  - any positive integer power of a trig function is written sin^n x
;;    (Emmy rewrites only the square).

(defn- sum? [x] (and (seq? x) (contains? '#{+ -} (first x)) (> (count x) 2)))

(def ^:private trig? '#{sin cos tan cot sec csc})

(defn- positive-integer? [n] (and (integer? n) (pos? n)))

(def ^:private textbook
  (R/ruleset
   (integral (? ?f sum?) ??rest) => (integral (paren (? ?f)) ??rest)
   (derivative (? ?f sum?) ?v) => (derivative (paren (? ?f)) ?v)
   (eval-at (? ?f sum?) ??rest) => (eval-at (paren (? ?f)) ??rest)
   (log (abs ?x)) => (ln-abs ?x)
   (ln (abs ?x)) => (ln-abs ?x)
   (ln ?x) => (log ?x)
   (expt ((? ?f trig?) ?x) (? ?n positive-integer?)) => (trig-power ?f ?n ?x)))

;; A fraction bar groups its numerator and denominator by itself, and in a
;; sum or product needs no parentheses either. Emmy gives / precedence 0
;; (so nothing inside a fraction is parenthesized), which parenthesizes a
;; fraction standing in a sum; as the non-infix operator `frac` it is
;; grouped like a function application instead. TeX only.
(def ^:private fraction-bars
  (R/ruleset
   (/ ?a ?b) => (frac ?a ?b)
   (/ ?b) => (frac 1 ?b)))

(def ^:private prepare
  (R/bottom-up (R/attempt textbook)))

(def ^:private prepare-TeX
  (R/bottom-up (R/attempt fraction-bars)))

;; ## TeX

(def TeX-handlers
  "Emmy special-handler map typesetting the notation operators in TeX. Each
   handler receives its arguments already rendered by Emmy."
  {'integral   (fn [[f v a b]]
                 (str "\\int" (when a (str "_{" a "}^{" b "}")) " " f " \\, d" v))
   'plus-C     (fn [[f]] (str f " + C"))
   'eval-at    (fn [[f _ a b]] (str "\\Big[ " f " \\Big]_{" a "}^{" b "}"))
   'subst      (fn [[f u g]] (str f " \\,\\Big|_{" u " = " g "}"))
   'derivative (fn [[f v]] (str "\\frac{d}{d" v "} " f))
   'paren      (fn [[x]] (str "\\left(" x "\\right)"))
   'log        (fn [[x]] (str "\\ln\\left(" x "\\right)"))
   'ln-abs     (fn [[x]] (str "\\ln\\left|" x "\\right|"))
   'abs        (fn [[x]] (str "\\left|" x "\\right|"))
   'exp        (fn [[x]] (str "e^{" x "}"))
   'trig-power (fn [[f n x]] (str f "^{" n "}\\left(" x "\\right)"))
   'frac       (fn [[a b]] (str "\\frac{" a "}{" b "}"))})

(def ^:private TeX*
  ;; = sits below every arithmetic operator, so a fraction on either side of
  ;; = is not parenthesized; frac (not infix, so nothing inside it is
  ;; parenthesized) binds tighter than * and + but not than a power, so
  ;; (x/2)^3 keeps its parentheses.
  (render/TeX-renderer :special-handlers TeX-handlers
                       :precedence-map '{= -1 frac 7.5}))
(defn ->TeX
  "TeX of an Emmy expression that may hold notation operators."
  [expr]
  (TeX* (prepare-TeX (prepare (e/freeze expr)))))

;; ## Infix
;; emmy.expression.render keeps make-infix-renderer private and ->infix's
;; options inline; `infix-renderer` mirrors TeX-renderer for infix (an
;; upstream candidate).

(def ^:private infix-options
  {:precedence-map '{D 9, partial 9, expt 8, :apply 7, u- 6,
                     * 5, modulo 5, remainder 5, / 5,
                     + 4, - 4, not 4, = 3, > 3, < 3, >= 3, <= 3, and 2, or 1}
   :infix? '#{* + - / modulo remainder expt u- = > < >= <= and or}
   :juxtapose-multiply " "
   :rewrite-trig-squares false
   :special-handlers {}
   :render-primitive (fn [v] (render/->infix v))})

(defn infix-renderer
  "An infix renderer: Emmy's ->infix options with `opts` merged in
   (:special-handlers and :precedence-map merged, other keys replaced)."
  [& {:as opts}]
  (let [merged (merge-with (fn [a b] (if (map? a) (merge a b) b)) infix-options opts)]
    (apply @#'render/make-infix-renderer (mapcat identity merged))))

(def infix-handlers
  "Emmy special-handler map writing the notation operators as infix text."
  {'integral   (fn [[f v a b]] (str "∫" (when a (str "[" a ", " b "]")) " " f " d" v))
   'plus-C     (fn [[f]] (str f " + C"))
   'eval-at    (fn [[f _ a b]] (str "[" f "] from " a " to " b))
   'subst      (fn [[f u g]] (str f " with " u " = " g))
   'derivative (fn [[f v]] (str "d/d" v " " f))
   'paren      (fn [[x]] (str "(" x ")"))
   'abs        (fn [[x]] (str "|" x "|"))
   'log        (fn [[x]] (str "ln(" x ")"))
   'ln-abs     (fn [[x]] (str "ln|" x "|"))
   'trig-power (fn [[f n x]] (str f (@#'render/n->superscript n) "(" x ")"))})

(def ^:private infix* (infix-renderer :special-handlers infix-handlers))

(defn ->infix
  "Infix text of an Emmy expression that may hold notation operators."
  [expr]
  (infix* (prepare (e/freeze expr))))

(defn notation?
  "True when `expr` is (or holds at its root) a notation operator."
  [expr]
  (let [f (e/freeze expr)]
    (and (seq? f) (contains? operators (first f)))))

(defn tex-text
  "TeX for a plain-text run: \\text{...} with TeX specials escaped."
  [t]
  (str "\\text{" (-> t (s/replace "\\" "\\backslash ") (s/replace "{" "\\{")
                    (s/replace "}" "\\}") (s/replace "%" "\\%"))
       "}"))
