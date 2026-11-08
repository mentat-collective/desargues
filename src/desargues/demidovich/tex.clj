(ns desargues.demidovich.tex
  "A small TeX reader for the formulas of a problem book: TeX source in,
  an s-expression over Emmy's generic operator names out.

  It reads the forms that occur in Demidovich's chapter on integrals:
  \\int ... dx, \\frac, powers, \\sqrt and \\sqrt[n], the trigonometric,
  hyperbolic and inverse functions, \\ln and \\log (decimal, the book's
  convention), e^x, absolute values, implicit products and the TeX habit of
  applying a function to the juxtaposed factors that follow it (\\sin 2x).

  The output is plain data, e.g. (* 2 (sin x)), so the catalog stays EDN and
  any consumer can evaluate it: `->fn` binds the symbols to emmy.generic.

  Strata: tokenize (string -> tokens), parse (tokens -> s-expression),
  read-integral (statement -> {:integrand :var}), evaluate (s-expression ->
  value under an environment). A reader failure throws ex-info carrying
  :tex/reason, which the catalog records as data and never drops."
  (:require [clojure.string :as str]
            [emmy.generic :as g]))

;; ---------------------------------------------------------------------------
;; Vocabulary

(def functions
  "TeX function name -> s-expression head."
  {"sin" 'sin "cos" 'cos "tan" 'tan "tg" 'tan "cot" 'cot "ctg" 'cot
   "sec" 'sec "csc" 'csc "cosec" 'csc
   "sinh" 'sinh "cosh" 'cosh "tanh" 'tanh "coth" 'coth "sech" 'sech "csch" 'csch
   "arcsin" 'asin "arccos" 'acos "arctan" 'atan "arccot" 'acot "arcctg" 'acot
   "arcsinh" 'asinh "arccosh" 'acosh "arctanh" 'atanh
   "ln" 'log "log" 'log10 "lg" 'log10 "exp" 'exp})

(def ^:private greek
  {"alpha" 'alpha "beta" 'beta "gamma" 'gamma "lambda" 'lambda "mu" 'mu
   "varphi" 'phi "phi" 'phi "theta" 'theta "omega" 'omega "rho" 'rho
   "sigma" 'sigma "tau" 'tau "delta" 'delta "epsilon" 'epsilon
   "varepsilon" 'epsilon "xi" 'xi "eta" 'eta "nu" 'nu "kappa" 'kappa
   "psi" 'psi "zeta" 'zeta})

(def ^:private skipped-commands
  "Spacing and sizing commands that carry no math."
  #{"," ";" ":" "!" " " "quad" "qquad" "left" "right" "big" "Big" "bigg" "Bigg"
    "displaystyle" "limits" "nolimits" "mathrm" "operatorname" "text" "textrm"
    "mathit" "rm"})

;; ---------------------------------------------------------------------------
;; Normalization (string level)

(defn normalize
  "Undo the OCR habits that are not TeX: arc functions written as two words,
  the inline-math delimiters, \\left/\\right sizing, \\dfrac."
  [s]
  (-> s
      (str/replace #"\\\(|\\\)|\\\[|\\\]" " ")
      (str/replace #"\barc\s*(sin|cos|tan|cot|tg|ctg)\b" "\\\\arc$1")
      (str/replace #"\\(d|t)frac" "\\\\frac")
      (str/replace #"\\left\s*\\\{" "(") (str/replace #"\\right\s*\\\}" ")")
      (str/replace #"\\left\s*\|" " \\\\absopen ")
      (str/replace #"\\right\s*\|" " \\\\absclose ")
      (str/replace #"\\(left|right)\s*([(\[)\]])" "$2")
      (str/replace #"\\(left|right)\s*\." " ")
      (str/replace #"\\lvert" " \\\\absopen ") (str/replace #"\\rvert" " \\\\absclose ")
      (str/replace #"\\\{" "(") (str/replace #"\\\}" ")")
      (str/replace #"\\\s" " ")))

;; ---------------------------------------------------------------------------
;; Tokenizer

(defn- word-tokens
  "A run of letters: a known function name at its start, else single letters
  (TeX math reads `ax` as a times x)."
  [w]
  (loop [w w acc []]
    (if (str/blank? w)
      acc
      (if-let [f (some #(when (str/starts-with? w %) %)
                       ;; longest names first: arcsinh before arcsin before sin
                       (sort-by (comp - count) (keys functions)))]
        (recur (subs w (count f)) (conj acc [:fn (functions f)]))
        (recur (subs w 1) (conj acc [:var (symbol (subs w 0 1))]))))))

(defn tokenize
  "TeX string -> vector of tokens [:num n] [:var sym] [:fn head] [:op c]
  [:frac] [:sqrt] [:diff sym] [:abs-open] [:abs-close] [:bar] [:pi] [:cmd s]."
  [s]
  (let [s (normalize s)
        n (count s)]
    (loop [i 0 acc []]
      (if (>= i n)
        acc
        (let [c (.charAt s i)]
          (cond
            (Character/isWhitespace c) (recur (inc i) acc)

            (Character/isDigit c)
            (let [[m] (re-find #"^\d+(\.\d+)?" (subs s i))]
              (recur (+ i (count m))
                     (conj acc [:num (if (str/includes? m ".")
                                       (rationalize (parse-double m))
                                       (parse-long m))])))

            (= c \\)
            (let [[m cmd] (or (re-find #"^\\([A-Za-z]+)" (subs s i))
                              [(subs s i (min n (+ i 2))) (subs s (min n (inc i)) (min n (+ i 2)))])
                  i' (+ i (count m))]
              (cond
                (skipped-commands cmd)
                (if (#{"text" "textrm" "mathrm" "operatorname" "mathit"} cmd)
                  ;; \text{arc tan} / \operatorname{arctg}: read the braced word
                  (let [[whole inner] (re-find #"^\s*\{([^}]*)\}" (subs s i'))]
                    (if whole
                      (recur (+ i' (count whole))
                             (into acc (word-tokens (str/replace inner #"\s" ""))))
                      (recur i' acc)))
                  (recur i' acc))
                (functions cmd)  (recur i' (conj acc [:fn (functions cmd)]))
                (= cmd "frac")   (recur i' (conj acc [:frac]))
                (= cmd "sqrt")   (recur i' (conj acc [:sqrt]))
                (= cmd "int")    (recur i' (conj acc [:int]))
                (= cmd "pi")     (recur i' (conj acc [:pi]))
                (= cmd "infty")  (recur i' (conj acc [:var 'infinity]))
                (greek cmd)      (recur i' (conj acc [:var (greek cmd)]))
                (#{"cdot" "times"} cmd) (recur i' (conj acc [:op \*]))
                (= cmd "div")    (recur i' (conj acc [:op \/]))
                (= cmd "absopen")  (recur i' (conj acc [:abs-open]))
                (= cmd "absclose") (recur i' (conj acc [:abs-close]))
                (= cmd "DIFF")   (recur i' acc) ; never produced by users
                :else            (recur i' (conj acc [:cmd cmd]))))

            (Character/isLetter c)
            (let [m (re-find #"^[A-Za-z]+" (subs s i))] (recur (+ i (count m)) (into acc (word-tokens m))))

            (= c \|) (recur (inc i) (conj acc [:bar]))
            (= c \_)
            ;; subscript: fold into the previous symbol (phi_0 -> phi0)
            (let [[m sub] (or (re-find #"^_\{([A-Za-z0-9]+)\}" (subs s i))
                              (re-find #"^_([A-Za-z0-9])" (subs s i)))
                  prev (peek acc)]
              (if (and m (= :var (first prev)))
                (recur (+ i (count m)) (conj (pop acc) [:var (symbol (str (second prev) sub))]))
                (throw (ex-info "subscript" {:tex/reason :subscript :tex s}))))
            (#{\+ \- \* \/ \^ \( \) \[ \] \{ \} \= \< \> \, \; \. \' \!} c)
            (recur (inc i) (conj acc [:op c]))
            :else (recur (inc i) acc)))))))

;; ---------------------------------------------------------------------------
;; Parser (recursive descent over a token vector held in an atom cursor)

(defn- fail [reason tok & [more]]
  (throw (ex-info (str "TeX reader: " (name reason))
                  (merge {:tex/reason reason :tex/token tok} more))))

(defn- peek-tok [st] (get (:toks @st) (:pos @st)))
(defn- next-tok! [st] (let [t (peek-tok st)] (swap! st update :pos inc) t))
(defn- op? [t c] (and (= :op (first t)) (= c (second t))))
(defn- expect! [st c]
  (let [t (next-tok! st)]
    (when-not (op? t c) (fail :expected-delimiter t {:expected (str c)}))))

(declare parse-expr parse-factor)

(defn- simplify-product [fs]
  (let [fs (remove #(= 1 %) fs)]
    (case (count fs) 0 1 1 (first fs) (cons '* fs))))

(defn- parse-group
  "{...} or a single token argument (\\frac12, x^2)."
  [st]
  (let [t (peek-tok st)]
    (cond
      (op? t \{) (do (next-tok! st) (let [e (parse-expr st #{\}})] (expect! st \}) e))
      (nil? t) (fail :unexpected-end nil)
      (= :num (first t))
      ;; x^23 in TeX is x^2 3: a bare argument is ONE character
      (let [[_ n] (next-tok! st) ds (str n)]
        (if (and (integer? n) (> (count ds) 1))
          (do (swap! st update :toks
                     (fn [v] (let [p (:pos @st)]
                               (vec (concat (subvec v 0 p) [[:num (parse-long (subs ds 1))]] (subvec v p))))))
              (parse-long (subs ds 0 1)))
          n))
      :else (parse-factor st {:bare true}))))

(defn- parse-exponent [st base]
  (if (op? (peek-tok st) \^)
    (do (next-tok! st) (list 'expt base (parse-group st)))
    base))

(def ^:private closers #{\) \] \}})

(defn- term-stop?
  "Does token t end a run of juxtaposed factors?"
  [t ctx]
  (or (nil? t)
      (and (= :op (first t)) (#{\+ \- \) \] \} \= \< \> \, \; \.} (second t)))
      (= :abs-close (first t))
      (and (= :bar (first t)) (pos? (:abs-depth ctx 0)))
      (and (:fn-arg ctx) (#{:fn} (first t)))
      (and (:fn-arg ctx) (op? t \*))))

(def ^:dynamic *greedy-fn-args*
  "When true, an unparenthesized function argument also swallows a following
  \\sqrt or \\frac factor: `\\arctan x \\sqrt{3/5}` reads as arctan(x sqrt(3/5)).
  The book uses both conventions, so the catalog tries both readings."
  false)

(defn- parse-fn-arg
  "The argument of \\sin etc. without parentheses: the juxtaposed factors
  that follow, up to the next operator or function (\\sin 2x \\cos x)."
  [st ctx]
  (let [t (peek-tok st)]
    (if (or (op? t \() (op? t \[))
      (let [close (if (op? t \() \) \])]
        (next-tok! st)
        (let [e (parse-expr st #{close})]
          (expect! st close)
          (parse-exponent st e)))
      (loop [fs []]
        (let [t (peek-tok st)]
          (if (or (term-stop? t (assoc ctx :fn-arg true))
                  (and (seq fs) (not *greedy-fn-args*) (#{:frac :sqrt} (first t))))
            (if (empty? fs) (fail :missing-argument t) (simplify-product fs))
            (recur (conj fs (parse-factor st (assoc ctx :fn-arg true))))))))))

(defn- parse-atom [st ctx]
  (let [t (next-tok! st)]
    (case (first t)
      :num (second t)
      :var (if (= 'e (second t)) :e (second t))
      :pi 'pi
      :diff 1
      :frac (let [n (parse-group st) d (parse-group st)] (list '/ n d))
      :sqrt (let [idx (when (op? (peek-tok st) \[)
                        (next-tok! st)
                        (let [e (parse-expr st #{\]})] (expect! st \]) e))
                  r (parse-group st)]
              (if idx (list 'expt r (list '/ 1 idx)) (list 'sqrt r)))
      :fn (let [head (second t)
                pw (when (op? (peek-tok st) \^) (next-tok! st) (parse-group st))
                arg (parse-fn-arg st ctx)
                app (list head arg)]
            (cond (nil? pw) app
                  (= -1 pw) (fail :inverse-power-notation t)
                  :else (list 'expt app pw)))
      :abs-open (let [e (parse-expr st #{} (update ctx :abs-depth (fnil inc 0)))]
                  (when-not (= :abs-close (first (next-tok! st)))
                    (fail :unbalanced-abs t))
                  (list 'abs e))
      :bar (let [e (parse-expr st #{} (update ctx :abs-depth (fnil inc 0)))]
             (when-not (= :bar (first (next-tok! st)))
               (fail :unbalanced-abs t))
             (list 'abs e))
      :op (case (second t)
            \( (let [e (parse-expr st #{\)})] (expect! st \)) e)
            \[ (let [e (parse-expr st #{\]})] (expect! st \]) e)
            \{ (let [e (parse-expr st #{\}})] (expect! st \}) e)
            \- (list '- (parse-factor st ctx))
            \+ (parse-factor st ctx)
            (fail :unexpected-operator t))
      :int (fail :nested-integral t)
      :cmd (fail :unknown-command t {:command (second t)})
      nil (fail :unexpected-end t)
      (fail :unexpected-token t))))

(defn- parse-factor [st ctx]
  (let [a (parse-atom st ctx)
        a (parse-exponent st a)
        ;; postfix factorial / prime are not integrand material
        _ (when (or (op? (peek-tok st) \!) (op? (peek-tok st) \'))
            (fail :postfix-operator (peek-tok st)))]
    (if (= :e a) a a)))

(defn- e-power
  "e and e^u read as atoms; turn (expt :e u) into (exp u), bare :e into (exp 1)."
  [x]
  (cond (= :e x) '(exp 1)
        (and (seq? x) (= 'expt (first x)) (= :e (second x))) (list 'exp (e-power (nth x 2)))
        (seq? x) (apply list (map e-power x))
        :else x))

(defn- parse-term [st ctx]
  (loop [fs [(parse-factor st ctx)]]
    (let [t (peek-tok st)]
      (cond
        (term-stop? t ctx) (simplify-product fs)
        (op? t \*) (do (next-tok! st) (recur (conj fs (parse-factor st ctx))))
        (op? t \/) (do (next-tok! st)
                       (let [d (parse-factor st ctx)]
                         (recur [(list '/ (simplify-product fs) d)])))
        (= :bar (first t))  (recur (conj fs (parse-factor st ctx)))
        :else (recur (conj fs (parse-factor st ctx)))))))

(defn- parse-expr
  ([st closers-ok] (parse-expr st closers-ok {}))
  ([st _closers-ok ctx]
   (let [ctx (dissoc ctx :fn-arg)
         first-neg? (when (op? (peek-tok st) \-) (next-tok! st) true)
         _ (when (op? (peek-tok st) \+) (next-tok! st))
         t0 (parse-term st ctx)]
     (loop [acc [(if first-neg? (list '- t0) t0)]]
       (let [t (peek-tok st)]
         (cond
           (op? t \+) (do (next-tok! st) (recur (conj acc (parse-term st ctx))))
           (op? t \-) (do (next-tok! st) (recur (conj acc (list '- (parse-term st ctx)))))
           :else (if (= 1 (count acc)) (first acc) (cons '+ acc))))))))

(defn- run-parser [toks]
  (let [st (atom {:toks (vec toks) :pos 0})
        e (parse-expr st #{})
        rest-toks (drop (:pos @st) (:toks @st))]
    (when (seq rest-toks)
      (fail :trailing-tokens (first rest-toks) {:rest (vec (take 6 rest-toks))}))
    (e-power e)))

(defn read-tex
  "TeX expression string -> s-expression. Throws ex-info with :tex/reason."
  [s]
  (let [s (-> s str/trim (str/replace #"[.,;:\s]+$" ""))]
    (when (str/blank? s) (fail :empty nil))
    (run-parser (tokenize s))))

;; ---------------------------------------------------------------------------
;; Integrals

(def ^:private diff-re
  "d followed by the variable of integration, where `d` is not inside a word."
  #"(?<![A-Za-z\\])d\s*(\\lambda|\\varphi|\\theta|\\alpha|[a-zA-Z])(?![A-Za-z^_])")

(defn strip-ones
  "Remove multiplicative 1s the differential left behind."
  [e]
  (if (seq? e)
    (let [e (apply list (map strip-ones e))]
      (if (= '* (first e))
        (simplify-product (rest e))
        e))
    e))

(defn read-integral
  "A statement `\\int f(x) dx` -> {:integrand sexp :var sym}. The differential
  may sit at the end or inside a numerator (\\frac{x dx}{...}); the LAST
  occurrence is the differential (5a^4 x^6 dx)."
  [s]
  (let [s (normalize s)
        idx (str/index-of s "\\int")
        _ (when-not idx (fail :no-integral nil))
        body (subs s (+ idx 4))
        _ (when (str/includes? body "\\int") (fail :several-integrals nil))
        ms (let [m (re-matcher diff-re body)]
             (loop [acc []] (if (.find m) (recur (conj acc [(.start m) (.end m) (.group m 1)])) acc)))
        _ (when (empty? ms) (fail :no-differential nil))
        [a b v] (peek ms)
        var (let [v (str/replace v "\\" "")] (symbol (get {"varphi" "phi"} v v)))
        body' (str (subs body 0 a) " 1 " (subs body b))]
    {:integrand (let [e (read-tex body')]
                  ;; the 1 left where the differential stood
                  (strip-ones e))
     :var var}))

;; ---------------------------------------------------------------------------
;; Evaluation (s-expression -> Emmy value)

(def ops
  "s-expression head -> function over Emmy generic values."
  {'+ g/+ '- g/- '* g/* '/ g// 'expt g/expt 'sqrt g/sqrt 'abs g/abs
   'sin g/sin 'cos g/cos 'tan g/tan 'cot g/cot 'sec g/sec 'csc g/csc
   'sinh g/sinh 'cosh g/cosh 'tanh g/tanh 'coth g/coth 'sech g/sech 'csch g/csch
   'asin g/asin 'acos g/acos 'atan g/atan 'acot g/acot
   'asinh g/asinh 'acosh g/acosh 'atanh g/atanh
   'log g/log 'exp g/exp
   'log10 (fn [x] (g// (g/log x) (Math/log 10.0)))})

(defn evaluate
  "Evaluate s-expression e with env (symbol -> value)."
  [e env]
  (cond
    (number? e) e
    (= 'pi e) Math/PI
    (symbol? e) (if (contains? env e) (env e) (fail :unbound-symbol nil {:symbol e}))
    (seq? e) (let [f (or (ops (first e)) (fail :unknown-head nil {:head (first e)}))]
               (apply f (map #(evaluate % env) (rest e))))
    :else (fail :bad-expression nil {:expr e})))

(defn symbols-of
  "Free symbols of an s-expression (pi excluded)."
  [e]
  (cond (symbol? e) (if (= 'pi e) #{} #{e})
        (seq? e) (reduce into #{} (map symbols-of (rest e)))
        :else #{}))

(defn ->fn
  "s-expression -> (fn [x] ...) of the variable v with parameters bound by env."
  [e v env]
  (fn [x] (evaluate e (assoc env v x))))
