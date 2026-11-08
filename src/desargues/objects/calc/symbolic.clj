(ns desargues.objects.calc.symbolic
  "BOUNDARY stratum (JVM, Emmy): one-variable functions of the calculus
   objects, written as plain forms in the symbol x over the operator
   vocabulary of desargues.player.calculus-symbolic (+ - * / expt square cube
   sin cos tan exp log sqrt atan tanh), e.g. '(* x (sin x)).

   Emmy differentiates each form (D), simplifies it and renders it as infix
   and TeX; `fn1` packs the result with numeric (fn [x] ..) twins:

     (fn1 '(- 4 (square x)))
     ;; => {:body (- 4 (square x)) :f #fn :df #fn
     ;;     :infix {:f \"4 - x²\" :df \"-2 x\"} :tex {...}}

   `fn-catalog` is the macro a .cljc catalog uses so ClojureScript gets the
   same entries with no Emmy in the page (the derivation runs at
   macroexpansion time on the JVM). `poly` turns a polynomial form into exact
   rational coefficients, the basis of the exact readouts
   (desargues.objects.calc.readout)."
  (:require [clojure.walk :as walk]
            [desargues.player.calculus-symbolic :as cs]
            [emmy.env :as e]))

(declare poly)

(defn emmy-fn
  "The Emmy function (fn [x] ...) of form `body`."
  [body]
  (eval `(fn [~'x] ~(walk/postwalk-replace cs/ops body))))

(defn- render [expr]
  {:expr (e/freeze expr) :infix (e/->infix expr) :tex (e/->TeX expr)})

(defn- derive* [body]
  (let [f   (emmy-fn body)
        out {:f  (render (e/simplify (f 'x)))
             :df (render (e/simplify ((e/D f) 'x)))}]
    {:body body :expr (update-vals out :expr) :infix (update-vals out :infix)
     :tex (update-vals out :tex)}))

(def differentiate
  "{:body :expr :infix :tex}, each keyed :f :df: form `body` and its
   derivative (Emmy D, simplified). Memoized per form."
  (memoize derive*))

(defn fn-form
  "The form (fn [x] ...) computing frozen expression `x` numerically."
  [x]
  (list 'fn ['x] (cs/->form x)))

(def numeric
  "The JVM (fn [x] y) of frozen expression `x`, compiled once per form."
  (memoize (fn [x] (eval (fn-form x)))))

(defn fn1
  "Form `body` as a calculus object function: `differentiate` plus numeric
   :f :df."
  ([body] (fn1 body {}))
  ([body extra]
   (let [{:keys [expr] :as d} (differentiate body)]
     (merge extra d {:f (numeric (:f expr)) :df (numeric (:df expr))}))))

(defmacro fn-catalog
  "{k {:label s :body form ...}} expanded at compile time into
   {k {:label :body :infix :tex :poly :f (fn [x] ..) :df (fn [x] ..)}};
   :poly holds the exact coefficients of a polynomial body (else nil)."
  [m]
  (into {}
        (for [[k spec] m
              :let [{:keys [expr infix tex]} (differentiate (:body spec))]]
          [k (merge (update-vals spec (fn [v] (list 'quote v)))
                    {:infix infix :tex tex :poly (list 'quote (poly (:body spec)))
                     :f (fn-form (:f expr)) :df (fn-form (:df expr))})])))

;; ── Exact polynomial algebra (rationals) ─────────────────────────────────────

(defn- p+ [a b] (merge-with + a b))
(defn- p* [a b] (apply merge-with + {} (for [[i x] a [j y] b] {(+ i j) (* x y)})))
(defn- pk [k a] (update-vals a #(* k %)))
(defn- pow [a n] (reduce p* {0 1} (repeat n a)))
(defn- const [a] (when (every? zero? (keys (remove (comp zero? val) a))) (get a 0 0)))

(defn poly
  "Exact coefficients {power rational} of polynomial form `body` in x, or nil
   when it is not a polynomial (a sin, a variable power, division by x)."
  [body]
  (letfn [(go [f]
            (cond
              (rational? f) {0 f}
              (number? f)   (go (rationalize f))
              (= 'x f)      {1 1}
              (seq? f)
              (let [[op & args] f
                    ps (map go args)]
                (when (every? some? ps)
                  (case op
                    + (reduce p+ {0 0} ps)
                    * (reduce p* {0 1} ps)
                    - (if (= 1 (count ps)) (pk -1 (first ps))
                          (reduce p+ (first ps) (map #(pk -1 %) (rest ps))))
                    / (let [c (some-> (second ps) const)]
                        (when (and (= 2 (count ps)) c (not (zero? c))) (pk (/ 1 c) (first ps))))
                    square (pow (first ps) 2)
                    cube   (pow (first ps) 3)
                    expt   (let [n (some-> (second ps) const)]
                             (when (and (integer? n) (<= 0 n 32)) (pow (first ps) n)))
                    nil)))
              :else nil))]
    (some->> (go body) (remove (comp zero? val)) (into {}))))

(defn antiderivative
  "Coefficients of the antiderivative (constant 0) of polynomial `p`."
  [p]
  (into {} (map (fn [[i c]] [(inc i) (/ c (inc i))])) p))

(defn eval-poly
  "Exact value of polynomial `p` at rational `x`."
  [p x]
  (reduce + 0 (map (fn [[i c]] (* c (reduce * 1 (repeat i x)))) p)))

(defn definite
  "Exact integral of polynomial `p` over [a b] (rationals)."
  [p a b]
  (let [q (antiderivative p) a (rationalize a) b (rationalize b)]
    (- (eval-poly q b) (eval-poly q a))))

(defn poly-form
  "Polynomial `p` as a form in x (for Emmy to print)."
  [p]
  (cons '+ (for [[i c] (sort p)] (list '* c (list 'expt 'x i)))))

(defn infix
  "Emmy's simplified infix of form `expr` (symbols kept; pi prints as π)."
  [expr]
  (e/->infix (e/simplify (eval (walk/postwalk-replace (assoc cs/ops 'pi `e/pi) expr)))))
