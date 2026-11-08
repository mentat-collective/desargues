(ns desargues.player.calculus-symbolic
  "BOUNDARY stratum (JVM, Emmy): the partial derivatives of z = f(x, y),
   symbolically.

   A surface is written as a plain form in the symbols x and y over a small
   operator vocabulary (`ops`: + - * / expt square cube sin cos tan exp log
   sqrt atan tanh), e.g. '(* (sin x) (cos y)). `partials` turns it into an
   Emmy function, differentiates it with Emmy's `partial` and simplifies:

     (partials '(* (sin x) (cos y)))
     ;; => {:expr  {:f (* (sin x) (cos y)) :fx (* (cos x) (cos y)) :fy ...}
     ;;     :infix {:f \"sin(x) cos(y)\" :fx \"cos(x) cos(y)\" ...}
     ;;     :tex   {:f \"\\\\sin\\\\left(x\\\\right)\\\\,...\" ...}}

   `partials` is memoized: each surface is differentiated once (the cache key
   is the form). `->form` renders a simplified expression as host-neutral
   Clojure code over java.lang.Math / js Math (Math/sin, Math/pow ...), the
   same text on the JVM and in ClojureScript, and `numeric` compiles it into a
   (fn [x y] z) on the JVM.

   `surface-catalog` is the macro that lets a .cljc namespace carry
   Emmy-derived surfaces into ClojureScript, where Emmy is not loaded: the
   derivation runs at macroexpansion time on the JVM (cljs macros always do)
   and the expansion holds only the infix / TeX strings and numeric fns.
   desargues.player.calculus-surfaces is built with it."
  (:require [clojure.walk :as walk]
            [emmy.env :as e]))

(def ops
  "Operator symbols a surface form may use -> the Emmy generic they mean."
  {'+ `e/+ '- `e/- '* `e/* '/ `e// 'expt `e/expt 'square `e/square 'cube `e/cube
   'sin `e/sin 'cos `e/cos 'tan `e/tan 'exp `e/exp 'log `e/log 'sqrt `e/sqrt
   'atan `e/atan 'tanh `e/tanh})

(defn emmy-fn
  "The Emmy function (fn [x y] ...) of surface form `body`."
  [body]
  (eval `(fn [~'x ~'y] ~(walk/postwalk-replace ops body))))

(defn- render [expr]
  {:expr (e/freeze expr) :infix (e/->infix expr) :tex (e/->TeX expr)})

(defn- partials*
  [body]
  (let [f   (emmy-fn body)
        out {:f  (render (e/simplify (f 'x 'y)))
             :fx (render (e/simplify (((e/partial 0) f) 'x 'y)))
             :fy (render (e/simplify (((e/partial 1) f) 'x 'y)))}]
    {:body  body
     :expr  (update-vals out :expr)
     :infix (update-vals out :infix)
     :tex   (update-vals out :tex)}))

(def partials
  "{:body :expr :infix :tex} of surface form `body`, each keyed :f :fx :fy:
   the simplified function and its two partial derivatives (Emmy), frozen as
   forms, as infix strings and as TeX. Memoized per form."
  (memoize partials*))

(def math
  "Emmy's frozen function names -> the Math method computing them (the same
   symbol works on the JVM and in ClojureScript)."
  {'sin 'Math/sin 'cos 'Math/cos 'tan 'Math/tan 'exp 'Math/exp 'log 'Math/log
   'sqrt 'Math/sqrt 'expt 'Math/pow 'atan 'Math/atan 'tanh 'Math/tanh 'abs 'Math/abs})

(defn ->form
  "Frozen Emmy expression `x` as numeric Clojure code in x and y over
   + - * / and Math/*. Exact rationals become doubles. Throws on an operator
   it cannot render."
  [x]
  (cond
    (symbol? x) x
    (number? x) (double x)
    (seq? x)    (let [[op & args] x]
                  (cond
                    (math op)           (cons (math op) (map ->form args))
                    ('#{+ - * /} op)    (cons op (map ->form args))
                    (= 'square op)      (list 'Math/pow (->form (first args)) 2.0)
                    (= 'cube op)        (list 'Math/pow (->form (first args)) 3.0)
                    :else (throw (ex-info (str "cannot render " op) {:form x}))))
    :else (throw (ex-info (str "cannot render " (pr-str x)) {:form x}))))

(defn fn-form
  "The form (fn [x y] ...) computing frozen expression `x` numerically."
  [x]
  (list 'fn ['x 'y] (->form x)))

(def numeric
  "The JVM (fn [x y] z) of frozen expression `x`, compiled once per form."
  (memoize (fn [x] (eval (fn-form x)))))

(defn surface
  "Surface form `body` as a source uses it: {:body :infix :tex :expr} from
   `partials` plus numeric (fn [x y]) values :f :fx :fy (JVM). The JVM twin
   of one entry of `surface-catalog`."
  [body]
  (let [{:keys [expr] :as d} (partials body)]
    (merge d (update-vals expr numeric))))

(defmacro surface-catalog
  "A map {k {:label s :body form ...}} of surfaces, expanded at compile time
   into {k {:label :body :infix :tex :f :fx :fy}}: Emmy derives each surface
   on the JVM during macroexpansion and the expansion carries the infix and
   TeX strings plus (fn [x y] ...) forms for f and its partials, so it
   compiles in ClojureScript without Emmy."
  [m]
  (into {}
        (for [[k spec] m
              :let [{:keys [expr infix tex]} (partials (:body spec))]]
          [k (merge (update-vals spec (fn [v] (list 'quote v)))
                    {:infix infix :tex tex
                     :f (fn-form (:f expr)) :fx (fn-form (:fx expr)) :fy (fn-form (:fy expr))})])))
