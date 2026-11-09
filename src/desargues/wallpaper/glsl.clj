(ns desargues.wallpaper.glsl
  "The :glsl backend of field forms (desargues.wallpaper.field): one field
   source compiled to a GLSL function body, valid in GLSL ES 3.00 and in
   GLSL 330 (and in ISF, desargues.wallpaper.emit). Pure: no file, no GL.

   Stratified CPPB:
     Collect  `fld/lower`: the form as straight-line code (lets flattened,
              derived ops expanded), the shape every backend compiles from.
     Promote  `ir`: the lowered code as a GLSL expression tree, data:
                number | \"name\" | [:neg a] | [:bin \"+\" a b] | [:call \"sin\" a ..]
              Every lowered op maps to a GLSL op, or the whole compile is an
              (err {:error :field/unsupported :construct ..}) naming it. A
              construct is never dropped. A loop-n node stays one node
              (`bindings-ir`) and prints as a for loop with break
              (`statements-text`), never unrolled.
     Pipeline `function-text`: the tree printed as
                vec3 desargues_field(float fx, float fy, float p0, ..)
              plus the helpers it calls.
     Boundary none here; emit writes the files.

   `interpret` runs the same tree on the CPU (in double, or rounding every
   step to float32 as a GPU does): the parity check between the JVM shader
   and the emitted code.

   Semantic notes. GLSL's pow is undefined for x < 0 and for x = 0, y <= 0;
   the JVM's Math/pow is not. Every pow therefore compiles to the helper
   d_pow, which follows Math/pow on the reals (an integer exponent of a
   negative base keeps its sign) and returns 0 where Math/pow returns NaN.
   GLSL floats are 32-bit: expect about one level of 255 of difference
   against the JVM's doubles."
  (:require [clojure.string :as str]
            [desargues.wallpaper.field :as fld]
            [hive-dsl.result :as r]))

(def function-name
  "The name of the emitted field function."
  "desargues_field")

(def ^:private calls
  "Lowered java.lang.Math op -> [GLSL function, arity]."
  '{Math/sin ["sin" 1] Math/cos ["cos" 1] Math/tan ["tan" 1] Math/exp ["exp" 1]
    Math/log ["log" 1] Math/sqrt ["sqrt" 1] Math/abs ["abs" 1] Math/floor ["floor" 1]
    Math/atan2 ["atan" 2] Math/pow ["d_pow" 2] Math/min ["min" 2] Math/max ["max" 2]})

(def ^:private binops '#{+ - * /})

(def helpers
  "GLSL helper name -> its source."
  {"d_pow"
   (str "float d_pow(float x, float y) {\n"
        "  if (x > 0.0) return pow(x, y);\n"
        "  if (x == 0.0) return y > 0.0 ? 0.0 : (y == 0.0 ? 1.0 : 1.0e30);\n"
        "  float n = floor(y);\n"
        "  if (n != y) return 0.0;\n"
        "  float m = pow(-x, y);\n"
        "  return mod(n, 2.0) == 0.0 ? m : -m;\n"
        "}\n")})

(defn- unsupported [construct form why]
  (r/err :field/unsupported {:target :glsl :construct construct :form form
                             :message (str "GLSL cannot express " (pr-str construct) ": " why)}))

;; ── Promote: lowered code -> GLSL expression tree ──────────────────────────

(defn- ir-all [f xs]
  (reduce (fn [acc x] (let [v (f x)] (if (r/ok? v) (update acc :ok conj (:ok v)) (reduced v))))
          (r/ok []) xs))

(defn expr-ir
  "Lowered expression `e` as a GLSL expression tree, as a Result."
  [e]
  (cond
    (number? e) (let [d (double e)]
                  (if (Double/isFinite d) (r/ok d)
                      (unsupported d e "GLSL has no literal for a non-finite number")))
    (symbol? e) (r/ok (name e))
    (and (seq? e) (seq e))
    (let [[op & args] e
          n (count args)]
      (r/let-ok [as (ir-all expr-ir args)]
        (cond
          (and (= op '-) (= n 1)) (r/ok [:neg (first as)])
          (and (binops op) (= n 2)) (r/ok [:bin (name op) (first as) (second as)])
          (and (calls op) (= n (second (calls op)))) (r/ok (into [:call (first (calls op))] as))
          :else (unsupported op e "no GLSL op of this name and arity"))))
    :else (unsupported e e "not an expression")))

(defn test-ir
  "Lowered :until condition `t` as a condition tree, its expressions
   through `expr-ir`, as a Result:
     [:cmp \"<\" a b] | [:and c d] | [:or c d] | [:not c]"
  [expr-ir t]
  (case (first t)
    (< > <= >=) (r/let-ok [as (ir-all expr-ir (rest t))]
                  (r/ok (into [:cmp (name (first t))] as)))
    (and or) (r/let-ok [cs (ir-all #(test-ir expr-ir %) (rest t))]
               (r/ok (into [(keyword (name (first t)))] cs)))
    not (r/map-ok (test-ir expr-ir (second t)) (fn [c] [:not c]))))

(defn bindings-ir
  "Lowered `bindings` (fld/lower) as tree bindings, expressions through
   `expr-ir` (the target's), as a Result. A pair becomes [\"v0\" tree]; a
   loop node becomes
     {:loop \"v0\" :n N :state [[\"v1\" tree] ..]
      :until {:bindings [..] :test cond} | nil
      :bindings [..] :next [tree ..]}
   kept as one structured node, never unrolled."
  [expr-ir bindings]
  (ir-all (fn [b]
            (if-not (fld/loop-node? b)
              (let [[s e] b] (r/map-ok (expr-ir e) (fn [t] [(name s) t])))
              (let [{:loop/keys [i n state until bindings next]} b]
                (r/let-ok [st (ir-all (fn [[s e]] (r/map-ok (expr-ir e) (fn [t] [(name s) t]))) state)
                           ub (if until (bindings-ir expr-ir (:bindings until)) (r/ok nil))
                           ut (if until (test-ir expr-ir (:test until)) (r/ok nil))
                           bs (bindings-ir expr-ir bindings)
                           nx (ir-all expr-ir next)]
                  (r/ok {:loop (name i) :n n :state st
                         :until (when until {:bindings ub :test ut})
                         :bindings bs :next nx})))))
          bindings))

(defn trees
  "Every expression and condition tree of tree `bindings`, loops included."
  [bindings]
  (mapcat (fn [b]
            (if (map? b)
              (concat (map second (:state b)) (trees (:bindings (:until b)))
                      (some-> (:test (:until b)) vector) (trees (:bindings b)) (:next b))
              [(second b)]))
          bindings))

(defn ir
  "Field form `form` as {:params [sym ..] :bindings [[\"v0\" tree] ..]
   :rgb [tree tree tree]}, as a Result: (err {:error :field/bad ..}) when
   the form does not lower, (err {:error :field/unsupported ..}) when a
   construct has no GLSL equivalent."
  [form]
  (let [lowered (try (r/ok (fld/lower form))
                     (catch clojure.lang.ExceptionInfo e
                       (r/err :field/bad {:target :glsl :message (ex-message e) :form (:form (ex-data e))})))]
    (r/let-ok [{:keys [params bindings rgb]} lowered
               bs (bindings-ir expr-ir bindings)
               cs (ir-all expr-ir rgb)]
      (r/ok {:params params :bindings bs :rgb cs}))))

;; ── Pipeline: tree -> GLSL text ────────────────────────────────────────────

(defn literal
  "Double `d` as a GLSL float literal (always with a point or exponent)."
  [d]
  (let [s (str (double d))]
    (if (neg? (double d)) (str "(" s ")") s)))

(defn expr-text
  "GLSL text of expression tree `t`, fully parenthesised."
  [t]
  (cond
    (number? t) (literal t)
    (string? t) t
    :else (case (first t)
            :neg (str "(-" (expr-text (second t)) ")")
            :bin (let [[_ op a b] t] (str "(" (expr-text a) " " op " " (expr-text b) ")"))
            :call (let [[_ f & as] t] (str f "(" (str/join ", " (map expr-text as)) ")")))))

(defn- called
  "The GLSL function names tree `t` calls."
  [t]
  (if (vector? t)
    (into (if (= :call (first t)) #{(second t)} #{}) (mapcat called (rest t)))
    #{}))

(defn test-text
  "Text of condition tree `c` (see `test-ir`), expressions through `expr-text`."
  [expr-text c]
  (case (first c)
    :cmp (let [[_ op a b] c] (str "(" (expr-text a) " " op " " (expr-text b) ")"))
    :and (str "(" (test-text expr-text (nth c 1)) " && " (test-text expr-text (nth c 2)) ")")
    :or (str "(" (test-text expr-text (nth c 1)) " || " (test-text expr-text (nth c 2)) ")")
    :not (str "(!" (test-text expr-text (nth c 1)) ")")))

(def dialect
  "How GLSL spells the statements of `statements-text`."
  {:let (fn [s e] (str "float " s " = " e ";"))
   :var (fn [s e] (str "float " s " = " e ";"))
   :set (fn [s e] (str s " = " e ";"))
   :for (fn [i n] (str "for (int " i "_i = 0; " i "_i < " n "; " i "_i++) {"))
   :index (fn [i] (str "float " i " = float(" i "_i);"))})

(defn statements-text
  "Tree `bindings` (see `bindings-ir`) as statements, one per line at
   `indent`, in dialect `d` (see `dialect`) with expressions through
   `expr-text`. A loop node becomes its state variables then
     for (i_i = 0; i_i < N; i_i++) {
       i = float(i_i);  <until bindings>  if (cond) { break; }
       <body bindings>  s_next = ..;  s = s_next;
     }
   the next state computed into temporaries first, so every state
   updates at once, as on the JVM."
  [d expr-text indent bindings]
  (let [ind (fn [k] (apply str (repeat k "  ")))]
    (apply str
           (for [b bindings]
             (if-not (map? b)
               (str (ind indent) ((:let d) (first b) (expr-text (second b))) "\n")
               (let [{:keys [loop n state until next] :as node} b
                     in (inc indent)]
                 (str (apply str (for [[s t] state] (str (ind indent) ((:var d) s (expr-text t)) "\n")))
                      (ind indent) ((:for d) loop n) "\n"
                      (ind in) ((:index d) loop) "\n"
                      (when until
                        (str (statements-text d expr-text in (:bindings until))
                             (ind in) "if " (test-text expr-text (:test until)) " { break; }\n"))
                      (statements-text d expr-text in (:bindings node))
                      (apply str (for [[[s] t] (map vector state next)]
                                   (str (ind in) ((:let d) (str s "_next") (expr-text t)) "\n")))
                      (apply str (for [[s] state] (str (ind in) ((:set d) s (str s "_next")) "\n")))
                      (ind indent) "}\n")))))))

(defn function-text
  "{:helpers text :function text} of tree `tree` (see `ir`): the helper
   functions it calls, and
     vec3 desargues_field(float fx, float fy, float p0, ..) { .. }
   returning [r g b] (unclamped; the caller clamps)."
  [{:keys [params bindings rgb]}]
  (let [used (into (called (into [:x] rgb)) (mapcat called) (trees bindings))
        args (into ["float fx" "float fy"] (map #(str "float p" %) (range (count params))))]
    {:helpers (str/join "\n" (keep helpers (sort used)))
     :function (str "vec3 " function-name "(" (str/join ", " args) ") {\n"
                    (statements-text dialect expr-text 1 bindings)
                    "  return vec3(" (str/join ", " (map expr-text rgb)) ");\n"
                    "}\n")}))

;; ── A CPU reading of the emitted tree ──────────────────────────────────────

(defn d-pow
  "d_pow (see `helpers`) on the JVM."
  ^double [^double x ^double y]
  (cond
    (pos? x) (Math/pow x y)
    (zero? x) (cond (pos? y) 0.0 (zero? y) 1.0 :else 1.0e30)
    (not= (Math/floor y) y) 0.0
    :else (let [m (Math/pow (- x) y)] (if (even? (long y)) m (- m)))))

(def ^:private glsl-fns
  {"sin" #(Math/sin %) "cos" #(Math/cos %) "tan" #(Math/tan %) "exp" #(Math/exp %)
   "log" #(Math/log %) "sqrt" #(Math/sqrt %) "abs" #(Math/abs (double %))
   "floor" #(Math/floor %) "atan" #(Math/atan2 %1 %2) "d_pow" d-pow
   "min" #(Math/min (double %1) (double %2)) "max" #(Math/max (double %1) (double %2))})

(defn run-bindings
  "Env `env` (name -> double) after tree `bindings` (see `bindings-ir`),
   each expression read by (ev env tree), loops run as the emitted code
   runs them: the index as a float of the counter, the :until check
   first, every next state computed from the old one and then assigned."
  [ev env bindings]
  (let [holds? (fn holds? [env c]
                 (case (first c)
                   :cmp (let [[_ op a b] c, a (double (ev env a)), b (double (ev env b))]
                          (case op "<" (< a b) ">" (> a b) "<=" (<= a b) ">=" (>= a b)))
                   :and (and (holds? env (nth c 1)) (holds? env (nth c 2)))
                   :or (or (holds? env (nth c 1)) (holds? env (nth c 2)))
                   :not (not (holds? env (nth c 1)))))]
    (reduce
     (fn [env b]
       (if-not (map? b)
         (assoc env (first b) (ev env (second b)))
         (let [{idx :loop :keys [n state until next] :as node} b
               names (mapv first state)]
           (loop [k 0
                  env (reduce (fn [e [s t]] (assoc e s (ev env t))) env state)]
             (if (>= k (long n))
               env
               (let [e (assoc env idx (ev env (double k)))
                     e (if until (run-bindings ev e (:bindings until)) e)]
                 (if (and until (holds? e (:test until)))
                   env
                   (let [e (run-bindings ev e (:bindings node))
                         nx (mapv #(ev e %) next)]
                     (recur (inc k) (into env (map vector names nx)))))))))))
     env bindings)))

(defn interpret
  "[r g b] of tree `tree` (see `ir`) at scene point (`x`, `y`) with param
   values `args`, read the way the emitted GLSL reads it. `round` is applied
   to every intermediate value: identity for doubles, (comp double float)
   to mimic a GPU's float32."
  ([tree x y args] (interpret tree x y args identity))
  ([{:keys [bindings rgb]} x y args round]
   (let [env0 (into {"fx" (round (double x)) "fy" (round (double y))}
                    (map-indexed (fn [i a] [(str "p" i) (round (double a))])) args)
         ev (fn ev [env t]
              (round
               (cond
                 (number? t) (double t)
                 (string? t) (double (env t))
                 :else (case (first t)
                         :neg (- (double (ev env (second t))))
                         :bin (let [[_ op a b] t, a (ev env a), b (ev env b)]
                                (case op "+" (+ a b) "-" (- a b) "*" (* a b) "/" (/ a b)))
                         :call (let [[_ f & as] t] (apply (glsl-fns f) (map #(ev env %) as)))))))
         env (run-bindings ev env0 bindings)]
     (mapv #(ev env %) rgb))))

(defmethod fld/source :glsl [_ form _opts]
  (r/let-ok [tree (ir form)]
    (r/ok (assoc (function-text tree) :params (:params tree) :ir tree))))
