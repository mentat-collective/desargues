(ns desargues.wallpaper.wgsl
  "The :wgsl backend of field forms (desargues.wallpaper.field): one field
   source compiled to a WGSL function, the shading language of WebGPU (wgpu,
   naga). desargues.wallpaper.trama packages it as a fosfora trama source.
   Pure: no file, no GPU.

   Stratified CPPB, as desargues.wallpaper.glsl:
     Collect  `fld/lower`: the form as straight-line code.
     Promote  `ir`: the lowered code as a WGSL expression tree, data:
                number | \"name\" | [:neg a] | [:bin \"+\" a b] | [:call \"sin\" a ..]
              Sub-trees of literals only are folded here, in double: WGSL
              evaluates constant expressions when the shader is created and
              rejects a fold that overflows or leaves the domain (1.0 / 0.0,
              sqrt(-1.0)), so none reaches the text. A construct WGSL cannot
              express is an (err {:error :field/unsupported :target :wgsl
              :construct ..}) naming it, never dropped. A loop-n node stays
              one node (glsl/bindings-ir) and prints as
              for (var v0_i: i32 = 0; v0_i < N; v0_i++) { .. break; .. }
     Pipeline `function-text`:
                fn desargues_field(fx: f32, fy: f32, p0: f32, ..) -> vec3f
              plus the helpers it calls, all prefixed desargues_.
     Boundary none here; desargues.wallpaper.emit writes the files.

   `interpret` reads the same tree on the CPU (in double, or rounding every
   step to float32): the parity check against the JVM shader.

   Semantic notes. WGSL's pow is undefined for x < 0, so every pow compiles
   to desargues_pow, which follows Math/pow on the reals (an integer exponent
   of a negative base keeps its sign) and returns 0 where Math/pow is NaN,
   1e30 where it is infinite: the same helper as GLSL's d_pow. WGSL has no
   ?: (select instead) and no implicit int -> float; every literal is
   printed with a point. f32 throughout."
  (:require [clojure.string :as str]
            [desargues.wallpaper.field :as fld]
            [desargues.wallpaper.glsl :as glsl]
            [hive-dsl.result :as r]))

(def function-name
  "The name of the emitted field function."
  "desargues_field")

(def ^:private calls
  "Lowered java.lang.Math op -> [WGSL function, arity]."
  '{Math/sin ["sin" 1] Math/cos ["cos" 1] Math/tan ["tan" 1] Math/exp ["exp" 1]
    Math/log ["log" 1] Math/sqrt ["sqrt" 1] Math/abs ["abs" 1] Math/floor ["floor" 1]
    Math/atan2 ["atan2" 2] Math/pow ["desargues_pow" 2] Math/min ["min" 2] Math/max ["max" 2]})

(def ^:private binops '#{+ - * /})

(def f32-max
  "The largest finite f32; a literal beyond it is no WGSL f32."
  3.4028234663852886E38)

(def helpers
  "WGSL helper name -> its source."
  {"desargues_pow"
   (str "fn desargues_pow(x: f32, y: f32) -> f32 {\n"
        "  if (x > 0.0) { return pow(x, y); }\n"
        "  if (x == 0.0) { return select(select(1.0e30, 1.0, y == 0.0), 0.0, y > 0.0); }\n"
        "  let n = floor(y);\n"
        "  if (n != y) { return 0.0; }\n"
        "  let m = pow(-x, y);\n"
        "  return select(-m, m, n - 2.0 * floor(0.5 * n) == 0.0);\n"
        "}\n")})

(defn- unsupported [construct form why]
  (r/err :field/unsupported {:target :wgsl :construct construct :form form
                             :message (str "WGSL cannot express " (pr-str construct) ": " why)}))

(def ^:private wgsl-fns
  {"sin" #(Math/sin %) "cos" #(Math/cos %) "tan" #(Math/tan %) "exp" #(Math/exp %)
   "log" #(Math/log %) "sqrt" #(Math/sqrt %) "abs" #(Math/abs (double %))
   "floor" #(Math/floor %) "atan2" #(Math/atan2 %1 %2) "desargues_pow" glsl/d-pow
   "min" #(Math/min (double %1) (double %2)) "max" #(Math/max (double %1) (double %2))})

(defn- apply-op
  "Tree node `t` whose arguments are already doubles, evaluated."
  [t]
  (case (first t)
    :neg (- (double (second t)))
    :bin (let [[_ op a b] t, a (double a), b (double b)]
           (case op "+" (+ a b) "-" (- a b) "*" (* a b) "/" (/ a b)))
    :call (let [[_ f & as] t] (double (apply (wgsl-fns f) as)))))

(defn- literal-ok
  "Double `d` as a WGSL f32 literal tree, or the refusal naming `form`."
  [d form]
  (cond
    (not (Double/isFinite d)) (unsupported d form "no WGSL literal for a non-finite number")
    (> (Math/abs d) f32-max) (unsupported d form "beyond the f32 range")
    :else (r/ok d)))

(defn- ir-all [f xs]
  (reduce (fn [acc x] (let [v (f x)] (if (r/ok? v) (update acc :ok conj (:ok v)) (reduced v))))
          (r/ok []) xs))

(defn expr-ir
  "Lowered expression `e` as a WGSL expression tree, literal sub-trees
   folded, as a Result."
  [e]
  (cond
    (number? e) (literal-ok (double e) e)
    (symbol? e) (r/ok (name e))
    (and (seq? e) (seq e))
    (let [[op & args] e
          n (count args)]
      (r/let-ok [as (ir-all expr-ir args)
                 t (cond
                     (and (= op '-) (= n 1)) (r/ok [:neg (first as)])
                     (and (binops op) (= n 2)) (r/ok [:bin (name op) (first as) (second as)])
                     (and (calls op) (= n (second (calls op)))) (r/ok (into [:call (first (calls op))] as))
                     :else (unsupported op e "no WGSL op of this name and arity"))]
        (if (every? number? (rest (if (= :neg (first t)) t (drop 1 t))))
          (literal-ok (apply-op t) e)
          (r/ok t))))
    :else (unsupported e e "not an expression")))

(defn ir
  "Field form `form` as {:params [sym ..] :bindings [[\"v0\" tree] ..]
   :rgb [tree tree tree]}, as a Result: (err {:error :field/bad ..}) when
   the form does not lower, (err {:error :field/unsupported ..}) when a
   construct has no WGSL equivalent."
  [form]
  (let [lowered (try (r/ok (fld/lower form))
                     (catch clojure.lang.ExceptionInfo e
                       (r/err :field/bad {:target :wgsl :message (ex-message e) :form (:form (ex-data e))})))]
    (r/let-ok [{:keys [params bindings rgb]} lowered
               bs (glsl/bindings-ir expr-ir bindings)
               cs (ir-all expr-ir rgb)]
      (r/ok {:params params :bindings bs :rgb cs}))))

(defn literal
  "Double `d` as a WGSL float literal (always with a point or exponent,
   negative ones parenthesised)."
  [d]
  (let [s (str (double d))]
    (if (neg? (double d)) (str "(" s ")") s)))

(defn expr-text
  "WGSL text of expression tree `t`, fully parenthesised."
  [t]
  (cond
    (number? t) (literal t)
    (string? t) t
    :else (case (first t)
            :neg (str "(-" (expr-text (second t)) ")")
            :bin (let [[_ op a b] t] (str "(" (expr-text a) " " op " " (expr-text b) ")"))
            :call (let [[_ f & as] t] (str f "(" (str/join ", " (map expr-text as)) ")")))))

(defn- called
  "The function names tree `t` calls."
  [t]
  (if (vector? t)
    (into (if (= :call (first t)) #{(second t)} #{}) (mapcat called (rest t)))
    #{}))

(def dialect
  "How WGSL spells the statements of glsl/statements-text: a loop-n is
     for (var v0_i: i32 = 0; v0_i < N; v0_i++) { .. if (c) { break; } .. }
   its state in vars, everything else a let."
  {:let (fn [s e] (str "let " s ": f32 = " e ";"))
   :var (fn [s e] (str "var " s ": f32 = " e ";"))
   :set (fn [s e] (str s " = " e ";"))
   :for (fn [i n] (str "for (var " i "_i: i32 = 0; " i "_i < " n "; " i "_i++) {"))
   :index (fn [i] (str "let " i ": f32 = f32(" i "_i);"))})

(defn function-text
  "{:helpers text :function text} of tree `tree` (see `ir`): the helper
   functions it calls, and
     fn desargues_field(fx: f32, fy: f32, p0: f32, ..) -> vec3f { .. }
   returning the unclamped channels (the caller clamps)."
  [{:keys [params bindings rgb]}]
  (let [used (into (called (into [:x] rgb)) (mapcat called) (glsl/trees bindings))
        args (into ["fx: f32" "fy: f32"] (map #(str "p" % ": f32") (range (count params))))]
    {:helpers (str/join "\n" (keep helpers (sort used)))
     :function (str "fn " function-name "(" (str/join ", " args) ") -> vec3f {\n"
                    (glsl/statements-text dialect expr-text 1 bindings)
                    "  return vec3f(" (str/join ", " (map expr-text rgb)) ");\n"
                    "}\n")}))

(defn interpret
  "[r g b] of tree `tree` (see `ir`) at scene point (`x`, `y`) with param
   values `args`, read the way the emitted WGSL reads it. `round` is applied
   to every intermediate value: identity for doubles, (comp double float)
   to mimic f32."
  ([tree x y args] (interpret tree x y args identity))
  ([{:keys [bindings rgb]} x y args round]
   (let [env0 (into {"fx" (round (double x)) "fy" (round (double y))}
                    (map-indexed (fn [i a] [(str "p" i) (round (double a))])) args)
         ev (fn ev [env t]
              (round
               (cond
                 (number? t) (double t)
                 (string? t) (double (env t))
                 :else (apply-op (into [(first t)]
                                       (map-indexed (fn [i a] (if (and (not= :neg (first t)) (zero? i)) a (ev env a))))
                                       (rest t))))))
         env (glsl/run-bindings ev env0 bindings)]
     (mapv #(ev env %) rgb))))

(defmethod fld/source :wgsl [_ form _opts]
  (r/let-ok [tree (ir form)]
    (r/ok (assoc (function-text tree) :params (:params tree) :ir tree))))
