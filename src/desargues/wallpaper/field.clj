(ns desargues.wallpaper.field
  "Shaders as data: a FIELD FORM describes a per-pixel colour so the same
   source runs on every field-raster backend (desargues.wallpaper.render).

   A closure (fn ^long [^double x ^double y] 0xRRGGBB) runs only on the JVM.
   A field form is plain data each backend compiles for itself:

     {:params [u k]                         ; per-frame scalars
      :body   '(let [r2 (+ (* x x) (* y y))]
                 [(fract (* k r2)) (cos u) 0.5])}

   x and y are the sample's scene coordinates (y up); the body ends, through
   any number of lets, in [r g b], each channel a double in [0, 1].
   Vocabulary (closed):
     + - * /  sin cos tan exp log sqrt pow atan2 abs floor min max
     fract mix clamp  let
     frac wave osc  (the loop helpers of desargues.wallpaper.loop)
     smoothstep cospal  (GLSL's smoothstep; a cosine palette channel)
   and the constants pi and tau. Number literals become doubles. `bind` fixes the params for one frame:
   (bind form u k) is a BOUND field, what a loop spec's :field may return in
   place of a closure.

   `lower` normalises a form to straight-line code, the one shape every
   backend generates from: {:params [..] :bindings [[sym expr] ..] :rgb [r g b]},
   lets flattened and renamed apart (v0 v1 ..), params renamed p0 p1 .., x y
   renamed fx fy, the derived ops (fract mix clamp) expanded, every op binary
   (or unary -) and spelled as its java.lang.Math method. `jvm-shader` evals
   the lowered code once per form into a primitive fn; the raster backends
   (desargues.infrastructure.raster-field) emit raster kernels from it, and
   `source` (an open port keyed by target) prints shader text from it:
   :glsl is desargues.wallpaper.glsl, packaged as ISF / .frag layers by
   desargues.wallpaper.emit (doc/glsl-layers.md); :wgsl is
   desargues.wallpaper.wgsl, packaged as fosfora trama sources by emit's
   :target :trama (desargues.wallpaper.trama, doc/trama-layers.md).

   Channels quantise the same way everywhere: (quantize v) =
   floor(255 clamp(v, 0, 1) + 0.5)."
  (:require [desargues.backend :as backend]))

(set! *warn-on-reflection* true)

(def vocabulary
  "The symbols a field body may apply, besides let. The loop helpers
   (wave u) (osc u lo hi) (frac x) mirror desargues.wallpaper.loop;
   (smoothstep e0 e1 x) is GLSL's; (cospal t a b c d) is a cosine palette
   channel a + b cos 2π(c t + d). All derived ops lower to the primitive ones."
  '#{+ - * / sin cos tan exp log sqrt pow atan2 abs floor min max fract mix clamp
     frac wave osc smoothstep cospal})

(def forms
  "The special forms a field body may use besides the vocabulary: let, and
   the bounded loop loop-n (see `lower`)."
  '#{let loop-n})

(def max-loop-n
  "The largest literal bound a loop-n may take."
  256)

(def conditions
  "The symbols a loop-n :until condition may apply: comparisons of two
   expressions, and and/or/not of conditions."
  '#{< > <= >= and or not})

(def constants
  "Named constants a field body may use: pi and tau (2π), unless a param or
   let shadows them."
  {'pi Math/PI 'tau (* 2.0 Math/PI)})

(def ^:private tau (* 2.0 Math/PI))

(def ^:private math
  '{sin Math/sin cos Math/cos tan Math/tan exp Math/exp log Math/log sqrt Math/sqrt
    abs Math/abs floor Math/floor pow Math/pow atan2 Math/atan2 min Math/min max Math/max})

(def ^:private arity
  '{sin 1 cos 1 tan 1 exp 1 log 1 sqrt 1 abs 1 floor 1 pow 2 atan2 2 min 2 max 2
    fract 1 frac 1 mix 3 clamp 3 wave 1 osc 3 smoothstep 3 cospal 5})

(defn- bad [msg form]
  (throw (ex-info (str "Field form: " msg ": " (pr-str form)) {:reason :bad-field :form form})))

(defn- trivial? [e] (or (symbol? e) (number? e)))

(def ^:private ^:dynamic *names*
  "Volatile counter of the vN names the running `lower` has handed out."
  nil)

(defn- fresh
  "The next lowered name: v0 v1 .., in the order bindings are pushed."
  []
  (let [n (long @*names*)] (vswap! *names* inc) (symbol (str "v" n))))

(defn- loop-n? [form] (and (seq? form) (= 'loop-n (first form))))

(declare lower-expr lower-loop)

(defn- lower-let
  "Lower let `form`'s bindings under `env` into `out`; returns
   [env' body-form]. A binding [s0 s1 ..] (loop-n ..) binds a loop's final
   state, one symbol per state; s (loop-n ..) a one-state loop's."
  [env out form]
  (let [[_ bv & body] form]
    (when-not (and (vector? bv) (even? (count bv)) (= 1 (count body)))
      (bad "let takes [sym expr ..] and one body" form))
    [(reduce (fn [e [s v]]
               (cond
                 (loop-n? v)
                 (let [ss (if (vector? s) s [s])]
                   (when-not (every? simple-symbol? ss) (bad "let binds plain symbols" form))
                   (let [outs (lower-loop e out v)]
                     (when-not (= (count ss) (count outs))
                       (bad (str "loop-n has " (count outs) " state(s), bound to " (count ss) " symbol(s)") form))
                     (into e (map vector ss outs))))

                 (simple-symbol? s)
                 (let [l (lower-expr e out v) g (fresh)]
                   (vswap! out conj [g l])
                   (assoc e s g))

                 :else (bad "let binds plain symbols (a vector of them only for loop-n)" form)))
             env (partition 2 bv))
     (first body)]))

(defn- lower-test
  "Lower :until condition `form` under `env`, pushing the bindings it
   introduces onto `out`; returns the lowered condition: (< a b) (> a b)
   (<= a b) (>= a b) over lowered expressions, (and c d) (or c d) (not c)."
  [env out form]
  (when-not (and (seq? form) (seq form))
    (bad "an :until condition is a comparison (< > <= >=), or and/or/not of conditions" form))
  (let [[op & args] form
        n (count args)]
    (case op
      (< > <= >=) (do (when-not (= 2 n) (bad (str op " takes 2 arguments") form))
                      (let [a (lower-expr env out (first args))]
                        (list op a (lower-expr env out (second args)))))
      (and or) (do (when (zero? n) (bad (str op " takes at least one condition") form))
                   (reduce (fn [a b] (list op a (lower-test env out b)))
                           (lower-test env out (first args)) (rest args)))
      not (do (when-not (= 1 n) (bad "not takes 1 condition" form))
              (list 'not (lower-test env out (first args))))
      let (let [[env' body] (lower-let env out form)] (lower-test env' out body))
      (bad (str "unknown condition op " op "; conditions use " (sort conditions)) form))))

(defn- lower-next
  "Lower loop body `form` (lets ending in a vector of `k` expressions)
   under `env` into `out`; returns the lowered next-state expressions."
  [env out form k]
  (cond
    (and (seq? form) (= 'let (first form)))
    (let [[env' body] (lower-let env out form)] (lower-next env' out body k))

    (and (vector? form) (= k (count form)))
    (mapv #(lower-expr env out %) form)

    :else (bad (str "a loop-n body must end in a vector of its " k " next state(s)") form)))

(defn- lower-loop
  "Lower (loop-n [i N] [s0 init0 ..] body :until cond) under `env`: pushes
   one loop node onto `out` and returns the lowered state symbols, which
   hold the final state after it (see `lower`)."
  [env out form]
  (let [[_ iv sv body & opts] form
        [isym n] (when (vector? iv) iv)]
    (when-not (and (vector? iv) (= 2 (count iv)) (simple-symbol? isym)
                   (integer? n) (<= 1 n max-loop-n))
      (bad (str "loop-n takes [i N], N a literal integer in [1, " max-loop-n "]") form))
    (when-not (and (vector? sv) (seq sv) (even? (count sv)) (every? simple-symbol? (take-nth 2 sv)))
      (bad "loop-n takes a state vector [s0 init0 s1 init1 ..] of plain symbols" form))
    (when-not (or (empty? opts) (and (= 2 (count opts)) (= :until (first opts))))
      (bad "loop-n takes [i N] [state ..] body, then optionally :until cond" form))
    (let [pairs (partition 2 sv)
          inits (mapv #(lower-expr env out (second %)) pairs)
          fi (fresh)
          ss (mapv (fn [_] (fresh)) pairs)
          benv (into (assoc env isym fi) (map vector (map first pairs) ss))
          until (when (seq opts)
                  (let [cout (volatile! [])
                        t (lower-test benv cout (second opts))]
                    {:bindings @cout :test t}))
          bout (volatile! [])
          nexts (lower-next benv bout body (count pairs))]
      (when (some #(and (map? %) (< 1 (count (:loop/state %)))) (concat (:bindings until) @bout))
        (bad "a loop-n nested in a loop-n body or :until holds one state" form))
      (vswap! out conj {:loop/i fi :loop/n (long n) :loop/state (mapv vector ss inits)
                        :loop/until until :loop/bindings @bout :loop/next nexts})
      ss)))

(defn- lower-expr
  "Lower expression `form` under `env` (source symbol -> lowered symbol or
   constant), pushing the bindings it introduces onto volatile `out`;
   returns the lowered expression."
  [env out form]
  (let [sub (fn [e] (lower-expr env out e))
        share (fn [e] (if (trivial? e) e
                          (let [s (fresh)] (vswap! out conj [s e]) s)))
        fold (fn [op args] (reduce (fn [a b] (list op a (sub b))) (sub (first args)) (rest args)))
        wave (fn [u] (list '* 0.5 (list '+ 1.0 (list 'Math/cos (list '* tau u)))))]
    (cond
      (number? form) (double form)
      (symbol? form) (let [v (env form)] (if (nil? v) (bad "unbound symbol" form) v))
      (and (seq? form) (seq form))
      (let [[op & args] form
            n (count args)]
        (when-let [k (arity op)]
          (when-not (= k n) (bad (str op " takes " k " argument(s)") form)))
        (case op
          let (let [[env' body] (lower-let env out form)] (lower-expr env' out body))
          loop-n (let [ss (lower-loop env out form)]
                   (if (= 1 (count ss)) (first ss)
                       (bad (str "a loop-n with " (count ss) " states is bound with (let [[s0 s1 ..] (loop-n ..)] ..)") form)))
          + (if (zero? n) 0.0 (fold '+ args))
          * (if (zero? n) 1.0 (fold '* args))
          - (case n 0 (bad "- takes at least one argument" form) 1 (list '- (sub (first args))) (fold '- args))
          / (case n 0 (bad "/ takes at least one argument" form) 1 (list '/ 1.0 (sub (first args))) (fold '/ args))
          (fract frac) (let [t (share (sub (first args)))] (list '- t (list 'Math/floor t)))
          mix (let [[a b t] args, a (share (sub a))]
                (list '+ a (list '* (list '- (sub b) a) (sub t))))
          clamp (let [[v lo hi] args]
                  (list 'Math/min (list 'Math/max (sub v) (sub lo)) (sub hi)))
          wave (wave (sub (first args)))
          osc (let [[u lo hi] args, lo (share (sub lo))]
                (list '+ lo (list '* (list '- (sub hi) lo) (list '- 1.0 (wave (sub u))))))
          smoothstep (let [[e0 e1 v] args, e0 (share (sub e0))
                           t (share (list 'Math/min (list 'Math/max (list '/ (list '- (sub v) e0) (list '- (sub e1) e0)) 0.0) 1.0))]
                       (list '* t (list '* t (list '- 3.0 (list '* 2.0 t)))))
          cospal (let [[t a b c d] args]
                   (list '+ (sub a) (list '* (sub b) (list 'Math/cos (list '* tau (list '+ (list '* (sub c) (sub t)) (sub d)))))))
          (if-let [m (math op)]
            (apply list m (mapv sub args)) ; eager: bindings pushed in evaluation order
            (bad (str "unknown op " op "; the vocabulary is " (sort vocabulary)) form))))
      :else (bad "not an expression" form))))

(defn- lower-tail [env out form]
  (cond
    (and (seq? form) (= 'let (first form)))
    (let [[env' body] (lower-let env out form)] (lower-tail env' out body))

    (and (vector? form) (= 3 (count form)))
    (mapv #(lower-expr env out %) form)

    :else (bad "the body must end in [r g b]" form)))

(defn lower
  "Field form {:params [sym ..] :body form} as straight-line code
   {:params [sym ..] :bindings [[sym expr] ..] :rgb [r g b]} (see ns doc).
   Throws ex-info {:reason :bad-field} naming the offending form."
  [{:keys [params] :as form}]
  (when-not (and (map? form) (contains? form :body))
    (bad "a field form is {:params [..] :body ..}" form))
  (let [params (vec params)
        env (into (merge constants {'x 'fx 'y 'fy})
                  (map-indexed (fn [i p] [p (symbol (str "p" i))])) params)
        out (volatile! [])
        rgb (binding [*names* (volatile! 0)] (lower-tail env out (:body form)))]
    {:params params :bindings @out :rgb rgb}))

(defn loop-node?
  "True for a loop node of lowered :bindings (see `lower`); every other
   entry is a [sym expr] pair."
  [b]
  (map? b))

(defn bound-names
  "The symbols lowered binding `b` defines: [sym] for a pair, the loop's
   state symbols for a loop node (its index and body bindings are local
   to it)."
  [b]
  (if (loop-node? b) (mapv first (:loop/state b)) [(first b)]))

(defn test-form
  "Lowered :until condition `t` as Clojure built from comparisons and if
   alone (and/or/not expanded), the shape the raster kernels compile too."
  [t]
  (case (first t)
    (< > <= >=) t
    and (list 'if (test-form (nth t 1)) (test-form (nth t 2)) false)
    or (list 'if (test-form (nth t 1)) true (test-form (nth t 2)))
    not (list 'if (test-form (nth t 1)) false true)))

(declare value-bindings)

(defn loop-form
  "Lowered loop node `node` as one primitive Clojure loop over doubles that
   evaluates `tail` (a form over the state symbols, the rest of the code)
   once it stops:
     (loop [i 0.0 s0 init0 ..]
       (if (< i N)
         (let [<until bindings>]
           (if test
             (recur N s0 ..)                        ; break: state kept
             (let [<body bindings>] (recur (+ i 1.0) next0 ..))))
         tail))
   One exit, no allocation, nothing but loop/recur/let/if: the JVM shader
   and the raster kernels compile the same form. The index counts 0.0,
   1.0, .. exactly."
  [node tail]
  (let [{:loop/keys [i n state until bindings next]} node
        inner (fn [bs t] (list 'let (value-bindings bs) t))
        step (inner bindings (list* 'recur (list '+ i 1.0) next))]
    (list 'loop (into [i 0.0] cat state)
          (list 'if (list '< i (double n))
                (if until
                  (inner (:bindings until)
                         (list* 'if (test-form (:test until))
                                [(list* 'recur (double n) (map first state)) step]))
                  step)
                tail))))

(defn- symbols-of [x]
  (cond (symbol? x) #{x}
        (map? x) (into #{} (mapcat symbols-of) (vals x))
        (coll? x) (into #{} (mapcat symbols-of) x)
        :else #{}))

(defn value-bindings
  "Lowered `bindings` as one Clojure let vector for code whose tail recurs
   (and so cannot sit in a nested loop's exit): each loop node bound by
   value, [s (loop-form node s)] for each state s the rest of the code
   (`tail`, any forms) references, a loop of N iterations per such state.
   `lower` admits only one-state loops inside a loop; the raster kernels
   bind every loop this way."
  ([bindings] (value-bindings bindings nil))
  ([bindings tail]
   (let [bs (vec bindings)]
     (into []
           (mapcat (fn [k]
                     (let [b (bs k)]
                       (if-not (loop-node? b)
                         b
                         (let [ss (mapv first (:loop/state b))
                               used (if (= 1 (count ss)) (set ss)
                                        (symbols-of [(subvec bs (inc k)) tail]))]
                           (into [] (mapcat (fn [s] (when (used s) [s (loop-form b s)]))) ss))))))
           (range (count bs))))))

(defn let-form
  "Lowered `bindings` then `tail`, as Clojure: the pairs as one let, each
   loop node as a loop-form whose exit is the rest of the bindings and
   `tail` (which must not recur). Without loop nodes this is exactly
   (let [s e ..] tail)."
  [bindings tail]
  (let [[pairs [node & more]] (split-with (complement loop-node?) bindings)]
    (list 'let (into [] cat pairs)
          (if node (loop-form node (let-form more tail)) tail))))

(defn quantize
  "Channel `v` (clamped to [0, 1]) as a level 0..255, as a double."
  ^double [^double v]
  (Math/floor (+ (* 255.0 (Math/min (Math/max v 0.0) 1.0)) 0.5)))

(defn pack
  "0xRRGGBB of channels in [0, 1], each through `quantize`."
  ^long [^double r ^double g ^double b]
  (+ (* 65536 (long (quantize r))) (* 256 (long (quantize g))) (long (quantize b))))

;; ── bound fields ─────────────────────────────────────────────────────────

(defn bind
  "Field form `form` with its params fixed to `args` (numbers, in :params
   order) for one frame: {:field/form form :field/args [double ..]}."
  [form & args]
  (when-not (= (count (:params form)) (count args))
    (throw (ex-info (str "Field form takes " (count (:params form)) " params " (pr-str (:params form))
                         ", got " (count args))
                    {:reason :bad-field :params (:params form) :args args})))
  {:field/form form :field/args (mapv double args)})

(defn bound?
  "True for a bound field (see bind)."
  [x]
  (and (map? x) (contains? x :field/form)))

;; ── Port: field form -> shader source text ────────────────────────────────

(defmulti source
  "Field form `form` as shader source for `target` (a backend key, open:
   :glsl is desargues.wallpaper.glsl), at target-specific `opts`. Returns a
   hive-dsl Result: (ok compiled) or (err {:error :field/unsupported ..})
   naming the construct the target cannot express; never drops one. The
   :default loads the target's provider through desargues.backend."
  (fn [target _form _opts] target))

(defmethod source :default [k form opts]
  (backend/dispatch-default source "field source compiler" k [form opts]))

(backend/register-backend! :glsl
                           {:namespaces '[desargues.wallpaper.glsl]
                            :requires nil})

(backend/register-backend! :wgsl
                           {:namespaces '[desargues.wallpaper.wgsl]
                            :requires nil})

;; ── JVM: the lowered code as a primitive fn ──────────────────────────────

(defn jvm-form
  "(fn [ps] (fn ^long [^double fx ^double fy] packed)) of field form
   `form`: the params read from double[] ps once per frame, then per sample
   the lowered bindings and `pack`. Built without syntax-quote around the
   hints, which would qualify them (clojure.core/double is no primitive)."
  [form]
  (let [{:keys [params bindings rgb]} (lower form)
        ps (with-meta 'ps {:tag 'doubles})
        sig (with-meta [(with-meta 'fx {:tag 'double}) (with-meta 'fy {:tag 'double})] {:tag 'long})]
    (list 'fn [ps]
          (list 'let (vec (mapcat (fn [i] [(symbol (str "p" i)) (list 'aget ps i)]) (range (count params))))
                (list 'fn 'shade sig
                      (let-form bindings (cons `pack rgb)))))))

(def ^:private compiled
  "Field form -> its evaluated jvm-form."
  (atom {}))

(defn- jvm-fn [form]
  (or (get @compiled form)
      (let [f (binding [*ns* (the-ns 'desargues.wallpaper.field)] (eval (jvm-form form)))]
        (get (swap! compiled #(if (contains? % form) % (assoc % form f))) form))))

(defn jvm-shader
  "Bound field `bf` as a shader (fn ^long [^double x ^double y] 0xRRGGBB):
   the form compiled once (cached by value), its params applied."
  [bf]
  ((jvm-fn (:field/form bf)) (double-array (:field/args bf))))

;; ── an exemplar ──────────────────────────────────────────────────────────

(def quasicrystal
  "A 7-fold quasicrystal: 7 plane waves at angles kπ/7, k in [0, 7), their
   phase advancing 2πu (so it loops), summed and coloured through a cosine
   palette. Params [u scale]: phase and wave number (radians per unit)."
  (let [tau (* 2.0 Math/PI)
        waves (for [k (range 7)
                    :let [a (/ (* k Math/PI) 7.0)]]
                (list 'cos (list '+ (list '* 'scale (list '+ (list '* (Math/cos a) 'x) (list '* (Math/sin a) 'y)))
                                 (list '* tau 'u))))
        channel (fn [off] (list '+ 0.5 (list '* 0.5 (list 'cos (list '* tau (list '+ 't off))))))]
    {:params '[u scale]
     :body (list 'let ['s (list '/ (cons '+ waves) 7.0)
                       't (list 'fract (list '+ (list '* 0.5 's) 0.5))]
                 [(channel 0.0) (channel 0.33) (channel 0.67)])}))
