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
   desargues.wallpaper.emit (doc/glsl-layers.md).

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

(declare lower-expr)

(defn- lower-let
  "Lower let `form`'s bindings under `env` into `out`; returns
   [env' body-form]."
  [env out form]
  (let [[_ bv & body] form]
    (when-not (and (vector? bv) (even? (count bv)) (= 1 (count body)))
      (bad "let takes [sym expr ..] and one body" form))
    [(reduce (fn [e [s v]]
               (when-not (simple-symbol? s) (bad "let binds plain symbols" form))
               (let [l (lower-expr e out v) g (symbol (str "v" (count @out)))]
                 (vswap! out conj [g l])
                 (assoc e s g)))
             env (partition 2 bv))
     (first body)]))

(defn- lower-expr
  "Lower expression `form` under `env` (source symbol -> lowered symbol or
   constant), pushing the bindings it introduces onto volatile `out`;
   returns the lowered expression."
  [env out form]
  (let [sub (fn [e] (lower-expr env out e))
        share (fn [e] (if (trivial? e) e
                          (let [s (symbol (str "v" (count @out)))] (vswap! out conj [s e]) s)))
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
        rgb (lower-tail env out (:body form))]
    {:params params :bindings @out :rgb rgb}))

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
                      (list 'let (vec (mapcat identity bindings))
                            (cons `pack rgb)))))))

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
