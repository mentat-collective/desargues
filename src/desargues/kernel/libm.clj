(ns desargues.kernel.libm
  "Elementary functions as kernel forms, for targets whose own are not
   accurate enough. Pure: every function here returns data (a form built from
   +, -, *, /, a floor and value-position if), and nothing is evaluated.

   Why: raster's wasm backend has no sin/cos opcode, and lowers them to a
   degree-9 minimax (error up to 5e-6). That is fine for a picture and wrong
   for an integration: on the chaotic planar double pendulum it moved a 3 s
   RK4 trajectory by 3e-3 (these forms: 6e-14). They are fdlibm's: a 3-part
   Cody-Waite reduction by pi/2, then __kernel_sin / __kernel_cos on
   [-pi/4, pi/4]. Measured against java.lang.Math for |x| < 1e5 (the
   reduction multiplies k by a 33-bit pi/2 head, exact while k < 2^20):
   absolute error at most 2^-53, relative at most 2 ulps (1 on [-20, 20])
   except at results within ~1e-15 of a zero, where the reduction's ~1e-30
   residue is many ulps of a tiny number. desargues.kernel.libm-test pins it.

   `with-precise-trig` rewrites a form's (Math/sin x) and (Math/cos x) into
   these, so a backend can lower an Emmy kernel and keep its accuracy."
  (:require [clojure.walk :as walk]))

(def ^:private sin-coeffs
  "fdlibm __kernel_sin S1..S6."
  [-1.66666666666666324348e-01 8.33333333332248946124e-03 -1.98412698298579493134e-04
   2.75573137070700676789e-06 -2.50507602534068634195e-08 1.58969099521155010221e-10])

(def ^:private cos-coeffs
  "fdlibm __kernel_cos C1..C6."
  [4.16666666666666019037e-02 -1.38888888888741095749e-03 2.48015872894767294178e-05
   -2.75573143513906633035e-07 2.08757232129817482790e-09 -1.13596475577881948265e-11])

(def ^:private two-over-pi 6.36619772367581382433e-01)

(def ^:private pio2-parts
  "pi/2 as three doubles, head first (fdlibm pio2_1, pio2_2, pio2_3)."
  [1.57079632673412561417e+00 6.07710050630396597660e-11 2.02226624871116645580e-21])

(defn- horner
  "c0 + z (c1 + z (c2 + ...)) as a form."
  [z coeffs]
  (reduce (fn [acc c] (list '+ c (list '* z acc))) (last coeffs) (reverse (butlast coeffs))))

(defn trig-form
  "The form computing (sin x) for `which` :sin, or (cos x) for :cos. `floor`
   is the symbol the target spells floor with (Math/floor on the JVM and in
   raster). Locals are prefixed lm- so they cannot capture a name in `x`;
   `x` itself is evaluated once."
  [which x floor]
  (let [[p1 p2 p3] pio2-parts]
    (list 'let*
          ['lm-x x
           'lm-k (list floor (list '+ (list '* 'lm-x two-over-pi) 0.5))
           'lm-r (list '- (list '- (list '- 'lm-x (list '* 'lm-k p1)) (list '* 'lm-k p2)) (list '* 'lm-k p3))
           'lm-q (list '- 'lm-k (list '* 4.0 (list floor (list '* 'lm-k 0.25))))
           'lm-z (list '* 'lm-r 'lm-r)
           'lm-s (list '+ 'lm-r (list '* (list '* 'lm-z 'lm-r) (horner 'lm-z sin-coeffs)))
           'lm-hz (list '* 0.5 'lm-z)
           'lm-w (list '- 1.0 'lm-hz)
           'lm-c (list '+ 'lm-w (list '+ (list '- (list '- 1.0 'lm-w) 'lm-hz)
                                      (list '* (list '* 'lm-z 'lm-z) (horner 'lm-z cos-coeffs))))]
          ;; quadrant q = k mod 4: sin is s, c, -s, -c; cos is c, -s, -c, s
          (let [[a b c d] (case which
                            :sin '[lm-s lm-c (- 0.0 lm-s) (- 0.0 lm-c)]
                            :cos '[lm-c (- 0.0 lm-s) (- 0.0 lm-c) lm-s])]
            (list 'if (list '< 'lm-q 0.5) a
                  (list 'if (list '< 'lm-q 1.5) b
                        (list 'if (list '< 'lm-q 2.5) c d)))))))

(def ^:private trig-heads
  '{Math/sin :sin Math/cos :cos})

(defn with-precise-trig
  "`form` with every (Math/sin x) and (Math/cos x) replaced by trig-form,
   spelling floor as `floor` (default Math/floor). Rewrites bottom-up, so a
   nested call's argument is already rewritten."
  ([form] (with-precise-trig form 'Math/floor))
  ([form floor]
   (walk/postwalk
    (fn [x]
      (if-let [which (and (seq? x) (trig-heads (first x)))]
        (trig-form which (second x) floor)
        x))
    form)))
