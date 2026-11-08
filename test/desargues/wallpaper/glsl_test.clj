(ns desargues.wallpaper.glsl-test
  "The :glsl target of field forms. Each test form is compiled to a GLSL
   expression tree; the tree, read on the CPU the way the shader reads it
   (in double, and rounding every step to float32 as a GPU does), is held
   to the JVM shader of the same form within one level of 255 per channel
   over a grid of scene points. Forms: the quasicrystal plane-wave sum,
   Lieb-lattice Fermi contours (tessera's lieb-fermi as a field form),
   graphene |f| bands with a cosine palette, and `rings`, every op."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.field :as fld]
            [desargues.wallpaper.glsl :as glsl]
            [hive-dsl.result :as r]))

(def lieb
  "Two interleaved contour families of the Lieb bands +-E(k), drifting one
   level spacing per loop, and the cusp glow at the zone corner."
  {:params '[u k]
   :body '(let [kx (* k x) ky (* k y)
                cx (cos (* 0.5 kx)) cy (cos (* 0.5 ky))
                s (+ (* cx cx) (* cy cy))
                e (* 2 (sqrt s))
                pos (pow (wave (- (/ e 0.65) u)) 40)
                neg (pow (wave (- 0.5 (/ e 0.65) u)) 40)
                cusp (exp (* -46 s))]
            [(+ (* 0.18 pos) (* 0.77 neg) (* 0.25 cusp))
             (+ (* 0.69 pos) (* 0.43 neg) (* 0.6 cusp))
             (+ (* 0.86 pos) (* 0.26 neg) (* 0.78 cusp))])})

(def graphene
  "Bands of graphene's |f(k)| (lattice constant a), lines rising through
   the bands, coloured by a cosine palette, breathing with osc."
  {:params '[u a bands]
   :body '(let [k1 (* a x) k2 (+ (* -0.5 a x) (* 0.8660254037844386 a y))
                f2 (+ 3 (* 2 (cos k1)) (* 2 (cos k2)) (* 2 (cos (- k1 k2))))
                f (sqrt (max f2 0))
                t (frac (- (* bands f) u))
                line (smoothstep 0.08 0.0 (min t (- 1 t)))
                glow (osc u 0.3 1.0)
                h (/ f 3)]
            [(* glow (+ (* 0.2 line) (cospal h 0.5 0.5 1 0.0)))
             (* glow (+ (* 0.2 line) (cospal h 0.5 0.5 1 0.33)))
             (* glow (+ (* 0.2 line) (cospal h 0.5 0.5 1 0.67)))])})

(def rings
  "Every primitive op, in one shader (as in raster-field-test)."
  {:params '[u k]
   :body '(let [r2 (+ (* x x) (* y y))
                a (atan2 y x)
                w (fract (+ (* k (sqrt r2)) u))]
            [(clamp (mix 0.1 0.9 w) 0 1)
             (+ 0.5 (* 0.5 (sin (+ (* 3 a) (pow (abs x) 1.5)))))
             (/ (+ (exp (- r2)) (* 0.2 (floor (* 2 (cos (- y (tan 0.3)))))) (max 0 (min 1 (log (+ 1 r2))))) 3)])})

(def cases
  "[label form args] compiled and checked below."
  [["quasicrystal" fld/quasicrystal [0.4 3.0]]
   ["lieb" lieb [0.3 3.8]]
   ["graphene" graphene [0.25 (/ (* 4.0 Math/PI) 3.0) 2.0]]
   ["rings" rings [0.25 2.0]]])

(defn- channel-diffs
  "Per-channel level differences between the JVM shader of `form` at `args`
   and the GLSL tree read with `round`, over a 64 x 36 grid of the default
   view."
  [form args round]
  (let [tree (:ir (:ok (fld/source :glsl form {})))
        ^clojure.lang.IFn$DDL sh (fld/jvm-shader (apply fld/bind form args))]
    (for [i (range 64) j (range 36)
          :let [x (* 0.222 (- i 31.5)) y (* 0.222 (- j 17.5))
                c (.invokePrim sh x y)
                g (apply fld/pack (glsl/interpret tree x y args round))]
          s [0 8 16]]
      (Math/abs (- (bit-and (bit-shift-right c s) 255) (bit-and (bit-shift-right g s) 255))))))

(deftest every-case-compiles
  (doseq [[label form] cases]
    (testing label
      (let [res (fld/source :glsl form {})
            {:keys [function params]} (:ok res)]
        (is (r/ok? res))
        (is (= (:params form) params))
        (is (str/starts-with? function (str "vec3 " glsl/function-name "(float fx, float fy")))
        (is (not (str/includes? function "Math/")))))))

(deftest the-glsl-tree-reads-as-the-jvm-shader
  (doseq [[label form args] cases
          [rounding round] [["double" identity] ["float32" (comp double float)]]]
    (testing (str label " " rounding)
      (is (<= (reduce max (channel-diffs form args round)) 1)))))

(deftest what-glsl-cannot-say-is-refused-by-name
  (testing "a non-finite literal has no GLSL spelling"
    (let [res (fld/source :glsl {:params '[u] :body [(/ 1.0 0.0) 0 0]} {})]
      (is (r/err? res))
      (is (= :field/unsupported (:error res)))
      (is (= ##Inf (:construct res)))
      (is (str/includes? (:message res) "GLSL cannot express"))))
  (testing "an op of the lowered code with no GLSL mapping is named"
    (let [res (glsl/expr-ir '(Math/cbrt v0))]
      (is (= :field/unsupported (:error res)))
      (is (= 'Math/cbrt (:construct res)))))
  (testing "a form that does not lower is an err, not a throw"
    (let [res (fld/source :glsl {:params '[u] :body '[(noise x) 0 0]} {})]
      (is (= :field/bad (:error res)))
      (is (= '(noise x) (:form res))))))

(deftest pow-follows-the-jvm-where-glsl-is-undefined
  (doseq [[x y] [[2.0 0.5] [-2.0 3.0] [-2.0 2.0] [-2.0 0.5] [0.0 2.0] [0.0 0.0] [3.0 -1.0]]]
    (let [j (Math/pow x y)]
      (testing (str x " ^ " y)
        (is (= (if (Double/isNaN j) 0.0 j) (glsl/d-pow x y))))))
  (is (str/includes? (:helpers (:ok (fld/source :glsl rings {}))) "float d_pow(float x, float y)")))

(deftest the-loop-helpers-lower-like-desargues-wallpaper-loop
  (let [form {:params '[u] :body '[(wave u) (osc u 0.2 0.9) (frac (* 3 u))]}
        ^clojure.lang.IFn$DDL sh (fld/jvm-shader (fld/bind form 0.3))
        lp-wave (* 0.5 (+ 1.0 (Math/cos (* 2.0 Math/PI 0.3))))]
    (is (= (fld/pack lp-wave (+ 0.2 (* 0.7 (- 1.0 lp-wave))) (let [t (* 3.0 0.3)] (- t (Math/floor t))))
           (.invokePrim sh 0.0 0.0)))))
