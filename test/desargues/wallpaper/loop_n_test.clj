(ns desargues.wallpaper.loop-n-test
  "loop-n, the bounded loop of field forms. On the JVM a loop-n shades bit
   for bit as its hand-unrolled equivalent; lowering keeps it as one node
   (the emitted GLSL/WGSL has one for loop, not N copies); the GLSL and WGSL
   trees, read on the CPU, equal the JVM shader; the :raster kernel equals
   :jvm; the escape-time Mandelbrot exemplar emits a trama source that naga
   validates (when naga is found, as in trama-test); bad loops are refused
   by name."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [desargues.backend :as backend]
            [desargues.wallpaper.emit :as emit]
            [desargues.wallpaper.field :as fld]
            [desargues.wallpaper.glsl :as glsl]
            [desargues.wallpaper.render :as r]
            [desargues.wallpaper.trama-test :as tt]
            [desargues.wallpaper.wgsl :as wgsl])
  (:import (java.awt.image BufferedImage DataBufferInt)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def ^:private N 24)

(def ^:private step
  "One step of the test dynamics over state a b k and index i: a
   Mandelbrot-like map with a sine kick, so the index and every state matter."
  '[(+ (- (* a a) (* b b)) x (* 0.01 (sin i)))
    (+ (* 2 a b) y)
    (+ k 1)])

(defn- unroll
  "Field body: `n` steps of `step` written out by hand, the state threaded
   through lets (a0 b0 k0 -> a1 b1 k1 ..), the index a literal."
  [n]
  (let [sym (fn [s j] (symbol (str s j)))
        binds (mapcat (fn [j]
                        (let [sm {'a (sym "a" j) 'b (sym "b" j) 'k (sym "k" j) 'i (double j)}
                              [na nb nk] (walk/postwalk-replace sm step)]
                          [(sym "a" (inc j)) na (sym "b" (inc j)) nb (sym "k" (inc j)) nk]))
                      (range n))]
    (list 'let (into '[a0 0 b0 0 k0 0] binds)
          [(list 'fract (sym "a" n)) (list 'fract (sym "b" n)) (list '/ (sym "k" n) n)])))

(def ^:private looped
  {:params '[]
   :body (list 'let ['[a b k] (list 'loop-n ['i N] '[a 0 b 0 k 0] step)]
               '[(fract a) (fract b) (/ k 24)])})

(def ^:private escaping
  "loop-n with :until: escape time, and/or/not in the condition."
  {:params '[lim]
   :body (list 'let ['[a b k] (list 'loop-n ['i N] '[a 0 b 0 k 0] step
                                    :until '(or (> (+ (* a a) (* b b)) lim) (not (< k 100))))]
               '[(fract a) (fract b) (/ k 24)])})

(defn- grid [] (for [i (range -24 25) j (range -16 17)] [(* 0.06 i) (* 0.06 j)]))

(defn- shade [form args]
  (let [^clojure.lang.IFn$DDL f (fld/jvm-shader (apply fld/bind form args))]
    (fn [[x y]] (.invokePrim f (double x) (double y)))))

(defn- channels
  "(fn [x y] [r g b]) of a param-less form, the raw doubles before pack."
  [form]
  (let [{:keys [bindings rgb]} (fld/lower form)]
    (binding [*ns* (the-ns 'desargues.wallpaper.loop-n-test)]
      (eval (list 'fn '[fx fy] (fld/let-form bindings rgb))))))

(deftest loop-n-is-the-unrolled-form-bit-for-bit
  (let [a (shade looped []) b (shade {:params [] :body (unroll N)} [])]
    (is (every? (fn [p] (= (a p) (b p))) (grid))))
  (testing "the raw channels too, not only the packed pixel"
    (let [fa (channels looped) fb (channels {:params [] :body (unroll N)})]
      ;; bitwise: diverging orbits end in NaN, which = never equates
      (is (every? (fn [[x y]] (java.util.Arrays/equals (double-array (fa x y)) (double-array (fb x y))))
                  (grid))))))

(deftest until-stops-at-the-first-step-it-holds
  (let [lim 4.0
        ref (fn [x y]
              (loop [i 0 a 0.0 b 0.0 k 0.0]
                (if (or (= i N) (> (+ (* a a) (* b b)) lim) (not (< k 100.0)))
                  [a b k]
                  (recur (inc i) (+ (+ (- (* a a) (* b b)) x) (* 0.01 (Math/sin (double i))))
                         (+ (* (* 2.0 a) b) y) (+ k 1.0)))))
        f (shade escaping [lim])]
    (is (every? (fn [[x y]] (let [[a b k] (ref x y)]
                              (= (f [x y]) (fld/pack (- a (Math/floor a)) (- b (Math/floor b)) (/ k 24.0)))))
                (grid)))))

(deftest lower-keeps-one-loop-node
  (let [{:keys [bindings]} (fld/lower fld/mandelbrot)
        loops (filter fld/loop-node? bindings)]
    (is (= 1 (count loops)))
    (is (= 96 (:loop/n (first loops))))
    (is (= 3 (count (:loop/state (first loops))))))
  (doseq [t [:glsl :wgsl]]
    (let [text (:function (:ok (fld/source t fld/mandelbrot {})))]
      (testing (name t)
        (is (= 1 (count (re-seq #"for \(" text))))
        (is (= 1 (count (re-seq #"break;" text))))
        (is (< (count (str/split-lines text)) 40))))))

(deftest glsl-and-wgsl-trees-read-as-the-jvm-shader
  (doseq [[label form args] [["looped" looped []] ["escaping" escaping [4.0]]
                             ["mandelbrot" fld/mandelbrot [0.3 0.9 -0.6 0.0]]]
          :let [f (shade form args)
                gt (:ir (:ok (fld/source :glsl form {})))
                wt (:ir (:ok (fld/source :wgsl form {})))]]
    (testing label
      (is (some? gt))
      (is (some? wt))
      (is (every? (fn [[x y :as p]]
                    (= (f p) (apply fld/pack (glsl/interpret gt x y args))
                       (apply fld/pack (wgsl/interpret wt x y args))))
                  (grid))))))

(def ^:private nested
  {:params '[]
   :body '(let [s (loop-n [i 8] [s 0]
                    [(+ s (loop-n [j 4] [t 0] [(+ t (* i j))]))])
                [p q] (loop-n [i 3] [p 1 q 2] [q (+ p q)])]
            [(/ s 100) (/ p 10) (/ q 10)])})

(deftest nested-and-value-bound-loops
  ;; sum_{i<8} sum_{j<4} i j = 28 * 6 = 168; three steps from (1, 2): (2, 3) (3, 5) (5, 8)
  (is (= (fld/pack 1.68 0.5 0.8) ((shade nested []) [0.0 0.0])))
  (is (= [1.68 0.5 0.8] ((channels nested) 0.0 0.0)))
  (let [{:keys [bindings rgb]} (fld/lower nested)]
    (is (= [1.68 0.5 0.8] (eval (list 'let (into '[fx 0.0 fy 0.0] (fld/value-bindings bindings rgb)) rgb)))))
  (doseq [t [:glsl :wgsl]]
    (let [tree (:ir (:ok (fld/source t nested {})))]
      (is (= [1.68 0.5 0.8] ((if (= t :glsl) glsl/interpret wgsl/interpret) tree 0 0 [])) (name t)))))

(def ^:private raster? (backend/available? :raster))

(defn- pixels ^ints [^BufferedImage img]
  (.getData ^DataBufferInt (.getDataBuffer (.getRaster img))))

(deftest raster-kernel-matches-jvm
  (if-not raster?
    (is true "raster not on classpath (-A:dynamics); skipped")
    (doseq [[label form args] [["escaping" escaping [4.0]] ["nested" nested []]
                               ["mandelbrot" fld/mandelbrot [0.3 0.9 -0.6 0.0]]]
            ss [1 2]]
      (testing (str label " ss " ss)
        (let [bf (apply fld/bind form args)
              opts {:ss ss :view [3.6 2.0]}]
          (is (java.util.Arrays/equals (pixels (r/field-raster :jvm (r/image 96 54) bf opts))
                                       (pixels (r/field-raster :raster (r/image 96 54) bf opts)))))))))

(def ^:private mandel-layer
  {:id :mandelbrot :title "Mandelbrot (loop-n)" :form fld/mandelbrot :period 20 :blend :screen
   :target :trama :view [3.6 2.0]
   :params [['zoom {:default 0.9 :min 0.1 :max 50.0}]
            ['cx {:default -0.6 :min -2.0 :max 1.0}]
            ['cy {:default 0.0 :min -1.5 :max 1.5}]]})

(deftest mandelbrot-trama-source-validates-with-naga
  (let [src (val (first (:ok (emit/layer-files mandel-layer))))]
    (is (str/includes? src "for (var v"))
    (is (str/includes? src "{ break; }"))
    (if-let [naga @#'tt/naga]
      (let [pre (slurp (io/resource "desargues/wallpaper/golden/trama-stub-preamble.wgsl"))
            f (io/file (str (Files/createTempDirectory "loopn" (make-array FileAttribute 0))) "mandelbrot.wgsl")]
        (spit f (str pre src))
        (let [{:keys [exit out err]} (sh/sh naga (str f))]
          (is (zero? exit) (str out err))))
      (is true "no naga: structural checks only"))))

(deftest bad-loops-are-refused-by-name
  (doseq [[label body] {"a bound past 256" '(let [s (loop-n [i 257] [s 0] [(+ s 1)])] [s s s])
                        "a symbolic bound" '(let [s (loop-n [i n] [s 0] [(+ s 1)])] [s s s])
                        "a zero bound" '(let [s (loop-n [i 0] [s 0] [(+ s 1)])] [s s s])
                        "no state" '(let [s (loop-n [i 4] [] [])] [s s s])
                        "a body of the wrong width" '(let [s (loop-n [i 4] [s 0] [s s])] [s s s])
                        "a bad :until" '(let [s (loop-n [i 4] [s 0] [(+ s 1)] :until (+ s 1))] [s s s])
                        "an unknown option" '(let [s (loop-n [i 4] [s 0] [(+ s 1)] :while (> s 1))] [s s s])
                        "binding count mismatch" '(let [[a b] (loop-n [i 4] [s 0] [(+ s 1)])] [a b 0])
                        "a multi-state loop as a value" '[(loop-n [i 4] [a 0 b 0] [a b]) 0 0]
                        "a multi-state loop nested" '(let [s (loop-n [i 4] [s 0]
                                                               [(let [[p q] (loop-n [j 2] [p 0 q 0] [q p])] (+ s p))])]
                                                       [s s s])
                        "the index outside its loop" '(let [s (loop-n [i 4] [s 0] [(+ s 1)])] [i s s])}]
    (testing label
      (let [e (is (thrown? clojure.lang.ExceptionInfo (fld/lower {:params [] :body body})))]
        (is (= :bad-field (:reason (ex-data e))))))))

(deftest straight-line-forms-lower-as-before
  (testing "no loop-n: the same names and shape as before loop-n"
    (is (= '{:params [u k]
             :bindings [[v0 (+ (* fx fx) (* fy fy))] [v1 (+ (* p1 v0) p0)]]
             :rgb [(- v1 (Math/floor v1)) (Math/cos v0) 0.5]}
           (fld/lower {:params '[u k]
                       :body '(let [r2 (+ (* x x) (* y y))]
                                [(fract (+ (* k r2) u)) (cos r2) 0.5])})))))
