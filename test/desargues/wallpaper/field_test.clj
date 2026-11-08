(ns desargues.wallpaper.field-test
  "Lowering of field forms: every symbol the lowered code references is
   bound before it is used, and every backend reads the same code alike."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.field :as fld]
            [desargues.wallpaper.glsl :as glsl]
            [desargues.wallpaper.wgsl :as wgsl]))

(def contour
  "The form tessera-fieldforms reported: frac and wave, which share an
   intermediate, nested inside the args of a Math op (pow)."
  {:params '[u step sharp]
   :body '(let [e (sqrt (+ (* x x) (* y y)))]
            [(pow (wave (frac (- (/ e step) u))) sharp)
             (min (frac (* 2 e)) (wave (+ e u)))
             (atan2 (frac y) (fract (- x u)))])})

(defn- symbols-in [e]
  (cond (symbol? e) #{e}
        (seq? e) (into #{} (mapcat symbols-in) (rest e))
        :else #{}))

(defn- unbound-refs
  "The vN symbols referenced in lowered code `l` before (or without) their binding."
  [{:keys [bindings rgb]}]
  (let [step (fn [[bound missing] [s e]]
               [(conj bound s) (into missing (remove #(or (bound %) (not (re-matches #"v\d+" (name %))))) (symbols-in e))])
        [bound missing] (reduce step [#{} #{}] bindings)]
    (into missing (comp (mapcat symbols-in) (filter #(re-matches #"v\d+" (name %))) (remove bound)) rgb)))

(deftest every-referenced-binding-is-bound-in-order
  (testing "shared intermediates inside Math-op args are bound before use"
    (is (empty? (unbound-refs (fld/lower contour)))))
  (testing "the exact reported expression"
    (is (empty? (unbound-refs (fld/lower {:params '[u step sharp e]
                                          :body '[(pow (wave (frac (- (/ e step) u))) sharp) 0 0]}))))))

(deftest jvm-glsl-and-wgsl-agree-on-the-reported-form
  (let [args [0.3 0.7 6.0]
        ^clojure.lang.IFn$DDL sh (fld/jvm-shader (apply fld/bind contour args))
        gt (:ir (:ok (fld/source :glsl contour {})))
        wt (:ir (:ok (fld/source :wgsl contour {})))]
    (is (some? gt))
    (is (some? wt))
    (doseq [i (range -8 9) j (range -5 6)
            :let [x (* 0.37 i) y (* 0.41 j)]]
      (is (= (.invokePrim sh x y)
             (apply fld/pack (glsl/interpret gt x y args))
             (apply fld/pack (wgsl/interpret wt x y args)))
          (str [x y])))))
