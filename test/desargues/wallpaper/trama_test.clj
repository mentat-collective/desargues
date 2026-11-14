(ns desargues.wallpaper.trama-test
  "The :wgsl target and the :trama emitter. The WGSL tree of every
   glsl-test form, read on the CPU (double and float32), is held to the JVM
   shader within one level of 255; the trama manifest is a golden file and
   carries only fosfora's keys, loop_rate in slot 0, the declared params
   after it in order (append-only, byte-identical on re-emission); the
   refusals are named. When naga is found ($DESARGUES_NAGA, the system
   property desargues.naga, else naga on PATH) each emitted file is
   validated behind golden/trama-stub-preamble.wgsl; without it the test
   says so and runs the structural checks only."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.emit :as emit]
            [desargues.wallpaper.field :as fld]
            [desargues.wallpaper.glsl-test :as gt]
            [desargues.wallpaper.trama :as trama]
            [desargues.wallpaper.wgsl :as wgsl]
            [hive-dsl.result :as r]
            [desargues.wallpaper.emit-test :as et])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def graphene-contours
  "Graphene band contours: |f(k)| level lines rising one step per loop,
   a glow at the Fermi level, Dirac points lit where |f| vanishes."
  {:params '[u a fermi step sharp]
   :body '(let [k1 (* a x)
                k2 (+ (* -0.5 a x) (* 0.8660254037844386 a y))
                f2 (+ 3 (* 2 (cos k1)) (* 2 (cos k2)) (* 2 (cos (- k1 k2))))
                e (sqrt (max f2 0))
                c (pow (wave (frac (- (/ e step) u))) sharp)
                d (- e fermi)
                fl (exp (* -40 (* d d)))
                dirac (exp (* -30 f2))]
            [(+ (* 0.15 c) (* 0.9 fl) (* 0.3 dirac))
             (+ (* 0.55 c) (* 0.6 fl) (* 0.8 dirac))
             (+ (* 0.85 c) (* 0.3 fl) (* 0.9 dirac))])})

(def layers
  "The example trama layers (also emitted to out/trama/)."
  [{:id :quasicrystal :title "7-fold quasicrystal" :form fld/quasicrystal :period 12 :blend :screen
    :target :trama :params [['scale {:default 3.0 :min 1.0 :max 8.0 :label "Wave number"}]]}
   {:id :graphene-contours :title "Graphene band contours" :form graphene-contours :period 16
    :blend :lighten :target :trama :view [8.0 4.5]
    :params [['a {:default (/ (* 4.0 Math/PI) 3.0) :min 1.0 :max 6.0}]
             ['fermi {:default 1.0 :min 0.0 :max 3.0}]
             ['step {:default 0.25 :min 0.05 :max 1.0}]
             ['sharp {:default 12.0 :min 1.0 :max 60.0}]]}
   {:id :rings :title "Rings (every op)" :form gt/rings :period 10 :blend :add :target :trama
    :params [['k {:default 2.0 :min 0.5 :max 8.0}]]}])

(defn- text-of [layer] (val (first (:ok (emit/layer-files layer)))))

(defn- manifest-of
  "The parsed manifest comment of trama source `src`."
  [src]
  (json/read-str (subs src (count "/*! trama\n") (str/index-of src "\n*/"))))

(def ^:private naga
  (let [v (or (System/getenv "DESARGUES_NAGA")
              (System/getProperty "desargues.naga")
              (let [{:keys [exit out]} (sh/sh "sh" "-c" "command -v naga")]
                (when (zero? exit) (str/trim out))))]
    (when-not v
      (println "[trama-test] naga not found ($DESARGUES_NAGA, -Ddesargues.naga or PATH); structural checks only"))
    v))

(defn- channel-diffs
  "Per-channel level differences, JVM shader vs the WGSL tree read with
   `round`, over a 64 x 36 grid of the default view."
  [form args round]
  (let [tree (:ir (:ok (fld/source :wgsl form {})))
        ^clojure.lang.IFn$DDL sh (fld/jvm-shader (apply fld/bind form args))]
    (for [i (range 64) j (range 36)
          :let [x (* 0.222 (- i 31.5)) y (* 0.222 (- j 17.5))
                c (.invokePrim sh x y)
                g (apply fld/pack (wgsl/interpret tree x y args round))]
          s [0 8 16]]
      (Math/abs (- (bit-and (bit-shift-right c s) 255) (bit-and (bit-shift-right g s) 255))))))

(deftest the-wgsl-tree-reads-as-the-jvm-shader
  (doseq [[label form args] (conj gt/cases ["graphene-contours" graphene-contours [0.3 4.18 1.0 0.25 12.0]])
          [rounding round] [["double" identity] ["float32" (comp double float)]]]
    (testing (str label " " rounding)
      (is (<= (reduce max (channel-diffs form args round)) 1)))))

(deftest the-manifest-is-golden-and-has-only-fosforas-keys
  (let [src (text-of (first layers))
        golden (slurp (io/resource "desargues/wallpaper/golden/quasicrystal.trama-manifest.json"))
        m (manifest-of src)]
    (is (= (str/trim golden) (subs src (count "/*! trama\n") (str/index-of src "\n*/"))))
    (is (= ["name" "id" "kind" "inputs" "params" "rates" "integers"] (vec (keys (json/read-str golden :key-fn identity)))))
    (is (= #{"name" "id" "kind" "inputs" "params" "rates" "integers"} (set (keys m))))
    (is (every? #(= #{"type" "name" "default" "min" "max"} (set (keys %))) (get m "params")))
    (is (= ["source" 0 ["loop_rate"] []] (mapv m ["kind" "inputs" "rates" "integers"])))))

(deftest loop-rate-is-slot-0-and-params-follow-in-declared-order
  (let [g (second layers)
        names #(mapv (fn [p] (get p "name")) (get (manifest-of (text-of %)) "params"))]
    (is (= ["loop_rate" "a" "fermi" "step" "sharp"] (names g)))
    (testing "the slots the shader reads match the manifest"
      (let [src (text-of g)]
        (is (str/includes? src "fract(param(0u) / 16.0)"))
        (is (str/includes? src "desargues_field(desargues_p.x, desargues_p.y, desargues_phase, param(1u), param(2u), param(3u), param(4u))"))))
    (testing "declared order wins over the form's order"
      (is (= ["loop_rate" "sharp" "step" "fermi" "a"] (names (update g :params (comp vec reverse))))))
    (testing "append-only: a new param takes the next slot, the old ones stay"
      (let [g2 (-> g (assoc :form (update graphene-contours :params conj 'glow))
                   (update :params conj ['glow {:default 0.5 :min 0.0 :max 1.0}]))]
        (is (= (conj (names g) "glow") (names g2)))))
    (testing "re-emitting the same spec is byte-identical"
      (is (= (text-of g) (text-of g) (text-of (into {} (reverse (seq g)))))))))

(deftest integer-controls-snap-in-the-shader-with-stable-float-slots
  (let [layer (assoc et/integer-layer :target :trama)
        src (text-of layer)
        m (manifest-of src)
        checked (:ok (trama/check layer))
        continuous (update layer :params (fn [ps] (mapv (fn [[s spec]] [s (dissoc spec :integer?)]) ps)))]
    (is (= ["mode" "loop_count"] (get m "integers")))
    (is (= ["loop_rate" "mode" "gain" "loop_count"] (mapv #(get % "name") (get m "params"))))
    (is (every? #(= "Float" (get % "type")) (get m "params")))
    (is (= {'mode 1 'gain 2 'loop-count 3} (trama/slots checked)))
    (is (= (trama/slots checked) (trama/slots (:ok (trama/check continuous)))))
    (is (= (dissoc (manifest-of (text-of continuous)) "integers") (dissoc m "integers")))
    (is (str/includes? src "desargues_phase, floor(param(3u) + 0.5), param(2u), floor(param(1u) + 0.5))"))
    (is (str/includes? src "fract(param(0u) / 10.0)"))
    (testing "legacy maps use explicit order for both slots and integer names"
      (let [legacy (assoc layer :params (into {} (:params layer)) :order '[loop-count gain mode])]
        (is (= ["loop_count" "mode"] (get (manifest-of (text-of legacy)) "integers")))))
    (is (= src (text-of layer)))
    (when naga
      (let [dir (str (Files/createTempDirectory "integer-trama" (make-array FileAttribute 0)))
            f (io/file dir "integer-controls.wgsl")
            pre (slurp (io/resource "desargues/wallpaper/golden/trama-stub-preamble.wgsl"))]
        (spit f (str pre src))
        (let [{:keys [exit out err]} (sh/sh naga (str f))]
          (is (zero? exit) (str out err)))))))

(deftest fractional-host-values-cannot-bypass-discrete-controls
  (doseq [target [:trama :pfx]
          :let [layer (assoc et/integer-layer :target target)
                fs (:ok (emit/layer-files layer))
                src (get fs "integer_controls.wgsl")
                discrete (re-seq #"floor\(param\((\d+)u\) \+ ([0-9.]+)\)" src)]
          round [identity (comp double float)]]
    (testing (str target " fractional automation, including negative values and ties")
      (is (= ["3" "1"] (mapv second discrete)))
      (doseq [[_ _ half] discrete
              :let [expr (:ok (wgsl/expr-ir (list 'Math/floor (list '+ 'p0 (Double/parseDouble half)))))
                    tree {:bindings [] :rgb [expr expr expr]}]
              [value expected] [[-2.6 -3.0] [-2.5 -2.0] [-2.4 -2.0] [-0.5 0.0]
                                [0.49 0.0] [0.5 1.0] [2.49 2.0] [2.5 3.0] [3.0 3.0]]]
        (is (= [expected expected expected] (wgsl/interpret tree 0 0 [value] round)))))))

(deftest every-file-is-structurally-a-trama-source
  (doseq [l layers
          :let [src (text-of l)]]
    (testing (name (:id l))
      (is (str/starts-with? src "/*! trama\n"))
      (is (str/includes? src (str "// desargues: blend=" (name (:blend l)) "\n")))
      (is (str/includes? src "@fragment\nfn fs_main(@builtin(position) frag_coord: vec4f) -> @location(0) vec4f {"))
      (is (str/includes? src "return vec4f(clamp(desargues_c, vec3f(0.0), vec3f(1.0)), 1.0);"))
      (is (not (re-find #"Math/|nil|NaN|Infinity|u\.time|\?|var<uniform>|struct |fn param\(" src)))
      (is (every? #(str/starts-with? % "desargues_")
                  (remove #{"fs_main"} (map second (re-seq #"\bfn ([A-Za-z0-9_]+)\(" src)))))
      (is (every? (fn [[o c]] (= (count (filter #{o} src)) (count (filter #{c} src)))) [[\( \)] [\{ \}]])))))

(deftest naga-validates-every-file
  (if-not naga
    (is true "no naga: structural checks only")
    (let [pre (slurp (io/resource "desargues/wallpaper/golden/trama-stub-preamble.wgsl"))
          dir (str (Files/createTempDirectory "trama" (make-array FileAttribute 0)))]
      (doseq [l layers
              :let [f (io/file dir (str (name (:id l)) ".wgsl"))]]
        (spit f (str pre (text-of l)))
        (testing (name (:id l))
          (let [{:keys [exit out err]} (sh/sh naga (str f))]
            (is (zero? exit) (str out err))))))))

(deftest bad-trama-layers-are-refused-by-name
  (let [q (first layers)
        sixteen (vec (for [i (range 16)] (symbol (str "p" (char (+ 97 i))))))
        many (assoc q :form {:params (into '[u] sixteen) :body '[0 0 0]}
                    :params (mapv (fn [s] [s {:default 0.5 :min 0 :max 1}]) sixteen))]
    (testing "17 slots (loop_rate + 16 params) is one too many"
      (is (= :trama/too-many-params (:error (emit/layer-files many)))))
    (testing "16 slots fit"
      (is (r/ok? (emit/layer-files (-> many (update :params pop) (update-in [:form :params] pop))))))
    (doseq [[label id] {"upper case" "Quasi" "a dot" "q.x" "a space" "q x"}]
      (testing (str "a bad id: " label)
        (is (= :trama/bad-id (:error (emit/layer-files (assoc q :id id)))))))
    (testing "a kebab id becomes snake_case"
      (is (= ["graphene_contours.wgsl"] (keys (:ok (emit/layer-files (second layers)))))))
    (testing "a param without a range"
      (is (= :trama/bad-param (:error (emit/layer-files (assoc q :params [['scale {:default 3.0}]]))))))
    (testing "a default outside its range"
      (is (= :trama/bad-param (:error (emit/layer-files (assoc q :params [['scale {:default 9.0 :min 1 :max 8}]]))))))
    (testing "a construct WGSL cannot express: the compiler names it, nothing is written"
      (let [dir (str (Files/createTempDirectory "trama" (make-array FileAttribute 0)))
            res (emit/emit! {:id :bad :target :trama :form {:params '[u] :body [(/ 1.0 0.0) 0 0]}} dir)]
        (is (= :field/unsupported (:error res)))
        (is (= :wgsl (:target res)))
        (is (empty? (.list (io/file dir))))))
    (testing "an op of the lowered code with no WGSL mapping is named"
      (is (= 'Math/cbrt (:construct (wgsl/expr-ir '(Math/cbrt v0))))))
    (testing "a constant fold WGSL would reject at shader creation"
      (is (= :field/unsupported (:error (wgsl/expr-ir '(Math/sqrt -1.0))))))
    (testing "an unknown field op"
      (is (= :field/bad (:error (emit/layer-files (assoc q :form {:params '[u scale] :body '[(noise x) 0 0]}))))))))

(deftest desargues-pow-follows-the-jvm
  (is (str/includes? (text-of (nth layers 2)) "fn desargues_pow(x: f32, y: f32) -> f32"))
  (doseq [[x y] [[2.0 0.5] [-2.0 3.0] [-2.0 2.0] [-2.0 0.5] [0.0 2.0] [0.0 0.0] [3.0 -1.0]]]
    (let [j (Math/pow x y)
          t (:ir (:ok (fld/source :wgsl {:params '[a b] :body '[(pow a b) 0 0]} {})))]
      (is (= (if (Double/isNaN j) 0.0 j) (first (wgsl/interpret t 0 0 [x y]))) (str x " ^ " y)))))

(deftest emit-writes-the-trama-files
  (let [dir (str (Files/createTempDirectory "trama" (make-array FileAttribute 0)))
        res (emit/emit-all! layers dir)]
    (is (every? r/ok? (vals res)))
    (is (= #{"quasicrystal.wgsl" "graphene_contours.wgsl" "rings.wgsl"} (set (.list (io/file dir)))))))

