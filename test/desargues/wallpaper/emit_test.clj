(ns desargues.wallpaper.emit-test
  "Layers emitted from field forms. Every emitted shader (ISF, GLSL ES
   3.00, GLSL 330) is checked structurally, and compiled by
   glslangValidator when one is found ($DESARGUES_GLSLANG, the system
   property desargues.glslang, else PATH);
   without one the compile tests say so and run the structural checks
   only. The ISF header of the quasicrystal layer is a golden file."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.emit :as emit]
            [desargues.wallpaper.field :as fld]
            [desargues.wallpaper.glsl-test :as gt]
            [hive-dsl.result :as r])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def layers
  "One layer per test form of glsl-test."
  [{:id :quasicrystal :title "7-fold quasicrystal" :form fld/quasicrystal :period 12 :blend :screen
    :params {'scale {:default 3.0 :min 1.0 :max 8.0 :label "Wave number"}}}
   {:id :lieb-fermi :form gt/lieb :period 18 :blend :add :view [8.0 4.5]
    :params {'k {:default 3.8 :min 1.0 :max 8.0}}}
   {:id :graphene-bands :form gt/graphene :blend :screen :categories ["Generator" "Physics"]
    :params {'a {:default (/ (* 4.0 Math/PI) 3.0) :min 1.0 :max 6.0 :label "Lattice constant"}
             'bands {:default 2.0 :min 1.0 :max 6.0}}}
   {:id :rings :form gt/rings :params {'k {:default 2.0}}}])

(defn- files [layer] (:ok (emit/layer-files layer)))

(def ^:private validator
  (let [v (or (System/getenv "DESARGUES_GLSLANG")
              (System/getProperty "desargues.glslang")
              (let [{:keys [exit out]} (sh/sh "sh" "-c" "command -v glslangValidator")]
                (when (zero? exit) (str/trim out))))]
    (when-not v
      (println "[emit-test] glslangValidator not found ($DESARGUES_GLSLANG or PATH); structural checks only"))
    v))

(defn- isf-preamble
  "What an ISF host declares before the layer's code (GLSL 1.20 path):
   the built-ins and one uniform per input."
  [header]
  (str "#version 120\nuniform vec2 RENDERSIZE;\nuniform float TIME;\nvarying vec2 isf_FragNormCoord;\n"
       (apply str (for [{:strs [NAME]} (get header "INPUTS")] (str "uniform float " NAME ";\n")))))

(defn- header-of
  "The parsed JSON header comment of ISF source `fs`."
  [fs]
  (json/read-str (subs fs 2 (str/index-of fs "*/"))))

(defn- balanced? [s]
  (every? (fn [[o c]] (= (count (filter #{o} s)) (count (filter #{c} s)))) [[\( \)] [\{ \}]]))

(defn- validate!
  "glslangValidator's {:exit :out} on fragment source `src`."
  [src]
  (let [f (io/file (str (Files/createTempDirectory "glsl" (make-array FileAttribute 0))) "layer.frag")]
    (spit f src)
    (sh/sh validator (str f))))

(deftest every-layer-compiles-to-every-variant
  (doseq [l layers]
    (testing (name (:id l))
      (let [fs (files l) id (name (:id l))]
        (is (= [(str id ".fs") (str id ".frag") (str id ".330.frag") (str id ".json")] (vec (keys fs))))
        (doseq [[n text] fs]
          (is (balanced? text) n)
          (is (not (re-find #"Math/|nil|NaN|Infinity" text)) n))))))

(deftest the-shaders-are-valid-glsl
  (if-not validator
    (is true "no glslangValidator: structural checks only")
    (doseq [l layers
            :let [fs (files l) id (name (:id l))
                  isf (get fs (str id ".fs"))]
            [variant src] [["isf" (str (isf-preamble (header-of isf)) isf)]
                           ["es300" (get fs (str id ".frag"))]
                           ["330" (get fs (str id ".330.frag"))]]]
      (testing (str id " " variant)
        (let [{:keys [exit out]} (validate! src)]
          (is (zero? exit) out))))))

(deftest the-isf-header-is-golden
  (let [fs (get (files (first layers)) "quasicrystal.fs")
        golden (slurp (io/resource "desargues/wallpaper/golden/quasicrystal.isf-header.json"))]
    (is (= (str/trim golden) (subs fs 2 (str/index-of fs "*/"))))
    (is (= (json/read-str golden) (header-of fs)))))

(deftest inputs-carry-the-phase-the-rate-and-the-params
  (let [h (header-of (get (files (nth layers 2)) "graphene-bands.fs"))
        ins (get h "INPUTS")]
    (is (= ["u" "rate" "a" "bands"] (mapv #(get % "NAME") ins)))
    (is (every? #(= "float" (get % "TYPE")) ins))
    (is (= [0.0 1.0] ((juxt #(get % "MIN") #(get % "MAX")) (first ins))))
    (is (= "Lattice constant" (get (nth ins 2) "LABEL")))
    (is (= ["Generator" "Physics"] (get h "CATEGORIES")))
    (is (str/includes? (get h "DESCRIPTION") "blend: screen"))
    (is (= "screen" (get-in h ["DESARGUES" "blend"]))))
  (testing "the manifest names the same uniforms, with roles"
    (let [m (json/read-str (get (files (first layers)) "quasicrystal.json"))]
      (is (= ["u" "rate" "scale"] (mapv #(get % "name") (get m "uniforms"))))
      (is (= {"name" "scale" "type" "float" "default" 3.0 "min" 1.0 "max" 8.0 "label" "Wave number" "role" "param"}
             (nth (get m "uniforms") 2)))
      (is (= ["phase" "rate" "param"] (mapv #(get % "role") (get m "uniforms"))))
      (is (= "quasicrystal.frag" (get m "frag")))
      (is (= "TIME" (get m "time"))))))

(deftest an-unbound-layer-loops-on-its-clock
  (let [fs (get (files (first layers)) "quasicrystal.fs")]
    (testing "phase = fract(u + rate * TIME / period): at TIME = period it is u again"
      (is (str/includes? fs "float desargues_phase = fract(u + rate * TIME / 12.0);")))
    (testing "the field takes the phase where the form's u was, the params by name"
      (is (str/includes? fs "desargues_field(desargues_p.x, desargues_p.y, desargues_phase, scale)")))
    (testing "the view is fitted as render fits it: whole window visible"
      (is (str/includes? fs "min(desargues_res.x / 14.222222222222221, desargues_res.y / 8.0)")))))

(deftest bad-layers-are-refused-and-nothing-is-written
  (let [q (first layers)]
    (doseq [[label l] {"no default for a param" (assoc q :params {})
                       "a param the form lacks" (assoc-in q [:params 'zoom] {:default 1})
                       "no such phase" (assoc q :phase 't)
                       "an unknown blend" (assoc q :blend :glitter)
                       "a reserved name" (assoc q :form (assoc fld/quasicrystal :params '[u rate])
                                                  :params {'rate {:default 1}})}]
      (testing label
        (is (= :layer/invalid (:error (emit/layer-files l)))))))
  (testing "a form GLSL cannot express: the compiler's err, no files"
    (let [dir (str (Files/createTempDirectory "layers" (make-array FileAttribute 0)))
          res (emit/emit! {:id :bad :form {:params '[u] :body [(/ 1.0 0.0) 0 0]}} dir)]
      (is (= :field/unsupported (:error res)))
      (is (empty? (.list (io/file dir)))))))

(deftest emit-writes-the-layer-files
  (let [dir (str (Files/createTempDirectory "layers" (make-array FileAttribute 0)))
        res (emit/emit-all! layers dir)]
    (is (every? r/ok? (vals res)))
    (is (= 16 (count (.list (io/file dir)))))
    (is (str/starts-with? (slurp (io/file dir "lieb-fermi.frag")) "#version 300 es"))))
