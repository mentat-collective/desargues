(ns desargues.wallpaper.pfx-test
  "The :pfx emitter (fosfora layer effects). The .pfx JSON is a golden
   file with exactly fosfora's keys; loop_rate keeps its raw value at slot
   0 and the shader reads its integral from the extra slot after the
   packed params; the slot cap and the built-in name check refuse by name;
   the shader tree is held to the JVM shader as for :trama."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.emit :as emit]
            [desargues.wallpaper.field :as fld]
            [desargues.wallpaper.pfx :as pfx]
            [desargues.wallpaper.trama-test :as tt]
            [desargues.wallpaper.wgsl :as wgsl]
            [hive-dsl.result :as r]
            [desargues.wallpaper.emit-test :as et])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def quasi
  {:id :quasicrystal :title "7-fold quasicrystal" :form fld/quasicrystal :period 12 :blend :screen
   :target :pfx :pfx {:name "Seven Fold"}
   :params [['scale {:default 3.0 :min 1.0 :max 8.0 :label "Wave number"}]]})

(def graphene
  (-> (second tt/layers)
      (assoc :target :pfx :pfx {:name "Graphene Contours" :description "Band contours" :author "me"
                                :category "light"})))

(defn- files [layer] (:ok (emit/layer-files layer)))
(defn- pfx-json [layer] (json/read-str (get (files layer) (str (:trama/id (:ok (pfx/check layer))) ".pfx"))))
(defn- shader [layer] (get (files layer) (str (:trama/id (:ok (pfx/check layer))) ".wgsl")))

(deftest the-pfx-is-golden-and-has-only-fosforas-keys
  (let [text (get (files quasi) "quasicrystal.pfx")
        golden (slurp (io/resource "desargues/wallpaper/golden/quasicrystal.pfx.json"))]
    (is (= golden text))
    (is (= ["name" "author" "description" "shader" "category" "inputs" "rates"]
           (vec (keys (json/read-str text :key-fn identity)))))
    (is (= ["quasicrystal.pfx" "quasicrystal.wgsl"] (keys (files quasi))))
    (testing "defaults: author tessera, category pattern, description from the title"
      (is (= ["Seven Fold" "tessera" "7-fold quasicrystal" "quasicrystal.wgsl" "pattern" ["loop_rate"]]
             (mapv (pfx-json quasi) ["name" "author" "description" "shader" "category" "rates"]))))
    (testing "configurable metadata"
      (is (= ["Graphene Contours" "me" "Band contours" "light" "graphene_contours.wgsl"]
             (mapv (pfx-json graphene) ["name" "author" "description" "category" "shader"]))))
    (testing "the name falls back to :title"
      (is (= "7-fold quasicrystal" (get (pfx-json (dissoc quasi :pfx)) "name"))))
    (testing "inputs are the trama params, loop_rate first"
      (is (= (get (#'tt/manifest-of (val (first (:ok (emit/layer-files (second tt/layers)))))) "params")
             (get (pfx-json graphene) "inputs"))))))

(deftest the-rate-integral-is-read-from-the-extra-slot
  (testing "1 param: loop_rate raw at 0, scale at 1, integral at 2"
    (is (= 2 (pfx/rate-slot (:ok (pfx/check quasi)))))
    (is (str/includes? (shader quasi) "fract(param(2u) / 12.0)"))
    (is (str/includes? (shader quasi) "desargues_field(desargues_p.x, desargues_p.y, desargues_phase, param(1u))")))
  (testing "4 params: integral at 5, the params stay at 1..4, slot 0 is never read"
    (let [s (shader graphene)]
      (is (= 5 (pfx/rate-slot (:ok (pfx/check graphene)))))
      (is (str/includes? s "fract(param(5u) / 16.0)"))
      (is (str/includes? s "desargues_phase, param(1u), param(2u), param(3u), param(4u))"))
      (is (not (str/includes? s "param(0u)")))))
  (testing "the trama emitter still reads the integral at slot 0"
    (is (str/includes? (val (first (:ok (emit/layer-files (second tt/layers))))) "fract(param(0u) / 16.0)"))))

(deftest integer-effects-snap-without-changing-the-pfx-contract
  (let [layer (assoc et/integer-layer :target :pfx)
        src (shader layer)
        m (pfx-json layer)
        continuous (update layer :params (fn [ps] (mapv (fn [[s spec]] [s (dissoc spec :integer?)]) ps)))]
    (testing "PFX has no integers UI metadata field: only shader-side snapping"
      (is (= (pfx-json continuous) m))
      (is (= ["name" "author" "description" "shader" "category" "inputs" "rates"] (vec (keys m))))
      (is (not (contains? m "integers"))))
    (is (= ["loop_rate" "mode" "gain" "loop_count"] (mapv #(get % "name") (get m "inputs"))))
    (is (= 4 (pfx/rate-slot (:ok (pfx/check layer)))))
    (is (str/includes? src "fract(param(4u) / 10.0)"))
    (is (not (str/includes? src "param(0u)")))
    (is (str/includes? src "desargues_phase, floor(param(3u) + 0.5), param(2u), floor(param(1u) + 0.5))"))
    (when-let [naga @#'tt/naga]
      (let [dir (str (Files/createTempDirectory "integer-pfx" (make-array FileAttribute 0)))
            f (io/file dir "integer-controls.wgsl")
            pre (slurp (io/resource "desargues/wallpaper/golden/trama-stub-preamble.wgsl"))]
        (spit f (str pre src))
        (let [{:keys [exit out err]} (clojure.java.shell/sh naga (str f))]
          (is (zero? exit) (str out err)))))))

(deftest the-shader-is-a-layer-effect
  (doseq [l [quasi graphene]
          :let [s (shader l)]]
    (is (not (str/includes? s "/*! trama")))
    (is (not (str/includes? s "// desargues: blend")))
    (is (str/includes? s "@fragment\nfn fs_main(@builtin(position) frag_coord: vec4f) -> @location(0) vec4f {"))
    (is (str/includes? s "return vec4f(clamp(desargues_c, vec3f(0.0), vec3f(1.0)), 1.0);"))
    (is (not (re-find #"u\.time|var<uniform>|struct |fn param\(" s)))
    (is (every? #(str/starts-with? % "desargues_")
                (remove #{"fs_main"} (map second (re-seq #"\bfn ([A-Za-z0-9_]+)\(" s)))))
    (is (= (shader l) (shader (into {} (reverse (seq l))))) "byte-identical re-emission")))

(deftest naga-validates-every-shader
  (if-let [naga @#'tt/naga]
    (let [pre (slurp (io/resource "desargues/wallpaper/golden/trama-stub-preamble.wgsl"))
          dir (str (Files/createTempDirectory "pfx" (make-array FileAttribute 0)))]
      (doseq [l [quasi graphene]
              :let [f (io/file dir (str (name (:id l)) ".wgsl"))]]
        (spit f (str pre (shader l)))
        (let [{:keys [exit out err]} (clojure.java.shell/sh naga (str f))]
          (is (zero? exit) (str out err)))))
    (is true "no naga: structural checks only")))

(deftest the-shader-tree-reads-as-the-jvm-shader
  (doseq [[label form args] [["quasicrystal" fld/quasicrystal [0.3 3.0]]
                             ["graphene" tt/graphene-contours [0.3 4.18 1.0 0.25 12.0]]]
          round [identity (comp double float)]]
    (testing label
      (let [tree (:ir (:ok (fld/source :wgsl form {})))
            ^clojure.lang.IFn$DDL sh (fld/jvm-shader (apply fld/bind form args))]
        (is (<= (reduce max (for [i (range 32) j (range 18)
                                  :let [x (* 0.444 (- i 15.5)) y (* 0.444 (- j 8.5))
                                        c (.invokePrim sh x y)
                                        g (apply fld/pack (wgsl/interpret tree x y args round))]
                                  s [0 8 16]]
                              (Math/abs (- (bit-and (bit-shift-right c s) 255) (bit-and (bit-shift-right g s) 255)))))
                1))))))

(defn- with-params [q n]
  (let [ss (vec (for [i (range n)] (symbol (str "p" (char (+ 97 i))))))]
    (assoc q :form {:params (into '[u] ss) :body '[0 0 0]}
           :params (mapv (fn [s] [s {:default 0.5 :min 0 :max 1}]) ss))))

(deftest bad-layer-effects-are-refused-by-name
  (testing "loop_rate + 15 params + the rate slot is 17: refused (trama would take it)"
    (is (r/ok? (emit/layer-files (assoc (with-params quasi 15) :target :trama))))
    (is (= :pfx/too-many-slots (:error (emit/layer-files (with-params quasi 15))))))
  (testing "loop_rate + 14 params + the rate slot is 16: fits, integral at 15"
    (is (r/ok? (emit/layer-files (with-params quasi 14))))
    (is (str/includes? (shader (with-params quasi 14)) "fract(param(15u) / 12.0)")))
  (testing "a fosfora built-in name, in any case or spacing"
    (doseq [n ["Tessera" "aurora" "Lattice  Pulse" " Prism " "Raster" "Stress"]]
      (is (= :pfx/name-collision (:error (emit/layer-files (assoc-in quasi [:pfx :name] n)))) n)))
  (testing "names near a built-in are fine"
    (doseq [n ["Graphene Fermi" "Kaleido Quasicrystal" "Raster Stress" "Lattice"]]
      (is (r/ok? (emit/layer-files (assoc-in quasi [:pfx :name] n))) n)))
  (testing "a blank name"
    (is (= :pfx/bad-name (:error (emit/layer-files (assoc-in quasi [:pfx :name] "  "))))))
  (testing "the trama rules still apply"
    (is (= :trama/bad-id (:error (emit/layer-files (assoc quasi :id "Quasi")))))
    (is (= :trama/bad-param (:error (emit/layer-files (assoc quasi :params [['scale {:default 3.0}]])))))))

(deftest the-builtin-list
  (is (= 60 (count pfx/builtin-names) (count (distinct pfx/builtin-names))))
  (is (pfx/builtin-name? "Lattice 445")))

(deftest emit-writes-both-files
  (let [dir (str (Files/createTempDirectory "pfx" (make-array FileAttribute 0)))]
    (is (r/ok? (emit/emit! graphene dir)))
    (is (= #{"graphene_contours.pfx" "graphene_contours.wgsl"} (set (.list (io/file dir)))))))
