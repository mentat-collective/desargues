(ns desargues.lesson.construction-render-test
  "Constructions have a render path with no optional alias: the :jvm kernel
   compiler samples the board, desargues.board.picture lays it out as a Mafs
   fragment for the notebook and as a scene graph for the headless PNG host."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [desargues.board :as board]
            [desargues.board.compiler :as compiler]
            [desargues.board.picture :as picture]
            [desargues.lesson.export :as ex])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def midpoint
  {:id :midpoint :kind :construction
   :window {:x [-1 3] :y [-1 2]}
   :points [{:id :A :at [0 0]} {:id :B :at [2 0]} {:id :C :at [0.5 1.5]}
            {:id :M :op :mid :of [:A :B]}
            {:id :P :op :meet :lines [[:A :C] [:B :M]]}]
   :draw [[:polygon [:A :B :C]] [:segment :C :M] [:point :M {:label "M"}]]
   :checks [{:distance [:A :M] :label "|AM|"} {:distance [:M :B] :label "|MB|"}]})

(defn- read-all [s] (read-string (str "[" s "]")))

(defn- tmp-dir []
  (str (Files/createTempDirectory "construction-render-test" (make-array FileAttribute 0))))

(deftest the-jvm-compiler-samples-a-construction
  (let [b (board/sample-board midpoint)
        prims (picture/primitives b)
        pt (fn [label] (:at (first (filter #(and (= :point (:prim %)) (= label (:label %))) prims))))]
    (is (nil? (:board/kernel b)) "nothing compiled to wasm")
    (is (= [1.0 0.0] (pt "M")) "M is the midpoint of AB")
    (is (= [0.5 1.5] (pt "C")) "a free point is drawn where it starts")
    (is (= #{{:prim :value :label "|AM|" :value 1.0} {:prim :value :label "|MB|" :value 1.0}}
           (set (filter #(= :value (:prim %)) prims))))
    (testing "it emits no WebAssembly, and says so"
      (is (= :unsupported
             (try (compiler/wasm-bytes (compiler/kernel-compiler :jvm) nil "x") nil
                  (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))

(deftest a-construction-block-is-a-mafs-figure-in-the-notebook
  (let [l {:lesson/id :c :lesson/title "c" :lesson/version "1"
           :lesson/slides [{:slide/id :s :slide/title "s"
                            :slide/blocks [{:block/id :mid :block/kind :construction :construction midpoint}]}]}
        s (ex/notebook-source l)
        forms (read-all s)
        frag (some #(when (and (seq? %) (= 'ev/fragment (first %))) (second (second %))) forms)]
    (is (not (str/includes? s "TODO")))
    (is (some #(and (seq? %) (= 'def (first %)) (= 'block-mid (second %))) forms) "the data stays")
    (is (= 'mafs.core/Mafs (first frag)) "a real figure form")
    (is (= {:x [-1.0 3.0] :y [-1.0 2.0]} (:view-box (second frag))))
    (is (some #(= ['mafs.core/Point {:x 1.0 :y 0.0 :color "#F0AC5F"}] %) (filter vector? frag)) "M, at (1, 0)")
    (is (some #(= 'mafs.core/Polygon (first %)) (filter vector? frag)))
    (is (some #(= 'mafs.line/Segment (first %)) (filter vector? frag)))
    (is (some #(= ['mafs.core/Text {:x 1.0 :y 0.0 :attach "ne" :color "#F0AC5F"} "M"] %) (filter vector? frag)))
    (is (= s (ex/notebook-source l)) "deterministic")))

(deftest media-of-a-construction-is-a-png
  (let [d (tmp-dir)
        l {:lesson/id :c :lesson/title "c" :lesson/version "1"
           :lesson/slides [{:slide/id :s :slide/title "s"
                            :slide/blocks [{:block/id :mid :block/kind :construction :construction midpoint}
                                           {:block/id :mid-png :block/kind :media :from :mid :format :png}]}]}
        m (ex/build! l {:out d})]
    (if (seq (:skipped m))
      (is (= [{:block :mid-png :format :png :from :mid :reason :no-awt}] (:skipped m))
          "the only reason a construction PNG may be skipped is a JVM without AWT")
      (let [f (io/file d "media/mid-png.png")
            img (javax.imageio.ImageIO/read f)]
        (is (contains? (:files m) "media/mid-png.png"))
        (is (= [960 540] [(.getWidth img) (.getHeight img)]))
        (is (str/includes? (slurp (io/file d "notebook.clj")) "data:image/png;base64,"))
        (is (= {:reproducible? true :mismatches []} (ex/verify! d)))))))
