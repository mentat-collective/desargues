(ns desargues.tex-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [desargues.tex :as tex]
            [desargues.tex.label :as label]
            [desargues.tex.typeset :as ts]))

(defn- label-node [latex color size]
  {:node :text :text (str label/prefix latex) :opts {:color color :font-size size}})

(def graph
  {:scene "demo"
   :nodes {:a (label-node "p \\land q" :gold 30)
           :b (label-node "\\neg p" :white 24)
           :c {:node :text :text "plain" :opts {:color :teal}}}})

(defn- temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory
            "desargues-tex-test" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- delete-tree! [^java.io.File dir]
  (doseq [f (reverse (file-seq dir))] (.delete ^java.io.File f)))

(deftest label-keys
  (is (= ["x" "#F0AC5F"] (label/key-of "x" :gold)))
  (is (= ["x" "#123456"] (label/key-of "x" "#123456")))
  (is (= #{["p \\land q" "#F0AC5F"] ["\\neg p" "#FFFFFF"]} (label/wanted-keys [graph]))))

(deftest texify-with-a-manifest
  (let [manifest {["p \\land q" "#F0AC5F"] {:file "assets/tex/aaa.svg" :w 20.0 :h 10.0}}
        {g :graph missing :missing} (tex/texify graph manifest)]
    (testing "a known label becomes an :image sized em = size/60, extent / 10 pt"
      (is (= {:node :image :content "assets/tex/aaa.svg" :opts {:width 1.0 :height 0.5}}
             (get-in g [:nodes :a]))))
    (testing "an unknown label shows its source and is reported"
      (is (= "\\neg p" (get-in g [:nodes :b :text])))
      (is (= :text (get-in g [:nodes :b :node])))
      (is (= #{"\\neg p"} missing)))
    (testing "other nodes and graph keys are untouched"
      (is (= (get-in graph [:nodes :c]) (get-in g [:nodes :c])))
      (is (= "demo" (:scene g))))))

(deftest plan-is-what-the-manifest-lacks
  (let [have {["\\neg p" "#FFFFFF"] {:file "f" :w 1 :h 1}}]
    (is (= [{:key ["p \\land q" "#F0AC5F"] :name (tex/asset-name ["p \\land q" "#F0AC5F"])}]
           (tex/plan [graph] have)))
    (is (re-matches #"[0-9a-f]{12}\.svg" (tex/asset-name ["x" "#FFFFFF"])))))

(deftest document-and-extent
  (let [doc (ts/document "\\textcolor{gold}{p}" :teal)]
    (is (str/includes? doc "\\definecolor{gold}{HTML}{F0AC5F}"))
    (is (str/includes? doc "\\definecolor{labelfg}{HTML}{5CD0B3}"))
    (is (str/includes? doc "\\usepackage{amssymb}")))
  (is (= [12.5 7.25] (ts/svg-extent "<?xml?>\n<svg version='1.1' width='12.5pt' height='7.25pt' viewBox='0 0 1 1'>")))
  (is (nil? (ts/svg-extent "<svg></svg>"))))

(deftest ensure-with-the-fake-typesetter
  (let [dir (temp-dir)]
    (try
      (let [fake (ts/fake-typesetter #(= % "\\neg p"))
            r1 (tex/ensure! fake (str dir) [graph] :href-prefix "assets/tex/")
            k ["p \\land q" "#F0AC5F"]
            entry (get-in r1 [:manifest k])]
        (testing "typesets only what is missing, writes SVG + manifest"
          (is (= [k] (:added r1)))
          (is (= #{["\\neg p" "#FFFFFF"]} (set (keys (:failed r1)))))
          (is (= {:file (str "assets/tex/" (tex/asset-name k)) :w 45.0 :h 10.0} entry))
          (is (.exists (io/file dir (tex/asset-name k))))
          (is (= (:manifest r1) (tex/read-manifest (str dir)))))
        (testing "a second run asks the typesetter only for the failure"
          (reset! (:calls fake) [])
          (let [r2 (tex/ensure! fake (str dir) [graph] :href-prefix "assets/tex/")]
            (is (= [["\\neg p" "#FFFFFF"]] @(:calls fake)))
            (is (= [] (:added r2)))
            (is (= (:manifest r1) (:manifest r2))))))
      (finally (delete-tree! dir)))))

(deftest latex-round-trip
  (let [latex (ts/latex-typesetter)]
    (if-not (ts/available? latex)
      (println "desargues.tex-test: latex/dvisvgm absent, skipping round trip")
      (let [dir (temp-dir)]
        (try
          (let [g {:nodes {:a (label-node "p \\to \\textcolor{gold}{q}" :teal 24)}}
                {m :manifest added :added failed :failed} (tex/ensure! latex (str dir) [g])
                {tg :graph missing :missing} (tex/texify g m)
                {:keys [file w h]} (m ["p \\to \\textcolor{gold}{q}" "#5CD0B3"])]
            (is (empty? failed) (pr-str failed))
            (is (= 1 (count added)))
            (is (< 10 w 60) (str "width pt " w))
            (is (< 4 h 15) (str "height pt " h))
            (is (str/includes? (slurp file) "<svg"))
            (is (= :image (get-in tg [:nodes :a :node])))
            (is (empty? missing)))
          (finally (delete-tree! dir)))))))
