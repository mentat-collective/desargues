(ns desargues.tex.typeset
  "The Typesetter port: LaTeX in, SVG out.

   ITypesetter is the one capability: -typeset a LaTeX math string in a colour
   and answer {:svg string :w pt :h pt} (extent in TeX points at the 10 pt
   design size) or {:error {...}}. Two adapters:

     LatexTypesetter  latex + dvisvgm in a fresh temp directory
     FakeTypesetter   deterministic SVG, no processes; records what it was asked

   The document source and the SVG extent parse are pure functions here, so
   the adapter is only the process boundary."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [desargues.tex.label :as label]))

(defprotocol ITypesetter
  (-typeset [this latex colour]
    "Typeset `latex` (math mode) in `colour` (palette keyword or hex string).
     Returns {:svg string :w pt :h pt} or {:error map}."))

(defn typeset
  "Typeset through any ITypesetter. See -typeset."
  [typesetter latex colour]
  (-typeset typesetter latex colour))

;; ---------------------------------------------------------------- pure half

(defn- hex-digits [c] (str/upper-case (subs (label/hex c) 1)))

(defn colour-definitions
  "\\definecolor lines for every palette colour by its keyword name, so
   \\textcolor{gold}{...} works inside a label."
  []
  (str/join "\n" (for [[k _] (sort-by key label/palette)]
                   (str "\\definecolor{" (name k) "}{HTML}{" (hex-digits k) "}"))))

(defn document
  "The complete standalone LaTeX document for one label: `latex` in display
   math, coloured `colour`."
  [latex colour]
  (str "\\documentclass[preview,border=0pt]{standalone}\n"
       "\\usepackage{amsmath}\n\\usepackage{amssymb}\n\\usepackage{xcolor}\n"
       (colour-definitions) "\n"
       "\\definecolor{labelfg}{HTML}{" (hex-digits colour) "}\n"
       "\\begin{document}\n"
       "\\color{labelfg}$\\displaystyle " latex "$\n"
       "\\end{document}\n"))

(defn- pt-attr [svg attr]
  (when-let [[_ v] (re-find (re-pattern (str "<svg[^>]*\\s" attr "=['\"]([0-9.eE+-]+)(?:pt)?['\"]")) svg)]
    (parse-double v)))

(defn svg-extent
  "The [w h] in TeX points of an SVG written by dvisvgm, or nil."
  [svg]
  (let [w (pt-attr svg "width") h (pt-attr svg "height")]
    (when (and w h) [w h])))

;; ---------------------------------------------------------------- adapters

(defn- temp-dir! []
  (.toFile (java.nio.file.Files/createTempDirectory
            "desargues-tex" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- delete-tree! [^java.io.File dir]
  (doseq [f (reverse (file-seq dir))] (.delete ^java.io.File f)))

(defn- run [dir & args]
  (apply sh/sh (concat args [:dir dir])))

(defrecord LatexTypesetter [latex-bin dvisvgm-bin]
  ITypesetter
  (-typeset [_ latex colour]
    (let [dir (temp-dir!)]
      (try
        (spit (io/file dir "label.tex") (document latex colour))
        (let [l (run dir latex-bin "-interaction=nonstopmode" "-halt-on-error" "label.tex")]
          (if-not (zero? (:exit l))
            {:error {:stage :latex :latex latex :log (:out l)}}
            (let [d (run dir dvisvgm-bin "--no-fonts" "--exact-bbox" "-o" "label.svg" "label.dvi")
                  out (io/file dir "label.svg")]
              (if-not (and (zero? (:exit d)) (.exists out))
                {:error {:stage :dvisvgm :latex latex :log (str (:out d) (:err d))}}
                (let [svg (slurp out)]
                  (if-let [[w h] (svg-extent svg)]
                    {:svg svg :w w :h h}
                    {:error {:stage :extent :latex latex}}))))))
        (catch java.io.IOException e
          {:error {:stage :process :latex latex :message (.getMessage e)}})
        (finally (delete-tree! dir))))))

(defn latex-typesetter
  "A LatexTypesetter over the latex and dvisvgm binaries (default: from PATH)."
  ([] (latex-typesetter "latex" "dvisvgm"))
  ([latex-bin dvisvgm-bin] (->LatexTypesetter latex-bin dvisvgm-bin)))

(defn available?
  "True when both binaries of a LatexTypesetter run."
  [{:keys [latex-bin dvisvgm-bin]}]
  (try (and (zero? (:exit (sh/sh latex-bin "--version")))
            (zero? (:exit (sh/sh dvisvgm-bin "--version"))))
       (catch java.io.IOException _binary-not-found false)))

(defrecord FakeTypesetter [calls fail?]
  ITypesetter
  (-typeset [_ latex colour]
    (swap! calls conj [latex colour])
    (if (fail? latex)
      {:error {:stage :fake :latex latex}}
      (let [w (* 5.0 (count latex)) h 10.0]
        {:svg (str "<svg xmlns='http://www.w3.org/2000/svg' width='" w "pt' height='" h "pt'>"
                   "<!-- " (label/hex colour) " --></svg>")
         :w w :h h}))))

(defn fake-typesetter
  "A FakeTypesetter: width 5 pt per character, height 10 pt; fails on the
   latex strings for which `fail?` is truthy. Its :calls atom records every
   [latex colour] asked."
  ([] (fake-typesetter (constantly false)))
  ([fail?] (->FakeTypesetter (atom []) fail?)))
