(ns desargues.tex.label
  "TeX labels inside scenes, the pure half.

   A scene records a TeX label as a text node whose text is \"tex:\" followed
   by the LaTeX; the label's colour and size are the node's own options. After
   recording, a typesetter swaps each such node for an :image of its typeset
   SVG (desargues.tex). Until then the node shows its source."
  (:require [clojure.string :as str]
            [desargues.scene :as s]))

(def prefix "tex:")

(defn label
  "A TeX label centred at [x y]. :size is the font size in px the formula
   should match (60 px per world unit); :color a scene colour keyword."
  [latex at & {:keys [size color] :or {size 24 color :white}}]
  (-> (s/text (str prefix latex) :font-size size :color color)
      (s/move-to at)))

(defn tex-node?
  "True when a recorded scene node is a TeX label."
  [nd]
  (and (map? nd) (= :text (:node nd)) (string? (:text nd))
       (str/starts-with? (:text nd) prefix)))

(defn node-latex [nd] (subs (:text nd) (count prefix)))

(defn node-color [nd] (get-in nd [:opts :color] :white))

(defn node-size [nd] (get-in nd [:opts :font-size] 24))

(def palette
  "Scene colour keywords as hex: plato's scene palette, which is Manim's."
  {:teal "#5CD0B3" :gold "#F0AC5F" :grey "#8A8F98" :gray "#8A8F98" :red "#FC6255"
   :yellow "#FFFF3B" :white "#FFFFFF" :blue "#58C4DD" :green "#83C167"
   :purple "#9A72AC" :orange "#FF862F" :pink "#FF69B4" :black "#000000"})

(defn hex
  "A colour as a hex string: palette keywords map through `palette` (unknown
   keywords fall back to white); strings pass through unchanged."
  [c]
  (if (keyword? c) (palette c "#FFFFFF") c))

(defn key-of
  "The manifest key of a label: [latex hex-colour]."
  [latex color]
  [latex (hex color)])

(defn node-key
  "The manifest key of a recorded TeX label node."
  [nd]
  (key-of (node-latex nd) (node-color nd)))

(defn wanted-keys
  "Every manifest key [latex hex-colour] the recorded scene graphs ask for."
  [graphs]
  (set (for [g graphs nd (vals (:nodes g)) :when (tex-node? nd)]
         (node-key nd))))

(defn wanted
  "Every [latex colour] the recorded scene graphs ask to be typeset."
  [graphs]
  (set (for [g graphs nd (vals (:nodes g)) :when (tex-node? nd)]
         [(node-latex nd) (node-color nd)])))
