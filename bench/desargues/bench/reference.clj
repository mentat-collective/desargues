(ns desargues.bench.reference
  "The reference subject every desargues bench renders: a titled grid of
   bordered text and math cells. It depends on the layout algebra only, so a
   render bench (desargues.bench.render, its child JVMs) can load it without
   the pure-pipeline suites of desargues.bench (Emmy, physics, geometry)."
  (:require [desargues.layout.core :as l]))

(defn layout-tree
  "A title over a 4-column grid of `n` bordered cells (text and math alternating)."
  [n]
  (let [cols 4
        rows (max 1 (quot n cols))]
    (apply l/column {:width :fill :height :fill :padding 0.5 :spacing 0.2 :align :center-x}
           (l/text "Benchmark" {:font-size 36})
           (for [r (range rows)]
             (apply l/row {:width :fill :spacing 0.2}
                    (for [c (range cols)]
                      (l/box {:width [:portion 1] :border :white}
                        (if (even? (+ r c))
                          (l/text (str "cell " r "," c))
                          (l/math "\\alpha_{ij}")))))))))
