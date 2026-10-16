(ns desargues.calc-emmy-check
  "Smoke check of the ->emmy-viewers target without a browser:

     clojure -M:notebooks -m desargues.calc-emmy-check

   Builds every demo figure's emmy-viewers fragment (Mafs or MathBox, and the
   live Leva viewer for the plane ones) and evaluates the notebook once
   through Clerk (no server), exiting non-zero on any error."
  (:require [desargues.calc-emmy :as ce]
            [desargues.objects.calc.demos :as demos]
            [nextjournal.clerk :as clerk]))

(defn -main [& _]
  (doseq [[k {:keys [figure controls]}] (sort-by key demos/demos)]
    (let [space? (= :space (get-in figure [:view :view]))
          frag   (if space? (ce/figure->mathbox figure) (ce/figure->mafs figure))]
      (when-not space? (ce/figure->viewer figure controls))
      (println k (if space? "mathbox" "mafs") (count (pr-str frag)) "chars")))
  (clerk/build! {:paths ["notebooks/calculus_objects.clj"] :out-path "/tmp/calc-notebook"})
  (println "notebook built: /tmp/calc-notebook")
  (shutdown-agents)
  (System/exit 0))
