(ns desargues.core
  (:require [desargues.emmy-manim-examples :as ex]
            [desargues.manim-quickstart :as mq]
            [desargues.videos.render :as render]
            [emmy.env :as e])
  (:gen-class))

(defn -main
  "Render the bundled demos (needs the Manim env, see `clojure -M:doctor`).

   Usage:
     clojure -M:run                 brachistochrone, full derivation
     clojure -M:run intro           the brachistochrone intro only
     clojure -M:run step N          derivation step N (1-12)
     clojure -M:run quickstart      Manim quickstart (pink circle)
     clojure -M:run derivative      sin(x) beside its derivative"
  [& args]
  (println "=== desargues: mathematical animation generator ===\n")

  (let [cmd (first args)]
    (case cmd
      nil
      (do
        (println "Rendering full brachistochrone derivation...")
        (render/render-full-derivation))

      "intro"
      (do
        (println "Rendering brachistochrone intro...")
        (render/render-intro))

      "step"
      (let [n (Integer/parseInt (second args))]
        (println (str "Rendering brachistochrone step " n "..."))
        (render/render-step n))

      "quickstart"
      (do
        (println "Running Manim quickstart demo...")
        (mq/quickstart!))

      "derivative"
      (let [my-func    (fn [x] (e/sin x))
            deriv-func (e/D my-func)]
        (println "Initializing Python and Manim...")
        (mq/init!)
        (println "\nCreating mathematical function: sin(x)")
        (println "Computing derivative using Emmy...")
        (println "\nLaTeX representations:")
        (println "  f(x)  =" (e/->TeX (my-func 'x)))
        (println "  f'(x) =" (e/->TeX (deriv-func 'x)))
        (println "\nRendering animation...")
        (ex/create-derivative-animation my-func))

      (println (str "Unknown command: " cmd "\n"
                    "Usage: clojure -M:run [intro|step N|quickstart|derivative]"))))

  (println "\n=== Done! ==="))
