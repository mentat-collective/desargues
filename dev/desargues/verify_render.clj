(ns desargues.verify-render
  "Standalone end-to-end check that a MathFunction renders (function-oriented
   api reconciliation). Runs on the main thread via `lein run -m
   desargues.verify-render` to avoid nREPL/libpython GIL-thread stalls."
  (:require [libpython-clj2.python :as py]
            [emmy.env :as e]))

(defn -main [& _]
  (require 'desargues.manim-quickstart)
  ((resolve 'desargues.manim-quickstart/init!))          ; config-driven python init
  (require 'desargues.api 'desargues.infrastructure.manim-adapter
           'desargues.domain.math-expression 'desargues.domain.protocols)
  (let [create-function (resolve 'desargues.domain.math-expression/create-function)
        derivative      (resolve 'desargues.domain.protocols/derivative)
        to-mobject      (resolve 'desargues.domain.protocols/to-mobject)
        f   (create-function 'f (fn [x] (e/sin x)))
        df  (derivative f)
        mf  (to-mobject f)
        mdf (to-mobject df)]
    (println "f  =" (:name f) "-> mobject" (type mf)  "wrapping" (type (:py-obj mf)))
    (println "f' =" (:name df) "-> mobject" (type mdf) "wrapping" (type (:py-obj mdf)))
    (println "RENDER-OK: MathFunction is renderable end-to-end (Emmy -> LaTeX -> MathTex)"))
  (shutdown-agents)
  (System/exit 0))
