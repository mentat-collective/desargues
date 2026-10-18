(ns desargues.lesson.sci-extensions
  "The viewer bundle of a lesson's static build (desargues.lesson.static-build):
   Clerk's static app plus emmy-viewers' SCI namespaces (Mafs, MathBox, Leva,
   reagent ...). Compiled locally by shadow-cljs, because the prebuilt
   emmy-viewers bundle on Clerk's CDN (emmy.clerk/custom-js) throws while it
   initializes and leaves the page blank."
  (:require [emmy.viewer.plot :as vp]
            [emmy.viewer.sci]
            [nextjournal.clerk.sci-env]))

;; MathBox's Format calls its :expr with (x i time delta ...); emmy-viewers'
;; Ticks hands it emmy.viewer.plot/format-number, which has only 1 and 2
;; arities, so every tick label of a MathBox scene throws "Invalid arity".
;; Accept and ignore the extra arguments.
(let [f vp/format-number]
  (set! vp/format-number
        (fn
          ([x] (f x 2))
          ([x n] (f x (if (number? n) n 2)))
          ([x _ & _] (f x 2)))))

(emmy.viewer.sci/install!)
