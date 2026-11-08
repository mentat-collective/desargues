(ns desargues.lesson.sci-extensions
  "The viewer bundle of a lesson's static build (desargues.lesson.static-build):
   Clerk's static app plus emmy-viewers' SCI namespaces (Mafs, MathBox, Leva,
   reagent ...). Compiled locally by shadow-cljs, because the prebuilt
   emmy-viewers bundle on Clerk's CDN (emmy.clerk/custom-js) throws while it
   initializes and leaves the page blank."
  (:require [emmy.viewer.sci]
            [nextjournal.clerk.sci-env]))

(emmy.viewer.sci/install!)
