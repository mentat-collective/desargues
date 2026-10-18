(ns desargues.lesson.static-build
  "Static Clerk build of a generated lesson notebook, for the browser check
   (dev/e2e/lesson_render.mjs, `bb lesson:e2e`):

     clojure -M:notebooks:lesson-e2e -m desargues.lesson.static-build \\
       dist/lessons/area-under-curve/notebook.clj dist/lesson-e2e/area-under-curve

   Goes through emmy.clerk/build! with a viewer bundle that shadow-cljs
   compiles here from desargues.lesson.sci-extensions (Clerk's static app plus
   emmy-viewers' SCI namespaces), with the emmy-viewers CSS in place. The
   prebuilt emmy-viewers bundle on Clerk's CDN (emmy.clerk/custom-js, the
   default) throws on load and leaves the page blank. npm installs the JS deps
   into ./package.json and ./node_modules (gitignored). Exits non-zero when
   the build throws."
  (:require [emmy.clerk :as ec]
            [clojure.java.io :as io]
            [clojure.data.json :as json]))

(def npm-pins
  "npm versions pinned in ./package.json before the bundle build. mathbox's
   deps.cljs asks for three \">= 0.118.0 || <= 0.150.0\", which npm resolves
   to the newest three; three >= 0.160's CJS entry calls process.emitWarning,
   which the browser bundle has no polyfill for, and the page then throws
   before Mafs mounts. emmy-viewers pins 0.150.0 too."
  {"three" "0.150.0"})

(defn pin-npm-deps!
  "Merges [[npm-pins]] into ./package.json (created when missing), so that
   clerk-utils' npm install keeps them."
  []
  (let [f   (io/file "package.json")
        pkg (if (.exists f) (json/read-str (slurp f)) {})
        pkg (update pkg "dependencies" merge npm-pins)]
    (spit f (str (json/write-str pkg :indent true) "\n"))))

(defn build!
  "Builds `notebook` (a path) statically into `out-path`, as its index.html,
   with a viewer bundle shadow-cljs compiles from desargues.lesson.sci-extensions
   (npm deps from the classpath's deps.cljs go into ./node_modules)."
  [notebook out-path]
  (pin-npm-deps!)
  (ec/build! {:paths [notebook] :index notebook :out-path out-path
              :cljs-namespaces '[desargues.lesson.sci-extensions]}))

(defn -main [notebook out-path & _]
  (let [code (try (build! notebook out-path)
                  (println "static build:" out-path)
                  0
                  (catch Throwable t
                    (binding [*out* *err*] (println "static build FAILED:" (ex-message t)))
                    (.printStackTrace t)
                    1))]
    (shutdown-agents)
    (System/exit code)))
