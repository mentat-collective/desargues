(ns desargues.devaot.launch
  "Launcher for an AppCDS run on the devaot classes. The JVM writes a class
   archive only when every classpath entry is a jar (a non-empty directory
   aborts the dump), and the dev classpath has source dirs (src, dev, the
   git deps). So the JVM gets only the jars on -cp, and this launcher adds the
   source dirs (system property desargues.devaot.dirs, ':'-separated) in a
   DynamicClassLoader before it calls the real main:

     java -XX:AOTCache=target/devaot.aot \\
          -cp target/devaot.jar:<jars> -Ddesargues.devaot.dirs=<dirs> \\
          clojure.main -m desargues.devaot.launch clojure.main [args]

   `bb devaot:run` and the bench's jar-only launch profiles build that argv.
   desargues.devaot copies this file into target/devaot.jar, so it loads
   before the dirs are there.")

(def dirs-property "desargues.devaot.dirs")

(defn split-classpath
  "Classpath string -> {:jars [..] :dirs [..]}, order kept."
  [cp]
  (let [entries (remove #(= "" %) (.split (str cp) ":"))
        jar?    #(.endsWith ^String % ".jar")]
    {:jars (vec (filter jar? entries)) :dirs (vec (remove jar? entries))}))

(defn -main
  "Add the dirs, then run `main-ns`/-main (clojure.main/main for
   clojure.main: a REPL or a script) with `args` under that loader."
  [main-ns & args]
  (let [cl (clojure.lang.DynamicClassLoader. (.getContextClassLoader (Thread/currentThread)))]
    (doseq [d (:dirs (split-classpath (System/getProperty dirs-property)))]
      (.addURL cl (.toURL (.toURI (java.io.File. ^String d)))))
    (.setContextClassLoader (Thread/currentThread) cl)
    (with-bindings {clojure.lang.Compiler/LOADER cl}
      (apply (or (requiring-resolve (symbol main-ns "-main"))
                 (requiring-resolve (symbol main-ns "main")))
             args))))
