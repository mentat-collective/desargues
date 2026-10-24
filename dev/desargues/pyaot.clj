(ns desargues.pyaot
  "Opt-in fast start for the Python bridge: AOT-compile libpython-clj and
   dtype-next into a classes dir and pre-generate the JNA binding class of
   libpython, so a fresh JVM neither compiles their Clojure source nor
   generates the binding bytecode inside initialize!.

     clojure -M:pyaot-build          # writes target/pyaot (gitignored)
     clojure -M:pyaot:dev ...        # put the classes on the classpath
     clojure -M:pyaot:c1:dev ...     # plus C1-only JIT (see deps.edn)

   desargues.manim.core/init! picks the binding class up when it is on the
   classpath (`core/prebuilt-library`) and falls back to runtime generation
   when it is not. Rebuild after any libpython-clj or dtype-next upgrade:
   the classes are tied to the versions they were compiled from (the build
   writes them to target/pyaot/desargues/pyaot-stamp.edn)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def default-out "target/pyaot")

(def namespaces
  "Bridge namespaces desargues loads; compile pulls in their transitive deps
   (tech.v3.datatype, ham-fisted, insn, data.json, tools.logging...)."
  '[libpython-clj2.python
    libpython-clj2.python.ffi
    libpython-clj2.python.class
    libpython-clj2.python.base
    libpython-clj2.python.np-array
    tech.v3.datatype.ffi.jna])

(defn on-classpath?
  "Is directory `dir` an entry of classpath string `cp`?"
  [cp dir]
  (let [canon #(.getCanonicalPath (io/file %))]
    (boolean (some #(= (canon dir) (canon %))
                   (remove str/blank? (str/split (or cp "") #":"))))))

(defn- dep-stamp
  "Classpath jars of the compiled libraries, to tell a stale build apart."
  [cp]
  (vec (sort (filter #(re-find #"libpython-clj|dtype-next|ham-fisted|insn" %)
                     (str/split cp #":")))))

(defn build!
  "Compile `namespaces` and the libpython binding class into `out`. `out` must
   be on the classpath (Clojure's compile requires it)."
  [{:keys [out] :or {out default-out}}]
  (let [cp (System/getProperty "java.class.path")]
    (when-not (on-classpath? cp out)
      (throw (ex-info (str out " is not on the classpath; run clojure -M:pyaot-build")
                      {:out out})))
    (.mkdirs (io/file out))
    (binding [*compile-path* out *compile-files* true]
      (doseq [n namespaces] (compile n)))
    (let [cls  @(requiring-resolve 'desargues.manim.core/prebuilt-library-class)
          gen! (requiring-resolve 'libpython-clj2.python.ffi/define-library!)]
      (binding [*compile-path* out *compile-files* true]
        (gen! (symbol cls)))
      (let [stamp (io/file out "desargues" "pyaot-stamp.edn")]
        (io/make-parents stamp)
        (spit stamp (pr-str {:library-class cls :namespaces namespaces :deps (dep-stamp cp)})))
      {:out out :library-class cls})))

(defn -main [& [out]]
  (println "pyaot:" (build! (cond-> {} out (assoc :out out))))
  (shutdown-agents)
  (System/exit 0))
