(ns desargues.devaot
  "Opt-in fast cold start for the whole dev classpath: AOT-compile the
   library closure of the dev and bench namespaces (emmy, sci, malli,
   typed.clj, libpython-clj, dtype-next, core.async, ...) into a classes dir.
   With the classes on the path an AppCDS archive can hold them too (the JVM
   archives only classes the app class loader defines, i.e. not Clojure
   source compiled at runtime).

     clojure -M:dev:bench:devaot-build   # once: writes target/devaot (gitignored)
     clojure -M:dev:devaot               # REPL on the classes
     clojure -M:dev:devaot:cds           # plus an AppCDS archive, made on first exit

   desargues' own namespaces (every source file under a project directory on
   the build classpath) are compiled along the way but pruned from the
   output, so they always load from source: the REPL edit/reload loop never
   meets a stale class or a second identity for one of its protocols or
   records. The libpython binding class (desargues.pyaot) is written into
   the same dir, so :devaot replaces :pyaot.

   The classes are tied to the library versions they were compiled from
   (target/devaot/desargues/devaot-stamp.edn records the build classpath):
   rebuild after any dependency change."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [desargues.pyaot :as pyaot]))

(def default-out "target/devaot")

(def roots
  "Namespaces whose load closure is compiled. Roots that are not on the
   classpath are skipped (bench/ is there only with the :bench paths)."
  '[desargues.bench.render
    desargues.bench
    desargues.manim.core
    desargues.manim.patches
    desargues.manim.frames
    desargues.scene
    desargues.scene.manim
    desargues.devx.repl
    desargues.devx.watcher
    desargues.devx.reload
    typed.clojure
    clojure.spec.test.alpha
    libpython-clj2.python
    libpython-clj2.python.np-array])

;; ── pure ─────────────────────────────────────────────────────────────────

(defn ns-path
  "Namespace symbol -> its resource path stem (\"desargues/scene_data\")."
  [n]
  (-> (str n) (str/replace "-" "_") (str/replace "." "/")))

(defn ns-paths
  "Relative source file paths -> the set of their path stems without
   extension (\"desargues/scene/data\")."
  [rel-files]
  (into #{} (keep #(second (re-matches #"(.+)\.clj[cs]?" %))) rel-files))

(defn local-class?
  "Is class file `rel` (\"desargues/scene$fn__12.class\") generated from one of
   the namespace path stems in `nses`? A namespace owns its __init class, its
   fn classes (stem$...) and its deftype/defrecord/gen-class classes (stem/X)."
  [nses rel]
  (boolean
   (when-let [[_ base] (re-matches #"(.+)\.class" rel)]
     (let [stem   (first (str/split base #"\$" 2))
           init   (str/replace stem #"__init$" "")
           parent (some->> (str/last-index-of stem "/") (subs stem 0))]
       (some nses [stem init parent])))))

(defn local-dirs
  "Classpath entries of `cp` that are directories under `project-dir`,
   minus the `exclude` dirs (build output) and anything under them."
  [cp project-dir exclude]
  (let [canon #(.getCanonicalPath (io/file %))
        pd    (canon project-dir)
        ex    (mapv canon exclude)
        under (fn [d root] (or (= d root) (str/starts-with? d (str root "/"))))]
    (->> (str/split (or cp "") #":")
         (remove str/blank?)
         (filter #(.isDirectory (io/file %)))
         (map canon)
         (filter #(str/starts-with? % (str pd "/")))
         (remove (fn [d] (some #(under d %) ex)))
         distinct
         vec)))

;; ── effects ──────────────────────────────────────────────────────────────

(defn rel-files
  "Relative paths of the regular files under `dir` (none if it is missing)."
  [dir]
  (let [root (.toPath (io/file dir))]
    (->> (file-seq (io/file dir))
         (filter #(.isFile ^java.io.File %))
         (mapv #(str (.relativize root (.toPath ^java.io.File %)))))))

(defn- on-classpath? [n]
  (some #(io/resource (str (ns-path n) %)) [".clj" ".cljc" "__init.class"]))

(defn prune-local!
  "Delete from `out` the classes of the namespaces whose source lives in
   `local` dirs; returns how many."
  [out local]
  (let [nses (ns-paths (mapcat rel-files local))
        hits (filterv #(local-class? nses %) (rel-files out))]
    (run! #(io/delete-file (io/file out %)) hits)
    (count hits)))

(defn jar!
  "Pack the classes dir `dir` into jar file `jar`. A jar, not the dir, goes on
   the runtime classpath: AppCDS archives only classes loaded from jars."
  [dir jar]
  (let [tmp (io/file (str jar ".tmp"))]
    (io/make-parents tmp)
    (with-open [out (java.util.jar.JarOutputStream. (io/output-stream tmp))]
      (doseq [rel (sort (rel-files dir))]
        (.putNextEntry out (java.util.jar.JarEntry. (str/replace rel java.io.File/separator "/")))
        (io/copy (io/file dir rel) out)
        (.closeEntry out)))
    (.renameTo tmp (io/file jar))
    jar))

(defn source-loaded
  "Loaded namespaces (symbols) that ran from source: neither `out` nor any
   jar holds their __init class, and they are not desargues' own (`local?`)."
  [loaded out local?]
  (->> loaded
       (remove local?)
       (remove #(or (.exists (io/file out (str (ns-path %) "__init.class")))
                    (io/resource (str (ns-path %) "__init.class"))))
       sort vec))

(defn compile-stragglers!
  "Compile the library namespaces the root pass did not write (`require`
   skips a namespace that was already loaded before *compile-files* was on,
   e.g. by user.clj or the tool itself); returns them."
  [out local?]
  (let [todo (source-loaded (map symbol (map str @@#'clojure.core/*loaded-libs*)) out local?)]
    (binding [*compile-path* out]
      (doseq [n todo]
        (try (compile n) (catch Throwable _ nil))))
    todo))

(def launcher-source
  "Resource path of desargues.devaot.launch, shipped in the jar as source so
   a jar-only classpath (AppCDS) can start through it."
  "desargues/devaot/launch.clj")

(defn build!
  "Compile the load closure of `roots` into `out`, then the library
   namespaces that slipped through (compile-stragglers!), prune desargues'
   own namespaces, write the libpython binding class, the launcher source and
   a stamp, and pack the lot into `out`.jar. `out` must be on the classpath
   (Clojure's compile requires it)."
  [{:keys [out] :or {out default-out}}]
  (let [cp (System/getProperty "java.class.path")]
    (when-not (pyaot/on-classpath? cp out)
      (throw (ex-info (str out " is not on the classpath; run clojure -M:dev:bench:devaot-build")
                      {:out out})))
    (.mkdirs (io/file out))
    (let [present (filterv on-classpath? roots)
          t0      (System/nanoTime)
          local   (local-dirs cp "." [out "target"])
          nses    (ns-paths (mapcat rel-files local))
          local?  #(contains? nses (ns-path %))]
      (binding [*compile-path* out *compile-files* true]
        (run! require present))
      (let [strays (compile-stragglers! out local?)
            pruned (prune-local! out local)
            cls    (pyaot/write-library-class! out)
            stamp  (io/file out "desargues" "devaot-stamp.edn")
            res    {:out out :roots present :local-dirs local :pruned pruned
                    :stragglers strays :library-class cls :classes (count (rel-files out))}]
        (io/make-parents (io/file out launcher-source))
        (io/copy (slurp (io/resource launcher-source)) (io/file out launcher-source))
        (spit stamp (pr-str (assoc res :classpath cp)))
        (assoc res
               :jar (jar! out (str out ".jar"))
               :build-s (/ (- (System/nanoTime) t0) 1e9))))))

(defn -main [& [out]]
  (println "devaot:" (dissoc (build! (cond-> {} out (assoc :out out))) :roots :local-dirs))
  (shutdown-agents)
  (System/exit 0))
