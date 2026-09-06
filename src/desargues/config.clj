(ns desargues.config
  "Environment-driven configuration (hive-di defconfig).

   Replaces hardcoded interpreter/library paths. Every field is overridable
   via its env var. This is the Collect layer of the pipeline — the single
   source of truth for environment settings, resolved (and type-coerced) once
   at the boundary.

   Defaults are DERIVED from the running environment, never baked to one
   machine: this namespace ships to library consumers, so a literal home
   directory here is a bug that only reproduces on someone else's box."
  (:require [clojure.java.io :as io]
            [hive-di.core :refer [defconfig env]]
            [libpython-clj2.python :as py]
            [clojure.java.shell :as sh]
            [clojure.string :as str]))

(def ^:private python-abis
  "Probed newest-first. A manim env pins exactly one of these."
  ["3.13" "3.12" "3.11" "3.10"])

(defn- conda-prefix
  "Root of the conda env providing Manim, or nil. DESARGUES_CONDA_PREFIX wins,
   so a consumer can target a manim env that is not the active one."
  []
  (or (System/getenv "DESARGUES_CONDA_PREFIX")
      (System/getenv "CONDA_PREFIX")))

(defn- first-existing
  [paths]
  (first (filter #(and % (.exists (io/file %))) paths)))

(defn- conda-python []
  (when-let [p (conda-prefix)]
    (first-existing [(str p "/bin/python")])))

(defn- conda-libpython []
  (when-let [p (conda-prefix)]
    (first-existing (map #(str p "/lib/libpython" % ".so") python-abis))))

(defn- conda-site-packages []
  (when-let [p (conda-prefix)]
    (first-existing (map #(str p "/lib/python" % "/site-packages") python-abis))))

(defn- detect-project-root
  "Repo root, derived from where this namespace was loaded from
   (<root>/src/desargues/config.clj). Resolves under a source checkout and
   under a tools.deps git checkout in ~/.gitlibs; nil when loaded from a jar,
   where the .py scene files are not on disk anyway."
  []
  (when-let [u (io/resource "desargues/config.clj")]
    (when (= "file" (.getProtocol u))
      (-> (io/file (.toURI u))
          .getParentFile .getParentFile .getParentFile
          .getAbsolutePath))))

(defconfig ManimConfig
  :python-exe    (env "DESARGUES_MANIM_PYTHON"
                      :default (conda-python)
                      :type :string)
  :library-path  (env "DESARGUES_MANIM_LIBPYTHON"
                      :default (conda-libpython)
                      :type :string)
  :site-packages (env "DESARGUES_MANIM_SITEPACKAGES"
                      :default (conda-site-packages)
                      :type :string)
  :project-root  (env "DESARGUES_PROJECT_ROOT"
                      :default (detect-project-root)
                      :type :string)
  :quality       (env "DESARGUES_QUALITY"
                      :default "medium_quality"
                      :type :string))

(defn manim-config
  "Resolve the manim configuration to a typed map, throwing on invalid config."
  []
  (let [result (resolve-ManimConfig)]
    (or (:ok result)
        (throw (ex-info
                (str "Invalid manim configuration. No Manim conda env was detected and "
                     "no explicit override was supplied. Either activate the env (so "
                     "CONDA_PREFIX is set), point DESARGUES_CONDA_PREFIX at it, or set "
                     "DESARGUES_MANIM_PYTHON / DESARGUES_MANIM_LIBPYTHON / "
                     "DESARGUES_MANIM_SITEPACKAGES / DESARGUES_PROJECT_ROOT individually.")
                {:result       result
                 :conda-prefix (conda-prefix)})))))

(defn probe-manim-import
  "Run `import manim` in the resolved python.
   {:ok? true :version s :python-exe p} or {:ok? false :error s :python-exe p|nil};
   a config that does not resolve is :ok? false with the resolver's message."
  []
  (try
    (let [{:keys [python-exe]} (manim-config)
          {:keys [exit out err]} (sh/sh python-exe "-c" "import manim, sys; print(manim.__version__)")]
      (if (zero? exit)
        {:ok? true :version (str/trim out) :python-exe python-exe}
        {:ok? false :error (or (last (str/split-lines (str err))) "import failed") :python-exe python-exe}))
    (catch Throwable t
      {:ok? false :error (ex-message t) :python-exe nil})))

(defn manim-ready?
  "True when the resolved python can import manim (the guard for Manim suites)."
  []
  (:ok? (probe-manim-import)))

(defn add-project-to-syspath!
  "Prepend the directory holding the desargues Python scene modules onto
   Python sys.path so py/import-module can load them. The scene files
   (manim_examples.py, emmy_manim_scenes.py, ...) live under `<root>/py`, so
   the no-arg form adds that directory; pass a subdir (or nil for the bare
   project root) to add a different one. Returns the dir added."
  ([] (add-project-to-syspath! "py"))
  ([subdir]
   (let [root (:project-root (manim-config))
         dir  (if subdir (str root "/" subdir) root)
         sys  (py/import-module "sys")]
     (py/call-attr (py/get-attr sys "path") "insert" 0 dir)
     dir)))
