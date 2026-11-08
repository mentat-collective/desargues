(ns desargues.manim.core
  "Core Manim bindings - initialization and module access.

   This namespace provides:
   - Python/Manim initialization
   - Module and class access helpers
   - Base rendering functionality"
  (:require [libpython-clj2.python :as py]
            [libpython-clj2.python.ffi :as py-ffi]
            [libpython-clj2.python.class :as py-class]
            [desargues.config :as config]
            [clojure.java.io :as io]))

;; ============================================================================
;; State
;; ============================================================================

(defonce ^:private initialized? (atom false))
(defonce ^:private manim-module (atom nil))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn preload-paths
  "Colon-separated preload string -> vector of non-blank paths. nil-safe."
  [s]
  (into [] (remove clojure.string/blank?) (clojure.string/split (or s "") #":")))

(defn- preload-libs!
  "dlopen each path RTLD_GLOBAL so a later extension import finds it by soname."
  [paths]
  (when (seq paths)
    (let [ctypes (py/import-module "ctypes")
          mode   (py/get-attr ctypes "RTLD_GLOBAL")
          cdll   (py/get-attr ctypes "CDLL")]
      (doseq [p paths]
        (py/call-attr-kw cdll "__call__" [p] {:mode mode})))))

(defmulti import-plan
  "Import mode name -> {:env {var value} :defer [module ...]}: environment set
   inside Python before `import manim` (only where not already set) and modules
   deferred to first use. \"eager\" is the original plain import."
  identity)

(defmethod import-plan "eager" [_] {:env {} :defer []})

(defmethod import-plan "lean" [_]
  {:env   {"OPENBLAS_NUM_THREADS" "1"}
   :defer ["networkx" "bs4" "pygments" "pygments.lexers"
           "pygments.formatters.html" "pygments.styles"]})

(defmethod import-plan :default [mode]
  (throw (ex-info (str "Unknown :import-mode " (pr-str mode) "; one of "
                       (pr-str (sort (remove #{:default} (keys (methods import-plan))))))
                  {:import-mode mode})))

(def ^:private deferred-imports
  (delay
    (:globals (py/run-simple-string
               (slurp (clojure.java.io/resource "desargues/manim/deferred_imports.py"))))))

(defn- import-manim!
  "Boundary: apply `plan`, import manim, end the deferral window."
  [{:keys [env defer]}]
  (let [os-env (py/get-attr (py/import-module "os") "environ")]
    (doseq [[k v] env]
      (py/call-attr os-env "setdefault" k v)))
  (if (seq defer)
    (let [{:strs [install seal]} @deferred-imports
          stubs (install (py/->py-list defer))]
      (try (py/import-module "manim")
           (finally (seal stubs))))
    (py/import-module "manim")))

(def prebuilt-library-class
  "Name of the libpython JNA binding class `clojure -M:pyaot-build` writes."
  "desargues.pyaot.PythonLibrary")

(defn class-on-classpath
  "Default class finder: name -> Class, or nil when it is not on the classpath."
  [class-name]
  (try (Class/forName class-name) (catch ClassNotFoundException _ nil)))

(defn prebuilt-library
  "libpython-clj :library-definition for the pre-generated binding class, or
   nil (libpython-clj then generates the bindings at runtime). `find-class`
   is name -> Class or nil."
  ([] (prebuilt-library class-on-classpath))
  ([find-class]
   (when-let [cls (find-class prebuilt-library-class)]
     {:library-class cls})))

(defn init!
  "Initialize Python with the Manim conda environment.
   Call this ONCE at the start of your REPL session.
   
   Options:
   - :python-executable - path to python binary
   - :library-path - path to libpython shared library
   - :site-packages - path to site-packages directory
   - :preload-libs - colon-separated shared libs loaded before `import manim`
   - :import-mode - \"lean\" (default) or \"eager\", see `import-plan`
   - :library-definition - libpython-clj binding class; defaults to
     `prebuilt-library` (nil unless `clojure -M:pyaot-build` ran and the
     :pyaot alias is on, see desargues.pyaot)"
  ([]
   (init! {}))
  ([{:keys [python-executable library-path site-packages preload-libs import-mode
            library-definition]
     :or {python-executable (:python-exe (config/manim-config))
          library-path (:library-path (config/manim-config))
          site-packages (:site-packages (config/manim-config))
          preload-libs (:preload-libs (config/manim-config))
          import-mode (:import-mode (config/manim-config))
          library-definition (prebuilt-library)}}]
   (when-not @initialized?
     (let [plan (import-plan import-mode)]
       (py/initialize!
        :python-executable python-executable
        :library-path library-path
        :library-definition library-definition)

       ;; Add site-packages to Python path
       (let [sys (py/import-module "sys")]
         (py/call-attr (py/get-attr sys "path") "insert" 0 site-packages))

       ;; An embedding host lacks python's RPATH; pathops needs libskia preloaded
       (preload-libs! (preload-paths preload-libs))

       ;; Cache manim module
       (reset! manim-module (import-manim! plan))
       (reset! initialized? true)
       (println "Manim initialized successfully!")))
   @manim-module))

(defn initialized?*
  "Check if Manim has been initialized."
  []
  @initialized?)

(defn manim
  "Get the manim module. Initializes if needed."
  []
  (when-not @initialized?
    (init!))
  @manim-module)

;; ============================================================================
;; Module Access Helpers
;; ============================================================================

(defn get-class
  "Get a class from the manim module by name."
  [class-name]
  (py/get-attr (manim) (name class-name)))

(defn get-constant
  "Get a constant from the manim module by name."
  [const-name]
  (py/get-attr (manim) (name const-name)))

(defn call-method
  "Call a method on a Python object with args."
  [obj method-name & args]
  (apply py/call-attr obj (name method-name) args))

(defn call-method-kw
  "Call a method on a Python object with positional and keyword args."
  [obj method-name args kwargs]
  (py/call-attr-kw obj (name method-name) (vec args) (or kwargs {})))

(defn get-attr
  "Get an attribute from a Python object."
  [obj attr-name]
  (py/get-attr obj (name attr-name)))

(defn set-attr!
  "Set an attribute on a Python object."
  [obj attr-name value]
  (py/set-attr! obj (name attr-name) value))

;; ============================================================================
;; Python Interop Utilities
;; ============================================================================

(defn ->py-list
  "Convert a Clojure sequence to a Python list."
  [coll]
  (py/->py-list coll))

(defn ->py-dict
  "Convert a Clojure map to a Python dict."
  [m]
  (py/->py-dict m))

(defn ->py-tuple
  "Convert a Clojure sequence to a Python tuple."
  [coll]
  (py/->py-tuple coll))

(defn py->clj
  "Convert a Python object to Clojure."
  [obj]
  (py/->jvm obj))

(defn fn-arities
  "Set of fixed arities a Clojure fn declares, read from its invoke methods."
  [f]
  (into #{}
        (comp (filter #(= "invoke" (.getName ^java.lang.reflect.Method %)))
              (map #(alength (.getParameterTypes ^java.lang.reflect.Method %))))
        (.getDeclaredMethods (class f))))

(def ^:private updater-shims
  (delay
    (:globals (py/run-simple-string
               (str "def desargues_timed_updater(f):\n"
                    "    return lambda mob, dt: f(mob, dt)\n"
                    "def desargues_plain_updater(f):\n"
                    "    return lambda mob: f(mob)\n")))))

(defn ->py-updater
  "A manim updater from `f`. A Clojure fn becomes a Python lambda manim can
   inspect.signature: arity 2 is time-based (mob dt), otherwise (mob).
   Anything else (already a Python callable) passes through."
  [f]
  (if (fn? f)
    (let [shim (if (contains? (fn-arities f) 2)
                 "desargues_timed_updater"
                 "desargues_plain_updater")]
      ((get @updater-shims shim) f))
    f))

;; ============================================================================
;; Scene Rendering
;; ============================================================================

(defn render-scene!
  "Render a scene instance.
   
   Options:
   - :output-file - custom output filename (without path or extension)
   
   Note: Manim Community Edition's Scene.render() doesn't accept
   keyword arguments. For quality control, use Manim's config system."
  ([scene]
   (render-scene! scene {}))
  ([scene {:keys [output-file]}]
   (let [manim (py/import-module "manim")
         config (py/get-attr manim "config")
         scene-name (py/get-attr (py/get-attr scene "__class__") "__name__")
         final-name (or output-file scene-name)
         video-dir (py/get-attr config "video_dir")
         ;; Get current output file path from config
         current-output (py/get-attr config "output_file")]
     ;; Render the scene
     (py/call-attr scene "render")
     ;; Rename the output file if needed
     (when (and final-name (not= final-name "MyTestScene"))
       (let [os (py/import-module "os")
             shutil (py/import-module "shutil")
             ;; Find the actual output file - use the current output path
             current-path (str current-output)
             new-path (clojure.string/replace current-path
                                              #"[^/]+\.mp4$"
                                              (str final-name ".mp4"))]
         (when (py/call-attr (py/get-attr os "path") "exists" current-path)
           (py/call-attr shutil "move" current-path new-path)
           (println (str "Renamed to: " new-path)))))
     (println (str "Rendering complete! Output: " final-name ".mp4")))))

(defn create-scene-class
  "Create a custom Scene subclass with a construct function.

   The construct-fn receives `self` as its argument.
   Uses make-tuple-instance-fn to properly handle the self parameter.

   Example:
   (create-scene-class \"MyScene\"
     (fn [self]
       (let [circle (circle)]
         (play! self (create circle)))))"
  [class-name construct-fn]
  (let [Scene (get-class "Scene")
        ;; Wrap the construct function to properly receive self
        wrapped-construct (py-class/make-tuple-instance-fn construct-fn)
        scene-class (py/create-class class-name [Scene] {"construct" wrapped-construct})]
    ;; Explicitly set __name__ so Manim uses it for output filename
    (py/set-attr! scene-class "__name__" class-name)
    scene-class))

;; ============================================================================
;; Scene Helper Functions
;; ============================================================================

(defn play!
  "Play an animation in a scene."
  [scene & animations]
  (doseq [anim animations]
    (py/call-attr scene "play" anim)))

(defn wait!
  "Wait for a duration in a scene."
  ([scene]
   (wait! scene 1))
  ([scene duration]
   (py/call-attr scene "wait" duration)))

(defn add!
  "Add mobjects to a scene without animation."
  [scene & mobjects]
  (doseq [mob mobjects]
    (py/call-attr scene "add" mob)))

(defn remove!
  "Remove mobjects from a scene."
  [scene & mobjects]
  (doseq [mob mobjects]
    (py/call-attr scene "remove" mob)))

(defn clear!
  "Clear all mobjects from a scene."
  [scene]
  (py/call-attr scene "clear"))

;; ============================================================================
;; Macros for Convenient Scene Definition
;; ============================================================================

(defmacro defscene
  "Define a Manim scene class.
   
   Example:
   (defscene MyScene []
     (let [c (circle)]
       (play! self (create c))
       (wait! self 2)))"
  [name args & body]
  `(def ~name
     (create-scene-class ~(str name)
                         (fn [~'self]
                           ~@body))))

(defmacro with-scene
  "Execute body in context of a new scene, then render.
   
   Example:
   (with-scene
     (let [c (circle)]
       (play! self (create c))))"
  [& body]
  `(let [scene-class# (create-scene-class "TempScene"
                                          (fn [~'self]
                                            ~@body))
         scene# (scene-class#)]
     (render-scene! scene#)))
