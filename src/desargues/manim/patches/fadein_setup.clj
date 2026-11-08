(ns desargues.manim.patches.fadein-setup
  "fadein-setup: FadeIn.begin without two deep copies of the faded group.

   Stock FadeIn.begin copies its mobject twice with copy.deepcopy: the target
   snapshot (target_copy) and the start state, which fade(1) then turns
   transparent. This patch builds both with a lean family copy
   (fadein_setup.py next to this file): the same classes and attribute
   values, with fresh numpy arrays, lists and dicts and every other value
   shared, family references re-pointed at the copies. The steps that follow
   (align_data, fade, shift, scale, interpolate) run unchanged, so every
   frame is the stock one bit for bit.

   Zero path (:zero?, on by default): when the guards hold (straight path,
   no updaters, every member with the stock copy / align methods, the root
   with the stock shift / scale), the target snapshot shares rebound arrays
   with the mobject and skips align_data (the mobject against its own twin
   is a no-op), and the start copy shares only the point arrays (fade writes
   colour arrays in place; shift and scale rebind points first). Frames are
   still the stock ones; any FadeIn outside the guards takes the lean path.

   Covered: the Cairo renderer; family members whose class keeps Mobject's
   copy and __deepcopy__ (or svg-path-shared-copy's). A member with its own
   copy semantics is deep-copied as stock does; the OpenGL renderer runs the
   stock begin."
  (:require [clojure.java.io :as io]
            [libpython-clj2.python :as py]
            [malli.core :as m]
            [desargues.manim.patch-protocols :as pp]))

(def shim-resource
  "Classpath resource holding the Python side of the patch."
  "desargues/manim/patches/fadein_setup.py")

(def ^:private shims
  (delay
    (let [g (:globals (py/run-simple-string
                       "import sys as _sys, types as _types
def _desargues_load_module(name, src):
    m = _sys.modules.get(name)
    if m is None:
        m = _types.ModuleType(name)
        exec(compile(src, name, 'exec'), m.__dict__)
        _sys.modules[name] = m
    return m.__dict__"))]
      ((get g "_desargues_load_module") "desargues_fadein_setup"
       (slurp (io/resource shim-resource))))))

(defn- call-shim [nm & args]
  (py/->jvm (apply (get @shims nm) args)))

(def Stats
  "FadeIn begins since the Python side was loaded: zero (shared-array path),
   lean (lean-copy path) and stock (handed back to Manim, e.g. under OpenGL)."
  [:map [:lean nat-int?] [:zero nat-int?] [:stock nat-int?]])

(defrecord FadeInSetup [zero?]
  pp/IManimPatch
  (-patch-id [_] :fadein-setup)
  (-install! [_] (boolean (call-shim "desargues_fadein_setup_install" (boolean zero?))))
  (-uninstall! [_] (boolean (call-shim "desargues_fadein_setup_uninstall")))
  (-installed? [_] (boolean (call-shim "desargues_fadein_setup_installed" (boolean zero?)))))

(def Opts
  "fadein-setup options: :zero? (default true) takes the shared-array path
   whenever its guards hold; false keeps every FadeIn on the lean-copy path."
  [:map [:zero? {:optional true} :boolean]])

(defn fadein-setup
  "The patch value (see ns doc). Constructing it touches no Python and reads
   no Manim config."
  ([] (fadein-setup {}))
  ([opts]
   (when-not (m/validate Opts opts)
     (throw (ex-info "Invalid fadein-setup options" {:opts opts :explain (m/explain Opts opts)})))
   (->FadeInSetup (get opts :zero? true))))

(defn stats
  "The lean / zero / stock begin counters (see `Stats`)."
  []
  (m/coerce Stats (update-keys (call-shim "desargues_fadein_setup_stats") keyword)))

(defn lean-copy
  "The lean family copy of mobject `mob` the patch uses (a Python handle)."
  [mob]
  ((get @shims "desargues_fadein_setup_lean_copy") mob))
