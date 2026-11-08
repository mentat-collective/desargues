(ns desargues.manim.patches.native-display
  "native-display: Camera.display_multiple_non_background_colored_vmobjects
   draws a whole batch of VMobjects with ONE call into the Zig library under
   native/zigpath (`bb native:path`, src/display.zig): for each VMobject the
   path, the background stroke, the fill and the stroke, with Manim's solid
   or linear-gradient sources, line width, join and cap, in Manim's order and
   with Manim's doubles. Opt in with DESARGUES_PATH_BUILDER=native-display.

   Covered: solid and gradient (sheen) fill and stroke colours, background
   stroke, stroke width, joint_type and cap_style, non-finite points, empty
   VMobjects, the camera-bgra-pre colour swap. Anything else falls back to
   Manim for that VMobject (or the whole batch): a Camera subclass overriding
   display / colour / transform methods (ThreeDCamera, MovingCamera frame
   overrides), a VMobject class overriding the path, rgba, width or gradient
   accessors, n_points_per_cubic_curve other than 4, non-3D points, a context
   that is not a plain cairo.Context. Background-image VMobjects never reach
   this method. Without the built library nothing is installed."
  (:require [clojure.java.io :as io]
            [libpython-clj2.python :as py]
            [malli.core :as m]
            [desargues.manim.patch-protocols :as pp]
            [desargues.manim.patches :as mp]))

(def shim-resource
  "Classpath resource holding the Python side of the patch."
  "desargues/manim/patches/native_display.py")

(def ^:private shims
  (delay
    ;; Its own module, not __main__ (where py/run-simple-string runs): the
    ;; other patch shims there share names such as _state and _ctx_ptr.
    (let [g (:globals (py/run-simple-string
                       "import sys as _sys, types as _types
def _desargues_load_module(name, src):
    m = _sys.modules.get(name)
    if m is None:
        m = _types.ModuleType(name)
        exec(compile(src, name, 'exec'), m.__dict__)
        _sys.modules[name] = m
    return m.__dict__"))]
      ((get g "_desargues_load_module") "desargues_native_display"
       (slurp (io/resource shim-resource))))))

(defn- call-shim [nm & args]
  (py/->jvm (apply (get @shims nm) args)))

(def Stats
  "Counters since the Python side was loaded: VMobjects drawn natively,
   VMobjects handed back to Manim, native batch calls."
  [:map [:native nat-int?] [:fallback nat-int?] [:batches nat-int?]])

(defrecord NativeDisplay [lib-path]
  pp/IManimPatch
  (-patch-id [_] :native-display)
  (-install! [_]
    (boolean (and (mp/native-path-available? lib-path)
                  (call-shim "desargues_native_display_install" lib-path))))
  (-uninstall! [_] (boolean (call-shim "desargues_native_display_uninstall")))
  (-installed? [_] (boolean (call-shim "desargues_native_display_installed"))))

(defn native-display
  "Draw each batch of VMobjects with one call into the Zig library (see ns
   doc). -install! is false when the library is not built. Constructing it
   reads no Manim config."
  ([] (native-display (mp/native-path-lib)))
  ([lib-path] (->NativeDisplay lib-path)))

(defn stats
  "The native / fallback / batches counters (see `Stats`)."
  []
  (m/coerce Stats (update-keys (call-shim "desargues_native_display_stats") keyword)))

(defmethod mp/path-builder-patches "native-display" [_]
  [(mp/native-cairo-path) (native-display)])
