(ns desargues.manim.patches.background-cache
  "background-cache: Camera.init_background, memoised.

   Every Scene builds a CairoRenderer and a Camera, and Camera.__init__
   calls init_background, which allocates an [h w 4] array and fills it
   with the background colour (np.zeros, then a broadcast assignment). With
   camera-bgra-pre installed it is copied once more to swap red and blue.
   On the live path every edit is a fresh Scene, so at 480p that is a few ms
   of CPU per edit and at 1080p tens of ms, as much as rendering a dozen
   frames.

   The patch keeps the array init_background built, keyed on everything it
   reads: the Camera class, pixel height and width, channel count, dtype,
   the background colour and opacity as Manim's integer RGBA, and whether
   camera-bgra-pre is installed. A later Camera with the same key gets the
   kept array. The kept array is made read-only, so a shared background can
   never be written through. Cameras with a background_image, or whose key
   cannot be read, run Manim's init_background. Bounded store (oldest
   evicted). The store outlives uninstall, because the live loop installs
   the patch around each render; it is read only while installed, and
   `clear!` empties it.
   Frames are byte-identical: the array is the one Manim built for that key.
   Constructing the record reads no Manim config and imports no Python."
  (:require [clojure.java.io :as io]
            [libpython-clj2.python :as py]
            [malli.core :as m]
            [desargues.manim.patch-protocols :as pp]))

(def shim-resource
  "Classpath resource holding the Python side of the patch."
  "desargues/manim/patches/background_cache.py")

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
      ((get g "_desargues_load_module") "desargues_background_cache"
       (slurp (io/resource shim-resource))))))

(defn- call-shim [nm & args]
  (py/->jvm (apply (get @shims nm) args)))

(def Stats
  "Counters since the Python side was loaded (hits, misses) and the number
   of backgrounds kept now (size)."
  [:map [:hits nat-int?] [:misses nat-int?] [:size nat-int?]])

(def MaxEntries [:int {:min 1}])

(defrecord BackgroundCache [max-entries]
  pp/IManimPatch
  (-patch-id [_] :background-cache)
  (-install! [_] (boolean (call-shim "desargues_bg_cache_install" max-entries)))
  (-uninstall! [_] (boolean (call-shim "desargues_bg_cache_uninstall")))
  (-installed? [_] (boolean (call-shim "desargues_bg_cache_installed"))))

(defn background-cache
  "Reuse the background array of an earlier Camera with the same size,
   dtype and colour (see ns doc), keeping at most `max-entries` (default 8)."
  ([] (background-cache 8))
  ([max-entries] (->BackgroundCache (m/coerce MaxEntries max-entries))))

(defn stats
  "The hits / misses / size counters (see `Stats`)."
  []
  (m/coerce Stats (update-keys (call-shim "desargues_bg_cache_stats") keyword)))

(defn clear!
  "Drop every kept background and reset the counters."
  []
  (boolean (call-shim "desargues_bg_cache_clear")))
