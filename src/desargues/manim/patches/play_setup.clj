(ns desargues.manim.patches.play-setup
  "play-setup: per-frame setup an animated counter or a deep scene pays on
   every Scene.play, cut without changing a pixel.

   Parts (play_setup.py next to this file):
   - :decimal-glyphs  DecimalNumber._string_to_mob hands out a lean family
                      copy of the glyph already scaled to the number's font
                      size, instead of deep-copying and rescaling the
                      template for every character of every set_value.
   - :family-walk     Mobject.get_family walks the tree once and dedupes
                      only when an id repeats, instead of at every level.

   Both return what stock returns (same lists, same classes, same array
   values), so frames stay SHA-256 identical. A record names its parts;
   `play-setup` with no argument installs all of them.

   Constructing the record touches no Python and reads no Manim config."
  (:require [clojure.java.io :as io]
            [libpython-clj2.python :as py]
            [malli.core :as m]
            [desargues.manim.patch-protocols :as pp]))

(def shim-resource
  "Classpath resource holding the Python side of the patch."
  "desargues/manim/patches/play_setup.py")

(def all-parts
  "Every part, in install order."
  [:decimal-glyphs :family-walk])

(def Part (into [:enum] all-parts))

(def Parts
  [:and [:vector Part] [:fn {:error/message "distinct parts"} #(or (empty? %) (apply distinct? %))]])

(def Stats
  "_string_to_mob calls since the Python side was loaded: lean (a scaled
   glyph reused), scaled (a glyph scaled once and kept), stock (handed back
   to Manim: kwargs, OpenGL, or a class with its own copy)."
  [:map [:lean nat-int?] [:scaled nat-int?] [:stock nat-int?]])

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
      ((get g "_desargues_load_module") "desargues_play_setup"
       (slurp (io/resource shim-resource))))))

(defn- shim [nm] (get @shims nm))

(defn- call-shim [nm & args]
  (py/->jvm (apply (shim nm) args)))

(defn- py-names [parts] (py/->py-list (mapv name parts)))

(defrecord PlaySetup [parts]
  pp/IManimPatch
  (-patch-id [_] :play-setup)
  (-install! [_] (boolean (call-shim "desargues_play_setup_install" (py-names parts))))
  (-uninstall! [_] (boolean (call-shim "desargues_play_setup_uninstall")))
  (-installed? [_] (boolean (call-shim "desargues_play_setup_installed" (py-names parts)))))

(defn play-setup
  "The patch value with `parts` (default `all-parts`)."
  ([] (play-setup all-parts))
  ([parts]
   (let [parts (vec parts)]
     (when-not (m/validate Parts parts)
       (throw (ex-info "Invalid play-setup parts" {:parts parts :explain (m/explain Parts parts)})))
     (->PlaySetup parts))))

(defn installed-parts
  "The parts in place right now, as keywords."
  []
  (mapv keyword (call-shim "desargues_play_setup_parts")))

(defn stats
  "The decimal-glyph counters (see `Stats`)."
  []
  (m/coerce Stats (update-keys (call-shim "desargues_play_setup_stats") keyword)))

(defn lean-copy
  "The lean family copy of mobject `mob` the patch uses (a Python handle)."
  [mob]
  ((shim "desargues_play_setup_lean_copy") mob))
