(ns desargues.manim.patches.preview-aa
  "preview-aa: cheaper Cairo rasterizing for previews. LOSSY: frames differ
   from Manim's, so the patch belongs only to profiles that admit lossy
   patches (:preview), never to :fast, :stock or a final render.

   It wraps Camera.display_multiple_non_background_colored_vmobjects (Manim's,
   or native-display's when that is installed first) and, per batch, inside a
   cairo save/restore on the Camera's context:
   - :tolerance     cairo_set_tolerance (Manim leaves cairo's 0.1): coarser
                    curve flattening, fewer edges to rasterize;
   - :antialias     a cairo antialias mode (:fast, :good, :none, ...) used
                    for the VMobjects whose largest painted alpha is at most
                    :aa-max-alpha (every VMobject when :aa-max-alpha is nil),
                    in runs so Manim's draw order is kept;
   - :skip-invisible? VMobjects whose fill and stroked alphas are all 0 are
                    not drawn at all (exact: they paint nothing).
   Constructing the record reads no Manim config and imports no Python."
  (:require [clojure.java.io :as io]
            [libpython-clj2.python :as py]
            [malli.core :as m]
            [desargues.manim.patch-protocols :as pp]))

(def shim-resource
  "Classpath resource holding the Python side of the patch."
  "desargues/manim/patches/preview_aa.py")

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
      ((get g "_desargues_load_module") "desargues_preview_aa"
       (slurp (io/resource shim-resource))))))

(defn- call-shim [nm & args]
  (py/->jvm (apply (get @shims nm) args)))

(def antialias-modes
  "cairo_antialias_t by name."
  {:default 0 :none 1 :gray 2 :subpixel 3 :fast 4 :good 5 :best 6})

(def Knobs
  "What preview-aa changes; nil leaves cairo's setting alone."
  (m/schema
   [:map
    [:tolerance [:maybe [:double {:min 0.01 :max 10.0}]]]
    [:antialias [:maybe (into [:enum] (keys antialias-modes))]]
    [:aa-max-alpha [:maybe [:double {:min 0.0 :max 1.0}]]]
    [:skip-invisible? :boolean]]))

(def default-knobs
  "The settings the :preview profile ships (measured: PSNR >= 40 dB per frame
   against the exact render on the bench layout and anim-heavy arms)."
  {:tolerance 0.3 :antialias nil :aa-max-alpha nil :skip-invisible? true})

(def Stats
  "Counters since the Python side was loaded."
  [:map [:batches nat-int?] [:vmobjects nat-int?] [:skipped nat-int?] [:coarse nat-int?]])

(defrecord PreviewAA [tolerance antialias aa-max-alpha skip-invisible?]
  pp/IManimPatch
  (-patch-id [_] :preview-aa)
  (-install! [_]
    (boolean (call-shim "desargues_preview_aa_install"
                        tolerance (some-> antialias antialias-modes) aa-max-alpha
                        (boolean skip-invisible?))))
  (-uninstall! [_] (boolean (call-shim "desargues_preview_aa_uninstall")))
  (-installed? [_] (boolean (call-shim "desargues_preview_aa_installed"))))

(defn preview-aa
  "The preview-aa patch with `knobs` merged over `default-knobs` (see ns doc).
   Throws on knobs outside `Knobs`."
  ([] (preview-aa {}))
  ([knobs]
   (let [k (merge default-knobs
                  (cond-> knobs
                    (number? (:tolerance knobs)) (update :tolerance double)
                    (number? (:aa-max-alpha knobs)) (update :aa-max-alpha double)))]
     (when-not (m/validate Knobs k)
       (throw (ex-info "Invalid preview-aa knobs" {:knobs k :explain (m/explain Knobs k)})))
     (map->PreviewAA k))))

(defn stats
  "The batches / vmobjects / skipped / coarse counters (see `Stats`)."
  []
  (m/coerce Stats (update-keys (call-shim "desargues_preview_aa_stats") keyword)))
