(ns desargues.manim.patches
  "Reversible Manim patches behind desargues.manim.patch-protocols/IManimPatch.

   svg-path-shared-copy: Mobject.copy is a full copy.deepcopy, and every
   VMobjectFromSVGPath (one per glyph of a Text / MathTex) carries the parsed
   svgelements Path it was built from. That Path is read only while the points
   are generated at construction, yet deepcopy clones it on every copy (FadeIn
   copies the whole group twice per play). The patch gives VMobjectFromSVGPath
   a __deepcopy__ that shares the Path between original and copy and copies
   the rest as Manim does, so the copies' points, colours and frames are the
   same.

   native-cairo-path: Camera.set_cairo_context_path builds each VMobject's
   path with one pycairo call per Bezier segment. The patch hands the points
   array (zero-copy) and the context's cairo_t* to the Zig library under
   native/zigpath (`bb native:path`), which issues the same cairo calls. Opt
   in with DESARGUES_PATH_BUILDER=native (`install-path-builder!`); without
   the built library nothing is installed and Manim builds its own paths."
  (:require [desargues.manim.patch-protocols :as pp]
            [libpython-clj2.python :as py]
            [desargues.config :as config]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def ^:private shim-source
  "import manim.mobject.svg.svg_mobject as _svgm
from manim.mobject.mobject import Mobject as _Mobject

def desargues_svg_copy_install():
    C = _svgm.VMobjectFromSVGPath
    if '_desargues_svg_copy' in C.__dict__:
        return True
    def __deepcopy__(self, memo):
        po = self.__dict__.get('path_obj')
        if po is not None:
            memo.setdefault(id(po), po)
        return _Mobject.__deepcopy__(self, memo)
    C.__deepcopy__ = __deepcopy__
    C._desargues_svg_copy = True
    return True

def desargues_svg_copy_uninstall():
    C = _svgm.VMobjectFromSVGPath
    if '_desargues_svg_copy' not in C.__dict__:
        return False
    del C.__deepcopy__
    del C._desargues_svg_copy
    return True

def desargues_svg_copy_installed():
    return '_desargues_svg_copy' in _svgm.VMobjectFromSVGPath.__dict__
")

(def ^:private shims
  (delay (:globals (py/run-simple-string shim-source))))

(defn- call-shim [nm] (boolean (py/->jvm ((get @shims nm)))))

(defrecord SvgPathSharedCopy []
  pp/IManimPatch
  (-patch-id [_] :svg-path-shared-copy)
  (-install! [_] (call-shim "desargues_svg_copy_install"))
  (-uninstall! [_] (call-shim "desargues_svg_copy_uninstall"))
  (-installed? [_] (call-shim "desargues_svg_copy_installed")))

(def svg-path-shared-copy
  "Share the parsed SVG Path between a glyph and its copies (see ns doc)."
  (->SvgPathSharedCopy))

(def ^:private native-path-source
  "import ctypes as _ct, os as _os
import numpy as _np
import cairo as _cairo
from manim.camera.camera import Camera as _Camera
from manim.mobject.types.vectorized_mobject import VMobject as _VM

_STOCK = ('consider_points_equals_2d', 'gen_subpaths_from_points_2d',
          '_gen_subpaths_from_points', 'gen_cubic_bezier_tuples_from_points')
_OFF = object.__basicsize__
_state = {}

def _ctx_ptr(ctx):
    return _ct.c_void_p.from_address(id(ctx) + _OFF).value

def _layout_ok(lib):
    probe = lib.desargues_path_probe_tolerance
    probe.restype = _ct.c_double
    probe.argtypes = [_ct.c_void_p]
    ctx = _cairo.Context(_cairo.ImageSurface(_cairo.FORMAT_ARGB32, 1, 1))
    ctx.set_tolerance(0.3125)
    return probe(_ctx_ptr(ctx)) == 0.3125

def desargues_native_path_install(lib_path):
    if '_desargues_native_path' in _Camera.__dict__:
        return True
    if not _os.path.isfile(lib_path):
        return False
    lib = _ct.PyDLL(lib_path)
    if lib.desargues_path_abi() != 1 or not _layout_ok(lib):
        return False
    build = lib.desargues_build_path
    build.restype = _ct.c_int
    build.argtypes = [_ct.c_void_p, _ct.c_void_p, _ct.c_size_t, _ct.c_size_t, _ct.c_double]
    orig = _Camera.__dict__['set_cairo_context_path']
    stock_types = {}
    ctx_ptr = _ctx_ptr
    def stock(t):
        r = stock_types.get(t)
        if r is None:
            r = all(getattr(t, n) is getattr(_VM, n) for n in _STOCK)
            stock_types[t] = r
        return r
    def set_cairo_context_path(self, ctx, vmobject):
        if (type(ctx) is not _cairo.Context or vmobject.n_points_per_cubic_curve != 4
                or not stock(type(vmobject))):
            return orig(self, ctx, vmobject)
        points = self.transform_points_pre_display(vmobject, vmobject.points)
        if len(points) == 0:
            return self
        pts = _np.ascontiguousarray(points, dtype=_np.float64)
        if pts.ndim != 2 or pts.shape[1] < 2:
            return orig(self, ctx, vmobject)
        build(ctx_ptr(ctx), pts.ctypes.data, pts.shape[0], pts.shape[1],
              float(vmobject.tolerance_for_point_equality))
        return self
    _state['orig'] = orig
    _state['lib'] = lib
    _Camera.set_cairo_context_path = set_cairo_context_path
    _Camera._desargues_native_path = True
    return True

def desargues_native_path_uninstall():
    if '_desargues_native_path' not in _Camera.__dict__:
        return False
    _Camera.set_cairo_context_path = _state.pop('orig')
    del _Camera._desargues_native_path
    return True

def desargues_native_path_installed():
    return '_desargues_native_path' in _Camera.__dict__
")

(def ^:private native-path-shims
  (delay (:globals (py/run-simple-string native-path-source))))

(defn- call-native-shim [nm & args]
  (boolean (py/->jvm (apply (get @native-path-shims nm) args))))

(def native-path-lib-name "libdesargues_path.so")

(defn native-path-lib
  "Path of the native path library: DESARGUES_NATIVE_PATH_LIB, else
   <project-root>/native/zigpath/zig-out/lib/libdesargues_path.so. nil when
   neither resolves."
  ([] (let [cfg (config/manim-config)]
        (native-path-lib (:native-path-lib cfg) (:project-root cfg))))
  ([explicit project-root]
   (cond
     (not (str/blank? explicit)) explicit
     project-root (str (io/file project-root "native" "zigpath" "zig-out" "lib" native-path-lib-name)))))

(defn native-path-available?
  "Is the native path library built at `lib-path`?"
  [lib-path]
  (boolean (and lib-path (.isFile (io/file lib-path)))))

(defrecord NativeCairoPath [lib-path]
  pp/IManimPatch
  (-patch-id [_] :native-cairo-path)
  (-install! [_]
    (and (native-path-available? lib-path)
         (call-native-shim "desargues_native_path_install" lib-path)))
  (-uninstall! [_] (call-native-shim "desargues_native_path_uninstall"))
  (-installed? [_] (call-native-shim "desargues_native_path_installed")))

(defn native-cairo-path
  "Build each VMobject's cairo path with one call into the Zig library
   (native/zigpath) instead of one pycairo call per Bezier segment. Same
   cairo calls, same doubles, same order. -install! is false (Manim keeps its
   own path building) when the library is not built."
  ([] (native-cairo-path (native-path-lib)))
  ([lib-path] (->NativeCairoPath lib-path)))

(defn install!
  "Install `patch`. Returns true when it is in place."
  [patch]
  (pp/-install! patch))

(defn uninstall!
  "Uninstall `patch`. Idempotent; true when a patch was removed."
  [patch]
  (pp/-uninstall! patch))

(defn installed?
  "Is `patch` in place?"
  [patch]
  (pp/-installed? patch))

(defn with-patches*
  "Run `f` with every patch in `patches` installed, uninstalling after the
   ones that were not already in place."
  [patches f]
  (let [fresh (vec (remove installed? patches))]
    (try
      (run! install! fresh)
      (f)
      (finally (run! uninstall! (rseq fresh))))))

(defmulti path-builder-patches
  "Path-builder name (DESARGUES_PATH_BUILDER) -> the patches it installs.
   \"manim\" installs none; \"native\" the Zig path builder."
  (fn [nm] (some-> nm name str/trim str/lower-case)))

(defmethod path-builder-patches "manim" [_] [])
(defmethod path-builder-patches nil [_] [])
(defmethod path-builder-patches "" [_] [])
(defmethod path-builder-patches "native" [_] [(native-cairo-path)])

(defmethod path-builder-patches :default [nm]
  (throw (ex-info (str "Unknown path builder " (pr-str nm) "; one of "
                       (pr-str (sort (filter string? (keys (methods path-builder-patches))))))
                  {:path-builder nm})))

(defn install-path-builder!
  "Install the path builder named by config (or `nm`), uninstalling the
   other path-builder patches. Returns {:path-builder nm :installed [ids]};
   a native builder whose library is not built installs nothing."
  ([] (install-path-builder! (:path-builder (config/manim-config))))
  ([nm]
   (let [want (path-builder-patches nm)]
     (uninstall! (native-cairo-path))
     {:path-builder nm
      :installed (into [] (comp (filter install!) (map pp/-patch-id)) want)})))
