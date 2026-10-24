(ns desargues.manim.patches
  "Reversible Manim patches behind desargues.manim.patch-protocols/IManimPatch.

   svg-path-shared-copy: Mobject.copy is a full copy.deepcopy, and every
   VMobjectFromSVGPath (one per glyph of a Text / MathTex) carries the parsed
   svgelements Path it was built from. That Path is read only while the points
   are generated at construction, yet deepcopy clones it on every copy (FadeIn
   copies the whole group twice per play). The patch gives VMobjectFromSVGPath
   a __deepcopy__ that shares the Path between original and copy and copies
   the rest as Manim does, so the copies' points, colours and frames are the
   same."
  (:require [desargues.manim.patch-protocols :as pp]
            [libpython-clj2.python :as py]))

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

(def ^:private bgra-shim-source
  "import numpy as _np
from PIL import Image as _Image
import manim.camera.camera as _cam

_SW = [2, 1, 0, 3]
_KEYS = ('set_cairo_context_color', 'init_background',
         'display_point_cloud', 'overlay_PIL_image')

def desargues_bgra_install():
    C = _cam.Camera
    if '_desargues_bgra' in C.__dict__:
        return True
    orig = {k: C.__dict__[k] for k in _KEYS}
    def set_cairo_context_color(self, ctx, rgbas, vmobject):
        return orig['set_cairo_context_color'](self, ctx, _np.asarray(rgbas)[:, _SW], vmobject)
    def init_background(self):
        orig['init_background'](self)
        if self.background.shape[-1] == 4:
            self.background = _np.ascontiguousarray(self.background[..., _SW])
    def display_point_cloud(self, pmobject, points, rgbas, thickness, pixel_array):
        return orig['display_point_cloud'](self, pmobject, points,
                                           _np.asarray(rgbas)[:, _SW], thickness, pixel_array)
    def overlay_PIL_image(self, pixel_array, image):
        r, g, b, a = image.convert('RGBA').split()
        return orig['overlay_PIL_image'](self, pixel_array, _Image.merge('RGBA', (b, g, r, a)))
    C.set_cairo_context_color = set_cairo_context_color
    C.init_background = init_background
    C.display_point_cloud = display_point_cloud
    C.overlay_PIL_image = overlay_PIL_image
    C._desargues_bgra = orig
    return True

def desargues_bgra_uninstall():
    C = _cam.Camera
    orig = C.__dict__.get('_desargues_bgra')
    if orig is None:
        return False
    for k, f in orig.items():
        setattr(C, k, f)
    del C._desargues_bgra
    return True

def desargues_bgra_installed():
    return '_desargues_bgra' in _cam.Camera.__dict__
")

(def ^:private bgra-shims
  (delay (:globals (py/run-simple-string bgra-shim-source))))

(defn- call-bgra-shim [nm] (boolean (py/->jvm ((get @bgra-shims nm)))))

(defrecord CameraBgraPre []
  pp/IManimPatch
  (-patch-id [_] :camera-bgra-pre)
  (-install! [_] (call-bgra-shim "desargues_bgra_install"))
  (-uninstall! [_] (call-bgra-shim "desargues_bgra_uninstall"))
  (-installed? [_] (call-bgra-shim "desargues_bgra_installed")))

(def camera-bgra-pre
  "Camera.pixel_array as premultiplied BGRA: cairo's own ARGB32 bytes on a
   little-endian host, the layout JavaFX uploads as is. Manim already draws
   premultiplied pixels and only swaps red and blue at its colour sources
   (set_source_rgba(rgb[2::-1]), RGBA background, point clouds, PIL
   overlays); the patch undoes that swap at those sources, so each frame
   reaches a :bgra-pre IRaster with no per-pixel conversion."
  (->CameraBgraPre))

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
