(ns desargues.manim.patches.surface-reuse
  "surface-reuse: Manim's per-frame bookkeeping on the live path, without the
   numpy copies.

   Each CairoRenderer.render calls Camera.reset, which is
   set_pixel_array(background): np.array(background) (a full copy) and then
   an in-place copy into pixel_array. render then hands write_frame
   get_frame(), another np.array copy of pixel_array. Camera already caches
   one cairo surface and context per pixel_array buffer (keyed by id), and
   the in-place copy keeps that buffer, so the context is built once.

   The patch:
   - Camera.reset: when pixel_array and background are same-shaped C buffers
     and the background is one opaque colour (the default background), one
     cairo paint() of that colour (operator SOURCE, identity matrix, no clip)
     on the cached context; any other background is one np.copyto. Cameras
     that override set_pixel_array keep Manim's reset.
   - CairoRenderer.render: with neither a movie nor PNGs being written, the
     frame handed to write_frame is camera.pixel_array itself (no copy); a
     sink must copy what it keeps, as the live sinks do. With output files
     Manim's copy is kept.
   Frames are byte-identical: a SOURCE paint of an opaque 8-bit colour
   writes exactly its bytes, which is what the copy wrote."
  (:require [desargues.manim.patch-protocols :as pp]
            [libpython-clj2.python :as py]))

(def ^:private shim-source
  "import sys as _sys
import numpy as _np
import cairo as _cairo
import manim.camera.camera as _cam
import manim.renderer.cairo_renderer as _cr
from manim.utils import file_ops as _fo

_LE = _sys.byteorder == 'little'
_SR = {}

def _bg_paint(cam, bg):
    memo = cam.__dict__.get('_desargues_bg_memo')
    if memo is not None and memo[0] is bg:
        return memo[1]
    rgb = None
    if (_LE and bg.dtype == _np.uint8 and bg.ndim == 3 and bg.shape[-1] == 4
            and bg.size and bg[0, 0, 3] == 255):
        p = bg[0, 0]
        if (bg == p).all():
            rgb = (p[2] / 255.0, p[1] / 255.0, p[0] / 255.0)
    cam.__dict__['_desargues_bg_memo'] = (bg, rgb)
    return rgb

def _reset(self):
    pa = self.__dict__.get('pixel_array')
    bg = self.background
    if (pa is None or type(self).set_pixel_array is not _cam.Camera.set_pixel_array
            or not isinstance(bg, _np.ndarray) or pa.shape != bg.shape
            or pa.dtype != bg.dtype or not pa.flags.c_contiguous):
        return _SR['reset'](self)
    rgb = _bg_paint(self, bg)
    if rgb is None:
        _np.copyto(pa, bg)
        return self
    ctx = self.get_cairo_context(pa)
    ctx.save()
    ctx.reset_clip()
    ctx.identity_matrix()
    ctx.set_operator(_cairo.OPERATOR_SOURCE)
    ctx.set_source_rgba(rgb[0], rgb[1], rgb[2], 1.0)
    ctx.paint()
    ctx.restore()
    return self

def _render(self, scene, time, moving_mobjects=None):
    self.update_frame(scene, moving_mobjects)
    if _fo.write_to_movie() or _fo.is_png_format():
        return self.add_frame(self.get_frame())
    return self.add_frame(self.camera.pixel_array)

def desargues_surface_reuse_install():
    C, R = _cam.Camera, _cr.CairoRenderer
    if '_desargues_surface_reuse' in C.__dict__:
        return True
    _SR['reset'] = C.__dict__['reset']
    _SR['render'] = R.__dict__['render']
    C.reset = _reset
    R.render = _render
    C._desargues_surface_reuse = True
    return True

def desargues_surface_reuse_uninstall():
    C, R = _cam.Camera, _cr.CairoRenderer
    if '_desargues_surface_reuse' not in C.__dict__:
        return False
    C.reset = _SR.pop('reset')
    R.render = _SR.pop('render')
    del C._desargues_surface_reuse
    return True

def desargues_surface_reuse_installed():
    return '_desargues_surface_reuse' in _cam.Camera.__dict__
")

(def ^:private shims
  (delay (:globals (py/run-simple-string shim-source))))

(defn- call-shim [nm] (boolean (py/->jvm ((get @shims nm)))))

(defrecord SurfaceReuse []
  pp/IManimPatch
  (-patch-id [_] :surface-reuse)
  (-install! [_] (call-shim "desargues_surface_reuse_install"))
  (-uninstall! [_] (call-shim "desargues_surface_reuse_uninstall"))
  (-installed? [_] (call-shim "desargues_surface_reuse_installed")))

(def surface-reuse
  "Reset the Camera buffer with one cairo paint() (or one np.copyto) and hand
   the live sink the Camera buffer itself (see ns doc)."
  (->SurfaceReuse))
