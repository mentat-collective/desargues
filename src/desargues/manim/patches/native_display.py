import ctypes as _ct, os as _os, math as _math
import numpy as _np
import cairo as _cairo
from manim.camera.camera import Camera as _Camera, LINE_JOIN_MAP as _JOINS, CAP_STYLE_MAP as _CAPS
from manim.mobject.types.vectorized_mobject import VMobject as _VM

_OFF = object.__basicsize__
_META = 13
_state = {}

_CAM_STOCK = ('display_vectorized', 'set_cairo_context_path', 'apply_fill', 'apply_stroke',
              'transform_points_pre_display', 'get_fill_rgbas', 'get_stroke_rgbas')
_VM_STOCK = ('consider_points_equals_2d', 'gen_subpaths_from_points_2d',
             '_gen_subpaths_from_points', 'gen_cubic_bezier_tuples_from_points',
             'get_fill_rgbas', 'get_stroke_rgbas', 'get_stroke_width',
             'get_gradient_start_and_end_points')
_JOIN = {k: (-1.0 if v is None else float(int(v))) for k, v in _JOINS.items()}
_CAP = {k: (-1.0 if v is None else float(int(v))) for k, v in _CAPS.items()}
_ZERO = _np.zeros((1, 4))


def _ctx_ptr(ctx):
    return _ct.c_void_p.from_address(id(ctx) + _OFF).value


def _layout_ok(lib):
    probe = lib.desargues_display_probe_tolerance
    probe.restype = _ct.c_double
    probe.argtypes = [_ct.c_void_p]
    ctx = _cairo.Context(_cairo.ImageSurface(_cairo.FORMAT_ARGB32, 1, 1))
    ctx.set_tolerance(0.3125)
    return probe(_ctx_ptr(ctx)) == 0.3125


def _manims(f, owner, name):
    # Is f Manim's own owner.name?
    return (getattr(f, '__module__', '') or '').startswith('manim.') and \
        getattr(f, '__qualname__', '') == owner + '.' + name


def _cam_ok(t, n):
    f = getattr(t, n)
    if _manims(f, 'Camera', n):
        return True
    # The native path builder issues Manim's cairo path ops (desargues.manim.patches).
    return (n == 'set_cairo_context_path' and '_desargues_native_path' in _Camera.__dict__
            and f is _Camera.__dict__['set_cairo_context_path'])


def _cam_mode(t):
    # None: Manim's own loop; 0 / 1: native, with the swap_rb flag. Not cached:
    # other patches come and go, and this runs once per batch.
    if not all(_cam_ok(t, n) for n in _CAM_STOCK):
        return None
    color = getattr(t, 'set_cairo_context_color')
    if _manims(color, 'Camera', 'set_cairo_context_color'):
        return 0
    bgra = _Camera.__dict__.get('_desargues_bgra')
    if (bgra is not None and color is _Camera.__dict__['set_cairo_context_color']
            and _manims(bgra['set_cairo_context_color'], 'Camera', 'set_cairo_context_color')):
        return 1
    return None


def _vm_ok(t, cache):
    r = cache.get(t)
    if r is None:
        r = all(getattr(t, n) is getattr(_VM, n) and _manims(getattr(_VM, n), 'VMobject', n)
                for n in _VM_STOCK)
        cache[t] = r
    return r


def _rgbas(vm, name):
    try:
        return getattr(vm, name)
    except AttributeError:
        return _ZERO


def desargues_native_display_install(lib_path):
    if '_desargues_native_display' in _Camera.__dict__:
        return True
    if not _os.path.isfile(lib_path):
        return False
    lib = _ct.PyDLL(lib_path)
    if lib.desargues_display_abi() != 1 or not _layout_ok(lib):
        return False
    batch = lib.desargues_display_batch
    batch.restype = _ct.c_int
    batch.argtypes = [_ct.c_void_p, _ct.c_void_p, _ct.c_void_p, _ct.c_size_t, _ct.c_void_p,
                      _ct.c_double, _ct.c_double, _ct.c_int]
    orig = _Camera.__dict__['display_multiple_non_background_colored_vmobjects']
    cam_cache, vm_cache = {}, {}
    stats = _state.setdefault('stats', {'native': 0, 'fallback': 0, 'batches': 0})
    ctx_ptr, isfinite, concat, asarr, f64 = _ctx_ptr, _np.isfinite, _np.concatenate, _np.asarray, _np.float64
    JOIN, CAP, _nd = _JOIN, _CAP, _np.ndarray

    def flush(self, ctx, pts, meta, cols, k, swap):
        if k == 0:
            return
        P = _np.ascontiguousarray(concat(pts), dtype=f64) if pts else _np.zeros((0, 3))
        M = _np.array(meta, dtype=f64)
        C = _np.ascontiguousarray(concat(cols), dtype=f64)
        fw = self.frame_width
        batch(ctx_ptr(ctx), P.ctypes.data, M.ctypes.data, k, C.ctypes.data,
              float(self.cairo_line_width_multiple), float(fw / fw), swap)
        stats['batches'] += 1
        stats['native'] += k

    def display_multiple_non_background_colored_vmobjects(self, vmobjects, pixel_array):
        swap = _cam_mode(type(self))
        if swap is None:
            return orig(self, vmobjects, pixel_array)
        ctx = self.get_cairo_context(pixel_array)
        if type(ctx) is not _cairo.Context:
            for vm in vmobjects:
                self.display_vectorized(vm, ctx)
            return None
        pts, meta, cols, k = [], [], [], 0
        for vm in vmobjects:
            p = vm.points
            if (vm.n_points_per_cubic_curve != 4 or not _vm_ok(type(vm), vm_cache)
                    or p.ndim != 2 or p.shape[1] != 3):
                flush(self, ctx, pts, meta, cols, k, swap)
                pts, meta, cols, k = [], [], [], 0
                stats['fallback'] += 1
                self.display_vectorized(vm, ctx)
                continue
            fill = _rgbas(vm, 'fill_rgbas')
            bg = _rgbas(vm, 'background_stroke_rgbas')
            st = _rgbas(vm, 'stroke_rgbas')
            sw = vm.stroke_width
            if isinstance(sw, str):
                sw = int(sw)
            sw = max(0.0, sw)
            bw = max(0.0, vm.background_stroke_width)
            nf, nb, ns = len(fill), len(bg), len(st)
            if not (type(fill) is _nd and type(bg) is _nd and type(st) is _nd
                    and fill.ndim == 2 and bg.ndim == 2 and st.ndim == 2
                    and fill.shape[1] == 4 and bg.shape[1] == 4 and st.shape[1] == 4):
                flush(self, ctx, pts, meta, cols, k, swap)
                pts, meta, cols, k = [], [], [], 0
                stats['fallback'] += 1
                self.display_vectorized(vm, ctx)
                continue
            if nf != 1 or (bw != 0 and nb != 1) or (sw != 0 and ns != 1):
                g = vm.get_gradient_start_and_end_points()
                g = asarr(g, dtype=f64)
                if g.shape != (2, 3) or not isfinite(g).all():
                    flush(self, ctx, pts, meta, cols, k, swap)
                    pts, meta, cols, k = [], [], [], 0
                    stats['fallback'] += 1
                    self.display_vectorized(vm, ctx)
                    continue
                gx0, gy0, gx1, gy1 = g[0, 0], g[0, 1], g[1, 0], g[1, 1]
            else:
                gx0 = gy0 = gx1 = gy1 = 0.0
            pts.append(p)
            cols.append(fill); cols.append(bg); cols.append(st)
            meta.extend((len(p), vm.tolerance_for_point_equality, sw, bw,
                         JOIN[vm.joint_type], CAP[vm.cap_style], nf, nb, ns, gx0, gy0, gx1, gy1))
            k += 1
        flush(self, ctx, pts, meta, cols, k, swap)
        return None

    _state['orig'] = orig
    _state['lib'] = lib
    _Camera.display_multiple_non_background_colored_vmobjects = display_multiple_non_background_colored_vmobjects
    _Camera._desargues_native_display = True
    return True


def desargues_native_display_uninstall():
    if '_desargues_native_display' not in _Camera.__dict__:
        return False
    _Camera.display_multiple_non_background_colored_vmobjects = _state.pop('orig')
    del _Camera._desargues_native_display
    return True


def desargues_native_display_installed():
    return '_desargues_native_display' in _Camera.__dict__


def desargues_native_display_stats():
    return dict(_state.get('stats', {'native': 0, 'fallback': 0, 'batches': 0}))
