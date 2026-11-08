import numpy as _np
import manim.camera.camera as _cam
from manim.utils.color import color_to_int_rgba as _c2i

# The store outlives install/uninstall: a live loop installs the patch for
# each render, and a store dropped on uninstall would miss on every edit.
# It is read only while the patch is installed; desargues_bg_cache_clear
# empties it.
_BC = {'max': 8, 'hits': 0, 'misses': 0, 'live': None, 'store': {}}


def _key(cam):
    if cam.background_image is not None:
        return None
    try:
        rgba = tuple(int(x) for x in _c2i(cam.background_color, cam.background_opacity))
        return (type(cam), int(cam.pixel_height), int(cam.pixel_width),
                int(cam.n_channels), _np.dtype(cam.pixel_array_dtype).str, rgba,
                '_desargues_bgra' in _cam.Camera.__dict__)
    except Exception:
        return None


def _wrap(orig):
    state = {'on': True}

    def init_background(self):
        key = _key(self) if state['on'] else None
        if key is None:
            return orig(self)
        store = _BC['store']
        bg = store.get(key)
        if bg is not None:
            _BC['hits'] += 1
            self.background = bg
            return None
        _BC['misses'] += 1
        r = orig(self)
        bg = self.background
        if isinstance(bg, _np.ndarray) and bg.flags.c_contiguous and bg.flags.owndata:
            bg.flags.writeable = False
            while len(store) >= _BC['max']:
                store.pop(next(iter(store)))
            store[key] = bg
        return r

    init_background._desargues_bg_cache = (orig, state)
    return init_background


def desargues_bg_cache_install(max_entries):
    _BC['max'] = max(1, int(max_entries))
    store = _BC['store']
    while len(store) > _BC['max']:
        store.pop(next(iter(store)))
    if desargues_bg_cache_installed():
        return True
    w = _wrap(_cam.Camera.__dict__['init_background'])
    _cam.Camera.init_background = w
    _BC['live'] = w
    return True


def desargues_bg_cache_uninstall():
    w = _BC['live']
    if w is None:
        return False
    orig, state = w._desargues_bg_cache
    state['on'] = False
    if _cam.Camera.__dict__.get('init_background') is w:
        _cam.Camera.init_background = orig
    _BC['live'] = None
    return True


def desargues_bg_cache_installed():
    w = _BC['live']
    return w is not None and _cam.Camera.__dict__.get('init_background') is w


def desargues_bg_cache_clear():
    _BC['store'].clear()
    _BC['hits'] = 0
    _BC['misses'] = 0
    return True


def desargues_bg_cache_stats():
    return {'hits': _BC['hits'], 'misses': _BC['misses'], 'size': len(_BC['store'])}
