"""Python side of desargues.manim.patches.fadein-setup.

FadeIn.begin copies its mobject twice with Mobject.copy (copy.deepcopy):
once as the target snapshot, once as the start that fade(1) turns
transparent. Interpolation only reads the copies' points, colour arrays and
widths, and fade / shift / scale / align_data only write those arrays or
rebind attributes. lean_copy builds the same family (same classes, same
attribute values) but copies only what those steps write: every numpy array
and every list or dict attribute; any other value is shared with the source.
References between family members are re-pointed at the copies, as deepcopy's
memo would. A class with its own copy / __deepcopy__ is deep-copied as stock
does, sharing the memo, so identities stay consistent across the family.
"""
import copy as _copy
import numpy as _np
from manim import config as _config
from manim.constants import RendererType as _RT
from manim.animation.fading import FadeIn as _FadeIn
from manim.animation.transform import Transform as _Transform
from manim.animation.animation import Animation as _Animation
from manim.mobject.mobject import Mobject as _Mobject

_state = {'lean': 0, 'stock': 0}
_ORIG_DEEPCOPY = _Mobject.__deepcopy__


def _lean_ok(cls):
    # No class below Mobject redefines copy or __deepcopy__; the one
    # exception is svg-path-shared-copy's __deepcopy__, which only shares
    # path_obj, as a lean copy does anyway.
    if cls.copy is not _Mobject.copy or _Mobject.__deepcopy__ is not _ORIG_DEEPCOPY:
        return False
    return (cls.__deepcopy__ is _Mobject.__deepcopy__
            or '_desargues_svg_copy' in cls.__dict__)


def _copy_value(v, memo):
    t = type(v)
    if isinstance(v, _np.ndarray):
        # one copy per source array, so arrays shared inside the family stay
        # shared in the copy, as with deepcopy's memo
        c = memo.get(id(v))
        if c is None:
            c = v.copy()
            memo[id(v)] = c
        return c
    if t is list:
        return list(v)
    if t is dict:
        return dict(v)
    return v


def _lean(mob, memo, made):
    c = memo.get(id(mob))
    if c is not None:
        return c
    cls = mob.__class__
    if not _lean_ok(cls):
        return _copy.deepcopy(mob, memo)
    new = cls.__new__(cls)
    memo[id(mob)] = new
    made.append(new)
    d = {k: _copy_value(v, memo) for k, v in mob.__dict__.items()}
    d['submobjects'] = [_lean(s, memo, made) for s in mob.submobjects]
    d['original_id'] = str(id(mob))
    new.__dict__.update(d)
    return new


def _repoint(made, memo):
    for m in made:
        d = m.__dict__
        for k, v in d.items():
            if k == 'submobjects':
                continue
            if isinstance(v, _Mobject):
                c = memo.get(id(v))
                if c is not None:
                    d[k] = c
            elif type(v) is list:
                if any(isinstance(x, _Mobject) for x in v):
                    d[k] = [memo.get(id(x), x) for x in v]
            elif type(v) is dict:
                if any(isinstance(x, _Mobject) for x in v.values()):
                    d[k] = {kk: memo.get(id(x), x) for kk, x in v.items()}


def lean_copy(mob):
    memo, made = {}, []
    new = _lean(mob, memo, made)
    _repoint(made, memo)
    return new


def _cairo():
    return _config.renderer != _RT.OPENGL


def _begin(self):
    if not _cairo():
        _state['stock'] += 1
        return _Transform.begin(self)
    _state['lean'] += 1
    # Transform.begin with the target snapshot made by lean_copy
    self.target_mobject = self.create_target()
    self.target_copy = lean_copy(self.target_mobject)
    self.mobject.align_data(self.target_copy)
    _Animation.begin(self)


def _create_starting_mobject(self):
    if not _cairo():
        return self._create_faded_mobject(fadeIn=True)
    # _Fade._create_faded_mobject(fadeIn=True) on a lean copy
    faded = lean_copy(self.mobject)
    faded.fade(1)
    direction_modifier = -1 if not self.point_target else 1
    faded.shift(self.shift_vector * direction_modifier)
    faded.scale(self.scale_factor)
    return faded


_MISSING = object()
_PATCHED = {'begin': _begin, 'create_starting_mobject': _create_starting_mobject}
_saved = {}


def desargues_fadein_setup_install():
    if _saved:
        return True
    for name, fn in _PATCHED.items():
        _saved[name] = _FadeIn.__dict__.get(name, _MISSING)
        setattr(_FadeIn, name, fn)
    return True


def desargues_fadein_setup_uninstall():
    if not _saved:
        return False
    for name, orig in _saved.items():
        if orig is _MISSING:
            delattr(_FadeIn, name)
        else:
            setattr(_FadeIn, name, orig)
    _saved.clear()
    return True


def desargues_fadein_setup_installed():
    return bool(_saved)


def desargues_fadein_setup_stats():
    return dict(_state)


def desargues_fadein_setup_lean_copy(mob):
    return lean_copy(mob)
