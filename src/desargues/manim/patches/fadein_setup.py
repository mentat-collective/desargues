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

The zero path (on by default) goes further when it is provably safe: the
target snapshot shares with the mobject the arrays interpolate rebinds on
every member with points (points, colour arrays, widths, sheen), since the
first interpolate(0) replaces them on the mobject before anything writes
them in place; align_data between a mobject and its own twin changes
nothing, so it is skipped. The start copy shares only the point arrays of
members with points (fade writes colour arrays in place, so those are
fresh; shift rebinds every such member's points before scale writes them).
Guards: Cairo, the straight-line path function, no updaters in the family,
Mobject.shift on the root, and every member's class keeps the stock copy
and align methods. Anything else takes the lean path.
"""
import copy as _copy
import numpy as _np
from manim import config as _config
from manim.constants import RendererType as _RT
from manim.animation.fading import FadeIn as _FadeIn
from manim.animation.transform import Transform as _Transform
from manim.animation.animation import Animation as _Animation
from manim.mobject.mobject import Mobject as _Mobject
from manim.mobject.types.vectorized_mobject import VMobject as _VMobject
from manim.utils.bezier import interpolate as _interpolate

_state = {'lean': 0, 'zero': 0, 'stock': 0}
_mode = {'zero': True}
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


# The arrays Transform.interpolate_submobject rebinds on every member with
# points (Mobject.interpolate, VMobject.interpolate_color): a snapshot may
# share these with the mobject, since the animation's first interpolate
# replaces them on the mobject before anything writes them in place.
_REBOUND = frozenset(['points', 'fill_rgbas', 'stroke_rgbas', 'background_stroke_rgbas',
                      'stroke_width', 'background_stroke_width', 'sheen_direction',
                      'sheen_factor'])


def _share_none(mob, k):
    return False


def _share_rebound(mob, k):
    return k in _REBOUND and len(mob.points) > 0


def _share_points(mob, k):
    return k == 'points' and len(mob.points) > 0


def _lean(mob, memo, made, share=_share_none):
    c = memo.get(id(mob))
    if c is not None:
        return c
    cls = mob.__class__
    if not _lean_ok(cls):
        return _copy.deepcopy(mob, memo)
    new = cls.__new__(cls)
    memo[id(mob)] = new
    made.append(new)
    d = {k: (v if isinstance(v, _np.ndarray) and share(mob, k) else _copy_value(v, memo))
         for k, v in mob.__dict__.items()}
    d['submobjects'] = [_lean(s, memo, made, share) for s in mob.submobjects]
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


def lean_copy(mob, share=_share_none):
    memo, made = {}, []
    new = _lean(mob, memo, made, share)
    _repoint(made, memo)
    return new


_ALIGN = ('align_data', 'null_point_align', 'align_submobjects')


def _zero_member_ok(cls):
    if not _lean_ok(cls):
        return False
    if any(getattr(cls, n) is not getattr(_Mobject, n) for n in _ALIGN):
        return False
    ap = cls.align_points
    return ap is _Mobject.align_points or ap is _VMobject.align_points


def _zero_ok(self):
    if self.path_func is not _interpolate:
        return False
    root = type(self.mobject)
    # the start copy's points go through Mobject.shift, then
    # (V)Mobject.scale -> apply_points_function_about_point, which rebind
    # every member's points before writing them
    if (root.shift is not _Mobject.shift
            or root.scale not in (_Mobject.scale, _VMobject.scale)
            or root.apply_points_function_about_point
            is not _Mobject.apply_points_function_about_point):
        return False
    root = self.mobject
    fam = root.get_family()
    return (all(_zero_member_ok(m.__class__) for m in fam)
            and not any(m.updaters for m in fam))


def _cairo():
    return _config.renderer != _RT.OPENGL


def _begin(self):
    if not _cairo():
        _state['stock'] += 1
        return _Transform.begin(self)
    self.target_mobject = self.create_target()
    self._desargues_zero = _mode['zero'] and _zero_ok(self)
    if self._desargues_zero:
        _state['zero'] += 1
        # the target is the mobject itself (FadeIn.create_target), so its
        # snapshot is a twin: align_data(twin) is a no-op and is skipped
        self.target_copy = lean_copy(self.target_mobject, _share_rebound)
        _Animation.begin(self)
        return
    _state['lean'] += 1
    # Transform.begin with the target snapshot made by lean_copy
    self.target_copy = lean_copy(self.target_mobject)
    self.mobject.align_data(self.target_copy)
    _Animation.begin(self)


def _create_starting_mobject(self):
    if not _cairo():
        return self._create_faded_mobject(fadeIn=True)
    # _Fade._create_faded_mobject(fadeIn=True) on a lean copy
    share = _share_points if getattr(self, '_desargues_zero', False) else _share_none
    faded = lean_copy(self.mobject, share)
    faded.fade(1)
    direction_modifier = -1 if not self.point_target else 1
    faded.shift(self.shift_vector * direction_modifier)
    faded.scale(self.scale_factor)
    return faded


_MISSING = object()
_PATCHED = {'begin': _begin, 'create_starting_mobject': _create_starting_mobject}
_saved = {}


def desargues_fadein_setup_install(zero=True):
    _mode['zero'] = bool(zero)
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
    _mode['zero'] = True
    return True


def desargues_fadein_setup_installed(zero=None):
    return bool(_saved) and (zero is None or _mode['zero'] == bool(zero))


def desargues_fadein_setup_stats():
    return dict(_state)


def desargues_fadein_setup_lean_copy(mob):
    return lean_copy(mob)
