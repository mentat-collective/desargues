"""Python side of desargues.manim.patches.play-setup.

Two parts, each installed on its own:

decimal-glyphs
    DecimalNumber._string_to_mob deep-copies the cached glyph template
    (string_to_mob_map) for every character of every set_value, then scales
    the copy to the number's font size. ChangeDecimalToValue calls set_value
    once per frame, so a counter spends most of its frame in copy.deepcopy.
    This part keeps, per (glyph string, font size), the glyph already scaled,
    and hands out a lean family copy of it: same classes and attribute
    values, fresh numpy arrays, lists and dicts, every other value shared,
    references to mobjects re-pointed at their copies as deepcopy's memo
    would. The scaled glyph is computed by the stock steps (copy of the
    template, then the font_size setter), so its points are the stock ones
    bit for bit. Calls with extra kwargs (the ellipsis, the unit) and the
    OpenGL renderer run the stock method; so does a glyph whose family holds
    a class with its own copy semantics.

family-walk
    Mobject.get_family rebuilds the list with remove_list_redundancies at
    every level of the tree. This part walks the tree once and runs the
    redundancy pass only when an id repeats, which yields the same list
    (keep-last order is preserved under flattening). A submobject whose class
    overrides get_family is asked for its own family.
"""
import copy as _copy
import numpy as _np
from manim import config as _config
from manim.constants import RendererType as _RT
from manim.mobject.mobject import Mobject as _Mobject
from manim.mobject.text import numbers as _nm
from manim.utils.iterables import remove_list_redundancies as _rlr

_DecimalNumber = _nm.DecimalNumber
_state = {'lean': 0, 'scaled': 0, 'stock': 0}
_ORIG_DEEPCOPY = _Mobject.__deepcopy__
_scaled = {}
# (glyph, font size) pairs kept; a scene that animates font sizes would
# otherwise grow the table without bound
_MAX_SCALED = 4096


# ---------------------------------------------------------------- lean copy

def _lean_ok(cls):
    if cls.copy is not _Mobject.copy or _Mobject.__deepcopy__ is not _ORIG_DEEPCOPY:
        return False
    return (cls.__deepcopy__ is _Mobject.__deepcopy__
            or '_desargues_svg_copy' in cls.__dict__)


def _family_ok(mob):
    return all(_lean_ok(m.__class__) for m in mob.get_family())


def _copy_value(v, memo):
    if isinstance(v, _np.ndarray):
        # one copy per source array, as with deepcopy's memo
        c = memo.get(id(v))
        if c is None:
            c = v.copy()
            memo[id(v)] = c
        return c
    t = type(v)
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


def _ref(x, memo, made):
    return _lean(x, memo, made) if isinstance(x, _Mobject) else x


def _repoint(made, memo):
    # mobjects referenced outside the submobject tree (MathTex keeps VGroups
    # in id_to_vgroup_dict) are copied too, as deepcopy does
    i = 0
    while i < len(made):
        d = made[i].__dict__
        for k, v in d.items():
            if k == 'submobjects':
                continue
            if isinstance(v, _Mobject):
                d[k] = _lean(v, memo, made)
            elif type(v) is list:
                if any(isinstance(x, _Mobject) for x in v):
                    d[k] = [_ref(x, memo, made) for x in v]
            elif type(v) is dict:
                if any(isinstance(x, _Mobject) for x in v.values()):
                    d[k] = {kk: _ref(x, memo, made) for kk, x in v.items()}
        i += 1


def lean_copy(mob):
    memo, made = {}, []
    new = _lean(mob, memo, made)
    _repoint(made, memo)
    return new


# ---------------------------------------------------------- decimal-glyphs

_ORIG_STRING_TO_MOB = _DecimalNumber.__dict__['_string_to_mob']


def _string_to_mob(self, string, mob_class=None, **kwargs):
    if kwargs or _config.renderer == _RT.OPENGL:
        _state['stock'] += 1
        return _ORIG_STRING_TO_MOB(self, string, mob_class, **kwargs)
    if mob_class is None:
        mob_class = self.mob_class
    cache = _nm.string_to_mob_map
    if string not in cache:
        cache[string] = mob_class(string)
    tmpl = cache[string]
    fs = self._font_size
    key = (string, fs)
    hit = _scaled.get(key)
    if hit is not None and hit[0] is tmpl:
        _state['lean'] += 1
        return lean_copy(hit[1])
    # the stock steps, on the stock copy
    mob = tmpl.copy()
    mob.font_size = fs
    if _family_ok(mob):
        if len(_scaled) >= _MAX_SCALED:
            _scaled.clear()
        _scaled[key] = (tmpl, lean_copy(mob))
        _state['scaled'] += 1
    else:
        _state['stock'] += 1
    return mob


# ------------------------------------------------------------- family-walk

_ORIG_GET_FAMILY = _Mobject.__dict__['get_family']


def _walk(m, out):
    out.append(m)
    for s in m.submobjects:
        if type(s).get_family is _get_family:
            _walk(s, out)
        else:
            out.extend(s.get_family())


def _get_family(self, recurse=True):
    out = []
    _walk(self, out)
    if len(out) > 1 and len(set(map(id, out))) != len(out):
        return _rlr(out)
    return out


# ---------------------------------------------------------------- boundary

_PARTS = {
    'decimal-glyphs': (_DecimalNumber, '_string_to_mob', _string_to_mob, _ORIG_STRING_TO_MOB),
    'family-walk': (_Mobject, 'get_family', _get_family, _ORIG_GET_FAMILY),
}


def _installed(part):
    cls, name, fn, _ = _PARTS[part]
    return cls.__dict__.get(name) is fn


def desargues_play_setup_install(parts):
    for p in parts:
        cls, name, fn, _ = _PARTS[p]
        setattr(cls, name, fn)
    return True


def desargues_play_setup_uninstall():
    was = any(_installed(p) for p in _PARTS)
    for p, (cls, name, _, orig) in _PARTS.items():
        setattr(cls, name, orig)
    _scaled.clear()
    return was


def desargues_play_setup_installed(parts):
    return bool(parts) and all(_installed(p) for p in parts)


def desargues_play_setup_parts():
    return [p for p in _PARTS if _installed(p)]


def desargues_play_setup_stats():
    return dict(_state)


def desargues_play_setup_lean_copy(mob):
    return lean_copy(mob)
