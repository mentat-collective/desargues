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

   glyph-cache: Text and MathTex (and Tex, a MathTex subclass) construction
   reads a font or a LaTeX build, parses the SVG and generates the cairo
   paths. The patch wraps their __init__: the first construction of a key is
   built as usual and a deep copy is kept; a later construction with the same
   key becomes a deep copy of the kept build. The key holds the class, the
   positional and keyword arguments, the set_default partials of the class
   chain and the Manim config the build reads (pixel_width / pixel_height,
   renderer, text_dir, tex_template), so a config change misses. Arguments
   whose repr carries an object address are not cached. Bounded LRU."
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

(def ^:private glyph-cache-source
  "import copy as _copy
from collections import OrderedDict as _OD
from manim import config as _config
from manim.mobject.text.text_mobject import Text as _Text
from manim.mobject.text.tex_mobject import MathTex as _MathTex, Tex as _Tex
from manim.mobject.mobject import Mobject as _Mob
from manim.mobject.types.vectorized_mobject import VMobject as _VMob
from manim.mobject.svg.svg_mobject import SVGMobject as _SVGMob

_GC = {'store': _OD(), 'max': 256, 'hits': 0, 'misses': 0, 'uncacheable': 0}
_GC_CLASSES = (_Text, _MathTex)
_GC_TYPES = (_Text, _MathTex, _Tex)

def _gc_defaults(C):
    init = C.__dict__.get('__init__')
    return repr((getattr(init, 'args', None), getattr(init, 'keywords', None)))

def _gc_key(self, args, kwargs):
    try:
        k = repr((type(self).__module__, type(self).__qualname__, args,
                  sorted(kwargs.items()),
                  _config['pixel_width'], _config['pixel_height'],
                  str(_config.renderer), str(_config.get_dir('text_dir')),
                  repr(_config['tex_template']),
                  [_gc_defaults(C) for C in (_Mob, _VMob, _SVGMob, type(self))]))
    except Exception:
        return None
    if ' object at 0x' in k:
        return None
    return k

def _gc_fill(self, cached):
    memo = {id(cached): self}
    for k, v in cached.__dict__.items():
        setattr(self, k, _copy.deepcopy(v, memo))
    self.original_id = str(id(cached))

def _gc_wrap(C):
    orig = C.__dict__['__init__']
    def __init__(self, *args, **kwargs):
        bound = orig.__get__(self, type(self))
        if type(self) not in _GC_TYPES or self.__dict__:
            return bound(*args, **kwargs)
        key = _gc_key(self, args, kwargs)
        if key is None:
            _GC['uncacheable'] += 1
            return bound(*args, **kwargs)
        store = _GC['store']
        cached = store.get(key)
        if cached is not None:
            store.move_to_end(key)
            _GC['hits'] += 1
            _gc_fill(self, cached)
            return None
        _GC['misses'] += 1
        bound(*args, **kwargs)
        store[key] = _copy.deepcopy(self)
        while len(store) > _GC['max']:
            store.popitem(last=False)
        return None
    __init__._desargues_glyph_cache_orig = orig
    __init__.__wrapped__ = orig
    return __init__

def desargues_glyph_cache_install(max_entries):
    _GC['max'] = int(max_entries)
    for C in _GC_CLASSES:
        if '_desargues_glyph_cache' not in C.__dict__:
            w = _gc_wrap(C)
            C.__init__ = w
            C._desargues_glyph_cache = w
    return True

def desargues_glyph_cache_uninstall():
    removed = False
    for C in _GC_CLASSES:
        if '_desargues_glyph_cache' in C.__dict__:
            C.__init__ = C.__dict__['_desargues_glyph_cache']._desargues_glyph_cache_orig
            del C._desargues_glyph_cache
            removed = True
    return removed

def desargues_glyph_cache_installed():
    return all('_desargues_glyph_cache' in C.__dict__ for C in _GC_CLASSES)

def desargues_glyph_cache_stats():
    return {'hits': _GC['hits'], 'misses': _GC['misses'],
            'uncacheable': _GC['uncacheable'],
            'size': len(_GC['store']), 'max': _GC['max']}

def desargues_glyph_cache_clear():
    _GC['store'].clear()
    _GC['hits'] = 0; _GC['misses'] = 0; _GC['uncacheable'] = 0
    return True
")

(def ^:private glyph-shims
  (delay (:globals (py/run-simple-string glyph-cache-source))))

(defn- call-glyph [nm & args] (py/->jvm (apply (get @glyph-shims nm) args)))

(defrecord GlyphCache [max-entries]
  pp/IManimPatch
  (-patch-id [_] :glyph-cache)
  (-install! [_] (boolean (call-glyph "desargues_glyph_cache_install" max-entries)))
  (-uninstall! [_] (boolean (call-glyph "desargues_glyph_cache_uninstall")))
  (-installed? [_] (boolean (call-glyph "desargues_glyph_cache_installed"))))

(defn glyph-cache
  "A Text/MathTex build cache holding at most `max-entries` built mobjects
   (LRU). A construction whose class, arguments, class defaults and the
   relevant Manim config (pixel size, renderer, text_dir, tex_template) match
   a cached build becomes a deep copy of it; anything unrepresentable in the
   key is built normally."
  ([] (glyph-cache 256))
  ([max-entries] (->GlyphCache max-entries)))

(defn glyph-cache-stats
  "{:hits :misses :uncacheable :size :max} of the glyph cache (counters since
   the last `glyph-cache-clear!`)."
  []
  (update-keys (call-glyph "desargues_glyph_cache_stats") keyword))

(defn glyph-cache-clear!
  "Drop every cached build and reset the counters."
  []
  (boolean (call-glyph "desargues_glyph_cache_clear")))

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
