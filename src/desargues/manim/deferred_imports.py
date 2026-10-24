"""Deferred imports for `import manim` (desargues.manim.core :import-mode "deferred").

install(spec) puts a placeholder module in sys.modules for each named module
that is not loaded yet. While manim imports, attribute reads on a placeholder
answer DeferredAttr proxies (module-level aliases such as
`nx.layout.circular_layout` or `from scipy.spatial.transform import Rotation`).
seal(stubs) removes the placeholders from sys.modules; from then on any
attribute read on a placeholder imports the real module and forwards to it,
and a DeferredAttr resolves on first call or attribute read.
"""

import importlib
import sys
import types

_UNSET = object()


class DeferredModule(types.ModuleType):
    def __init__(self, name):
        super().__init__(name)
        object.__setattr__(self, "_d_real", None)
        object.__setattr__(self, "_d_sealed", False)

    def _d_resolve(self):
        real = object.__getattribute__(self, "_d_real")
        if real is None:
            name = object.__getattribute__(self, "__name__")
            if sys.modules.get(name) is self:
                del sys.modules[name]
            real = importlib.import_module(name)
            object.__setattr__(self, "_d_real", real)
        return real

    def __getattr__(self, attr):
        unresolved = object.__getattribute__(self, "_d_real") is None
        if unresolved and attr.startswith("__"):
            raise AttributeError(attr)
        if unresolved and not object.__getattribute__(self, "_d_sealed"):
            return DeferredAttr(self, (attr,))
        return getattr(self._d_resolve(), attr)


class DeferredAttr:
    __slots__ = ("_d_mod", "_d_path", "_d_val")

    def __init__(self, mod, path):
        object.__setattr__(self, "_d_mod", mod)
        object.__setattr__(self, "_d_path", path)
        object.__setattr__(self, "_d_val", _UNSET)

    def _d_target(self):
        val = object.__getattribute__(self, "_d_val")
        if val is _UNSET:
            val = object.__getattribute__(self, "_d_mod")._d_resolve()
            for a in object.__getattribute__(self, "_d_path"):
                val = getattr(val, a)
            object.__setattr__(self, "_d_val", val)
        return val

    def __getattr__(self, attr):
        mod = object.__getattribute__(self, "_d_mod")
        if (object.__getattribute__(self, "_d_val") is _UNSET
                and not object.__getattribute__(mod, "_d_sealed")
                and not attr.startswith("__")):
            return DeferredAttr(mod, object.__getattribute__(self, "_d_path") + (attr,))
        return getattr(self._d_target(), attr)

    def __call__(self, *args, **kwargs):
        return self._d_target()(*args, **kwargs)

    def __repr__(self):
        return "<deferred %s.%s>" % (
            object.__getattribute__(self, "_d_mod").__name__,
            ".".join(object.__getattribute__(self, "_d_path")))


def install(names):
    """Placeholder every name not yet in sys.modules -> {name: placeholder}."""
    stubs = {}
    for name in names:
        if name not in sys.modules:
            stubs[name] = sys.modules[name] = DeferredModule(name)
    return stubs


def seal(stubs):
    """End the import window: placeholders leave sys.modules and resolve on use."""
    for name, stub in stubs.items():
        object.__setattr__(stub, "_d_sealed", True)
        if sys.modules.get(name) is stub:
            del sys.modules[name]
    return len(stubs)
