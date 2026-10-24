# zigpath

Builds Cairo paths for Manim VMobjects natively. It makes one call per
VMobject. Manim's `Camera.set_cairo_context_path` makes one pycairo call per
Bezier segment.

`desargues_build_path(cairo_t*, const double* points, n, stride, atol)` issues
the same `new_path`, `new_sub_path`, `move_to`, `curve_to` and `close_path`
calls as Manim, in the same order and with the same doubles. Frames are
byte-identical.

```sh
DESARGUES_CONDA_PREFIX=$HOME/anaconda3/envs/manim bb native:path   # zig 0.16
DESARGUES_PATH_BUILDER=native ...                                   # opt in
```

The library lands in `zig-out/lib/libdesargues_path.so`. It is not committed,
and `DESARGUES_NATIVE_PATH_LIB` overrides where it is looked up. The loader
and the patch live in `desargues.manim.patches/native-cairo-path`. Without the
library, Manim builds its own paths.
