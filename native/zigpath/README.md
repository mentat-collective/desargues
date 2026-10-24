# zigpath

Builds Cairo paths for Manim VMobjects natively. It makes one call per
VMobject. Manim's `Camera.set_cairo_context_path` makes one pycairo call per
Bezier segment.

`desargues_build_path(cairo_t*, const double* points, n, stride, atol)` issues
the same `new_path`, `new_sub_path`, `move_to`, `curve_to` and `close_path`
calls as Manim, in the same order and with the same doubles. Frames are
byte-identical.

`desargues_display_batch` (src/display.zig) goes further. It draws a whole
batch of VMobjects in one call: the path, the background stroke, the fill and
the stroke of each, with Manim's solid or linear-gradient sources, line
width, join and cap. It replaces `display_vectorized` for each VMobject in
`Camera.display_multiple_non_background_colored_vmobjects`. The loader lives
in `desargues.manim.patches.native-display`. VMobjects and cameras that
override Manim's display methods are drawn by Manim. `src/lib.zig` is the
library root.

```sh
DESARGUES_CONDA_PREFIX=$HOME/anaconda3/envs/manim bb native:path   # zig 0.16
DESARGUES_PATH_BUILDER=native ...                                   # opt in: paths
DESARGUES_PATH_BUILDER=native-display ...                           # paths, fill, stroke
```

The library lands in `zig-out/lib/libdesargues_path.so`. It is not committed,
and `DESARGUES_NATIVE_PATH_LIB` overrides where it is looked up. The loader
and the patch live in `desargues.manim.patches/native-cairo-path`. Without the
library, Manim builds its own paths.
