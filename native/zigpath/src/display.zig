//! Manim's Camera.display_vectorized for a whole batch of VMobjects, one call.
//!
//! For each VMobject, in order, `desargues_display_batch` issues the cairo
//! calls manim makes in display_vectorized:
//!
//!   set_cairo_context_path            (path.zig, after the isfinite check of
//!                                      transform_points_pre_display)
//!   apply_stroke(background=True)     when the background width is > 0
//!   apply_fill                        always
//!   apply_stroke                      when the stroke width is > 0
//!
//! with set_cairo_context_color's solid source (one rgba) or linear gradient
//! (several rgbas, offsets as np.linspace(0, 1, n) computes them).
//!
//! Inputs, all float64 C order:
//!   pts   every VMobject's points back to back, 3 doubles per point
//!   meta  `meta_cols` doubles per VMobject (see Meta)
//!   cols  rgba rows back to back: fill rows, background stroke rows, stroke
//!         rows, per VMobject
//! line_mult is camera.cairo_line_width_multiple and ratio is
//! frame_width / frame_width, applied as Manim does: (w * line_mult) * ratio.
//! swap_rb = 0: the source is (b, g, r, a) of each row, as Manim passes it;
//! 1: (r, g, b, a), for the camera-bgra-pre patch, which swaps the columns
//! before Manim reverses them.

const std = @import("std");
const path = @import("path.zig");

const cairo_t = path.cairo_t;
const cairo_pattern_t = opaque {};

extern "cairo" fn cairo_set_source_rgba(cr: *cairo_t, r: f64, g: f64, b: f64, a: f64) callconv(.c) void;
extern "cairo" fn cairo_set_source(cr: *cairo_t, p: *cairo_pattern_t) callconv(.c) void;
extern "cairo" fn cairo_pattern_create_linear(x0: f64, y0: f64, x1: f64, y1: f64) callconv(.c) *cairo_pattern_t;
extern "cairo" fn cairo_pattern_add_color_stop_rgba(p: *cairo_pattern_t, off: f64, r: f64, g: f64, b: f64, a: f64) callconv(.c) void;
extern "cairo" fn cairo_pattern_destroy(p: *cairo_pattern_t) callconv(.c) void;
extern "cairo" fn cairo_fill_preserve(cr: *cairo_t) callconv(.c) void;
extern "cairo" fn cairo_stroke_preserve(cr: *cairo_t) callconv(.c) void;
extern "cairo" fn cairo_set_line_width(cr: *cairo_t, w: f64) callconv(.c) void;
extern "cairo" fn cairo_set_line_join(cr: *cairo_t, j: c_int) callconv(.c) void;
extern "cairo" fn cairo_set_line_cap(cr: *cairo_t, c: c_int) callconv(.c) void;
extern "cairo" fn cairo_status(cr: *cairo_t) callconv(.c) c_int;
extern "cairo" fn cairo_get_tolerance(cr: *cairo_t) callconv(.c) f64;

/// Column indices of one VMobject's meta row.
const Meta = struct {
    const npts = 0; // number of points (rows of 3 in pts)
    const atol = 1; // tolerance_for_point_equality
    const stroke_w = 2; // get_stroke_width(), after max(0.0, w)
    const bg_w = 3; // get_stroke_width(background=True)
    const join = 4; // cairo line join, -1 = LineJointType.AUTO (left alone)
    const cap = 5; // cairo line cap, -1 = CapStyleType.AUTO
    const nfill = 6; // fill rgba rows
    const nbg = 7; // background stroke rgba rows
    const nstroke = 8; // stroke rgba rows
    const gx0 = 9; // gradient start and end, after transform_points_pre_display
    const gy0 = 10;
    const gx1 = 11;
    const gy1 = 12;
};
pub const meta_cols: usize = 13;

const zero_point = [3]f64{ 0, 0, 0 };

inline fn count(m: [*]const f64, col: usize) usize {
    return @intFromFloat(m[col]);
}

fn allFinite(p: [*]const f64, n: usize) bool {
    var i: usize = 0;
    while (i < n * 3) : (i += 1) {
        if (!std.math.isFinite(p[i])) return false;
    }
    return true;
}

inline fn stop(pat: *cairo_pattern_t, off: f64, row: [*]const f64, swap_rb: bool) void {
    if (swap_rb) {
        cairo_pattern_add_color_stop_rgba(pat, off, row[0], row[1], row[2], row[3]);
    } else {
        cairo_pattern_add_color_stop_rgba(pat, off, row[2], row[1], row[0], row[3]);
    }
}

/// Camera.set_cairo_context_color for `n` rgba rows at `rows`.
fn setColor(cr: *cairo_t, rows: [*]const f64, n: usize, m: [*]const f64, swap_rb: bool) void {
    if (n == 1) {
        if (swap_rb) {
            cairo_set_source_rgba(cr, rows[0], rows[1], rows[2], rows[3]);
        } else {
            cairo_set_source_rgba(cr, rows[2], rows[1], rows[0], rows[3]);
        }
        return;
    }
    const pat = cairo_pattern_create_linear(m[Meta.gx0], m[Meta.gy0], m[Meta.gx1], m[Meta.gy1]);
    // np.linspace(0, 1, n): arange(n) * (1.0 / (n - 1)), last forced to 1;
    // n = 0 gives a pattern without stops, as Manim's empty loop does.
    if (n > 0) {
        const step: f64 = 1.0 / @as(f64, @floatFromInt(n - 1));
        var i: usize = 0;
        while (i < n) : (i += 1) {
            const off: f64 = if (i == n - 1) 1.0 else @as(f64, @floatFromInt(i)) * step;
            stop(pat, off, rows + i * 4, swap_rb);
        }
    }
    cairo_set_source(cr, pat);
    cairo_pattern_destroy(pat);
}

/// Camera.apply_stroke with width `w` (already max(0.0, w)).
fn stroke(cr: *cairo_t, rows: [*]const f64, n: usize, m: [*]const f64, w: f64, line_mult: f64, ratio: f64, swap_rb: bool) void {
    if (!(w > 0)) return;
    setColor(cr, rows, n, m, swap_rb);
    cairo_set_line_width(cr, w * line_mult * ratio);
    const join = m[Meta.join];
    if (join >= 0) cairo_set_line_join(cr, @intFromFloat(join));
    const cap = m[Meta.cap];
    if (cap >= 0) cairo_set_line_cap(cr, @intFromFloat(cap));
    cairo_stroke_preserve(cr);
}

/// Display `k` VMobjects on `cr`. Returns cairo_status(cr) at the end.
export fn desargues_display_batch(cr: *cairo_t, pts: [*]const f64, meta: [*]const f64, k: usize, cols: [*]const f64, line_mult: f64, ratio: f64, swap_rb_flag: c_int) c_int {
    const swap_rb = swap_rb_flag != 0;
    var p = pts;
    var c = cols;
    var j: usize = 0;
    while (j < k) : (j += 1) {
        const m = meta + j * meta_cols;
        const n = count(m, Meta.npts);
        // transform_points_pre_display: non-finite points become one origin.
        if (n == 0) {
            // Manim returns before new_path: the previous path is painted again.
        } else if (allFinite(p, n)) {
            _ = path.desargues_build_path(cr, p, n, 3, m[Meta.atol]);
        } else {
            _ = path.desargues_build_path(cr, &zero_point, 1, 3, m[Meta.atol]);
        }
        p += n * 3;
        const nfill = count(m, Meta.nfill);
        const nbg = count(m, Meta.nbg);
        const nstroke = count(m, Meta.nstroke);
        const fill_rows = c;
        const bg_rows = c + nfill * 4;
        const stroke_rows = bg_rows + nbg * 4;
        c = stroke_rows + nstroke * 4;
        stroke(cr, bg_rows, nbg, m, m[Meta.bg_w], line_mult, ratio, swap_rb);
        setColor(cr, fill_rows, nfill, m, swap_rb);
        cairo_fill_preserve(cr);
        stroke(cr, stroke_rows, nstroke, m, m[Meta.stroke_w], line_mult, ratio, swap_rb);
    }
    return cairo_status(cr);
}

/// ABI probe: 1 = the batch layout above (meta_cols 13).
export fn desargues_display_abi() c_int {
    return 1;
}

/// Layout probe, as in path.zig: cairo_get_tolerance through the cairo_t*.
export fn desargues_display_probe_tolerance(cr: *cairo_t) f64 {
    return cairo_get_tolerance(cr);
}
