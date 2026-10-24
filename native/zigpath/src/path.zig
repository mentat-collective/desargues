//! Cairo path building for Manim VMobjects, one call per VMobject.
//!
//! `desargues_build_path` issues the same cairo calls, in the same order and
//! with the same doubles, as manim's Camera.set_cairo_context_path:
//!
//!   new_path
//!   for each subpath (split where consecutive anchors differ in 2D):
//!     new_sub_path, move_to(start), curve_to(p1, p2, p3) per cubic,
//!     close_path when the first and last points are equal in 2D
//!
//! The caller has already applied transform_points_pre_display, so every
//! point is finite and the array is the one Manim would iterate.

const std = @import("std");

const cairo_t = opaque {};

extern "cairo" fn cairo_new_path(cr: *cairo_t) callconv(.c) void;
extern "cairo" fn cairo_new_sub_path(cr: *cairo_t) callconv(.c) void;
extern "cairo" fn cairo_move_to(cr: *cairo_t, x: f64, y: f64) callconv(.c) void;
extern "cairo" fn cairo_curve_to(cr: *cairo_t, x1: f64, y1: f64, x2: f64, y2: f64, x3: f64, y3: f64) callconv(.c) void;
extern "cairo" fn cairo_close_path(cr: *cairo_t) callconv(.c) void;
extern "cairo" fn cairo_status(cr: *cairo_t) callconv(.c) c_int;
extern "cairo" fn cairo_get_tolerance(cr: *cairo_t) callconv(.c) f64;

/// Cubic Bezier: anchor, handle, handle, anchor.
const nppcc: usize = 4;

/// np.isclose's rtol, as in VMobject.consider_points_equals_2d.
const rtol: f64 = 1.0e-5;

/// VMobject.consider_points_equals_2d, operation for operation.
inline fn equal2d(p0: [*]const f64, p1: [*]const f64, atol: f64) bool {
    if (@abs(p0[0] - p1[0]) > atol + rtol * @abs(p1[0])) return false;
    return @abs(p0[1] - p1[1]) <= atol + rtol * @abs(p1[1]);
}

fn emitSubpath(cr: *cairo_t, pts: [*]const f64, stride: usize, lo: usize, hi: usize, atol: f64) void {
    const len = hi - lo;
    const usable = len - len % nppcc;
    const first = pts + lo * stride;
    cairo_new_sub_path(cr);
    cairo_move_to(cr, first[0], first[1]);
    var i: usize = 0;
    while (i < usable) : (i += nppcc) {
        const p1 = pts + (lo + i + 1) * stride;
        const p2 = pts + (lo + i + 2) * stride;
        const p3 = pts + (lo + i + 3) * stride;
        cairo_curve_to(cr, p1[0], p1[1], p2[0], p2[1], p3[0], p3[1]);
    }
    if (equal2d(first, pts + (hi - 1) * stride, atol)) cairo_close_path(cr);
}

/// Build the path of one VMobject on `cr`.
///   points: n rows of `stride` doubles (C order; x, y first), read in place
///   atol:   the VMobject's tolerance_for_point_equality
/// Returns cairo_status(cr) after the calls (0 = CAIRO_STATUS_SUCCESS).
export fn desargues_build_path(cr: *cairo_t, points: [*]const f64, n: usize, stride: usize, atol: f64) c_int {
    if (n == 0) return cairo_status(cr);
    cairo_new_path(cr);
    // Split indices: 0, every k in nppcc, 2*nppcc, ... < n whose anchor does
    // not continue the previous curve, then n. Segments shorter than one
    // cubic are dropped, as in VMobject._gen_subpaths_from_points.
    var start: usize = 0;
    var k: usize = nppcc;
    while (k < n) : (k += nppcc) {
        if (!equal2d(points + (k - 1) * stride, points + k * stride, atol)) {
            if (k - start >= nppcc) emitSubpath(cr, points, stride, start, k, atol);
            start = k;
        }
    }
    if (n - start >= nppcc) emitSubpath(cr, points, stride, start, n, atol);
    return cairo_status(cr);
}

/// ABI probe: lets the loader check it has the library it expects.
export fn desargues_path_abi() c_int {
    return 1;
}

/// Layout probe: the loader sets a tolerance through pycairo and reads it
/// back here through the cairo_t* it extracted, so a wrong offset is caught
/// before any path is built.
export fn desargues_path_probe_tolerance(cr: *cairo_t) f64 {
    return cairo_get_tolerance(cr);
}
