//! `desargues.native.gfx`: the drawing primitives the cljrs painter calls.
//!
//! Canvases are tiny-skia pixmaps held in a thread-local table and named by an
//! integer handle, so nothing but numbers and strings crosses the boundary.
//! Paths arrive as one flat vector of numbers with an op code in front of each
//! segment (the four segment kinds of desargues.player.svg-path plus close):
//!
//!   0 x y            move
//!   1 x y            line
//!   2 x1 y1 x y      quad
//!   3 x1 y1 x2 y2 x y cubic
//!   4                close
//!
//! Colours are four numbers 0-255 (straight alpha). Text is rasterised with
//! ab_glyph from DejaVu Sans, the face the JVM's logical SansSerif resolves to
//! on this platform, so the native picture matches the headless golden.

use std::cell::RefCell;

use ab_glyph::{Font, FontVec, PxScale, ScaleFont, point};
use cljrs_interop::export;
use cljrs_value::Value;
use tiny_skia::{
    Color, FillRule, LineCap, LineJoin, Paint, PathBuilder, Pixmap, Stroke, StrokeDash, Transform,
};

thread_local! {
    static CANVASES: RefCell<Vec<Option<Pixmap>>> = const { RefCell::new(Vec::new()) };
    static FONT: RefCell<Option<FontVec>> = const { RefCell::new(None) };
}

const FONT_CANDIDATES: &[&str] = &[
    "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
    "/usr/share/fonts/TTF/DejaVuSans.ttf",
    "/usr/share/fonts/dejavu/DejaVuSans.ttf",
    "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf",
];

pub fn num(v: &Value) -> Result<f32, String> {
    match v {
        Value::Long(n) => Ok(*n as f32),
        Value::Double(d) => Ok(*d as f32),
        other => Err(format!("expected a number, got {}", other.type_name())),
    }
}

fn arg<'a>(args: &'a [Value], i: usize, what: &str) -> Result<&'a Value, String> {
    args.get(i).ok_or_else(|| format!("{what}: missing argument {i}"))
}

fn numv(v: &Value) -> Result<Vec<f32>, String> {
    match v {
        Value::Vector(vec) => vec.get().iter().map(num).collect(),
        Value::Nil => Ok(Vec::new()),
        other => Err(format!("expected a vector of numbers, got {}", other.type_name())),
    }
}

fn string(v: &Value) -> Result<String, String> {
    match v {
        Value::Str(s) => Ok(s.get().clone()),
        other => Ok(other.to_string()),
    }
}

pub fn with_canvas<R>(h: &Value, f: impl FnOnce(&mut Pixmap) -> R) -> Result<R, String> {
    let i = num(h)? as usize;
    CANVASES.with(|c| {
        let mut c = c.borrow_mut();
        match c.get_mut(i).and_then(|p| p.as_mut()) {
            Some(p) => Ok(f(p)),
            None => Err(format!("no canvas {i}")),
        }
    })
}

fn color(args: &[Value], from: usize) -> Result<Color, String> {
    let c: Vec<f32> = (0..4)
        .map(|k| arg(args, from + k, "colour").and_then(num))
        .collect::<Result<_, _>>()?;
    let b = |x: f32| x.round().clamp(0.0, 255.0) as u8;
    Ok(Color::from_rgba8(b(c[0]), b(c[1]), b(c[2]), b(c[3])))
}

fn paint(c: Color) -> Paint<'static> {
    let mut p = Paint::default();
    p.set_color(c);
    p.anti_alias = true;
    p
}

fn build_path(segs: &[f32]) -> Option<tiny_skia::Path> {
    let mut pb = PathBuilder::new();
    let mut i = 0;
    while i < segs.len() {
        let op = segs[i] as i32;
        let a = &segs[i + 1..];
        match op {
            0 if a.len() >= 2 => { pb.move_to(a[0], a[1]); i += 3 }
            1 if a.len() >= 2 => { pb.line_to(a[0], a[1]); i += 3 }
            2 if a.len() >= 4 => { pb.quad_to(a[0], a[1], a[2], a[3]); i += 5 }
            3 if a.len() >= 6 => { pb.cubic_to(a[0], a[1], a[2], a[3], a[4], a[5]); i += 7 }
            4 => { pb.close(); i += 1 }
            _ => return None,
        }
    }
    pb.finish()
}

/// (canvas w h) -> handle
#[export(ns = "desargues.native.gfx", variadic_min = 2)]
pub fn canvas(args: &[Value]) -> Result<Value, String> {
    let w = num(&args[0])? as u32;
    let h = num(&args[1])? as u32;
    let p = Pixmap::new(w.max(1), h.max(1)).ok_or("bad canvas size")?;
    let id = CANVASES.with(|c| {
        let mut c = c.borrow_mut();
        c.push(Some(p));
        c.len() - 1
    });
    Ok(Value::Long(id as i64))
}

/// (size c) -> [w h]
#[export(ns = "desargues.native.gfx", variadic_min = 1)]
pub fn size(args: &[Value]) -> Result<Value, String> {
    let (w, h) = with_canvas(&args[0], |p| (p.width(), p.height()))?;
    Ok(cljrs_interop::IntoValue::into_value(vec![Value::Long(w as i64), Value::Long(h as i64)]))
}

/// (clear! c r g b a)
#[export(ns = "desargues.native.gfx", name = "clear!", variadic_min = 5)]
pub fn clear_bang(args: &[Value]) -> Result<Value, String> {
    let col = color(args, 1)?;
    with_canvas(&args[0], |p| p.fill(col))?;
    Ok(Value::Nil)
}

/// (fill-path! c segs r g b a)
#[export(ns = "desargues.native.gfx", name = "fill-path!", variadic_min = 6)]
pub fn fill_path_bang(args: &[Value]) -> Result<Value, String> {
    let segs = numv(&args[1])?;
    let col = color(args, 2)?;
    if let Some(path) = build_path(&segs) {
        with_canvas(&args[0], |p| {
            p.fill_path(&path, &paint(col), FillRule::Winding, Transform::identity(), None)
        })?;
    }
    Ok(Value::Nil)
}

/// (stroke-path! c segs r g b a width dashes) — square caps and miter joins,
/// as java.awt.BasicStroke's defaults; dashes nil or a vector of pixels.
#[export(ns = "desargues.native.gfx", name = "stroke-path!", variadic_min = 7)]
pub fn stroke_path_bang(args: &[Value]) -> Result<Value, String> {
    let segs = numv(&args[1])?;
    let col = color(args, 2)?;
    let width = num(&args[6])?;
    let dashes = match args.get(7) { Some(v) => numv(v)?, None => Vec::new() };
    let mut stroke = Stroke { width, line_cap: LineCap::Square, line_join: LineJoin::Miter, ..Stroke::default() };
    if !dashes.is_empty() {
        let mut d = dashes.clone();
        if d.len() % 2 == 1 { d.extend(dashes.iter()); }
        stroke.dash = StrokeDash::new(d, 0.0);
    }
    if let Some(path) = build_path(&segs) {
        with_canvas(&args[0], |p| {
            p.stroke_path(&path, &paint(col), &stroke, Transform::identity(), None)
        })?;
    }
    Ok(Value::Nil)
}

fn load_font() -> Result<(), String> {
    FONT.with(|f| {
        if f.borrow().is_some() { return Ok(()); }
        for path in FONT_CANDIDATES {
            if let Ok(bytes) = std::fs::read(path) {
                let font = FontVec::try_from_vec(bytes).map_err(|e| e.to_string())?;
                *f.borrow_mut() = Some(font);
                return Ok(());
            }
        }
        Err("no sans-serif font found".to_string())
    })
}

/// ab_glyph scales by ascent-descent; Java sizes fonts by the em. Convert.
fn em_scale(font: &FontVec, px: f32) -> PxScale {
    let upem = font.units_per_em().unwrap_or(2048.0);
    PxScale::from(px * font.height_unscaled() / upem)
}

/// (text-metrics s px) -> [width ascent descent], descent positive (AWT FontMetrics)
#[export(ns = "desargues.native.gfx", variadic_min = 2)]
pub fn text_metrics(args: &[Value]) -> Result<Value, String> {
    let s = string(&args[0])?;
    let px = num(&args[1])?;
    load_font()?;
    let (w, a, d) = FONT.with(|f| {
        let f = f.borrow();
        let font = f.as_ref().unwrap();
        let sf = font.as_scaled(em_scale(font, px));
        let mut w = 0.0;
        let mut prev = None;
        for ch in s.chars() {
            let id = sf.glyph_id(ch);
            if let Some(p) = prev { w += sf.kern(p, id); }
            w += sf.h_advance(id);
            prev = Some(id);
        }
        (w, sf.ascent(), -sf.descent())
    });
    Ok(cljrs_interop::IntoValue::into_value(vec![
        Value::Double(w as f64), Value::Double(a as f64), Value::Double(d as f64),
    ]))
}

/// (fill-text! c s x y px r g b a) — `x y` is the left end of the baseline.
#[export(ns = "desargues.native.gfx", name = "fill-text!", variadic_min = 9)]
pub fn fill_text_bang(args: &[Value]) -> Result<Value, String> {
    let s = string(&args[1])?;
    let x0 = num(&args[2])?;
    let y0 = num(&args[3])?;
    let px = num(&args[4])?;
    let col = color(args, 5)?.to_color_u8();
    load_font()?;
    FONT.with(|f| {
        let f = f.borrow();
        let font = f.as_ref().unwrap();
        let scale = em_scale(font, px);
        let sf = font.as_scaled(scale);
        with_canvas(&args[0], |p| {
            let (w, h) = (p.width() as i32, p.height() as i32);
            let data = p.data_mut();
            let mut x = x0;
            let mut prev = None;
            for ch in s.chars() {
                let id = sf.glyph_id(ch);
                if let Some(pv) = prev { x += sf.kern(pv, id); }
                let g = id.with_scale_and_position(scale, point(x, y0));
                if let Some(og) = font.outline_glyph(g) {
                    let b = og.px_bounds();
                    og.draw(|gx, gy, cov| {
                        let px_ = b.min.x as i32 + gx as i32;
                        let py_ = b.min.y as i32 + gy as i32;
                        if px_ < 0 || py_ < 0 || px_ >= w || py_ >= h { return; }
                        let a = cov.clamp(0.0, 1.0) * col.alpha() as f32 / 255.0;
                        let i = ((py_ * w + px_) * 4) as usize;
                        let src = [col.red(), col.green(), col.blue()];
                        for k in 0..3 {
                            let d = data[i + k] as f32;
                            data[i + k] = (src[k] as f32 * a + d * (1.0 - a)).round() as u8;
                        }
                        let da = data[i + 3] as f32;
                        data[i + 3] = (255.0 * a + da * (1.0 - a)).round() as u8;
                    });
                }
                x += sf.h_advance(id);
                prev = Some(id);
            }
        })
    })?;
    Ok(Value::Nil)
}

/// (save-png! c path) -> path
#[export(ns = "desargues.native.gfx", name = "save-png!", variadic_min = 2)]
pub fn save_png_bang(args: &[Value]) -> Result<Value, String> {
    let path = string(&args[1])?;
    if let Some(dir) = std::path::Path::new(&path).parent() {
        let _ = std::fs::create_dir_all(dir);
    }
    with_canvas(&args[0], |p| p.save_png(&path))?.map_err(|e| e.to_string())?;
    Ok(cljrs_interop::IntoValue::into_value(path))
}

/// (blit! dst src x y) — copy canvas `src` onto `dst` at pixel x y.
#[export(ns = "desargues.native.gfx", name = "blit!", variadic_min = 4)]
pub fn blit_bang(args: &[Value]) -> Result<Value, String> {
    let src = with_canvas(&args[1], |p| p.clone())?;
    let x = num(&args[2])? as i32;
    let y = num(&args[3])? as i32;
    with_canvas(&args[0], |p| {
        p.draw_pixmap(x, y, src.as_ref(), &tiny_skia::PixmapPaint::default(), Transform::identity(), None)
    })?;
    Ok(Value::Nil)
}
