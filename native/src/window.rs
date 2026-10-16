//! `desargues.native.window`: one X11 window (winit + softbuffer) showing a
//! gfx canvas, pumped from the cljrs interpreter thread.
//!
//! winit's event loop wants to own the thread; cljrs wants the same thread
//! (its values are !Send). `pump_app_events` with a zero timeout reconciles the
//! two: the Clojure tick loop calls `(pump!)` once per frame, which drains
//! pending window events and returns. No second thread, no channel.

use std::cell::RefCell;
use std::num::NonZeroU32;
use std::rc::Rc;
use std::time::Duration;

use cljrs_interop::export;
use cljrs_value::Value;
use winit::application::ApplicationHandler;
use winit::dpi::{LogicalSize, PhysicalPosition};
use winit::event::{ElementState, MouseButton, MouseScrollDelta, WindowEvent};
use winit::event_loop::{ActiveEventLoop, EventLoop};
use winit::keyboard::{Key, ModifiersState, NamedKey};
use winit::platform::pump_events::{EventLoopExtPumpEvents, PumpStatus};
use winit::window::{Window, WindowId};

use crate::gfx::{num, with_canvas};

struct App {
    title: String,
    w: u32,
    h: u32,
    window: Option<Rc<Window>>,
    surface: Option<softbuffer::Surface<Rc<Window>, Rc<Window>>>,
    closed: bool,
    events: Vec<String>,
    cursor: (f64, f64),
    mods: ModifiersState,
}

// ── Raw input as EDN ────────────────────────────────────────────────────
// Pixels are physical window pixels, which are canvas pixels (show! copies
// the canvas 1:1 from the top-left). Units and signs stay winit's; the pure
// desargues.player.native-input (JVM) turns these into IPointer / IKeyboard
// events, so there is one translation and one hit test.

fn mods_edn(m: ModifiersState) -> String {
    let mut v = Vec::new();
    if m.shift_key() {
        v.push(":shift");
    }
    if m.alt_key() {
        v.push(":alt");
    }
    if m.control_key() {
        v.push(":ctrl");
    }
    if m.super_key() {
        v.push(":meta");
    }
    format!("[{}]", v.join(" "))
}

fn button_kw(b: MouseButton) -> Option<&'static str> {
    match b {
        MouseButton::Left => Some(":primary"),
        MouseButton::Middle => Some(":middle"),
        MouseButton::Right => Some(":secondary"),
        _ => None,
    }
}

/// The DOM KeyboardEvent.key name of a winit logical key: winit's named keys
/// already carry the DOM names ("ArrowLeft", "Escape"); space is " ".
fn key_name(k: &Key) -> Option<String> {
    match k {
        Key::Character(s) => Some(s.to_string()),
        Key::Named(NamedKey::Space) => Some(" ".into()),
        Key::Named(NamedKey::Super) => Some("Meta".into()),
        Key::Named(n) => Some(format!("{n:?}")),
        _ => None,
    }
}

fn elapsed_ms() -> f64 {
    thread_local! { static T0: std::time::Instant = std::time::Instant::now(); }
    T0.with(|t| t.elapsed().as_secs_f64() * 1000.0)
}

impl ApplicationHandler for App {
    fn resumed(&mut self, el: &ActiveEventLoop) {
        if self.window.is_some() {
            return;
        }
        let attrs = Window::default_attributes()
            .with_title(self.title.clone())
            .with_inner_size(LogicalSize::new(self.w, self.h));
        match el.create_window(attrs) {
            Ok(win) => {
                let win = Rc::new(win);
                let ctx = softbuffer::Context::new(win.clone()).ok();
                self.surface = ctx.and_then(|c| softbuffer::Surface::new(&c, win.clone()).ok());
                self.window = Some(win);
            }
            Err(e) => {
                self.events.push(format!("{{:type :error :message {:?}}}", e.to_string()));
                self.closed = true;
            }
        }
    }

    fn window_event(&mut self, _el: &ActiveEventLoop, _id: WindowId, ev: WindowEvent) {
        match ev {
            WindowEvent::CloseRequested => {
                self.closed = true;
                self.events.push("{:type :close}".into());
            }
            WindowEvent::ModifiersChanged(m) => self.mods = m.state(),
            WindowEvent::KeyboardInput { event, .. } if event.state == ElementState::Pressed => {
                if let Some(k) = key_name(&event.logical_key) {
                    let text = event.text.map(|t| format!(" :text {:?}", t.to_string())).unwrap_or_default();
                    self.events.push(format!("{{:type :key :key {k:?}{text} :mods {}}}", mods_edn(self.mods)));
                }
            }
            WindowEvent::CursorMoved { position: PhysicalPosition { x, y }, .. } => {
                self.cursor = (x, y);
                self.events.push(format!("{{:type :move :px [{x} {y}] :mods {}}}", mods_edn(self.mods)));
            }
            WindowEvent::MouseInput { state, button, .. } => {
                if let Some(b) = button_kw(button) {
                    let (x, y) = self.cursor;
                    let t = if state == ElementState::Pressed { "down" } else { "up" };
                    self.events.push(format!(
                        "{{:type :{t} :px [{x} {y}] :button {b} :mods {} :ms {}}}",
                        mods_edn(self.mods),
                        elapsed_ms()
                    ));
                }
            }
            WindowEvent::MouseWheel { delta, .. } => {
                let (x, y) = self.cursor;
                let d = match delta {
                    MouseScrollDelta::LineDelta(_, ly) => format!(":lines {}", ly as f64),
                    MouseScrollDelta::PixelDelta(p) => format!(":pixels {}", p.y),
                };
                self.events.push(format!("{{:type :wheel :px [{x} {y}] {d} :mods {}}}", mods_edn(self.mods)));
            }
            // Delivered by winit on macOS / iOS only; X11 has no pinch event.
            WindowEvent::PinchGesture { delta, .. } => {
                let (x, y) = self.cursor;
                self.events.push(format!("{{:type :pinch :px [{x} {y}] :delta {delta}}}"));
            }
            _ => {}
        }
    }
}

thread_local! {
    static LOOP: RefCell<Option<EventLoop<()>>> = const { RefCell::new(None) };
    static APP: RefCell<Option<App>> = const { RefCell::new(None) };
}

fn pump_once() {
    LOOP.with(|l| {
        APP.with(|a| {
            if let (Some(el), Some(app)) = (l.borrow_mut().as_mut(), a.borrow_mut().as_mut()) {
                if let PumpStatus::Exit(_) = el.pump_app_events(Some(Duration::ZERO), app) {
                    app.closed = true;
                }
            }
        })
    })
}

/// (open! title w h) -> true when a window could be created on this display.
#[export(ns = "desargues.native.window", name = "open!", variadic_min = 3)]
pub fn open_bang(args: &[Value]) -> Result<Value, String> {
    let title = match &args[0] { Value::Str(s) => s.get().clone(), v => v.to_string() };
    let w = num(&args[1])? as u32;
    let h = num(&args[2])? as u32;
    let el = EventLoop::new().map_err(|e| format!("no event loop: {e}"))?;
    LOOP.with(|l| *l.borrow_mut() = Some(el));
    APP.with(|a| {
        *a.borrow_mut() = Some(App {
            title, w, h, window: None, surface: None, closed: false, events: Vec::new(), cursor: (0.0, 0.0),
            mods: ModifiersState::empty(),
        })
    });
    for _ in 0..50 {
        pump_once();
        let ready = APP.with(|a| a.borrow().as_ref().map(|a| a.surface.is_some() || a.closed).unwrap_or(true));
        if ready {
            break;
        }
        std::thread::sleep(Duration::from_millis(10));
    }
    Ok(Value::Bool(APP.with(|a| a.borrow().as_ref().map(|a| a.surface.is_some()).unwrap_or(false))))
}

/// (pump!) -> vector of event EDN strings since the last pump.
#[export(ns = "desargues.native.window", name = "pump!", variadic_min = 0)]
pub fn pump_bang(_args: &[Value]) -> Result<Value, String> {
    pump_once();
    let evs = APP.with(|a| a.borrow_mut().as_mut().map(|a| std::mem::take(&mut a.events)).unwrap_or_default());
    Ok(cljrs_interop::IntoValue::into_value(
        evs.into_iter().map(cljrs_interop::IntoValue::into_value).collect::<Vec<Value>>(),
    ))
}

/// (open?) -> false once the user closed the window (or it never opened).
#[export(ns = "desargues.native.window", name = "open?", variadic_min = 0)]
pub fn open_p(_args: &[Value]) -> Result<Value, String> {
    Ok(Value::Bool(APP.with(|a| a.borrow().as_ref().map(|a| !a.closed && a.window.is_some()).unwrap_or(false))))
}

/// (inner-size) -> [w h] of the window in physical pixels, or nil.
#[export(ns = "desargues.native.window", name = "inner-size", variadic_min = 0)]
pub fn inner_size(_args: &[Value]) -> Result<Value, String> {
    Ok(APP.with(|a| {
        a.borrow().as_ref().and_then(|a| a.window.as_ref()).map(|w| {
            let s = w.inner_size();
            cljrs_interop::IntoValue::into_value(vec![Value::Long(s.width as i64), Value::Long(s.height as i64)])
        })
    }).unwrap_or(Value::Nil))
}

/// (show! canvas) — copy a gfx canvas into the window's framebuffer.
#[export(ns = "desargues.native.window", name = "show!", variadic_min = 1)]
pub fn show_bang(args: &[Value]) -> Result<Value, String> {
    let (cw, ch, rgba) = with_canvas(&args[0], |p| (p.width(), p.height(), p.data().to_vec()))?;
    APP.with(|a| -> Result<(), String> {
        let mut a = a.borrow_mut();
        let Some(app) = a.as_mut() else { return Ok(()) };
        let Some(win) = app.window.clone() else { return Ok(()) };
        let Some(surf) = app.surface.as_mut() else { return Ok(()) };
        let s = win.inner_size();
        let (w, h) = (s.width.max(1), s.height.max(1));
        surf.resize(NonZeroU32::new(w).unwrap(), NonZeroU32::new(h).unwrap()).map_err(|e| e.to_string())?;
        let mut buf = surf.buffer_mut().map_err(|e| e.to_string())?;
        for y in 0..h {
            for x in 0..w {
                let px = if x < cw && y < ch {
                    let i = ((y * cw + x) * 4) as usize;
                    // premultiplied over black == the premultiplied rgb
                    (rgba[i] as u32) << 16 | (rgba[i + 1] as u32) << 8 | rgba[i + 2] as u32
                } else {
                    0
                };
                buf[(y * w + x) as usize] = px;
            }
        }
        win.pre_present_notify();
        buf.present().map_err(|e| e.to_string())?;
        Ok(())
    })?;
    Ok(Value::Nil)
}

/// (close!) — drop the window.
#[export(ns = "desargues.native.window", name = "close!", variadic_min = 0)]
pub fn close_bang(_args: &[Value]) -> Result<Value, String> {
    APP.with(|a| {
        if let Some(app) = a.borrow_mut().as_mut() {
            app.surface = None;
            app.window = None;
            app.closed = true;
        }
    });
    pump_once();
    Ok(Value::Nil)
}

/// (sleep-ms! n)
#[export(ns = "desargues.native.window", name = "sleep-ms!", variadic_min = 1)]
pub fn sleep_ms_bang(args: &[Value]) -> Result<Value, String> {
    std::thread::sleep(Duration::from_millis(num(&args[0])?.max(0.0) as u64));
    Ok(Value::Nil)
}

/// (now-ms) -> monotonic milliseconds as a double (the IClock source).
#[export(ns = "desargues.native.window", name = "now-ms", variadic_min = 0)]
pub fn now_ms(_args: &[Value]) -> Result<Value, String> {
    Ok(Value::Double(elapsed_ms()))
}
