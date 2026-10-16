//! desargues-native: the player's native host.
//!
//! Embeds a cljrs (clojurust) runtime, registers the `#[export]` functions of
//! `gfx` (tiny-skia canvases, ab_glyph text) and `window` (winit + softbuffer),
//! puts the desargues `src/` (the pure .cljc player strata) and `native/cljrs/`
//! (the host adapter) on the source path, then runs a namespace's `-main`:
//!
//!   desargues-native <ns> [args...]
//!
//! The default namespace is `desargues.native.main`. Nothing in the player is
//! compiled for this host: frame, timeline, session, graph-source and
//! svg-path load from their .cljc files exactly as the JVM loads them.

mod gfx;
mod stdin;
mod window;

use std::path::PathBuf;

use cljrs_interop::{IntoValue, Registry, export};
use cljrs_runtime::tiered::{Env, eval};
use cljrs_value::Value;

thread_local! {
    static ARGS: std::cell::RefCell<Vec<String>> = const { std::cell::RefCell::new(Vec::new()) };
}

/// (args) -> the command-line arguments after the namespace.
#[export(ns = "desargues.native.sys", name = "args", variadic_min = 0)]
pub fn cli_args(_a: &[Value]) -> Result<Value, String> {
    Ok(ARGS.with(|a| a.borrow().iter().map(|s| s.as_str().into_value()).collect::<Vec<Value>>()).into_value())
}

/// (getenv name) -> string or nil.
#[export(ns = "desargues.native.sys", name = "getenv", variadic_min = 1)]
pub fn getenv(a: &[Value]) -> Result<Value, String> {
    let k = match &a[0] { Value::Str(s) => s.get().clone(), v => v.to_string() };
    Ok(std::env::var(k).ok().into_value())
}

/// (exit! code)
#[export(ns = "desargues.native.sys", name = "exit!", variadic_min = 1)]
pub fn exit_bang(a: &[Value]) -> Result<Value, String> {
    std::process::exit(gfx::num(&a[0])? as i32)
}

/// (read-line) -> one line of stdin without its newline, or nil at EOF.
#[export(ns = "desargues.native.sys", name = "read-line", variadic_min = 0)]
pub fn read_line(_a: &[Value]) -> Result<Value, String> {
    let mut s = String::new();
    match std::io::stdin().read_line(&mut s) {
        Ok(0) => Ok(Value::Nil),
        Ok(_) => Ok(s.trim_end_matches(['\n', '\r']).to_string().into_value()),
        Err(e) => Err(e.to_string()),
    }
}

fn eval_str(env: &mut Env, src: &str) -> Result<Value, String> {
    let mut p = cljrs_reader::Parser::new(src.to_string(), "<desargues-native>".to_string());
    let forms = p.parse_all().map_err(|e| format!("{e:?}"))?;
    let mut out = Value::Nil;
    for f in &forms {
        let _frame = cljrs_gc::push_alloc_frame();
        out = eval(f, env).map_err(|e| format!("{e:?}"))?;
    }
    Ok(out)
}

fn main() {
    let mut argv: Vec<String> = std::env::args().skip(1).collect();
    let ns = if argv.is_empty() { "desargues.native.main".to_string() } else { argv.remove(0) };
    ARGS.with(|a| *a.borrow_mut() = argv);

    let crate_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let root = std::env::var("DESARGUES_ROOT").map(PathBuf::from).unwrap_or_else(|_| crate_dir.join(".."));
    let paths = vec![crate_dir.join("cljrs"), root.join("src")];

    let _mutator = cljrs_gc::register_mutator();
    let runtime = cljrs_runtime::Runtime::builder()
        .execution_mode(cljrs_runtime::ExecutionMode::Tiered)
        .source_paths(paths)
        .build()
        .expect("cljrs runtime");
    cljrs_stdlib::install(&runtime);
    let globals = runtime.into_globals();
    let registry = Registry::new(globals.clone());
    for n in ["desargues.native.gfx", "desargues.native.window", "desargues.native.sys", "desargues.native.stdin"] {
        registry.env().mark_loaded(n);
    }

    let mut env = Env::new(globals, "user");
    cljrs_runtime::env::callback::push_eval_context(&env);
    let code = match eval_str(&mut env, &format!("(require '{ns}) ({ns}/-main)")) {
        Ok(_) => 0,
        Err(e) => {
            eprintln!("desargues-native: {e}");
            1
        }
    };
    cljrs_runtime::env::callback::pop_eval_context();
    std::process::exit(code);
}
