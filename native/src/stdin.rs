//! `desargues.native.stdin`: non-blocking line input.
//!
//! The JVM side of the bridge (desargues.player.native) writes one EDN command
//! per line. A reader thread owns stdin and hands lines over as plain Strings
//! (Send), so the interpreter thread never blocks and the window keeps pumping.

use std::sync::Mutex;
use std::sync::mpsc::{Receiver, channel};

use cljrs_interop::{IntoValue, export};
use cljrs_value::Value;

static RX: Mutex<Option<Receiver<Option<String>>>> = Mutex::new(None);

fn ensure_reader() {
    let mut rx = RX.lock().unwrap();
    if rx.is_none() {
        let (tx, r) = channel();
        std::thread::spawn(move || {
            let stdin = std::io::stdin();
            loop {
                let mut s = String::new();
                match stdin.read_line(&mut s) {
                    Ok(0) | Err(_) => {
                        let _ = tx.send(None);
                        break;
                    }
                    Ok(_) => {
                        if tx.send(Some(s.trim_end_matches(['\n', '\r']).to_string())).is_err() {
                            break;
                        }
                    }
                }
            }
        });
        *rx = Some(r);
    }
}

/// (poll-lines) -> vector of the lines received since the last call; the
/// keyword :eof appears once stdin closed.
#[export(ns = "desargues.native.stdin", name = "poll-lines", variadic_min = 0)]
pub fn poll_lines(_a: &[Value]) -> Result<Value, String> {
    ensure_reader();
    let rx = RX.lock().unwrap();
    let mut out: Vec<Value> = Vec::new();
    if let Some(r) = rx.as_ref() {
        while let Ok(m) = r.try_recv() {
            out.push(match m {
                Some(s) => s.into_value(),
                None => Value::keyword(cljrs_value::Keyword::simple("eof")),
            });
        }
    }
    Ok(out.into_value())
}
