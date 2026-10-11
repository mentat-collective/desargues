//! Damped-pendulum RK4 kernel for cljw's core-module FFI (wasm/load + wasm/call).
//! Same model and stepping as desargues.videos.physics (Pendulum + RK4Integrator):
//!   theta' = omega,  omega' = -(g/L) sin(theta) - mu*omega
//! Samples (theta, omega) every `every` steps into a guest-owned f64 buffer.
//!
//! Build:
//!   zig build-exe pendulum.zig -target wasm32-freestanding -O ReleaseFast \
//!     -fno-entry -rdynamic -fstrip -femit-bin=pendulum.wasm

const cap = 4096; // samples; two f64 each
var buf: [2 * cap]f64 = undefined;

export fn buf_addr() u32 {
    return @intCast(@intFromPtr(&buf));
}

export fn buf_cap() u32 {
    return cap;
}

inline fn accel(theta: f64, omega: f64) f64 {
    return -(g_over_l * @sin(theta)) - mu * omega;
}

// cljw's JIT dispatches exports of at most 5 parameters, so the model
// constants are set once and the run takes the rest.
var g_over_l: f64 = 9.8;
var mu: f64 = 0.0;
var dt: f64 = 0.01;

export fn set_params(g_over_l_: f64, mu_: f64, dt_: f64) u32 {
    g_over_l = g_over_l_;
    mu = mu_;
    dt = dt_;
    return 0; // void exports taking f64 have no JIT call shape either
}

/// Integrate n_steps of size dt from (theta0, omega0); write the initial state
/// and every `every`-th state to buf. Returns the number of samples written.
export fn pendulum_rk4(theta0: f64, omega0: f64, n_steps: u32, every: u32) u32 {
    var th = theta0;
    var om = omega0;
    var n: u32 = 0;
    buf[0] = th;
    buf[1] = om;
    n = 1;
    var i: u32 = 1;
    while (i <= n_steps) : (i += 1) {
        const k1t = om;
        const k1o = accel(th, om);
        const k2t = om + k1o * (dt / 2);
        const k2o = accel(th + k1t * (dt / 2), om + k1o * (dt / 2));
        const k3t = om + k2o * (dt / 2);
        const k3o = accel(th + k2t * (dt / 2), om + k2o * (dt / 2));
        const k4t = om + k3o * dt;
        const k4o = accel(th + k3t * dt, om + k3o * dt);
        th = th + (k1t + 2 * k2t + 2 * k3t + k4t) * (dt / 6);
        om = om + (k1o + 2 * k2o + 2 * k3o + k4o) * (dt / 6);
        if (i % every == 0 and n < cap) {
            buf[2 * n] = th;
            buf[2 * n + 1] = om;
            n += 1;
        }
    }
    return n;
}
