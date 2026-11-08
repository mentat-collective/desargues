// A minimal stand-in for fosfora's trama preamble, used ONLY to validate
// emitted trama sources with naga in desargues' tests. It declares what an
// emitted file may use (u.resolution, u.time, param(i)) and a fullscreen
// vertex stage. It is not fosfora's preamble; fosfora supplies its own.
struct PhosphorUniforms {
  time: f32,
  delta_time: f32,
  resolution: vec2f,
  params: array<vec4f, 4>,
}

@group(0) @binding(0) var<uniform> u: PhosphorUniforms;

fn param(i: u32) -> f32 {
  return u.params[i / 4u][i % 4u];
}

@vertex
fn vs_main(@builtin(vertex_index) vi: u32) -> @builtin(position) vec4f {
  let x = f32(i32(vi & 1u) * 4 - 1);
  let y = f32(i32(vi & 2u) * 2 - 1);
  return vec4f(x, y, 0.0, 1.0);
}

