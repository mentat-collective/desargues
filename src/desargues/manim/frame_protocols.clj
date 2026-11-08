(ns desargues.manim.frame-protocols
  "The frame-sink port: where a rendered frame goes. Pure, no Python.

   A sink receives every frame the renderer produces, in order. `rgba` is a
   dtype-next tensor of shape [h w 4] (:uint8). Unless the hook was installed
   with :copy? true it is a ZERO-COPY view of the renderer's numpy buffer and is
   only valid for the duration of the call: a sink that keeps a frame must
   clone it (`tech.v3.datatype/clone`). `n` is the frame index since the hook
   was installed or reset (0-based); a held frame (a wait) arrives once per index.")

(defprotocol IFrameSink
  (-frame! [sink rgba w h n]
    "Receive frame `n` (0-based) of width `w`, height `h`. Return value ignored."))
