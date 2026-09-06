(ns desargues.scene.protocols
  "Backend-neutral scene/animation PROTOCOLS — the DIP seam.

   These interfaces describe *what* a presentation needs (make a dot, glide it
   somewhere, play it on a stage, render the whole thing) without committing to
   *how* it is realized. `desargues.scene.manim` implements them over Manim
   today; a future raster/WebGL browser backend can implement the SAME
   protocols, and every consumer keeps working unchanged (OCP).

   Design notes:
   - Colors and directions are SEMANTIC KEYWORDS (:gold, :teal, :right). Each
     backend resolves them its own way — consumers never touch a Manim constant
     or a Python object.
   - Mobjects, animations and the `stage` are OPAQUE handles produced and
     consumed only through these protocols. Consumers never introspect them.
   - Every method is prefixed `-` so the clean names (text, dot, play!, render!)
     belong to the `desargues.scene` facade, and no name shadows clojure.core.

   Interfaces are segregated (ISP) so a partial backend (e.g. a headless mock
   that only needs IStudio + IStage) can implement a subset."
  (:refer-clojure :exclude []))

(defprotocol IStudio
  "Lifecycle + global render configuration."
  (-init! [backend]
    "Bring the backend up (idempotent). Returns the backend.")
  (-configure! [backend opts]
    "Apply global render settings. opts: {:width :height :fps :quality}."))

(defprotocol IMobjects
  "Construct and position visual primitives. `opts` are idiomatic kebab-case
   Clojure keywords; color-valued opts take semantic color keywords."
  (-text [backend s opts]              "A single line of text.")
  (-circle [backend opts]              "A circle outline.")
  (-dot [backend opts]                 "A small filled dot.")
  (-rectangle [backend opts]           "A rectangle.")
  (-rounded-rectangle [backend opts]   "A rounded rectangle.")
  (-decimal [backend value opts]       "A live decimal-number readout.")
  (-line [backend from to opts]        "A straight segment between two points (:color :width).")
  (-place-at [backend obj point]       "Move obj's center to point [x y (z)]. Returns obj.")
  (-place-next-to [backend obj ref direction opts]
    "Position obj beside ref along a direction keyword. Returns obj.")
  (-fill! [backend obj color opts]     "Set fill color/opacity. Returns obj.")
  (-stroke! [backend obj opts]         "Set stroke :color/:width/:opacity. Returns obj."))

(defprotocol IAnimations
  "Build animations (opaque handles) from mobjects. Nothing plays until an
   animation is handed to IStage/-play!."
  (-appear [backend obj opts]          "Fade a mobject in.")
  (-vanish [backend obj opts]          "Fade a mobject out.")
  (-draw [backend obj opts]            "Draw/create a mobject stroke-first.")
  (-morph [backend obj target opts]    "Transform obj into target.")
  (-emphasize [backend obj opts]       "Flash/indicate a mobject (:color opt).")
  (-recolor [backend obj color opts]   "Animate a mobject to a new color.")
  (-count-to [backend decimal value opts] "Animate a decimal readout to value.")
  (-glide [backend obj point opts]
    "Animate obj travelling to point. Encapsulates the target idiom so the
     consumer never sees generate-target/move-to-target.")
  (-connect [backend line from to opts]
    "Animate a line's endpoints to from/to (a rod following its bob).")
  (-together [backend anims opts]      "Play a seq of anims as one group (:lag-ratio).")
  (-stagger [backend anims opts]       "Play a seq of anims staggered (:lag-ratio)."))

(defprotocol IStage
  "The stage: run animations on a scene, and render whole scenes."
  (-play! [backend stage anims]        "Play one or more animations on the stage.")
  (-hold! [backend stage seconds]      "Hold the current frame for `seconds`.")
  (-render! [backend scene-name construct opts]
    "Build a named scene whose construct-fn receives the stage, and render it
     to <scene-name>.mp4. `construct` calls -play!/-hold! on its stage arg.")
  (-render-layout! [backend scene-name tree opts]
    "Realize a declarative layout tree and render it as <scene-name>.mp4 with a
     whole-scene reveal. opts: {:hold seconds}."))
