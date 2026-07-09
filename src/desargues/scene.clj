(ns desargues.scene
  "Backend-neutral animation FACADE — the single DIP seam a presentation depends
   on.

   A consumer (e.g. an econ money-creation animation) requires ONLY this
   namespace plus its own domain code. It never sees Manim, libpython, a Python
   object, or a rendering backend. It speaks in semantic terms — make a `dot`,
   `glide` it to a point, `play!` it on a stage, `render!` the scene — and colors
   are keywords (:gold, :teal). Which backend realizes those calls (Manim today,
   a browser/raster backend tomorrow) is decided HERE, behind the facade, so
   swapping it changes nothing for any consumer (Dependency Inversion + OCP).

   Backend selection:
     - default: the Manim backend, lazily required on first use (so merely
       loading this facade pulls in NO Python).
     - override globally: (set-backend! my-backend)
     - override dynamically: (with-backend my-backend (render! ...))"
  (:require [desargues.scene.protocols :as p]))

;; ============================================================================
;; Backend selection (the inversion point)
;; ============================================================================

(def ^:dynamic *backend*
  "The active scene backend, or nil to lazily fall back to the Manim backend."
  nil)

(defn- lazy-default
  "Require + construct the Manim backend on demand. Kept out of this ns's
   :require so the facade itself stays Python-free."
  []
  (require 'desargues.scene.manim)
  ((resolve 'desargues.scene.manim/manim-backend)))

(defonce ^:private default-backend* (delay (lazy-default)))

(defn backend
  "The backend in effect: the dynamic binding if set, else the default Manim
   backend (built once, lazily)."
  []
  (or *backend* @default-backend*))

(defn set-backend!
  "Install `b` as the global default backend."
  [b]
  (alter-var-root #'*backend* (constantly b)))

(defmacro with-backend
  "Evaluate body with `b` as the active backend."
  [b & body]
  `(binding [*backend* ~b] ~@body))

;; ============================================================================
;; Lifecycle
;; ============================================================================

(defn init!
  "Bring the backend up (idempotent)."
  []
  (p/-init! (backend)))

(defn configure!
  "Apply global render settings. opts: {:width :height :fps :quality}."
  [opts]
  (p/-configure! (backend) opts))

;; ============================================================================
;; Mobjects — construct + position + style. Colors are semantic keywords.
;; ============================================================================

(defn text     [s & {:as opts}]     (p/-text (backend) s opts))
(defn circle   [& {:as opts}]       (p/-circle (backend) opts))
(defn dot      [& {:as opts}]       (p/-dot (backend) opts))
(defn rectangle [& {:as opts}]      (p/-rectangle (backend) opts))
(defn rounded-rectangle [& {:as opts}] (p/-rounded-rectangle (backend) opts))
(defn decimal  [value & {:as opts}] (p/-decimal (backend) value opts))

(defn move-to
  "Move obj's center to point [x y] or [x y z]. Returns obj."
  [obj point]
  (p/-place-at (backend) obj point))

(defn next-to
  "Position obj beside ref along a direction keyword (:right :up ...). Returns obj."
  [obj ref direction & {:as opts}]
  (p/-place-next-to (backend) obj ref direction opts))

(defn fill!
  "Set obj's fill color (keyword) and opts (:opacity). Returns obj."
  [obj color & {:as opts}]
  (p/-fill! (backend) obj color opts))

(defn stroke!
  "Set obj's stroke (:color keyword, :width, :opacity). Returns obj."
  [obj & {:as opts}]
  (p/-stroke! (backend) obj opts))

;; ============================================================================
;; Animations — build opaque animation handles; nothing plays until play!.
;; ============================================================================

(defn appear    [obj & {:as opts}]        (p/-appear (backend) obj opts))
(defn vanish    [obj & {:as opts}]        (p/-vanish (backend) obj opts))
(defn draw      [obj & {:as opts}]        (p/-draw (backend) obj opts))
(defn morph     [obj target & {:as opts}] (p/-morph (backend) obj target opts))
(defn emphasize [obj & {:as opts}]        (p/-emphasize (backend) obj opts))
(defn recolor   [obj color & {:as opts}]  (p/-recolor (backend) obj color opts))
(defn count-to  [decimal value & {:as opts}] (p/-count-to (backend) decimal value opts))

(defn glide
  "Animate obj travelling to point [x y (z)]. Hides the target idiom."
  [obj point & {:as opts}]
  (p/-glide (backend) obj point opts))

(defn together
  "Play a seq of animations as one group. opts: {:lag-ratio n}."
  [anims & {:as opts}]
  (p/-together (backend) anims opts))

(defn stagger
  "Play a seq of animations staggered. opts: {:lag-ratio n}."
  [anims & {:as opts}]
  (p/-stagger (backend) anims opts))

;; ============================================================================
;; Stage — play animations on a scene, render whole scenes.
;; ============================================================================

(defn play!
  "Play one or more animations on the stage."
  [stage & anims]
  (p/-play! (backend) stage (vec anims)))

(defn hold!
  "Hold the current frame for `seconds` (default 1)."
  ([stage] (hold! stage 1))
  ([stage seconds] (p/-hold! (backend) stage seconds)))

(defn render!
  "Build and render a named imperative scene. `construct` is a 1-arg fn that
   receives the stage and drives it with play!/hold!. Writes <scene-name>.mp4.
   opts: {:output-file name}."
  [scene-name construct & {:as opts}]
  (p/-render! (backend) scene-name construct opts))

(defn render-layout!
  "Realize a declarative layout tree (see desargues.api / desargues.layout) and
   render it as <scene-name>.mp4 with a whole-scene reveal. opts: {:hold secs}."
  [scene-name tree & {:as opts}]
  (p/-render-layout! (backend) scene-name tree opts))
