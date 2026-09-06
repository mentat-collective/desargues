(ns desargues.scene.manim
  "Manim implementation of the backend-neutral scene protocols
   (`desargues.scene.protocols`).

   This is the ONLY place in the scene stack that knows Manim/libpython exists.
   It resolves semantic color/direction keywords to Manim constants, normalizes
   idiomatic kebab-case option keys to Manim's snake_case, and delegates every
   operation to the proven imperative `desargues.manim.*` bindings.

   A future browser/raster backend implements the SAME protocols with zero
   Python — consumers (e.g. econ presentations) never change."
  (:require [clojure.string :as str]
            [desargues.scene.protocols :as p]
            [desargues.manim.core :as man-core]
            [desargues.manim.mobjects :as man-mob]
            [desargues.manim.animations :as man-anim]
            [desargues.manim-quickstart :as man-quick]
            [desargues.layout.realize :as layout]
            [desargues.domain.protocols :as dom]))

;; ============================================================================
;; Keyword resolution — the semantic layer that keeps Python out of consumers
;; ============================================================================

(def ^:private color-opt-keys
  "Option keys whose VALUE is a color and must be resolved to a Manim constant."
  #{:color :fill-color :fill_color :stroke-color :stroke_color
    :background-color :background_color})

(defn- constant-name
  "kebab/underscore keyword or string -> Manim CONSTANT_NAME string."
  [x]
  (-> (name x) (str/replace "-" "_") (str/upper-case)))

(defn- resolve-const
  "A semantic keyword/string -> the Manim constant; anything else passes through
   (already a Python object, a number, etc.)."
  [v]
  (if (or (keyword? v) (string? v))
    (man-core/get-constant (constant-name v))
    v))

(defn- kebab->snake [k]
  (keyword (str/replace (name k) "-" "_")))

(defn- resolve-opts
  "Normalize an idiomatic opts map: kebab->snake keys, and resolve any
   color-valued option to a Manim constant. nil-safe."
  [opts]
  (persistent!
   (reduce-kv (fn [m k v]
                (assoc! m (kebab->snake k)
                        (if (contains? color-opt-keys k) (resolve-const v) v)))
              (transient {})
              (or opts {}))))

(defn- pad3
  "Manim points are 3-vectors; pad [x y] -> [x y 0.0], keep [x y z]."
  [pt]
  (let [v (vec pt)]
    (case (count v)
      3 v
      2 (conj v 0.0)
      (vec (take 3 (concat v [0.0 0.0 0.0]))))))

(defn- kw-apply
  "Call a variadic keyword fn: (f fixed... k1 v1 k2 v2 ...) from an opts map."
  [f fixed opts]
  (apply f (concat fixed (mapcat identity opts))))

;; ============================================================================
;; ManimBackend — implements every scene protocol by delegating to manim.*
;; ============================================================================

(defrecord ManimBackend []
  p/IStudio
  (-init! [b] (man-quick/init!) b)
  (-configure! [_ {:keys [width height fps quality]}]
    (let [cfg (man-core/get-attr (man-core/manim) "config")]
      (when width   (man-core/set-attr! cfg "pixel_width"  width))
      (when height  (man-core/set-attr! cfg "pixel_height" height))
      (when fps     (man-core/set-attr! cfg "frame_rate"   fps))
      (when quality (man-core/set-attr! cfg "quality"      quality))
      cfg))

  p/IMobjects
  (-text [_ s opts]              (kw-apply man-mob/text              [s]     (resolve-opts opts)))
  (-circle [_ opts]              (kw-apply man-mob/circle            []      (resolve-opts opts)))
  (-dot [_ opts]                 (kw-apply man-mob/dot               []      (resolve-opts opts)))
  (-rectangle [_ opts]           (kw-apply man-mob/rectangle         []      (resolve-opts opts)))
  (-rounded-rectangle [_ opts]   (kw-apply man-mob/rounded-rectangle []      (resolve-opts opts)))
  (-decimal [_ value opts]       (kw-apply man-mob/decimal-number    [value] (resolve-opts opts)))
  (-line [_ from to opts]
    ;; the facade's :width is Manim's stroke_width
    (let [opts (cond-> (dissoc opts :width)
                 (:width opts) (assoc :stroke_width (:width opts)))]
      (kw-apply man-mob/line [(pad3 from) (pad3 to)] (resolve-opts opts))))
  (-place-at [_ obj point]       (man-mob/move-to obj (pad3 point)))
  (-place-next-to [_ obj ref direction opts]
    (kw-apply man-mob/next-to [obj ref (resolve-const direction)] (resolve-opts opts)))
  (-fill! [_ obj color opts]     (kw-apply man-mob/set-fill   [obj (resolve-const color)] (resolve-opts opts)))
  (-stroke! [_ obj opts]         (kw-apply man-mob/set-stroke [obj]                       (resolve-opts opts)))

  p/IAnimations
  (-appear [_ obj opts]          (kw-apply man-anim/fade-in  [obj] (resolve-opts opts)))
  (-vanish [_ obj opts]          (kw-apply man-anim/fade-out [obj] (resolve-opts opts)))
  (-draw [_ obj opts]            (kw-apply man-anim/create   [obj] (resolve-opts opts)))
  (-morph [_ obj target opts]    (kw-apply man-anim/transform [obj target] (resolve-opts opts)))
  (-emphasize [_ obj opts]       (kw-apply man-anim/indicate [obj] (resolve-opts opts)))
  (-recolor [_ obj color opts]   (kw-apply man-anim/fade-to-color [obj (resolve-const color)] (resolve-opts opts)))
  (-count-to [_ decimal value opts]
    (kw-apply man-anim/change-decimal-to-value [decimal value] (resolve-opts opts)))
  (-glide [_ obj point opts]
    ;; collapse the generate-target -> position-target -> move-to-target idiom
    (man-anim/generate-target! obj)
    (man-mob/move-to (man-anim/get-target obj) (pad3 point))
    (kw-apply man-anim/move-to-target [obj] (resolve-opts opts)))
  (-connect [_ line from to opts]
    (kw-apply man-anim/move-endpoints [line (pad3 from) (pad3 to)] (resolve-opts opts)))
  (-together [_ anims opts]
    (apply man-anim/animation-group (concat anims (mapcat identity (resolve-opts opts)))))
  (-stagger [_ anims opts]
    (apply man-anim/lagged-start (concat anims (mapcat identity (resolve-opts opts)))))

  p/IStage
  (-play! [_ stage anims]        (apply man-core/play! stage anims))
  (-hold! [_ stage seconds]      (man-core/wait! stage seconds))
  (-render! [_ scene-name construct opts]
    (let [klass (man-core/create-scene-class scene-name construct)]
      (man-core/render-scene! (klass) {:output-file (or (:output-file opts) scene-name)})))
  (-render-layout! [_ scene-name tree opts]
    ;; Manim's config.output_file is sticky across renders in one process; clear
    ;; it so render-scene!'s rename targets THIS scene's file.
    (let [cfg (man-core/get-attr (man-core/manim) "config")]
      (man-core/set-attr! cfg "output_file" ""))
    (let [construct (fn [self]
                      (let [L    (layout/realize tree)
                            fade (dom/animate-creation L)] ; ManimAnimation wrapping FadeIn(vgroup)
                        (man-core/play! self (:py-obj fade))
                        (man-core/wait! self (or (:hold opts) 3))))
          klass     (man-core/create-scene-class scene-name construct)]
      (man-core/render-scene! (klass) {:output-file scene-name}))))

(defn manim-backend
  "Construct a Manim scene backend. Cheap — no Python is touched until the first
   render/constant lookup, so this is safe to build eagerly."
  []
  (->ManimBackend))
