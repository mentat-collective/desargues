(ns desargues.layout.realize
  "Manim BACKEND for the pure layout algebra in `desargues.layout.core`.

   This is the ONLY layout namespace that touches libpython/manim. It provides
   the injected `measure-fn` (pass 1), the pass-3 walk that turns a resolved
   layout tree into placed manim mobjects, and the `LayoutMobject` render bridge.

   ## Backend seam (SOLID)
   Everything manim-specific is cohesive and marked below with `;; >>> BACKEND`.
   To port to another renderer, reimplement exactly these pieces behind an
   `IBackend` protocol (measure / make-rect / realize-leaf / coord-convert /
   fade-in): the pure core need not change.

   ## Coordinate convention (the classic bug)
   Pure math is top-left origin, y-DOWN, layout units. We convert to manim
   (centered, y-UP) ONLY here, reading fw/fh LIVE from manim's config:
     mx = -fw/2 + margin + bx ;  my = fh/2 - margin - by."
  (:require [desargues.layout.core :as core]
            [desargues.manim.core :as mc]
            [desargues.manim.mobjects :as mob]
            [desargues.dsl.renderer :as rndr]
            [desargues.infrastructure.manim-adapter :as adapter]
            [desargues.domain.protocols :as p]
            [libpython-clj2.python :as py]
            [clojure.string :as str]
            [desargues.manim.mobject-cache :as mcache]
            [desargues.layout.mobject-key :as mk]))

(declare walk-realize!)

;; ============================================================================
;; >>> BACKEND: live frame + coordinate conversion
;; ============================================================================

(defn read-frame
  "Read manim's live frame [frame-width frame-height] (handles non-16:9 frames)."
  []
  (let [manim (mc/manim)
        cfg   (py/get-attr manim "config")]
    [(double (py/->jvm (py/get-attr cfg "frame_width")))
     (double (py/->jvm (py/get-attr cfg "frame_height")))]))

(defn- ->manim-center
  "Convert a layout ::box (top-left origin, y-DOWN) to a manim centered point."
  [box fw fh margin]
  (let [bx (+ (:x box) (/ (:w box) 2))
        by (+ (:y box) (/ (:h box) 2))]
    [(+ (- (/ fw 2)) margin bx)
     (- (/ fh 2) margin by)
     0]))

;; ============================================================================
;; Inherited style channel. The pure resolution (path -> effective style) now
;; lives in desargues.layout.core/collect-styles; here we only translate the
;; resolved style keywords into the manim-specific slant/weight constants.
;; ============================================================================

(defn- ->slant [s]
  (cond (nil? s) nil (string? s) s :else (str/upper-case (name s))))

(defn- ->weight [w]
  (cond (nil? w) nil (string? w) w :else (str/upper-case (name w))))

;; ============================================================================
;; >>> BACKEND: leaf mobject creation (used by measure AND reused in pass 3)
;; ============================================================================

(defn- kwseq [m] (mapcat identity m))

(defn- create-leaf-mob
  "Create the manim mobject for a leaf at its natural font-size, honoring the
   inherited style. Returns the raw py mobject."
  [node style]
  (let [{:keys [font-size color weight slant font]} style
        content (:content node)]
    (case (:type node)
      :text
      (apply mob/text content
             (kwseq (cond-> {}
                      font-size (assoc :font_size font-size)
                      color     (assoc :color (rndr/resolve-color color))
                      weight    (assoc :weight (->weight weight))
                      slant     (assoc :slant (->slant slant))
                      font      (assoc :font font))))
      :math
      (apply mob/math-tex content
             (kwseq (cond-> {}
                      font-size (assoc :font_size font-size)
                      color     (assoc :color (rndr/resolve-color color)))))
      :image
      (if (str/ends-with? (str content) ".svg")
        (mob/svg-mobject content)
        (mob/image-mobject content)))))

(defn- build-leaf
  "A leaf's natural-size build as {:mobject m :meta {:w :h}}."
  [node style]
  (let [m (create-leaf-mob node style)]
    {:mobject m
     :meta {:w (double (py/->jvm (py/get-attr m "width")))
            :h (double (py/->jvm (py/get-attr m "height")))}}))

(defn build-stamp
  "The Manim settings a mobject build reads beyond its description (pixel
   size, renderer, text and TeX settings), as a value for cache keys."
  []
  (let [cfg (py/get-attr (mc/manim) "config")
        g   (fn [k] (str (py/get-item cfg k)))]
    (mapv g ["pixel_width" "pixel_height" "renderer" "text_dir" "tex_dir"
             "tex_template" "tex_template_file"])))

(defn make-measure
  "Build the injected measure-fn : [node path] -> [w h]. Creates each leaf
   mobject ONCE and caches it by tree PATH (so duplicate identical child maps —
   e.g. five identical coins — each get their own mobject). With an active
   `mcache` (IMobjectCache) a leaf whose natural-size build is kept under its
   key (desargues.layout.mobject-key/leaf-key, `stamp` the backend settings)
   is measured from the kept size alone; pass 3 fetches its mobject only if
   it needs one."
  ([styles cache] (make-measure styles cache (mcache/null-cache) nil))
  ([styles cache mcache stamp]
   (fn [node path]
     (if-let [c (get @cache path)]
       [(:w c) (:h c)]
       (let [style (get styles path {})
             k     (when (mcache/active? mcache) (mk/leaf-key node style stamp))]
         (if-let [{:keys [w h]} (some->> k (mcache/kept-meta mcache))]
           (do (swap! cache assoc path {:w w :h h :key k :style style}) [w h])
           (let [{m :mobject {:keys [w h]} :meta}
                 (if k
                   (mcache/fetch! mcache k #(build-leaf node style))
                   (build-leaf node style))]
             (swap! cache assoc path {:mob m :w w :h h :key k :style style})
             [w h])))))))

;; ============================================================================
;; >>> BACKEND: bg / border rectangle
;; ============================================================================

(defn- make-rect
  "A bg/border rectangle stretched to the node's exact ::box w,h."
  [node box]
  (let [attrs (:attrs node)
        {:keys [w h]} box
        cr   (:corner-radius attrs)
        ;; constructing with :width/:height yields exact w,h (no aspect lock)
        rect (if cr
               (mob/rounded-rectangle :corner_radius cr :width w :height h)
               (mob/rectangle :width w :height h))]
    ;; fill
    (if-let [bg (:bg attrs)]
      (mob/set-fill rect (rndr/resolve-color bg) :opacity (or (:bg-opacity attrs) 1.0))
      (mob/set-fill rect (rndr/resolve-color :black) :opacity 0.0))
    ;; border / stroke
    (let [border (:border attrs)]
      (cond
        (vector? border) (let [[c bw] border]
                           (mob/set-stroke rect :color (rndr/resolve-color c)
                                           :width (or bw (:border-width attrs) 2)))
        border           (mob/set-stroke rect :color (rndr/resolve-color border)
                                         :width (or (:border-width attrs) 2))
        :else            (mob/set-stroke rect :width 0 :opacity 0)))
    rect))

;; ============================================================================
;; >>> BACKEND: pass-3 walk (resolved layout tree -> nested VGroups)
;; ============================================================================

(defn- fit-scale [fit w h mw mh]
  (case fit
    :fill (max (/ w mw) (/ h mh))
    (min (/ w mw) (/ h mh))))              ; :contain (default)

(defn- realize-leaf!
  "Fit a cached leaf mobject into its ::box and move it into place. A leaf
   measured from a kept size only (no :mob) is fetched from `mcache` first."
  [node path box cache fw fh margin mcache]
  (let [c   (get @cache path)
        m   (or (:mob c)
                (:mobject (mcache/fetch! mcache (:key c)
                                         #(build-leaf node (:style c)))))
        fit (or (:fit (:attrs node)) :contain)
        mw  (double (py/->jvm (py/get-attr m "width")))
        mh  (double (py/->jvm (py/get-attr m "height")))
        {:keys [w h]} box]
    (case fit
      :stretch (do (py/call-attr m "stretch_to_fit_width"  w)
                   (py/call-attr m "stretch_to_fit_height" h))
      :none    nil
      (let [s (fit-scale fit w h mw mh)]
        (when (and (pos? s) (not (Double/isNaN s)) (not (Double/isInfinite s)) (not= s 1.0))
          (mob/scale m s))))
    (mob/move-to m (->manim-center box fw fh margin))
    m))

(defn- walk-node!
  "Build one node's VGroup: bg/border rect first (behind), then the leaf
   mobject or child VGroups. Registers any node carrying {:id k} into `reg`."
  [node path {:keys [fw fh margin cache reg mcache] :as ctx}]
  (let [attrs (:attrs node)
        box   (get node core/box-key)
        vg    (mob/vgroup)
        add   (fn [m] (when m (py/call-attr vg "add" m)))]
    ;; bg/border spans the FULL box, added first (behind content)
    (when (or (:bg attrs) (:border attrs))
      (add (mob/move-to (make-rect node box) (->manim-center box fw fh margin))))
    (case (:type node)
      (:text :math :image) (add (realize-leaf! node path box cache fw fh margin mcache))
      :spacer              nil
      (doseq [[i c] (map-indexed vector (:children node))]
        (add (walk-realize! c (conj path i) ctx))))
    (when-let [id (:id attrs)]
      (swap! reg assoc id vg))
    vg))

(defn- walk-realize!
  "Build a VGroup mirroring the subtree (walk-node!). With an active `mcache`
   a non-root subtree whose key (desargues.layout.mobject-key/subtree-key) is
   kept is a copy of the kept build instead. The root is never kept: it
   changes on every edit, and keeping it would copy the whole scene."
  [node path {:keys [fw fh margin styles stamp mcache] :as ctx}]
  (if-let [k (when (and (seq path) (mcache/active? mcache))
               (mk/subtree-key node (get styles path {}) [fw fh margin] stamp))]
    (:mobject (mcache/fetch! mcache k (fn [] {:mobject (walk-node! node path ctx)})))
    (walk-node! node path ctx)))

;; ============================================================================
;; Render bridge — LayoutMobject (double-wrap so render-scene-impl's
;; `(:py-obj (p/to-mobject obj))` resolves to the vgroup, not nil)
;; ============================================================================

(defrecord LayoutMobject [vgroup inner registry tree]
  p/IRenderable
  (render     [_ _] inner)
  (to-mobject [_]   inner)                        ; -> ManimMobject record
  p/IAnimatable
  (create-animation [_ t o] (p/create-animation inner t o))
  (animate-creation [_]                           ; whole-scene fade-in
    (adapter/->ManimAnimation
     (py/call-attr-kw (mc/get-class "FadeIn") "__call__" [vgroup] {}) :FadeIn {}))
  (animate-transformation [_ target] (p/animate-transformation inner target)))

(defn realize
  "Realize a pure layout `tree` into a LayoutMobject placed & bounded in the frame.
   opts: {:margin 0.5 :frame [fw fh] :mobject-cache c}. `c` is an
   IMobjectCache (desargues.manim.mobject-cache) reused across calls: leaf
   builds and placed subtrees whose description did not change are copies of
   kept builds. Default: a NullCache, every mobject built anew. Requires
   python/manim initialized."
  ([tree] (realize tree {}))
  ([tree {:keys [margin frame mobject-cache] :or {margin 0.5}}]
   (mc/manim)                                      ; ensure manim is live
   (let [[fw fh] (or frame (read-frame))
         mcache  (or mobject-cache (mcache/null-cache))
         stamp   (when (mcache/active? mcache) (build-stamp))
         root    {:x 0 :y 0 :w (- fw (* 2 margin)) :h (- fh (* 2 margin))}
         cache   (atom {})                          ; path -> {:mob :w :h :key}
         styles  (core/collect-styles tree)
         measure (make-measure styles cache mcache stamp)
         placed  (core/resolve-layout tree root measure)   ; PURE pass 1+2
         reg     (atom {})
         vg      (walk-realize! placed [] {:fw fw :fh fh :margin margin :cache cache
                                           :reg reg :styles styles :stamp stamp
                                           :mcache mcache})]  ; pass 3
     (->LayoutMobject vg (adapter/->ManimMobject vg :VGroup {}) @reg placed))))

(defn by-id
  "The raw py mobject (a VGroup) registered under {:id id}, for targeted anims."
  [realized id]
  (get (:registry realized) id))
