(ns desargues.browser.canvas
  "BOUNDARY stratum: a browser host for the player (desargues.player).

   Implements every port of desargues.player.protocols over one DOM element:
   ISurface (a Canvas2D), IWindow (the element, PNG snapshots as data URLs),
   IControls (widgets in a side panel), IPointer (pointer events on the
   canvas, hit-tested with desargues.player.hit) and IClock
   (requestAnimationFrame). It is the browser twin of
   studio/desargues/player/javafx.clj: same node kinds, same units (scene
   units through desargues.player.frame/viewport, Manim stroke widths at
   1080p, font sizes per 120 px of scale), so a Frame looks the same in both.

   Painting is one `paint!` method per scene-graph node kind (OCP). :tex goes
   through KaTeX when window.katex is present (an HTML overlay above the
   canvas, since KaTeX typesets to DOM), else plain canvas text."
  (:require [clojure.string :as str]
            [desargues.player.frame :as frame]
            [desargues.player.protocols :as p]
            [goog.object :as gobj]
            [desargues.player.controls :as controls]
            [desargues.browser.input :as input]))

;; ── Colors and units ──────────────────────────────────────────────────────

(defn- hex
  "#RRGGBB of a palette keyword, or the CSS colour string itself."
  [k]
  (if (string? k) k (get frame/palette k (frame/palette :white))))

(defn- parse-hex [s]
  (when (and (string? s) (re-matches #"#[0-9a-fA-F]{6}" s))
    (mapv #(js/parseInt (subs s % (+ % 2)) 16) [1 3 5])))

(defn- rgba
  "A CSS colour for palette keyword / CSS string `k` at `alpha`."
  [k alpha]
  (if-let [[r g b] (parse-hex (hex k))]
    (str "rgba(" r "," g "," b "," alpha ")")
    (hex k)))

(defn- mix
  "CSS colour between `a` and `b` at `t` (0..1), like JavaFX Color.interpolate."
  [a b t]
  (let [ca (parse-hex (hex a)) cb (parse-hex (hex b))]
    (if (and ca cb)
      (let [[r g bl] (mapv #(js/Math.round (+ %1 (* (- %2 %1) t))) ca cb)]
        (str "rgb(" r "," g "," bl ")"))
      (hex (if (< t 0.5) a b)))))

(defn- stroke-color
  "The stroke of shape `s`, or nil for an explicit `:stroke {:color nil}`."
  [s]
  (when-not (and (contains? (:stroke s) :color) (nil? (get-in s [:stroke :color])))
    (let [c (or (get-in s [:stroke :color]) (get-in s [:opts :color]) :white)]
      (if-let [{:keys [to alpha]} (:recolor s)]
        (mix c to alpha)
        (hex c)))))

(defn- fill-color
  "The fill of shape `s`, or nil when it has none."
  [s]
  (when-let [{:keys [color opacity]} (:fill s)]
    (rgba color (or opacity 1.0))))

(defn- units->px [vp u] (* (:scale vp) u))

(defn- stroke-px
  "Manim stroke widths are pixels at 1080p (135 px per scene unit)."
  [vp s]
  (max 1.0 (* (or (get-in s [:stroke :width]) (get-in s [:opts :width]) 4)
              (/ (:scale vp) 135.0))))

(defn- emphasis-scale [s] (+ 1.0 (* 0.25 (:emphasis s 0.0))))

(defn- center [vp s] (frame/->px vp (:at s [0 0])))

(defn- font-px [vp s]
  (* (emphasis-scale s) (get-in s [:opts :font-size] 48) (/ (:scale vp) 120.0)))

(defn- stroke! [ctx vp s]
  (when-let [c (stroke-color s)]
    (set! (.-strokeStyle ctx) c)
    (set! (.-lineWidth ctx) (stroke-px vp s))
    (.stroke ctx)))

;; ── Painting: one method per node kind (OCP) ──────────────────────────────

(defmulti paint!
  "Paint shape `s` on 2D context `ctx` under viewport `vp`. `env` carries the
   host's per-frame extras (the TeX overlay)."
  (fn [_ctx _vp s _env] (:node s)))

(defmethod paint! :default [_ _ _ _] nil)

(defmethod paint! :dot [ctx vp s _]
  (let [[cx cy] (center vp s)
        r (* (units->px vp (get-in s [:opts :radius] 0.08)) (emphasis-scale s))]
    (set! (.-fillStyle ctx) (or (fill-color s) (stroke-color s) "transparent"))
    (.beginPath ctx)
    (.arc ctx cx cy r 0 (* 2 js/Math.PI))
    (.fill ctx)))

(defmethod paint! :circle [ctx vp s _]
  (let [[cx cy] (center vp s)
        r (* (units->px vp (get-in s [:opts :radius] 1.0)) (emphasis-scale s))]
    (when-let [f (fill-color s)]
      (set! (.-fillStyle ctx) f)
      (.beginPath ctx)
      (.arc ctx cx cy r 0 (* 2 js/Math.PI))
      (.fill ctx))
    ;; JavaFX strokeArc from 90 deg, clockwise on screen, by 360 * draw.
    (.beginPath ctx)
    (.arc ctx cx cy r (- (/ js/Math.PI 2)) (+ (- (/ js/Math.PI 2)) (* 2 js/Math.PI (:draw s 1.0))))
    (stroke! ctx vp s)))

(defmethod paint! :line [ctx vp s _]
  (let [[x0 y0] (frame/->px vp (:from s))
        [x1 y1] (frame/->px vp (:to s))
        d (:draw s 1.0)]
    (.beginPath ctx)
    (.moveTo ctx x0 y0)
    (.lineTo ctx (+ x0 (* d (- x1 x0))) (+ y0 (* d (- y1 y0))))
    (stroke! ctx vp s)))

(defmethod paint! :path [ctx vp s _]
  (let [pts     (mapv #(frame/->px vp %) (:points s))
        n       (js/Math.ceil (* (:draw s 1.0) (count pts)))
        closed? (get-in s [:opts :closed?])
        pts     (cond-> (subvec pts 0 (min n (count pts)))
                  closed? (conj (first pts)))]
    (when (> (count pts) 1)
      (.beginPath ctx)
      (let [[x y] (first pts)] (.moveTo ctx x y))
      (doseq [[x y] (rest pts)] (.lineTo ctx x y))
      (when-let [f (and closed? (fill-color s))]
        (set! (.-fillStyle ctx) f)
        (.fill ctx))
      (stroke! ctx vp s))))

(defn- paint-box! [ctx vp s corner]
  (let [[cx cy] (center vp s)
        k (emphasis-scale s)
        w (* k (units->px vp (get-in s [:opts :width] 4.0)))
        h (* k (units->px vp (get-in s [:opts :height] 2.0)))
        r (min (units->px vp corner) (/ w 2) (/ h 2))
        x (- cx (/ w 2)) y (- cy (/ h 2))]
    (.beginPath ctx)
    (.moveTo ctx (+ x r) y)
    (.arcTo ctx (+ x w) y (+ x w) (+ y h) r)
    (.arcTo ctx (+ x w) (+ y h) x (+ y h) r)
    (.arcTo ctx x (+ y h) x y r)
    (.arcTo ctx x y (+ x w) y r)
    (.closePath ctx)
    (when-let [f (fill-color s)]
      (set! (.-fillStyle ctx) f)
      (.fill ctx))
    (stroke! ctx vp s)))

(defmethod paint! :rectangle [ctx vp s _] (paint-box! ctx vp s 0.0))

(defmethod paint! :rounded-rectangle [ctx vp s _]
  (paint-box! ctx vp s (get-in s [:opts :corner-radius] 0.25)))

(def ^:private text-align {"start" "left" "middle" "center" "end" "right"})

(defn- paint-text!
  "Text at the shape's :at. :opts :anchor (SVG text-anchor) picks the
   alignment; a shape carrying :opts :size is SVG text, whose :at is the
   baseline, otherwise the text is centred vertically."
  [ctx vp s text]
  (let [[cx cy] (center vp s)]
    (set! (.-font ctx) (str (font-px vp s) "px sans-serif"))
    (set! (.-textAlign ctx) (text-align (get-in s [:opts :anchor]) "center"))
    (set! (.-textBaseline ctx) (if (get-in s [:opts :size]) "alphabetic" "middle"))
    (set! (.-fillStyle ctx) (or (stroke-color s) "transparent"))
    (.fillText ctx text cx cy)))

(defmethod paint! :text [ctx vp s _] (paint-text! ctx vp s (str (:text s))))

(defmethod paint! :decimal [ctx vp s _]
  (paint-text! ctx vp s (.toFixed (js/Number (:value s 0))
                                  (get-in s [:opts :num-decimal-places] 2))))

(defn- katex [] (gobj/get js/window "katex"))

(defn- tex-html*
  "KaTeX's HTML for `tex`, or nil when it does not parse."
  [tex]
  (try (.renderToString ^js (katex) tex #js {"displayMode" false "throwOnError" true})
       (catch :default _ nil)))

(def ^:private tex-html (memoize tex-html*))

(defmethod paint! :tex [ctx vp s {:keys [overlay]}]
  (let [html (when (and overlay (katex)) (tex-html (str (:tex s))))]
    (if html
      (let [[cx cy] (center vp s)]
        (overlay {:id (:id s) :html html :x cx :y cy :px (font-px vp s)
                  :color (or (stroke-color s) "transparent")
                  :alpha (.-globalAlpha ctx)
                  :start? (= "start" (get-in s [:opts :anchor]))
                  :write (if (get-in s [:opts :write])
                           (max 0 (min 1 (:draw s 1.0)))
                           1)}))
      (paint-text! ctx vp s (str (:tex s))))))

(defn- dash-px
  "Line dashes of shape `s` in pixels, or nil. :dash is a seq of lengths, or
   an SVG stroke-dasharray string (\"0.05,0.03\"), in scene units."
  [vp s]
  (when-let [d (:dash s)]
    (let [xs (if (string? d)
               (keep #(let [n (js/parseFloat %)] (when-not (js/isNaN n) n))
                     (str/split d #"[,\s]+"))
               (filter number? d))]
      (seq (map #(units->px vp %) xs)))))

(defmethod paint! :svg-path [ctx vp s _]
  ;; :d is in scene units with y up; map it the way frame/->px does.
  (let [{:keys [w h scale]} vp
        path (js/Path2D. (str (:d s)))]
    (.save ctx)
    (.translate ctx (/ w 2.0) (/ h 2.0))
    (.scale ctx scale (- scale))
    (when-let [f (fill-color s)]
      (set! (.-fillStyle ctx) f)
      (.fill ctx path))
    (when-let [c (stroke-color s)]
      (set! (.-strokeStyle ctx) c)
      (set! (.-lineWidth ctx) (/ (stroke-px vp s) scale))
      (when-let [d (dash-px vp s)]
        (.setLineDash ctx (clj->js (map #(/ % scale) d))))
      (.stroke ctx path))
    (.restore ctx)))

;; ── TeX overlay (KaTeX typesets to DOM, not to a canvas) ──────────────────

(defn- overlay-fn
  "An overlay callback writing into `layer`, recording ids it touched in `seen`."
  [layer cache seen]
  (fn [{:keys [id html x y px color alpha start? write]}]
    (vswap! seen conj id)
    (let [el (or (get @cache id)
                 (let [el (.createElement js/document "div")]
                   (set! (.. el -style -position) "absolute")
                   (set! (.. el -style -transform) "translate(-50%,-50%)")
                   (set! (.. el -style -whiteSpace) "nowrap")
                   (set! (.. el -style -pointerEvents) "none")
                   (.appendChild layer el)
                   (swap! cache assoc id el)
                   el))]
      (when-not (= html (gobj/get el "dsgHtml"))
        (set! (.-innerHTML el) html)
        (gobj/set el "dsgHtml" html))
      (set! (.. el -style -display) "block")
      (set! (.. el -style -left) (str x "px"))
      (set! (.. el -style -top) (str y "px"))
      (set! (.. el -style -fontSize) (str px "px"))
      (set! (.. el -style -color) color)
      (set! (.. el -style -opacity) (str alpha))
      ;; :anchor "start" pins the left edge; :write clips left to right
      (set! (.. el -style -transform) (if start? "translate(0,-50%)" "translate(-50%,-50%)"))
      (set! (.. el -style -clipPath)
            (if (and write (< write 1)) (str "inset(-20% " (* 100 (- 1 write)) "% -20% 0)") "none")))))

;; ── Frames ────────────────────────────────────────────────────────────────

(defn paint-frame!
  "Paint Frame `f` on `canvas` (CSS size `w` x `h`, backing store scaled by
   `dpr`): the frame's :background (default black), then each visible shape
   in draw order. `env` is passed to every paint! method."
  [canvas w h dpr f env]
  (let [ctx (.getContext canvas "2d")
        vp  (frame/viewport w h)]
    (.setTransform ctx dpr 0 0 dpr 0 0)
    (set! (.-globalAlpha ctx) 1.0)
    (.setLineDash ctx #js [])
    (set! (.-fillStyle ctx) (if-let [b (:background f)] (hex b) "#000000"))
    (.fillRect ctx 0 0 w h)
    (set! (.-lineCap ctx) "round")
    (set! (.-lineJoin ctx) "round")
    (doseq [s (frame/visible-shapes f)]
      (.save ctx)
      (set! (.-globalAlpha ctx) (:opacity s))
      (when-let [d (dash-px vp s)]
        (.setLineDash ctx (clj->js d)))
      (paint! ctx vp s env)
      (.restore ctx))))

(defn painted-pixels
  "How many pixels of `canvas` differ from its top-left (background) pixel:
   the host's own evidence that a frame painted something."
  [canvas]
  (let [w (.-width canvas) h (.-height canvas)
        data (.-data (.getImageData (.getContext canvas "2d") 0 0 w h))
        n (.-length data)]
    (loop [i 0 acc 0]
      (if (< i n)
        (recur (+ i 4)
               (if (or (not= (aget data i) (aget data 0))
                       (not= (aget data (+ i 1)) (aget data 1))
                       (not= (aget data (+ i 2)) (aget data 2)))
                 (inc acc) acc))
        acc))))

;; ── The host ──────────────────────────────────────────────────────────────

(defn- el [tag style]
  (let [e (.createElement js/document tag)]
    (set! (.. e -style -cssText) style)
    e))

(defn- fmt3 [v] (.toFixed (js/Number v) 3))

;; ── Widgets: one method per control kind (OCP) ─────────────────────────

(defmulti widget!
  "Build the DOM elements for control `spec` (desargues.player.controls,
   already normalised by controls/widget-spec). Returns {:nodes [Element ...]
   :set! (fn [shown-value])}. Setting a DOM value programmatically fires no
   input/change event, so :set! never calls on-change."
  (fn [spec _on-change] (controls/kind spec)))

(defmethod widget! :default [spec on-change]
  (widget! (assoc spec :kind :text) on-change))

(defmethod widget! :slider [{:keys [id min max value] :as spec} on-change]
  (let [text (controls/label spec)
        lbl  (el "label" "display:block;")
        sl   (el "input" "width:100%;")
        show! #(set! (.-textContent lbl) (str text "  " (fmt3 %)))]
    (set! (.-type sl) "range")
    (set! (.-min sl) min)
    (set! (.-max sl) max)
    (set! (.-step sl) (/ (- max min) 1000))
    (set! (.-value sl) value)
    (show! value)
    (.setAttribute sl "data-control" (name id))
    (.addEventListener sl "input"
                       (fn [_]
                         (let [v (js/parseFloat (.-value sl))]
                           (show! v)
                           (on-change id v))))
    {:nodes [lbl sl] :set! (fn [v] (set! (.-value sl) v) (show! v))}))

(defmethod widget! :toggle [{:keys [id value] :as spec} on-change]
  (let [lbl (el "label" "display:flex;gap:6px;align-items:center;")
        cb  (el "input" "")]
    (set! (.-type cb) "checkbox")
    (set! (.-checked cb) (boolean value))
    (.setAttribute cb "data-control" (name id))
    (.appendChild lbl cb)
    (.appendChild lbl (.createTextNode js/document (controls/label spec)))
    (.addEventListener cb "change" (fn [_] (on-change id (boolean (.-checked cb)))))
    {:nodes [lbl] :set! (fn [v] (set! (.-checked cb) (boolean v)))}))

(defmethod widget! :select [{:keys [id value options] :as spec} on-change]
  ;; <option value> is the option's index: the values themselves (keywords,
  ;; numbers) live in the spec, not in the DOM.
  (let [lbl (el "label" "display:block;")
        sel (el "select" "width:100%;")
        select! (fn [v] (when-let [i (controls/option-index spec v)]
                          (set! (.-value sel) (str i))))]
    (set! (.-textContent lbl) (controls/label spec))
    (doseq [[i {:keys [label]}] (map-indexed vector options)]
      (let [o (el "option" "")]
        (set! (.-value o) (str i))
        (set! (.-textContent o) label)
        (.appendChild sel o)))
    (select! value)
    (.setAttribute sel "data-control" (name id))
    (.addEventListener sel "change"
                       (fn [_] (on-change id (:value (nth options (js/parseInt (.-value sel)))))))
    {:nodes [lbl sel] :set! select!}))

(defmethod widget! :text [{:keys [id value] :as spec} on-change]
  ;; Enter or leaving the field submits; the pure parser decides.
  (let [lbl (el "label" "display:block;")
        tf  (el "input" "width:100%;box-sizing:border-box;")]
    (set! (.-textContent lbl) (controls/label spec))
    (set! (.-type tf) "text")
    (set! (.-value tf) (str (or value "")))
    (.setAttribute tf "data-control" (name id))
    (.addEventListener tf "change" (fn [_] (on-change id (.-value tf))))
    {:nodes [lbl tf] :set! (fn [v] (set! (.-value tf) (str v)))}))

(defmethod widget! :button [{:keys [id] :as spec} on-change]
  (let [b (el "button" "width:100%;")]
    (set! (.-textContent b) (controls/label spec))
    (.setAttribute b "data-control" (name id))
    (.addEventListener b "click" (fn [_] (on-change id nil)))
    {:nodes [b] :set! (fn [_] nil)}))

(defrecord CanvasHost [mount state]
  p/ISurface
  (-present! [this f]
    (let [{:keys [canvas w h dpr layer tex-cache]} @state]
      (when canvas
        (let [seen (volatile! #{})]
          (paint-frame! canvas w h dpr f {:overlay (overlay-fn layer tex-cache seen)})
          ;; hide TeX labels no visible shape painted this frame
          (doseq [[id e] @tex-cache]
            (when-not (contains? @seen id)
              (set! (.. e -style -display) "none")))
          (swap! state assoc :last-frame f))))
    this)
  (-size [_] [(:w @state) (:h @state)])

  p/IWindow
  (-show! [this {:keys [title w h]}]
    (let [dpr      (or (.-devicePixelRatio js/window) 1)
          root     (el "div" "display:flex;background:#000;font-family:sans-serif;")
          stage    (el "div" (str "position:relative;width:" w "px;height:" h "px;overflow:hidden;"))
          canvas   (el "canvas" (str "width:" w "px;height:" h "px;display:block;"))
          layer    (el "div" "position:absolute;inset:0;pointer-events:none;")
          controls (el "div" "width:260px;min-width:220px;padding:14px;background:#1b1b1f;color:#d8d8d8;font-size:13px;display:flex;flex-direction:column;gap:6px;")]
      (set! (.-width canvas) (* w dpr))
      (set! (.-height canvas) (* h dpr))
      (.setAttribute canvas "data-desargues" "stage")
      (when title (set! (.-title js/document) (str title)))
      (.appendChild stage canvas)
      (.appendChild stage layer)
      (.appendChild root stage)
      (.appendChild root controls)
      (set! (.-innerHTML mount) "")
      (.appendChild mount root)
      (swap! state assoc :root root :canvas canvas :layer layer :controls controls
             :w w :h h :dpr dpr :tex-cache (atom {})))
    this)
  (-close! [this]
    (input/unwire! state)
    (some-> (:root @state) .remove)
    (swap! state dissoc :root :canvas :layer :controls)
    this)
  (-snapshot! [_ path]
    ;; The browser cannot write a path: offer the PNG as a download named
    ;; `path` and return its data URL.
    (when-let [c (:canvas @state)]
      (let [url (.toDataURL c "image/png")
            a   (.createElement js/document "a")]
        (set! (.-href a) url)
        (set! (.-download a) (str path))
        (.click a)
        url)))

  p/IControls
  (-control! [this {:keys [id] :as spec} on-change]
    ;; One widget! method per kind; a hidden line under it shows refusals.
    (let [w   (widget! spec on-change)
          err (el "div" "display:none;color:#e07070;font-size:11px;")]
      (doseq [n (:nodes w)] (.appendChild (:controls @state) n))
      (.appendChild (:controls @state) err)
      (swap! state assoc-in [:widgets id] {:move! (:set! w) :error err}))
    this)
  (-set-control! [this id v]
    ;; Setting a DOM value fires no input event, so on-change stays quiet.
    (when-let [move! (get-in @state [:widgets id :move!])]
      (move! v))
    this)

  p/IControlFeedback
  (-control-error! [this id msg]
    (when-let [err (get-in @state [:widgets id :error])]
      (set! (.-textContent err) (str msg))
      (set! (.. err -style -display) (if msg "block" "none")))
    this)

  p/IPointer
  (-on-pointer! [this f]
    ;; Pointer, wheel, pinch and double-click events over the stage canvas
    ;; in scene units, hit-tested on the frame last painted
    ;; (desargues.browser.input).
    (input/wire-pointer! state f)
    this)

  p/IKeyboard
  (-on-key! [this f]
    (input/wire-keys! state f)
    this)

  p/IClock
  (-on-tick! [this f]
    (let [last (volatile! nil)
          loop (fn step [now]
                 (let [dt (if-let [l @last] (/ (- now l) 1000.0) 0.0)]
                   (vreset! last now)
                   (try (f dt)
                        (catch :default e (swap! state assoc :tick-error e)))
                   (when (:ticking? @state)
                     (swap! state assoc :raf (js/requestAnimationFrame step)))))]
      (swap! state assoc :ticking? true :raf (js/requestAnimationFrame loop)))
    this)
  (-stop-ticks! [this]
    (swap! state assoc :ticking? false)
    (some-> (:raf @state) js/cancelAnimationFrame)
    this))

(defn host
  "A fresh, unopened browser host that mounts into DOM element `mount`."
  [mount]
  (->CanvasHost mount (atom {})))
