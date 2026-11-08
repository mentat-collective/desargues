(ns desargues.player.headless
  "BOUNDARY stratum: a headless host for the player (desargues.player).

   Implements ONLY desargues.player.protocols/ISurface (ISP): Frames are
   painted into a java.awt.image.BufferedImage through Graphics2D. No window,
   no widgets, no clock, no JavaFX and no display, so it runs in CI and in
   any JVM. Paint semantics mirror the JavaFX host (studio/desargues/player/
   javafx.clj) node for node and unit for unit, so a PNG from here is what the
   window shows:

   - colours are palette keywords (:gold) or CSS colour strings (\"#8FC7E8\",
     \"rgb(...)\", CSS names); `:stroke {:color nil}` paints no outline;
   - Manim stroke widths are pixels at 1080p (135 px per scene unit);
   - :svg-path `:d` is in scene units with y up;
   - text honours :opts :anchor (start, middle, end) and, when it carries
     :opts :size (SVG text), sits on its baseline, otherwise is centred;
   - :dash is a seq of lengths or an SVG dasharray string, in scene units;
   - the Frame's :background fills first (default black);
   - :tex :opts :anchor \"start\" puts the left edge at :at (centred by
     default); :opts :write clips it to its :draw fraction, left to right;
   - :tex goes through JLaTeXMath when it is on the classpath (reflective,
     plain-text fallback).

   Painting is one `paint!` method per node kind (OCP). Everything here is
   JDK only (java.awt, javax.imageio), which is why it lives in src/ rather
   than studio/: it adds no dependency. java.awt.headless is set true on load
   unless the JVM already chose.

   API: `surface` (the ISurface), `image`, `frame->image`, `render-png`,
   `render-sequence`, and golden-image helpers `image-diff` and
   `golden-check`."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [desargues.player.frame :as frame]
            [desargues.player.protocols :as p]
            [desargues.player.svg-path :as svg-path]
            [desargues.player.space :as space])
  (:import [java.awt AlphaComposite BasicStroke Color Font Graphics2D RenderingHints Shape]
           [java.awt.geom AffineTransform Arc2D Arc2D$Double Ellipse2D$Double
            Line2D$Double Path2D$Double RoundRectangle2D$Double]
           [java.awt.image BufferedImage]
           [java.io File]
           [javax.imageio ImageIO]))

(when-not (System/getProperty "java.awt.headless")
  (System/setProperty "java.awt.headless" "true"))

;; ── Colours ────────────────────────────────────────────────────────────────

(def ^:private css-names
  "CSS colour names understood besides #hex and rgb()/rgba()."
  {"white" "#FFFFFF" "black" "#000000" "red" "#FF0000" "green" "#008000"
   "blue" "#0000FF" "yellow" "#FFFF00" "orange" "#FFA500" "purple" "#800080"
   "gray" "#808080" "grey" "#808080" "gold" "#FFD700" "pink" "#FFC0CB"
   "teal" "#008080" "maroon" "#800000" "cyan" "#00FFFF" "magenta" "#FF00FF"
   "transparent" "#00000000"})

(defn- clamp255 ^long [v] (max 0 (min 255 (Math/round (double v)))))

(defn- rgba ^Color [r g b a]
  (Color. (int (clamp255 r)) (int (clamp255 g)) (int (clamp255 b)) (int (clamp255 a))))

(defn- parse-hex [^String s]
  (let [h (subs s 1)
        h (if (#{3 4} (count h)) (apply str (mapcat #(repeat 2 %) h)) h)
        n #(Integer/parseInt (subs h % (+ % 2)) 16)]
    (case (count h)
      6 (Color. (int (n 0)) (int (n 2)) (int (n 4)))
      8 (Color. (int (n 0)) (int (n 2)) (int (n 4)) (int (n 6)))
      nil)))

(defn- parse-rgb [^String s]
  (when-let [[_ body] (re-matches #"(?i)rgba?\((.*)\)" s)]
    (let [[r g b a] (map #(edn/read-string (str/replace % "%" ""))
                         (remove str/blank? (str/split body #"[,\s/]+")))]
      (rgba r g b (* 255.0 (double (or a 1.0)))))))

(defn css-color
  "A java.awt.Color from a palette keyword (:gold, Manim's palette) or a CSS
   colour string: #RGB, #RRGGBB, #RRGGBBAA, rgb()/rgba() or a CSS name.
   Anything unknown is white, as in the JavaFX host."
  ^Color [k]
  (or (if (string? k)
        (let [s (str/trim k)]
          (try (cond (str/starts-with? s "#") (parse-hex s)
                     (re-find #"(?i)^rgb" s)   (parse-rgb s)
                     :else (some-> (css-names (str/lower-case s)) parse-hex))
               (catch Exception _ nil)))
        (parse-hex (get frame/palette k (frame/palette :white))))
      Color/WHITE))

(defn- lerp-color ^Color [^Color a ^Color b t]
  (let [t (double t) l (fn [x y] (+ (double x) (* t (- (double y) (double x)))))]
    (rgba (l (.getRed a) (.getRed b)) (l (.getGreen a) (.getGreen b))
          (l (.getBlue a) (.getBlue b)) (l (.getAlpha a) (.getAlpha b)))))

(defn- stroke-color
  "The stroke of shape `s`. An explicit `:stroke {:color nil}` (a fill-only
   shape) is fully transparent rather than falling back to white."
  ^Color [s]
  (if (and (contains? (:stroke s) :color) (nil? (get-in s [:stroke :color])))
    (Color. 0 0 0 0)
    (let [c (css-color (or (get-in s [:stroke :color]) (get-in s [:opts :color]) :white))]
      (if-let [{:keys [to alpha]} (:recolor s)]
        (lerp-color c (css-color to) alpha)
        c))))

(defn- fill-color
  "The fill of shape `s` (its :opacity times the colour's own alpha), or nil."
  [s]
  (when-let [{:keys [color opacity]} (:fill s)]
    (let [c (css-color color)]
      (rgba (.getRed c) (.getGreen c) (.getBlue c)
            (* (.getAlpha c) (double (or opacity 1.0)))))))

;; ── Units ──────────────────────────────────────────────────────────────────

(defn- units->px ^double [vp u] (* (double (:scale vp)) (double u)))

(defn- stroke-px
  "Manim stroke widths are pixels at 1080p (135 px per scene unit)."
  ^double [vp s]
  (max 1.0 (* (double (or (get-in s [:stroke :width]) (get-in s [:opts :width]) 4))
              (/ (double (:scale vp)) 135.0))))

(defn- emphasis-scale ^double [s] (+ 1.0 (* 0.25 (double (:emphasis s 0.0)))))

(defn- center [vp s] (frame/->px vp (:at s [0 0])))

(defn- dash-px
  "Line dashes of shape `s` in pixels, or nil. :dash is a seq of lengths, or
   an SVG stroke-dasharray string (\"0.05,0.03\"), in scene units."
  [vp s]
  (when-let [d (:dash s)]
    (let [xs (if (string? d)
               (edn/read-string (str "[" (str/replace d "," " ") "]"))
               d)
          px (map #(units->px vp %) (filter number? xs))]
      (when (and (seq px) (some pos? px)) px))))

(defn- stroke!
  "Stroke `shape` in pixel space with `s`'s colour, width and dashes."
  [^Graphics2D g vp s ^Shape shape]
  (let [d (dash-px vp s)]
    (.setStroke g (if d
                    (BasicStroke. (float (stroke-px vp s)) BasicStroke/CAP_SQUARE
                                  BasicStroke/JOIN_MITER (float 10.0)
                                  (float-array (map float d)) (float 0.0))
                    (BasicStroke. (float (stroke-px vp s)))))
    (.setColor g (stroke-color s))
    (.draw g shape)))

(defn- fill! [^Graphics2D g ^Color c ^Shape shape]
  (.setColor g c)
  (.fill g shape))

;; ── Painting: one method per node kind (OCP) ───────────────────────────────

(defmulti paint!
  "Paint shape `s` on Graphics2D `g` under viewport `vp` (frame/viewport).
   A new node kind is a new method; unknown kinds paint nothing."
  (fn [_g _vp s] (:node s)))

(defmethod paint! :default [_ _ _] nil)

(defmethod paint! :dot [^Graphics2D g vp s]
  (let [[cx cy] (center vp s)
        r (* (units->px vp (get-in s [:opts :radius] 0.08)) (emphasis-scale s))]
    (fill! g (or (fill-color s) (stroke-color s))
           (Ellipse2D$Double. (- cx r) (- cy r) (* 2 r) (* 2 r)))))

(defmethod paint! :circle [^Graphics2D g vp s]
  (let [[cx cy] (center vp s)
        r (* (units->px vp (get-in s [:opts :radius] 1.0)) (emphasis-scale s))]
    (when-let [f (fill-color s)]
      (fill! g f (Ellipse2D$Double. (- cx r) (- cy r) (* 2 r) (* 2 r))))
    ;; drawn clockwise from 12 o'clock, like the JavaFX strokeArc
    (stroke! g vp s (Arc2D$Double. (- cx r) (- cy r) (* 2 r) (* 2 r)
                                   90.0 (* -360.0 (double (:draw s 1.0))) Arc2D/OPEN))))

(defmethod paint! :line [^Graphics2D g vp s]
  (let [[x0 y0] (frame/->px vp (:from s))
        [x1 y1] (frame/->px vp (:to s))
        d (double (:draw s 1.0))]
    (stroke! g vp s (Line2D$Double. x0 y0 (+ x0 (* d (- x1 x0))) (+ y0 (* d (- y1 y0)))))))

(defn- polyline ^Path2D$Double [pts]
  (let [path (Path2D$Double.)]
    (when-let [[x y] (first pts)]
      (.moveTo path (double x) (double y))
      (doseq [[x y] (rest pts)] (.lineTo path (double x) (double y))))
    path))

(defmethod paint! :path [^Graphics2D g vp s]
  (let [pts     (mapv #(frame/->px vp %) (:points s))
        n       (long (Math/ceil (* (double (:draw s 1.0)) (count pts))))
        closed? (get-in s [:opts :closed?])
        pts     (cond-> (subvec pts 0 (min n (count pts)))
                  (and closed? (seq pts)) (conj (first pts)))]
    (when (> (count pts) 1)
      (let [path (polyline pts)]
        (when-let [f (and closed? (fill-color s))]
          (fill! g f path))
        (stroke! g vp s path)))))

(defn- paint-box! [^Graphics2D g vp s corner]
  (let [[cx cy] (center vp s)
        k (emphasis-scale s)
        w (* k (units->px vp (get-in s [:opts :width] 4.0)))
        h (* k (units->px vp (get-in s [:opts :height] 2.0)))
        a (* 2 (units->px vp corner))
        box (RoundRectangle2D$Double. (- cx (/ w 2)) (- cy (/ h 2)) w h a a)]
    (when-let [f (fill-color s)] (fill! g f box))
    (stroke! g vp s box)))

(defmethod paint! :rectangle [g vp s] (paint-box! g vp s 0.0))

(defmethod paint! :rounded-rectangle [g vp s]
  (paint-box! g vp s (get-in s [:opts :corner-radius] 0.25)))

(defn- font-px
  "Font size in pixels: Manim font sizes are relative to a 120 px/unit frame."
  ^double [vp s]
  (* (emphasis-scale s) (double (get-in s [:opts :font-size] 48))
     (/ (double (:scale vp)) 120.0)))

(defn- paint-text!
  "Text at the shape's :at. :opts :anchor (SVG text-anchor: start, middle,
   end) picks the alignment, centred by default; a shape carrying :opts :size
   is SVG text, whose :at is the baseline, otherwise the text is centred
   vertically."
  [^Graphics2D g vp s ^String text]
  (let [[cx cy] (center vp s)]
    (.setFont g (Font. Font/SANS_SERIF Font/PLAIN 1))
    (.setFont g (.deriveFont (.getFont g) (float (font-px vp s))))
    (let [fm (.getFontMetrics g)
          w  (double (.stringWidth fm text))
          x  (case (get-in s [:opts :anchor])
               "start" cx
               "end"   (- cx w)
               (- cx (/ w 2.0)))
          y  (if (get-in s [:opts :size])
               cy
               (+ cy (/ (- (.getAscent fm) (.getDescent fm)) 2.0)))]
      (.setColor g (stroke-color s))
      (.drawString g text (float x) (float y)))))

(defmethod paint! :text [g vp s] (paint-text! g vp s (str (:text s))))

(defmethod paint! :decimal [g vp s]
  (paint-text! g vp s (format (str "%." (get-in s [:opts :num-decimal-places] 2) "f")
                              (double (:value s 0)))))

(defn- tex-icon*
  "LaTeX `tex` typeset by JLaTeXMath at `px` pixels in `color`, as a
   javax.swing.Icon; nil when JLaTeXMath is absent or the source does not
   parse. Reflective on purpose: this host loads without JLaTeXMath."
  [tex px ^Color color]
  (try
    (let [fcls    (Class/forName "org.scilab.forge.jlatexmath.TeXFormula")
          formula (clojure.lang.Reflector/invokeConstructor fcls (object-array [(str tex)]))
          style   (.get (.getField (Class/forName "org.scilab.forge.jlatexmath.TeXConstants")
                                   "STYLE_DISPLAY") nil)
          icon    (clojure.lang.Reflector/invokeInstanceMethod
                   formula "createTeXIcon" (object-array [style (float px)]))]
      (clojure.lang.Reflector/invokeInstanceMethod icon "setForeground" (object-array [color]))
      icon)
    (catch Throwable _ nil)))

(def ^:private tex-icon (memoize tex-icon*))

(defn tex-available?
  "True when JLaTeXMath is on the classpath, so :tex is typeset."
  []
  (some? (tex-icon "x" 12.0 Color/WHITE)))

(defn write-fraction
  "How much of a :tex shape is written, 0..1: its :draw when it carries
   :opts :write (chalk written left to right), else 1 (all of it)."
  ^double [s]
  (if (get-in s [:opts :write])
    (max 0.0 (min 1.0 (double (:draw s 1.0))))
    1.0))

(defmethod paint! :tex [^Graphics2D g vp s]
  (let [[cx cy] (center vp s)
        px (Math/rint (font-px vp s))]
    (if-let [^javax.swing.Icon icon (tex-icon (str (:tex s)) px (stroke-color s))]
      (let [w  (.getIconWidth icon)
            h  (.getIconHeight icon)
            x  (int (Math/round (if (= "start" (get-in s [:opts :anchor])) cx (- cx (/ w 2.0)))))
            y  (int (Math/round (- cy (/ h 2.0))))
            wf (write-fraction s)
            clip (.getClip g)]
        (when (< wf 1.0) (.clipRect g x (- y 2) (int (Math/ceil (* wf w))) (+ h 4)))
        (.paintIcon icon nil g x y)
        (.setClip g clip))
      (paint-text! g vp s (str (:tex s))))))

(defn- svg-shape
  "The java.awt.geom path of SVG `d` (scene units, y up), in scene units."
  ^Path2D$Double [d]
  (let [path (Path2D$Double.)]
    (doseq [[k & xs] (svg-path/segments d)]
      (let [[a b c e f h] (map double xs)]
        (case k
          :move  (.moveTo path a b)
          :line  (.lineTo path a b)
          :quad  (.quadTo path a b c e)
          :cubic (.curveTo path a b c e f h)
          :close (.closePath path))))
    path))

(defmethod paint! :svg-path [^Graphics2D g vp s]
  ;; Map scene units (y up) to pixels the way frame/->px does, then fill and
  ;; stroke in pixel space so widths and dashes stay in pixels.
  (let [{:keys [w h scale]} vp
        xf    (doto (AffineTransform.)
                (.translate (/ (double w) 2.0) (/ (double h) 2.0))
                (.scale (double scale) (- (double scale))))
        shape (.createTransformedShape xf (svg-shape (:d s)))]
    (when-let [f (fill-color s)] (fill! g f shape))
    (stroke! g vp s shape)))

;; ── Frames into images ─────────────────────────────────────────────────────

(defn paint-frame!
  "Paint Frame `f` onto BufferedImage `img`: the frame's :background
   (palette keyword or CSS colour, default black), then each visible shape
   in draw order at its :opacity. A 3D frame (desargues.player.space) is
   projected to 2D first, so this 2D host renders it too. Returns `img`."
  ^BufferedImage [^BufferedImage img f]
  (let [f  (space/->2d f)
        g  (.createGraphics img)
        w  (.getWidth img)
        h  (.getHeight img)
        vp (frame/viewport w h)]
    (try
      (doto g
        (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
        (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING
                           RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
        (.setRenderingHint RenderingHints/KEY_STROKE_CONTROL RenderingHints/VALUE_STROKE_PURE)
        (.setRenderingHint RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY))
      (.setComposite g AlphaComposite/Src)
      (.setColor g (if-let [b (:background f)] (css-color b) Color/BLACK))
      (.fillRect g 0 0 w h)
      (doseq [s (frame/visible-shapes f)]
        (.setComposite g (AlphaComposite/getInstance
                          AlphaComposite/SRC_OVER
                          (float (max 0.0 (min 1.0 (double (:opacity s)))))))
        (paint! g vp s))
      img
      (finally (.dispose g)))))

(defn frame->image
  "Frame `f` painted into a fresh `w` x `h` ARGB image."
  ^BufferedImage [f w h]
  (paint-frame! (BufferedImage. (int w) (int h) BufferedImage/TYPE_INT_ARGB) f))

(defn write-png!
  "Write `img` as a PNG at `path` (parents created); returns the path string."
  [^BufferedImage img path]
  (let [file (io/file (str path))]
    (some-> (.getParentFile file) .mkdirs)
    (ImageIO/write img "png" file)
    (.getPath file)))

;; ── The host: ISurface only ────────────────────────────────────────────────

(defrecord HeadlessSurface [^BufferedImage img]
  p/ISurface
  (-present! [this f]
    (locking img (paint-frame! img f))
    this)
  (-size [_] [(.getWidth img) (.getHeight img)]))

(defn surface
  "A `w` x `h` (default 1280 x 720) offscreen ISurface. Each -present!
   repaints its image; read it with `image`."
  ([] (surface 1280 720))
  ([w h] (->HeadlessSurface (BufferedImage. (int w) (int h) BufferedImage/TYPE_INT_ARGB))))

(defn image
  "The BufferedImage behind headless surface `surf` (what was last presented)."
  ^BufferedImage [surf]
  (:img surf))

(def default-size
  "Pixel size of rendered frames when opts give none (720p, 16:9)."
  {:w 1280 :h 720})

(defn render-image
  "The frame of ISource `source` at time `t` (clamped to its duration) as an
   image. opts: {:w :h}."
  ^BufferedImage [source t {:keys [w h] :or {w (:w default-size) h (:h default-size)}}]
  (let [t    (max 0.0 (min (double t) (double (p/-duration source))))
        surf (surface w h)]
    (p/-present! surf (p/-frame-at source t))
    (image surf)))

(defn render-png
  "Write the frame of ISource `source` at time `t` to a PNG at `path`.
   opts: {:w :h} in pixels (default 1280 x 720). Returns the path."
  ([source t path] (render-png source t path {}))
  ([source t path opts]
   (write-png! (render-image source t opts) path)))

(defn render-sequence
  "Write `source` sampled at `fps` as numbered PNGs (frame-00000.png ...)
   into directory `dir`, from t = 0 through its duration inclusive. opts:
   {:w :h :prefix}. Returns the paths in order; ffmpeg -i frame-%05d.png
   turns them into a video."
  ([source fps dir] (render-sequence source fps dir {}))
  ([source fps dir {:keys [prefix] :or {prefix "frame-"} :as opts}]
   (let [dur (double (p/-duration source))
         n   (inc (long (Math/floor (+ 1e-9 (* dur (double fps))))))]
     (mapv (fn [i]
             (render-png source (/ (double i) (double fps))
                         (io/file (str dir) (format "%s%05d.png" prefix i))
                         opts))
           (range n)))))

;; ── Golden images ──────────────────────────────────────────────────────────

(defn read-png
  "The image at `path` (a file path or URL), or nil when there is none."
  ^BufferedImage [path]
  (let [src (if (instance? java.net.URL path) path (io/file (str path)))]
    (when (or (instance? java.net.URL src) (.exists ^File src))
      (ImageIO/read src))))

(defn- channel-delta ^long [^long a ^long b]
  (max (Math/abs (- (bit-and (bit-shift-right a 24) 255) (bit-and (bit-shift-right b 24) 255)))
       (Math/abs (- (bit-and (bit-shift-right a 16) 255) (bit-and (bit-shift-right b 16) 255)))
       (Math/abs (- (bit-and (bit-shift-right a 8) 255) (bit-and (bit-shift-right b 8) 255)))
       (Math/abs (- (bit-and a 255) (bit-and b 255)))))

(defn image-diff
  "Compare two images pixel by pixel. A pixel mismatches when any channel
   (ARGB) differs by more than `tolerance` (0-255). Returns {:same-size?
   :pixels :mismatched :fraction :max-delta}."
  [^BufferedImage a ^BufferedImage b tolerance]
  (if-not (and (= (.getWidth a) (.getWidth b)) (= (.getHeight a) (.getHeight b)))
    {:same-size? false}
    (let [w (.getWidth a) h (.getHeight a)
          tol (long tolerance)
          [bad mx] (loop [i 0 bad 0 mx 0]
                     (if (= i (* w h))
                       [bad mx]
                       (let [x (rem i w) y (quot i w)
                             d (channel-delta (.getRGB a x y) (.getRGB b x y))]
                         (recur (inc i) (if (> d tol) (inc bad) bad) (max mx d)))))]
      {:same-size? true :pixels (* w h) :mismatched bad
       :fraction (/ (double bad) (* w h)) :max-delta mx})))

(def default-tolerance
  "Golden-image tolerance: a pixel may differ by up to 48/255 in any channel
   (antialiasing and font rasterisation vary across JDKs and fonts), and up
   to 1% of the pixels may exceed that (glyph edges)."
  {:tolerance 48 :max-fraction 0.01})

(defn golden-check
  "Compare `actual` (a BufferedImage) with the golden PNG at `golden-path`.
   opts: {:tolerance :max-fraction} (see default-tolerance), :update? to
   (re)write the golden, :actual-out to save the actual image for
   inspection on failure. A missing golden fails unless :update?. Returns
   the image-diff map plus {:pass? :golden}."
  [^BufferedImage actual golden-path {:keys [update? actual-out] :as opts}]
  (let [{:keys [tolerance max-fraction]} (merge default-tolerance opts)
        golden (read-png golden-path)]
    (if (or update? (nil? golden))
      (if update?
        {:pass? true :updated? true :golden (write-png! actual golden-path)}
        {:pass? false :missing? true :golden (str golden-path)})
      (let [d     (image-diff actual golden tolerance)
            pass? (and (:same-size? d) (<= (double (:fraction d)) (double max-fraction)))]
        (when (and (not pass?) actual-out) (write-png! actual actual-out))
        (assoc d :pass? pass? :golden (str golden-path))))))
