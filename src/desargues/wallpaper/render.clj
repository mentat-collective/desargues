(ns desargues.wallpaper.render
  "BOUNDARY stratum of wallpaper loops: pixels and video.

   A loop spec is plain data plus functions of the phase u in [0, 1)
   (desargues.wallpaper.loop):

     {:id      :girih                  ; file name
      :title   \"...\"
      :seconds 12  :fps 30             ; loop length; N = fps·seconds frames
      :size    [1920 1080]
      :view    [14.22 8.0]             ; visible scene window, units, centred
      :field   (fn [u] shader)         ; optional: per-pixel base layer
      :frame   (fn [u] Frame)          ; optional: vector shapes over it
      :light   (fn [u] light)          ; optional: per-pixel brightness
      :ss      2}                      ; field supersampling, k x k per pixel

   A shader is (fn ^long [^double x ^double y] 0xRRGGBB) and a light is
   (fn ^double [^double x ^double y] factor), both in scene units (y up):
   primitive fns, so a pixel costs no boxing. Layers compose bottom-up:
   field (or the frame's :background), then the frame's shapes through the
   headless host, then every channel scaled by the light.

   Two ports, open by key:

     (field-raster backend img shader opts)   fills an image from a shader
                                              :jvm: rows in parallel on the JVM
     (encode! format spec out-path opts)      writes the loop's N frames
                                              :mp4 :webm :gif (ffmpeg), :png (a sequence)

   `field-raster`'s default loads the backend through desargues.backend, so
   an engine registered elsewhere (a GPU kernel) is a defmethod away."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [desargues.backend :as backend]
            [desargues.player.headless :as hl]
            [desargues.wallpaper.loop :as lp])
  (:import (java.awt AlphaComposite)
           (java.awt.image BufferedImage DataBufferInt)
           (java.io OutputStream)
           (java.util.function IntConsumer)
           (java.util.stream IntStream)))

(set! *warn-on-reflection* true)

(def default-view
  "Manim's frame: 128/9 x 8 scene units."
  [(/ 128.0 9.0) 8.0])

;; ── Pixels ────────────────────────────────────────────────────────────────

(defn rgb
  "Pack channels in [0, 1] into 0xRRGGBB."
  ^long [^double r ^double g ^double b]
  (let [c (fn ^long [^double v] (long (Math/round (* 255.0 (max 0.0 (min 1.0 v))))))]
    (bit-or (bit-shift-left (c r) 16) (bit-shift-left (c g) 8) (c b))))

(defn hex->rgb
  "0xRRGGBB of a #RRGGBB string."
  ^long [^String s]
  (Long/parseLong (subs s 1) 16))

(defn- pixels ^ints [^BufferedImage img]
  (.getData ^DataBufferInt (.getDataBuffer (.getRaster img))))

(defn image
  "A blank `w` x `h` RGB image."
  ^BufferedImage [w h]
  (BufferedImage. (int w) (int h) BufferedImage/TYPE_INT_RGB))

(defn- scene-scale
  "Pixels per scene unit when the `view` window fits a `w` x `h` image."
  ^double [w h [vw vh]]
  (min (/ (double w) (double vw)) (/ (double h) (double vh))))

(defn- par-rows! [^long h f]
  (.forEach (.parallel (IntStream/range 0 (int h)))
            (reify IntConsumer (accept [_ y] (f y)))))

;; ── Port: field rasteriser ────────────────────────────────────────────────

(defmulti field-raster
  "Fill BufferedImage `img` (TYPE_INT_RGB) from `shader` at opts
   {:view [w h] :ss k}. Dispatches on the backend key."
  (fn [backend _img _shader _opts] backend))

(defmethod field-raster :default [k img shader opts]
  (backend/dispatch-default field-raster "field rasteriser" k [img shader opts]))

(defmethod field-raster :jvm [_ ^BufferedImage img shader {:keys [view ss] :or {view default-view ss 1}}]
  (let [w (.getWidth img) h (.getHeight img)
        px (pixels img)
        s  (scene-scale w h view)
        inv (/ 1.0 s)
        cx (/ w 2.0) cy (/ h 2.0)
        k  (long ss)
        k2 (double (* k k))
        ^clojure.lang.IFn$DDL f shader]
    (par-rows!
     h
     (fn [^long py]
       (let [row (* py w)]
         (dotimes [px* w]
           (if (= k 1)
             (aset px (+ row px*)
                   (unchecked-int (.invokePrim f (* inv (- (+ px* 0.5) cx)) (* inv (- cy (+ py 0.5))))))
             (let [acc (long-array 3)]
               (dotimes [sy k]
                 (dotimes [sx k]
                   (let [x (* inv (- (+ px* (/ (+ sx 0.5) k)) cx))
                         y (* inv (- cy (+ py (/ (+ sy 0.5) k))))
                         c (.invokePrim f x y)]
                     (aset acc 0 (+ (aget acc 0) (bit-and (bit-shift-right c 16) 0xFF)))
                     (aset acc 1 (+ (aget acc 1) (bit-and (bit-shift-right c 8) 0xFF)))
                     (aset acc 2 (+ (aget acc 2) (bit-and c 0xFF))))))
               (aset px (+ row px*)
                     (unchecked-int
                      (bit-or (bit-shift-left (Math/round (/ (aget acc 0) k2)) 16)
                              (bit-shift-left (Math/round (/ (aget acc 1) k2)) 8)
                              (Math/round (/ (aget acc 2) k2))))))))))))
  img)

;; ── Layers ────────────────────────────────────────────────────────────────

(defn- fit-frame
  "Frame `f` rescaled from the `view` window onto the headless host's own
   Manim window, so shapes keep their place in the spec's scene units."
  [f [vw vh :as view]]
  (if (= view default-view)
    f
    (let [k (min (/ (first default-view) (double vw)) (/ (second default-view) (double vh)))
          sc (fn [p] (mapv #(* k (double %)) p))]
      (update f :shapes
              (partial mapv (fn [s]
                              (cond-> s
                                (:points s) (update :points (partial mapv sc))
                                (:at s)     (update :at sc)
                                (:from s)   (update :from sc)
                                (:to s)     (update :to sc)
                                (get-in s [:opts :radius]) (update-in [:opts :radius] * k)
                                (get-in s [:stroke :width]) (update-in [:stroke :width] * k))))))))

(defn- paint-shapes!
  "Paint frame `f`'s shapes over `img`. With `background?` the frame's own
   :background is painted first; otherwise the shapes go over what `img`
   already holds."
  [^BufferedImage img f background?]
  (let [w (.getWidth img) h (.getHeight img)]
    (if background?
      (let [layer (hl/frame->image f w h)
            g (.createGraphics img)]
        (.drawImage g layer 0 0 nil) (.dispose g))
      (let [layer (hl/frame->image (assoc f :background "rgba(0,0,0,0)") w h)
            g (.createGraphics img)]
        (.setComposite g AlphaComposite/SrcOver)
        (.drawImage g layer 0 0 nil)
        (.dispose g)))
    img))

(defn- light! [^BufferedImage img light {:keys [view] :or {view default-view}}]
  (let [w (.getWidth img) h (.getHeight img)
        px (pixels img)
        inv (/ 1.0 (scene-scale w h view))
        cx (/ w 2.0) cy (/ h 2.0)
        ^clojure.lang.IFn$DDD f light]
    (par-rows!
     h
     (fn [^long py]
       (let [row (* py w)
             y (* inv (- cy (+ py 0.5)))]
         (dotimes [x* w]
           (let [i (+ row x*)
                 c (aget px i)
                 k (.invokePrim f (* inv (- (+ x* 0.5) cx)) y)
                 ch (fn ^long [^long shift]
                      (min 255 (max 0 (Math/round (* k (bit-and (bit-shift-right c shift) 0xFF))))))]
             (aset px i (unchecked-int (bit-or (bit-shift-left (ch 16) 16)
                                               (bit-shift-left (ch 8) 8)
                                               (ch 0)))))))))
    img))

(defn render-frame
  "The loop `spec` at phase `u` as a BufferedImage. opts: {:size [w h]
   :backend k} override the spec's size and the field backend (:jvm)."
  ^BufferedImage [spec u & [{:keys [size backend] :or {backend :jvm}}]]
  (let [[w h] (or size (:size spec) [1280 720])
        view (or (:view spec) default-view)
        opts {:view view :ss (or (:ss spec) 1)}
        img (image w h)
        u (double u)]
    (when-let [field (:field spec)]
      (field-raster backend img (field u) opts))
    (when-let [frame-fn (:frame spec)]
      (paint-shapes! img (fit-frame (frame-fn u) view) (nil? (:field spec))))
    (when-let [light (:light spec)]
      (light! img (light u) opts))
    img))

;; ── Port: encoders ────────────────────────────────────────────────────────

(defn frames
  "Frame count of `spec` (fps · seconds), honouring opts {:fps :seconds}."
  [spec & [{:keys [fps seconds]}]]
  (lp/frame-count (or fps (:fps spec) 30) (or seconds (:seconds spec) 10)))

(defn- rgb-bytes ^bytes [^BufferedImage img]
  (let [px (pixels img) n (alength px) out (byte-array (* 3 n))]
    (dotimes [i n]
      (let [c (aget px i) j (* 3 i)]
        (aset out j (unchecked-byte (bit-shift-right c 16)))
        (aset out (+ j 1) (unchecked-byte (bit-shift-right c 8)))
        (aset out (+ j 2) (unchecked-byte c))))
    out))

(defn- each-frame!
  "Render `spec`'s frames in order, a batch at a time across cores, calling
   (sink i img) for frame i in order."
  [spec opts sink]
  (let [n (frames spec opts)
        batch (max 1 (.availableProcessors (Runtime/getRuntime)))
        progress (:progress opts)]
    (doseq [idx (partition-all batch (range n))]
      (let [imgs (doall (pmap #(render-frame spec (/ (double %) n) opts) idx))]
        (doseq [[i img] (map vector idx imgs)]
          (sink i img))
        (when progress (progress (inc (long (last idx))) n))))
    n))

(defn ffmpeg-args
  "The ffmpeg command line that reads raw RGB frames of `w` x `h` at `fps`
   on stdin and writes `out` as `format` (:mp4 :webm :gif). `crf`
   overrides the quality."
  [format w h fps out & [{:keys [crf]}]]
  (let [in ["ffmpeg" "-y" "-loglevel" "error" "-f" "rawvideo" "-pix_fmt" "rgb24"
            "-s" (str w "x" h) "-r" (str fps) "-i" "-"]]
    (case format
      :mp4  (into in ["-c:v" "libx264" "-preset" "slow" "-crf" (str (or crf 18))
                      "-pix_fmt" "yuv420p" "-movflags" "+faststart" (str out)])
      :webm (into in ["-c:v" "libvpx-vp9" "-crf" (str (or crf 30)) "-b:v" "0"
                      "-row-mt" "1" "-pix_fmt" "yuv420p" (str out)])
      :gif  (into in ["-filter_complex"
                      "split[a][b];[a]palettegen=stats_mode=diff[p];[b][p]paletteuse=dither=bayer:bayer_scale=4"
                      "-loop" "0" (str out)]))))

(defmulti encode!
  "Write loop `spec` to `out` as `format`. opts: {:size [w h] :fps :seconds
   :crf :backend :progress (fn [done total])}. Returns the output path."
  (fn [format _spec _out _opts] format))

(defn- pipe-ffmpeg! [format spec out opts]
  (let [[w h] (or (:size opts) (:size spec) [1280 720])
        fps (or (:fps opts) (:fps spec) 30)
        _ (some-> (io/file (str out)) .getParentFile .mkdirs)
        proc (.start (doto (ProcessBuilder. ^java.util.List (ffmpeg-args format w h fps out opts))
                       (.redirectErrorStream true)))
        ^OutputStream stdin (.getOutputStream proc)]
    (try
      (each-frame! spec (assoc opts :size [w h])
                   (fn [_ img] (.write stdin (rgb-bytes img))))
      (finally (.close stdin)))
    (let [log (slurp (.getInputStream proc))
          code (.waitFor proc)]
      (when-not (zero? code)
        (throw (ex-info (str "ffmpeg failed (" code "): " (str/trim log))
                        {:format format :out (str out) :exit code}))))
    (str out)))

(defmethod encode! :mp4 [f spec out opts] (pipe-ffmpeg! f spec out opts))
(defmethod encode! :webm [f spec out opts] (pipe-ffmpeg! f spec out opts))
(defmethod encode! :gif [f spec out opts] (pipe-ffmpeg! f spec out opts))

(defmethod encode! :png [_ spec out opts]
  (each-frame! spec opts
               (fn [i img] (hl/write-png! img (io/file (str out) (format "frame-%05d.png" i)))))
  (str out))

(defn still!
  "Write `spec` at phase `u` as a PNG at `out`."
  [spec u out & [opts]]
  (hl/write-png! (render-frame spec u opts) out))

