(ns desargues.infrastructure.raster-field
  "Boundary adapter: field forms (desargues.wallpaper.field) as raster
   kernels, the :raster and :gpu methods of render/field-raster.

   One pixel body, generated from the form's lowered code, in two kernels:
   - :raster, a deftm over a run of pixel indices [i0, i1), called on bands
     of rows in parallel on the JVM;
   - :gpu, the same body inside raster.par/map-void! over every pixel,
     compiled by raster to an OpenCL kernel (one work item per pixel).
   Both write the packed 0xRRGGBB of each pixel into a double[] (exact: 24
   bits), copied into the image. Supersampling (opts :ss k) averages the k x k
   samples' quantised levels and rounds, as render's :jvm method does, so the
   JVM kernel equals the :jvm form bit for bit; the GPU's cos/exp round
   differently and stay within one level.

   Kernel ABI: (out ps geo w [i0 i1 | n]), out the packed pixels, ps the
   form's params, geo [inv cx cy k] (scene units per pixel, image centre,
   supersampling), w the image width. Kernels compile once per form (cached
   by value; GPU kernels once per session). The GPU session is opened on
   first use and shared; `close-gpu!` releases it with its buffers. The frame
   buffers stay resident between calls and are reallocated (old ones freed)
   only when the image size changes: a loop renders N frames of one size.
   raster is on the classpath under the :dynamics alias."
  (:require [desargues.wallpaper.field :as fld]
            [desargues.wallpaper.render :as render]
            [raster.core]
            [raster.gpu :as gpu]
            [raster.par])
  (:import (java.awt.image BufferedImage DataBufferInt)
           (java.util.function IntConsumer)
           (java.util.stream IntStream)))

(set! *warn-on-reflection* true)

(defn- level [v]
  (list 'Math/floor (list '+ (list '* 255.0 (list 'Math/min (list 'Math/max v 0.0) 1.0)) 0.5)))

(defn pixel-body
  "The form writing pixel `i`'s packed colour into out, for lowered field
   `lowered` (fld/lower): its k x k samples, each through the lowered
   bindings and quantised, averaged per channel and rounded."
  [{:keys [params bindings rgb]} i]
  (let [[r g b] rgb]
    (list 'let (vec (concat ['py (list 'quot i 'w) 'px (list '- i (list '* 'py 'w))
                             'fpx (list 'double 'px) 'fpy (list 'double 'py)
                             'inv '(aget geo 0) 'cx '(aget geo 1) 'cy '(aget geo 2)
                             'kd '(aget geo 3) 'kk '(* kd kd)]
                            (mapcat (fn [j] [(symbol (str "p" j)) (list 'aget 'ps j)]) (range (count params)))))
          (list 'loop '[s 0.0 ar 0.0 ag 0.0 ab 0.0]
                (list 'if '(< s kk)
                      (list 'let (vec (concat '[sy (Math/floor (/ s kd)) sx (- s (* sy kd))
                                                fx (* inv (- (+ fpx (/ (+ sx 0.5) kd)) cx))
                                                fy (* inv (- cy (+ fpy (/ (+ sy 0.5) kd))))]
                                              (fld/value-bindings bindings rgb)))
                            (list 'recur '(+ s 1.0)
                                  (list '+ 'ar (level r)) (list '+ 'ag (level g)) (list '+ 'ab (level b))))
                      (list 'aset 'out i
                            '(+ (+ (* 65536.0 (Math/floor (+ (/ ar kk) 0.5)))
                                   (* 256.0 (Math/floor (+ (/ ag kk) 0.5))))
                                (Math/floor (+ (/ ab kk) 0.5)))))))))

(defn band-form
  "deftm `nm` (out ps geo w i0 i1) -> 0: pixel-body over i in [i0, i1)."
  [nm lowered]
  (list 'raster.core/deftm nm
        '[out :- (Array double) ps :- (Array double) geo :- (Array double)
          w :- Long i0 :- Long i1 :- Long] :- 'Long
        (list 'do
              (list 'loop '[i i0] (list 'when '(< i i1) (pixel-body lowered 'i) '(recur (+ i 1))))
              0)))

(defn par-form
  "deftm `nm` (out ps geo w n) -> Void: pixel-body as one raster.par/map-void!
   over n pixels, the shape raster compiles to a GPU kernel."
  [nm lowered]
  (list 'raster.core/deftm nm
        '[out :- (Array double) ps :- (Array double) geo :- (Array double)
          w :- Long n :- Long] :- 'Void
        (list 'raster.par/map-void! 'i 'n (pixel-body lowered 'i))))

(def ^:private compiled
  "[kind form] -> the kernel var."
  (atom {}))

(defn kernel
  "The kernel var of field form `form`, `kind` :band (band-form) or :par
   (par-form), defined once per form."
  [kind form]
  (or (get @compiled [kind form])
      (let [nm (symbol (str "field-" (name kind) "-" (gensym "k")))
            lowered (fld/lower form)
            v (binding [*ns* (the-ns 'desargues.infrastructure.raster-field)]
                (eval ((case kind :band band-form :par par-form) nm lowered)))]
        (get (swap! compiled #(if (contains? % [kind form]) % (assoc % [kind form] v))) [kind form]))))

;; ── shared frame plumbing ────────────────────────────────────────────────

(defn- geometry
  "[inv cx cy k] of an image `w` x `h` at render opts {:view :ss}."
  ^doubles [w h {:keys [view ss] :or {view render/default-view ss 1}}]
  (let [[vw vh] view
        s (min (/ (double w) (double vw)) (/ (double h) (double vh)))]
    (double-array [(/ 1.0 s) (/ w 2.0) (/ h 2.0) (double ss)])))

(defn- params ^doubles [bf]
  (let [a (:field/args bf)] (double-array (if (seq a) a [0.0]))))

(defn- form-of
  "The bound field `shader`, or an ex-info: raster backends run field forms,
   not closures."
  [backend shader]
  (if (fld/bound? shader)
    shader
    (throw (ex-info (str "The " backend " field rasteriser runs field forms (desargues.wallpaper.field/bind), not closures")
                    {:backend backend :reason :not-a-field-form}))))

(defn- blit! [^BufferedImage img ^doubles out]
  (let [px (.getData ^DataBufferInt (.getDataBuffer (.getRaster img)))]
    (dotimes [i (alength out)]
      (aset px i (unchecked-int (long (aget out i)))))
    img))

;; ── :raster, bands of rows in parallel on the JVM ────────────────────────

(def band-rows
  "Rows per parallel task of the :raster kernel."
  8)

(defn raster-field!
  "Fill `img` from bound field `bf` with the JVM band kernel."
  [^BufferedImage img bf opts]
  (let [w (.getWidth img) h (.getHeight img)
        f @(kernel :band (:field/form bf))
        out (double-array (* w h))
        ps (params bf)
        geo (geometry w h opts)
        bands (quot (+ h (dec band-rows)) band-rows)]
    (.forEach (.parallel (IntStream/range 0 (int bands)))
              (reify IntConsumer
                (accept [_ b]
                  (let [y0 (* b band-rows) y1 (min h (+ y0 band-rows))]
                    (f out ps geo (long w) (long (* y0 w)) (long (* y1 w)))))))
    (blit! img out)))

(defmethod render/field-raster :raster [k img shader opts]
  (raster-field! img (form-of k shader) opts))

;; ── :gpu, one work item per pixel ────────────────────────────────────────

(def default-device
  "The device the shared session opens: the first OpenCL device."
  :ocl:0)

(defonce ^:private gpu-state (atom nil))

(def ^:private gpu-lock (Object.))

(defn- session []
  (or @gpu-state
      (reset! gpu-state
              (try (gpu/make-session default-device)
                   (catch Throwable t
                     (throw (ex-info (str "GPU device " default-device " is not available (" (.getMessage t) ")")
                                     {:device default-device :reason :no-device} t)))))))

(defn close-gpu!
  "Release the shared GPU session (it reopens on the next :gpu call)."
  []
  (locking gpu-lock
    (when-let [s @gpu-state]
      (gpu/close-session! s)
      (reset! gpu-state nil))))

(defn- resident!
  "Make the session's frame buffers fit `n` pixels and `np` params: kept
   across calls (an animation renders frame after frame at one size, and
   allocation is ~7 ms of a 1080p frame), reallocated only when a size
   changes, the old ones freed first (raster's alloc! merges, so a buffer
   allocated again under its key would leak)."
  [sess n np]
  (let [want {:field-out n :field-ps np :field-geo 4}]
    (when-not (= want (:field/sizes @sess))
      (doseq [bk (keys want)]
        (when (get-in @sess [:buffers bk]) (gpu/free-buffer! sess bk)))
      (gpu/alloc! sess (into {} (map (fn [[bk len]] [bk [:double len nil]])) want))
      (swap! sess assoc :field/sizes want))))

(defn gpu-field!
  "Fill `img` from bound field `bf` with the par kernel on the GPU: params
   and geometry uploaded, one work item per pixel, the packed pixels
   downloaded (16 MB at 1080p, the largest cost) and copied in."
  [^BufferedImage img bf opts]
  (locking gpu-lock
    (let [w (.getWidth img) h (.getHeight img) n (* w h)
          sess (session)
          k [:field (:field/form bf)]
          ps (params bf)]
      (when-not (get-in @sess [:kernels k])
        (gpu/compile! sess k (kernel :par (:field/form bf)) {:dtype :double}))
      (resident! sess n (alength ps))
      (gpu/upload! sess :field-ps ps)
      (gpu/upload! sess :field-geo (geometry w h opts))
      (gpu/invoke! sess k {"out" :field-out "ps" :field-ps "geo" :field-geo}
                   ;; a Long scalar binds as an OpenCL int (as in gpu-solver); :long fails -51
                   [{:type :int :value (int w)}] n)
      (blit! img (gpu/download sess :field-out)))))

(defmethod render/field-raster :gpu [k img shader opts]
  (gpu-field! img (form-of k shader) opts))
