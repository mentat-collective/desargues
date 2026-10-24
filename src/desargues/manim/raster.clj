(ns desargues.manim.raster
  "Manim frames to any IRaster host (desargues.player.protocols), live.

   An IFrameSink (desargues.manim.frame-protocols) that, ON THE RENDER
   THREAD, copies the zero-copy numpy frame into a recycled buffer in the
   host's -raster-layout and publishes it into a newest-wins slot
   (desargues.player.raster). The host's IClock tick uploads the newest
   frame, so the UI thread only uploads. Optional pacing parks the render
   thread until a frame's wall-clock due time (scene fps), which is a wait on
   time, never on the UI.

   Sections:
   - pure pixel conversion (int words: RGBA bytes read little-endian)
   - RasterSink (IFrameSink) with optional pacing
   - play-live! (boundary): render a scene into a host, paced by its IClock"
  (:require [desargues.manim.frame-protocols :as fp]
            [desargues.manim.frames :as fr]
            [desargues.player.protocols :as p]
            [desargues.player.raster :as raster]
            [tech.v3.datatype :as dt]
            [tech.v3.datatype.native-buffer :as nb]
            [desargues.manim.patches :as mp])
  (:import [java.nio ByteBuffer ByteOrder]
           [java.util.concurrent.locks LockSupport]))

;; ── pure: pixel conversion ──────────────────────────────────────────────
;; An RGBA pixel read as a little-endian int is 0xAABBGGRR; a BGRA one is
;; 0xAARRGGBB, so RGBA -> BGRA swaps the low and the third byte.

(defn rgba->bgra-pre
  "One RGBA word (0xAABBGGRR) as a premultiplied BGRA word (0xAARRGGBB)."
  ^long [^long x]
  (let [a (bit-and (unsigned-bit-shift-right x 24) 0xFF)
        r (bit-and x 0xFF)
        g (bit-and (unsigned-bit-shift-right x 8) 0xFF)
        b (bit-and (unsigned-bit-shift-right x 16) 0xFF)]
    (if (== a 255)
      (bit-or (bit-shift-left a 24) (bit-shift-left r 16) (bit-shift-left g 8) b)
      (let [pm (fn ^long [^long c] (quot (+ (* a c) 127) 255))]
        (bit-or (bit-shift-left a 24) (bit-shift-left (pm r) 16)
                (bit-shift-left (pm g) 8) (pm b))))))

(defn rgba-pre->bgra-pre
  "One premultiplied RGBA word (0xAABBGGRR) as premultiplied BGRA
   (0xAARRGGBB): Manim's pixel_array is cairo's premultiplied buffer with red
   and blue swapped, so only the swap is left to undo."
  ^long [^long x]
  (bit-or (bit-and x 0xFF00FF00)
          (bit-shift-left (bit-and x 0xFF) 16)
          (bit-and (unsigned-bit-shift-right x 16) 0xFF)))

(defn convert-words!
  "Convert `src` Manim pixel words (premultiplied RGBA, read little-endian)
   in place to `layout` (:rgba is identity, :bgra-pre swaps red and blue).
   Returns src."
  ^ints [^ints src layout]
  (when (= :bgra-pre layout)
    (let [n (alength src)]
      (loop [i 0]
        (when (< i n)
          (let [x (aget src i)]
            (aset src i (unchecked-int
                         (bit-or (bit-and x (unchecked-int 0xFF00FF00))
                                 (bit-shift-left (bit-and x 0xFF) 16)
                                 (bit-and (unsigned-bit-shift-right x 16) 0xFF)))))
          (recur (inc i))))))
  src)

(defn words->bytes!
  "Write int words `src` into byte array `dst` little-endian. Returns dst."
  ^bytes [^ints src ^bytes dst]
  (-> (ByteBuffer/wrap dst) (.order ByteOrder/LITTLE_ENDIAN) .asIntBuffer (.put src))
  dst)

;; ── the sink ────────────────────────────────────────────────────────────

(defn- park-until! [^long due-ns]
  (loop []
    (let [left (- due-ns (System/nanoTime))]
      (when (pos? left) (LockSupport/parkNanos left) (recur)))))

(defn- fill-words!
  "Copy tensor `rgba` ([h w 4] uint8, zero-copy native or heap) into `ws`."
  [rgba ^ints ws]
  (if-let [nbuf (dt/as-native-buffer rgba)]
    (dt/copy! (nb/set-native-datatype nbuf :int32) ws)
    (let [bs (dt/->byte-array rgba)]
      (-> (ByteBuffer/wrap bs) (.order ByteOrder/LITTLE_ENDIAN) .asIntBuffer (.get ws))))
  ws)

(defn- copy-bytes!
  "Copy tensor `t` ([h w 4] uint8, zero-copy native or heap) byte for byte
   into `dst`: the frame is already in the host's layout."
  [t ^bytes dst]
  (if-let [nbuf (dt/as-native-buffer t)]
    (dt/copy! (nb/set-native-datatype nbuf :int8) dst)
    (let [^bytes bs (dt/->byte-array t)]
      (System/arraycopy bs 0 dst 0 (alength dst))))
  dst)

(defrecord RasterSink [slot layout src-layout fps scratch t0 conv-ns]
  fp/IFrameSink
  (-frame! [_ rgba w h n]
    (let [c0  (System/nanoTime)
          bs  (raster/acquire! slot w h)]
      (if (= src-layout layout)
        (copy-bytes! rgba bs)
        (let [len (* (long w) (long h))
              ^ints ws (let [^ints s @scratch]
                         (if (and s (= len (alength s))) s (reset! scratch (int-array len))))]
          (words->bytes! (convert-words! (fill-words! rgba ws) layout) bs)))
      (swap! conv-ns conj (- (System/nanoTime) c0))
      (when fps
        (let [start (or @t0 (reset! t0 (System/nanoTime)))]
          (park-until! (+ (long start) (long (* 1e9 (fr/frame-time n fps)))))))
      (raster/publish! slot bs w h n (System/nanoTime)))))

(defn raster-sink
  "A sink publishing every frame into `slot` in `layout`. :src-layout is the
   layout frames arrive in (default :rgba, Manim's own; :bgra-pre with the
   camera-bgra-pre patch): equal to `layout`, a frame is copied as is. :fps
   paces publication to scene time (frame n at first-frame + n/fps); nil
   publishes as fast as Manim renders."
  [slot layout {:keys [fps src-layout] :or {src-layout :rgba}}]
  (->RasterSink slot layout src-layout fps (atom nil) (atom nil) (atom [])))

;; ── boundary: play a scene into a host ──────────────────────────────────

(defn- set-frame-rate!
  "Set manim.config.frame_rate (after the quality preset, which resets it)."
  [fps]
  (let [import-module (requiring-resolve 'libpython-clj2.python/import-module)
        get-attr      (requiring-resolve 'libpython-clj2.python/get-attr)
        set-item!     (requiring-resolve 'libpython-clj2.python/set-item!)]
    (set-item! (get-attr (import-module "manim") "config") "frame_rate" (double fps))))

(defn native-layout
  "The layout cairo's ARGB32 surface has in memory on this host with the
   camera-bgra-pre patch in place: :bgra-pre on little-endian, nil otherwise
   (big-endian ARGB32 is ARGB bytes, no IRaster layout)."
  [byte-order]
  (when (= ByteOrder/LITTLE_ENDIAN byte-order) :bgra-pre))

(defn source-plan
  "Which source layout to render `layout` from: {:src-layout :patches}. A
   host wanting the native layout gets cairo's bytes as they are (the Camera
   patch `native-patch`); any other host gets Manim's RGBA, converted."
  [layout native native-patch native?]
  (if (and native? native (= layout native))
    {:src-layout native :patches [native-patch]}
    {:src-layout :rgba :patches []}))

(defn play-live!
  "Render `scene` (a zero-arg fn returning a Scene) on a fresh thread into
   IRaster `host`, shown on the host's IClock tick. Options: :quality
   (default \"low_quality\"), :frame-rate (overrides the preset's fps),
   :paced? (default true: publish frame n at first-frame + n/fps, a wait on
   time, never on the UI), :native? (default true: a host whose layout is
   cairo's native one gets frames with no conversion), :on-done (fn [report]).
   Returns {:presenter :sink :done (promise of the report)}."
  [host scene {:keys [quality frame-rate paced? native? on-done]
               :or {quality "low_quality" paced? true native? true}}]
  (let [slot (raster/slot)
        pres (raster/presenter host slot)
        fps  (or frame-rate ({"low_quality" 15 "medium_quality" 30 "high_quality" 60
                              "production_quality" 60} quality 15))
        layout (p/-raster-layout host)
        {:keys [src-layout patches]} (source-plan layout (native-layout (ByteOrder/nativeOrder))
                                                  mp/camera-bgra-pre native?)
        sink (raster-sink slot layout {:fps (when paced? fps) :src-layout src-layout})
        done (promise)
        t0   (System/nanoTime)]
    (p/-on-tick! host (fn [_dt] (raster/tick! pres)))
    (doto (Thread.
           (fn []
             (let [res (try {:frames (mp/with-patches* patches
                                       #(fr/render-live!
                                         (fn [] (when frame-rate (set-frame-rate! frame-rate))
                                           (scene))
                                         sink {:quality quality}))}
                            (catch Throwable t {:error t}))
                   ;; let the last frame reach the screen
                   _   (loop [k 0] (when (and (< k 50) (:pending? (raster/counts slot)))
                                     (Thread/sleep 5) (recur (inc k))))
                   rep (merge res (raster/report pres)
                              {:fps fps :start-ns t0 :end-ns (System/nanoTime)
                               :src-layout src-layout
                               :conv-ns @(:conv-ns sink)})]
               (p/-stop-ticks! host)
               (deliver done rep)
               (when on-done (on-done rep))))
           "desargues-live-render")
      (.setDaemon true)
      (.start))
    {:presenter pres :sink sink :done done}))
