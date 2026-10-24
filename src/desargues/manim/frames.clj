(ns desargues.manim.frames
  "Manim frame hook: every cairo frame reaches a Clojure IFrameSink, with
   no encoding and no file.

   Manim's CairoRenderer.add_frame calls SceneFileWriter.write_frame(frame,
   num_frames), which drops the frame when write_to_movie is off. `install!`
   wraps write_frame (class level) so the sink sees the frame first; a held
   frame (num_frames k, a wait) is delivered k times, one per index.
   `uninstall!` restores the original method.

   Crossing: by default the numpy RGBA buffer [h w 4] reaches the sink as a
   ZERO-COPY dtype-next tensor (same address as numpy), valid only during the
   call. :copy? true hands the sink a JVM-heap clone instead. :crossing
   :address passes the buffer address and shape instead of the array, and
   the JVM keeps one native view per buffer (`address-view`): with the
   surface-reuse patch Manim hands over the same Camera buffer every frame.

   Sections:
   - pure pacing math: frame <-> time at an fps, wall-clock schedule
   - sinks: NullSink (bench), CollectingSink (tests), fns are sinks
   - the Python hook: install!/uninstall!, render-live!"
  (:require [desargues.manim.frame-protocols :as fp]
            [libpython-clj2.python :as py]
            [libpython-clj2.python.base :as py-base]
            [tech.v3.datatype :as dt]
            [tech.v3.tensor :as dtt]
            [libpython-clj2.python.np-array]
            [tech.v3.datatype.native-buffer :as nb]))

;; ============================================================================
;; Pure: frame pacing
;; ============================================================================

(defn frame-time
  "Scene time (s) of frame `n` at `fps`: n/fps. Frame 0 is t=0."
  ^double [n fps]
  (/ (double n) (double fps)))

(defn frame-index
  "Index of the frame showing at scene time `t` (s) at `fps`: floor(t*fps).
   A t a hair below a frame boundary (float noise, 1e-9 frames) rounds up."
  ^long [t fps]
  (long (Math/floor (+ (* (double t) (double fps)) 1e-9))))

(defn frame-count
  "Frames Manim emits for an animation of `run-time` s at `fps`: the length of
   np.arange(0, run_time, 1/fps), i.e. ceil(run-time / (1/fps)) in doubles,
   0 for run-time <= 0."
  ^long [run-time fps]
  (let [x (/ (double run-time) (/ 1.0 (double fps)))]
    (if (pos? x) (long (Math/ceil x)) 0)))

(defn held-frame-count
  "Frames Manim emits for a frozen frame (a wait with nothing moving) of
   `duration` s at `fps`: int(duration / (1/fps)), as freeze_current_frame
   does, so it truncates where `frame-count` rounds up (0.5 s @15 -> 7, not 8)."
  ^long [duration fps]
  (max 0 (long (/ (double duration) (/ 1.0 (double fps))))))

(defn frame-due-ms
  "Wall-clock ms at which frame `n` is due, for playback started at `start-ms`."
  ^double [start-ms n fps]
  (+ (double start-ms) (* 1000.0 (frame-time n fps))))

(defn frame-lag
  "How many frames playback is behind at `now-ms` when frame `n` is next
   (started at `start-ms`): frame-index(now) - n, never negative. A presenter
   drops that many to catch up, or sleeps when it is 0 and the frame is early."
  ^long [start-ms now-ms n fps]
  (max 0 (- (frame-index (/ (- (double now-ms) (double start-ms)) 1000.0) fps)
            (long n))))

;; ============================================================================
;; Sinks
;; ============================================================================

(defrecord NullSink []
  fp/IFrameSink
  (-frame! [_ _ _ _ _] nil))

(defn null-sink
  "A sink that drops every frame: the crossing cost alone, for benches."
  []
  (->NullSink))

(defrecord CollectingSink [state]
  fp/IFrameSink
  (-frame! [_ rgba w h n]
    (swap! state (fn [s]
                   (-> s
                       (update :count inc)
                       (update :indices conj n)
                       (assoc :w w :h h
                              :last (dt/clone rgba)
                              :first-ns (or (:first-ns s) (System/nanoTime))))))))

(defn collecting-sink
  "A sink keeping the frame count, every index, the size, a CLONE of the last
   frame and the System/nanoTime of the first delivery. Read with `collected`."
  []
  (->CollectingSink (atom {:count 0 :indices [] :w nil :h nil :last nil :first-ns nil})))

(defn collected
  "Snapshot of a CollectingSink: {:count :indices :w :h :last :first-ns}."
  [^CollectingSink sink]
  @(:state sink))

(extend-protocol fp/IFrameSink
  clojure.lang.Fn
  (-frame! [f rgba w h n] (f rgba w h n)))

(defn frame!
  "Deliver one frame to `sink` (the protocol call, for adapters and tests)."
  [sink rgba w h n]
  (fp/-frame! sink rgba w h n))

;; ============================================================================
;; The Python hook
;; ============================================================================

(def ^:private shim-source
  "import numpy as _np
import manim.scene.scene_file_writer as _sfw

_U8 = _np.dtype(_np.uint8)

def _address(frame):
    if (frame.dtype == _U8 and frame.ndim == 3 and frame.shape[2] == 4
            and frame.flags.c_contiguous):
        return frame.__array_interface__['data'][0]
    return 0

def desargues_frames_install(cb, by_address=False):
    W = _sfw.SceneFileWriter
    if not hasattr(W, '_desargues_orig_write_frame'):
        W._desargues_orig_write_frame = W.write_frame
    orig = W._desargues_orig_write_frame
    if by_address:
        def deliver(frame):
            cb(frame, _address(frame), frame.shape[0], frame.shape[1])
    else:
        deliver = cb
    def write_frame(self, frame_or_renderer, num_frames=1):
        frame = frame_or_renderer
        if not isinstance(frame, _np.ndarray):
            frame = frame_or_renderer.get_frame()
        for _ in range(num_frames):
            deliver(frame)
        return orig(self, frame_or_renderer, num_frames=num_frames)
    W.write_frame = write_frame
    return True

def desargues_frames_uninstall():
    W = _sfw.SceneFileWriter
    if hasattr(W, '_desargues_orig_write_frame'):
        W.write_frame = W._desargues_orig_write_frame
        del W._desargues_orig_write_frame
        return True
    return False

def desargues_frames_installed():
    return hasattr(_sfw.SceneFileWriter, '_desargues_orig_write_frame')

def desargues_frames_drive(cb, frame, k, by_address=False):
    import time
    t0 = time.perf_counter_ns()
    if by_address:
        for _ in range(k):
            cb(frame, _address(frame), frame.shape[0], frame.shape[1])
    else:
        for _ in range(k):
            cb(frame)
    return time.perf_counter_ns() - t0
")

(def ^:private shims
  (delay (:globals (py/run-simple-string shim-source))))

(defn- shim [nm] (get @shims nm))

(defn address-view
  "A [h w 4] uint8 tensor over native memory at `addr`, reusing the view in
   `cache` (an atom of {:addr :h :w :t}) while the buffer key is unchanged:
   a fixed numpy buffer is wrapped once, not once per frame. The view is
   valid only while the caller holds the buffer it names."
  [cache ^long addr ^long h ^long w]
  (let [c @cache]
    (if (and c (== addr (long (:addr c))) (== h (long (:h c))) (== w (long (:w c))))
      (:t c)
      (let [t (-> (nb/wrap-address addr (* h w 4) :uint8 :little-endian nil)
                  (dtt/reshape [h w 4]))]
        (reset! cache {:addr addr :h h :w w :t t})
        t))))

(defn- deliver!
  [sink counter copy? t w h]
  (let [n (dec (long (swap! counter inc)))]
    (fp/-frame! sink (if copy? (dt/clone t) t) w h n)
    nil))

(defn frame-callback
  "The Python callable the hook calls once per frame index: views the numpy
   frame as a dtype-next tensor (zero-copy, or cloned with `copy?`) and hands
   it to `sink` with its width, height and a running index from `counter`
   (an atom of the next index). `crossing` :address takes the frame's buffer
   address and shape from Python and reuses one native view per buffer
   (`address-view`); :tensor (default) wraps each frame with as-tensor."
  ([sink counter copy?] (frame-callback sink counter copy? :tensor))
  ([sink counter copy? crossing]
   (let [opts {:arg-converter py-base/as-jvm
               :result-converter (fn [_] (py/->python nil))
               :name "desargues_frame_sink"}]
     (if (= :address crossing)
       (let [cache (atom nil)]
         (py/make-callable
          (fn [frame addr h w]
            (let [addr (long addr) h (long h) w (long w)]
              (if (zero? addr)
                (deliver! sink counter copy? (dtt/as-tensor frame) w h)
                (deliver! sink counter copy? (address-view cache addr h w) w h))))
          opts))
       (py/make-callable
        (fn [frame]
          (let [t  (dtt/as-tensor frame)
                sh (dt/shape t)]
            (deliver! sink counter copy? t (long (nth sh 1)) (long (nth sh 0)))))
        opts)))))

(defonce ^:private hook (atom nil))

(defn installed?
  "Is the write_frame wrapper in place?"
  []
  (boolean (py/->jvm ((shim "desargues_frames_installed")))))

(defn install!
  "Wrap SceneFileWriter.write_frame so every frame reaches `sink` before Manim
   writes (or, with write_to_movie off, drops) it. Replaces any sink already
   installed; the frame index restarts at 0. Options: :copy? (default false)
   clones each frame onto the JVM heap; :crossing :tensor (default) or
   :address (see `frame-callback`). Returns the hook {:sink :counter}."
  ([sink] (install! sink {}))
  ([sink {:keys [copy? crossing] :or {copy? false crossing :tensor}}]
   (let [counter (atom 0)
         cb      (frame-callback sink counter copy? crossing)]
     ((shim "desargues_frames_install") cb (= :address crossing))
     (reset! hook {:sink sink :counter counter :callback cb :copy? copy? :crossing crossing}))))

(defn uninstall!
  "Restore SceneFileWriter.write_frame. Idempotent; true when a hook was removed."
  []
  (reset! hook nil)
  (boolean (py/->jvm ((shim "desargues_frames_uninstall")))))

(defn frames-delivered
  "Frames delivered by the current hook (0 when none is installed)."
  []
  (if-let [{:keys [counter]} @hook] @counter 0))

;; ----------------------------------------------------------------------------
;; Live render: frames to the sink, nothing encoded or written
;; ----------------------------------------------------------------------------

(def live-config
  "Manim config for a sink-only render: no movie, no PNG, no last-frame
   save (it would skip every animation), no hashing of play calls (the cache
   is a file cache; with no file it is pure cost), no progress bar."
  {"write_to_movie"   false
   "save_last_frame"  false
   "disable_caching"  true
   "progress_bar"     "none"})

(defn- manim-config [] (py/get-attr (py/import-module "manim") "config"))

(defn with-live-config*
  "Run `f` with `live-config` (plus `overrides`, e.g. {\"quality\"
   \"low_quality\"}) applied to manim.config, restoring the previous values."
  [overrides f]
  (let [cfg  (manim-config)
        kvs  (merge live-config overrides)
        old  (into {} (map (fn [k] [k (py/get-item cfg k)])) (keys kvs))]
    (try
      (doseq [[k v] kvs] (py/set-item! cfg k v))
      (f)
      (finally
        (doseq [[k v] old] (py/set-item! cfg k v))))))

(defn render-live!
  "Render `scene` (a Scene instance, or a zero-arg fn returning one, called
   inside the live config so the renderer sees it) with every frame streamed
   to `sink` and nothing encoded or written. The hook is uninstalled after.
   Options: :quality (manim quality string, default \"low_quality\" = 480p15),
   :copy?, :crossing (see `install!`). Returns the number of frames delivered."
  ([scene sink] (render-live! scene sink {}))
  ([scene sink {:keys [quality copy? crossing] :or {quality "low_quality" crossing :tensor}}]
   (with-live-config* {"quality" quality}
     (fn []
       (let [{:keys [counter]} (install! sink {:copy? copy? :crossing crossing})]
         (try
           (let [s (if (fn? scene) (scene) scene)]
             (py/call-attr s "render")
             @counter)
           (finally (uninstall!))))))))

(defn crossing-ns
  "Python-measured ns to push `frame` (numpy) through the hook callback for
   `sink` `k` times: the per-frame Python -> JVM crossing cost, render excluded.
   :crossing as in `install!`."
  [frame sink k {:keys [copy? crossing] :or {crossing :tensor}}]
  (let [cb (frame-callback sink (atom 0) copy? crossing)]
    (py/->jvm ((shim "desargues_frames_drive") cb frame k (= :address crossing)))))
