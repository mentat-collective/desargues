(ns desargues.player.raster
  "PROTOCOL stratum: finished pixels from a producer thread to a host's tick.

   A producer (a Manim render thread) fills a buffer and publishes it into a
   newest-wins SLOT; the host's IClock tick takes whatever is newest and hands
   it to IRaster/-present-raster!. The producer never waits for the UI: a
   frame the tick did not take before the next one was published is dropped
   (counted). Buffers are recycled (at most three live: one being written,
   one pending, one being uploaded), so steady playback allocates nothing.

   Sections:
   - ports: IWallClock (time, injectable)
   - value objects: Raster and its malli schema
   - the slot: acquire!/publish! (producer), take!/release! (consumer)
   - the presenter: one tick -> at most one upload, timed on the UI thread"
  (:require [desargues.player.protocols :as p]
            [malli.core :as m]))

;; ── ports ────────────────────────────────────────────────────────────────

(defprotocol IWallClock
  "Monotonic time, injectable so the presenter's timing is testable."
  (-now-ns [clock] "Monotonic nanoseconds."))

(defrecord SystemClock []
  IWallClock
  (-now-ns [_] (System/nanoTime)))

(defn system-clock [] (->SystemClock))

;; ── value objects ────────────────────────────────────────────────────────

(defrecord Raster [bytes w h n t-ns])

(def RasterSchema
  [:map
   [:bytes bytes?]
   [:w pos-int?]
   [:h pos-int?]
   [:n nat-int?]
   [:t-ns int?]])

(defn frame-bytes
  "Bytes in a w x h 4-channel 8-bit image."
  ^long [w h]
  (* 4 (long w) (long h)))

(defn raster?
  "Is `x` a well-formed Raster holding exactly w*h*4 bytes?"
  [x]
  (and (m/validate RasterSchema x)
       (= (alength ^bytes (:bytes x)) (frame-bytes (:w x) (:h x)))))

;; ── the slot ─────────────────────────────────────────────────────────────

(defn slot
  "A fresh, empty newest-wins slot."
  []
  (atom {:pending nil :free [] :published 0 :overwritten 0 :taken 0 :allocated 0}))

(defn- sized? [len ^bytes bs] (= (long len) (alength bs)))

(defn acquire!
  "Producer: a byte array of w*h*4 to fill, a recycled one when free."
  ^bytes [slot w h]
  (let [len (frame-bytes w h)
        [old _] (swap-vals! slot
                            (fn [s]
                              (let [free (filterv (partial sized? len) (:free s))]
                                (if (seq free)
                                  (assoc s :free (pop free))
                                  (-> s (update :allocated inc) (assoc :free []))))))
        free (filterv (partial sized? len) (:free old))]
    (if (seq free) (peek free) (byte-array len))))

(defn publish!
  "Producer: make `bs` (from acquire!) the newest frame `n`. A frame still
   pending is dropped and its buffer recycled. Never blocks, never waits."
  [slot ^bytes bs w h n t-ns]
  (swap! slot (fn [{:keys [pending] :as s}]
                (cond-> (-> s
                            (assoc :pending (->Raster bs w h n t-ns))
                            (update :published inc))
                  pending (-> (update :overwritten inc)
                              (update :free conj (:bytes pending))))))
  nil)

(defn take!
  "Consumer: the newest pending Raster (owned by the caller until release!),
   or nil when nothing was published since the last take!."
  [slot]
  (let [[old _] (swap-vals! slot (fn [s] (if (:pending s)
                                           (-> s (assoc :pending nil) (update :taken inc))
                                           s)))]
    (:pending old)))

(defn release!
  "Consumer: hand a taken Raster's buffer back for reuse."
  [slot r]
  (when r (swap! slot update :free conj (:bytes r)))
  nil)

(defn counts
  "{:published :overwritten :taken :allocated :pending?} of `slot`."
  [slot]
  (let [s @slot]
    (assoc (select-keys s [:published :overwritten :taken :allocated])
           :pending? (some? (:pending s)))))

;; ── the presenter ────────────────────────────────────────────────────────

(defn presenter
  "A presenter of `slot` on `host` (an IRaster), timed with `clock`."
  ([host slot] (presenter host slot (system-clock)))
  ([host slot clock]
   {:host host :slot slot :clock clock
    :stats (atom {:shown 0 :ticks 0 :present-ns [] :first-shown-ns nil :last-n nil})}))

(defn tick!
  "One host tick: upload the newest frame, if any, recording how long the
   upload took on this (the UI) thread. Returns the Raster shown, or nil."
  [{:keys [host slot clock stats]}]
  (swap! stats update :ticks inc)
  (when-let [r (take! slot)]
    (let [t0 (-now-ns clock)]
      (try
        (p/-present-raster! host (:bytes r) (:w r) (:h r))
        (finally (release! slot r)))
      (let [t1 (-now-ns clock)]
        (swap! stats (fn [s] (-> s
                                 (update :shown inc)
                                 (update :present-ns conj (- t1 t0))
                                 (assoc :last-n (:n r))
                                 (update :first-shown-ns #(or % t1))))))
      r)))

(defn report
  "Presenter stats merged with its slot's counts."
  [{:keys [slot stats]}]
  (merge (counts slot) @stats))
