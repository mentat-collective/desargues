(ns desargues.player.session
  "PURE stratum of the player: a playback Session value and its transitions.

   A Session is {:graph :sched :t :playing? :speed :loop?}. Every transition is
   a pure fn Session -> Session, so the host's clock (a JavaFX AnimationTimer,
   requestAnimationFrame, a GTK tick callback) only has to call `tick` with
   elapsed seconds and present `current-frame`. Host-free .cljc."
  (:require [desargues.player.timeline :as timeline]))

(defn session
  "A paused Session at t = 0 over a recorded scene `graph`."
  ([graph] (session graph {}))
  ([graph opts]
   (merge {:graph graph
           :sched (timeline/schedule graph)
           :t 0.0
           :playing? false
           :speed 1.0
           :loop? true}
          opts)))

(defn duration [s] (get-in s [:sched :duration]))

(defn seek
  "Session moved to time `t`, clamped to [0 duration]."
  [s t]
  (assoc s :t (-> (double t) (max 0.0) (min (duration s)))))

(defn play [s] (assoc s :playing? true))
(defn pause [s] (assoc s :playing? false))
(defn toggle [s] (update s :playing? not))

(defn tick
  "Session advanced by `dt` wall seconds when playing. At the end it wraps
   (loop?) or pauses on the last frame."
  [s dt]
  (if-not (:playing? s)
    s
    (let [t (+ (:t s) (* (:speed s) (double dt)))
          d (duration s)]
      (cond
        (< t d)     (assoc s :t t)
        (:loop? s)  (assoc s :t (if (pos? d) (mod t d) 0.0))
        :else       (assoc s :t d :playing? false)))))

(defn reload
  "Session over a freshly recorded `graph`, keeping time (clamped) and the
   play state, so a parameter change re-records without a jump."
  [s graph]
  (seek (assoc s :graph graph :sched (timeline/schedule graph)) (:t s)))

(defn current-frame
  "The Frame the Session shows now."
  [s]
  (timeline/frame-at (:graph s) (:sched s) (:t s)))
