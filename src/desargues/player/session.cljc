(ns desargues.player.session
  "PURE stratum of the player: a playback Session value and its transitions.

   A Session is {:source :t :playing? :speed :loop?}, where :source is any
   desargues.player.protocols/ISource (a recorded scene graph, an alexandria
   scene, a plot...). Every transition is a pure fn Session -> Session, so the
   host's clock (a JavaFX AnimationTimer, requestAnimationFrame, a GTK tick
   callback) only calls `tick` with elapsed seconds and presents
   `current-frame`. Host-free .cljc."
  (:require [desargues.player.protocols :as p]))

(defn session
  "A paused Session at t = 0 over `source` (an ISource)."
  ([source] (session source {}))
  ([source opts]
   (merge {:source source
           :t 0.0
           :playing? false
           :speed 1.0
           :loop? true}
          opts)))

(defn duration [s] (double (p/-duration (:source s))))

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
  "Session over a fresh `source`, keeping time (clamped) and the play state,
   so a parameter change rebuilds the source without a jump."
  [s source]
  (seek (assoc s :source source) (:t s)))

(defn current-frame
  "The Frame the Session shows now."
  [s]
  (p/-frame-at (:source s) (:t s)))
