(ns desargues.wallpaper.loop
  "Pure layer of wallpaper loops: seamless time.

   A loop is a picture that is a function of a phase u in [0, 1). Every
   motion of a loop is built from functions periodic in u, so the picture at
   u = 1 is the picture at u = 0 and the video wraps without a seam. The
   frames of an N-frame loop are sampled at u = i / N for i in [0, N): the
   frame that would come after the last one is the first one.

   The helpers here are the periodic building blocks:

     (wave u)            cos 2πu mapped to [0, 1]; 1 at u = 0
     (osc u lo hi)       oscillates lo..hi..lo over one loop
     (pulse u)           0 at the ends, 1 at u = 1/2, flat (C1) at both ends
     (window u a b)      0 outside [a, b], a smooth bump of height 1 inside
     (circle u r)        [x y] on the circle of radius r, once around
     (shift u delay)     u delayed by `delay` loops, wrapped back into [0, 1)

   and `phases` lists the sample phases of an N-frame loop.")

(def ^:const tau (* 2.0 Math/PI))

(defn phases
  "The phases u = i/n, i in [0, n), of an `n`-frame loop."
  [n]
  (mapv #(/ (double %) (double n)) (range n)))

(defn frame-count
  "Frames in a loop of `seconds` at `fps` (at least 1)."
  [fps seconds]
  (max 1 (long (Math/round (* (double fps) (double seconds))))))

(defn frac
  "x mod 1, in [0, 1)."
  ^double [^double x]
  (- x (Math/floor x)))

(defn shift
  "Phase `u` delayed by `delay` loops, wrapped back into [0, 1)."
  ^double [^double u ^double delay]
  (frac (- u delay)))

(defn wave
  "(1 + cos 2πu) / 2: 1 at u = 0 (and 1), 0 at u = 1/2."
  ^double [^double u]
  (* 0.5 (+ 1.0 (Math/cos (* tau u)))))

(defn osc
  "Oscillates from `lo` at u = 0 to `hi` at u = 1/2 and back."
  ^double [^double u ^double lo ^double hi]
  (+ lo (* (- hi lo) (- 1.0 (wave u)))))

(defn smoothstep
  "Hermite smoothstep of x clamped to [0, 1]."
  ^double [^double x]
  (let [x (max 0.0 (min 1.0 x))]
    (* x x (- 3.0 (* 2.0 x)))))

(defn window
  "0 for u outside [a, b]; inside, a bump rising smoothly from 0 to 1 at the
   middle of [a, b] and back. Flat at both edges, so a motion gated by a
   window starts and stops without a jolt."
  ^double [^double u ^double a ^double b]
  (if (or (<= u a) (>= u b))
    0.0
    (let [s (/ (- u a) (- b a))]
      (* 0.5 (- 1.0 (Math/cos (* tau s)))))))

(defn pulse
  "A bump over the whole loop: 0 at u = 0 and 1, 1 at u = 1/2."
  ^double [^double u]
  (window (frac u) 0.0 1.0))

(defn circle
  "[x y] on the circle of radius `r` about the origin, at angle 2πu (plus
   `phase` turns)."
  ([u r] (circle u r 0.0))
  ([u r phase]
   (let [a (* tau (+ (double u) (double phase)))]
     [(* (double r) (Math/cos a)) (* (double r) (Math/sin a))])))

(defn periodic?
  "True when (f 0) and (f 1) agree within `eps`: the check every loop's
   motions should pass. `f` returns a number or a vector of numbers."
  ([f] (periodic? f 1e-9))
  ([f eps]
   (let [a (f 0.0) b (f 1.0)
         a (if (number? a) [a] a)
         b (if (number? b) [b] b)]
     (and (= (count a) (count b))
          (every? (fn [[x y]] (<= (Math/abs (- (double x) (double y))) eps))
                  (map vector a b))))))
