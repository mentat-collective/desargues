(ns desargues.player.svg-path
  "PURE stratum: an SVG path `d` string as absolute drawing segments.

   Hosts whose toolkit has no SVG-path parser of its own (java.awt.geom, a
   wgpu tessellator, a terminal) build their path from these segments:

     [:move x y] [:line x y] [:quad x1 y1 x y] [:cubic x1 y1 x2 y2 x y] [:close]

   Every command of the SVG path grammar is accepted, relative or absolute
   (M L H V C S Q T A Z). Smooth curves are made explicit and elliptical arcs
   become cubic Beziers, so a consumer handles four segment kinds. Coordinates
   come back in whatever units `d` used (scene units, y up, for the player).
   Host-free .cljc: no interop beyond Math functions JS shares.

   Limitation: arc flags must be separated from what follows them
   (\"a1 1 0 0 1 2 2\", not the compact \"a1 1 0 012 2\")."
  (:require [clojure.string :as str]))

(def ^:private token-re
  #"[MmLlHhVvCcSsQqTtAaZz]|[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?")

(def ^:private arity
  "Numbers each (upper-case) command consumes per repetition."
  {"M" 2 "L" 2 "H" 1 "V" 1 "C" 6 "S" 4 "Q" 4 "T" 2 "A" 7 "Z" 0})

(defn- upper [c] (str/upper-case c))
(defn- relative? [c] (not= c (upper c)))
(defn- letter? [tok] (contains? arity (upper tok)))

(defn tokens
  "The command letters and number strings of `d`, in order."
  [d]
  (re-seq token-re (str d)))

(defn commands
  "[[letter [numbers...]] ...] with implicit repetitions split out, so every
   entry carries exactly its command's arity. A repeated moveto is a lineto."
  [toks]
  (loop [toks toks cmd nil out []]
    (if-let [tok (first toks)]
      (if (letter? tok)
        (if (zero? (arity (upper tok)))
          (recur (rest toks) tok (conj out [tok []]))
          (recur (rest toks) tok out))
        (let [_    (when-not cmd
                     (throw (ex-info "SVG path: number before any command" {:token tok})))
              n    (arity (upper cmd))
              nums (mapv parse-double (take n toks))]
          (when (or (zero? n) (< (count nums) n))
            (throw (ex-info "SVG path: wrong number count" {:command cmd :numbers nums})))
          (recur (drop n toks)
                 (case cmd "M" "L" "m" "l" cmd)
                 (conj out [cmd nums]))))
      out)))

;; ── Elliptical arc -> cubics (SVG 1.1 implementation notes, F.6.5) ────────

(defn- angle [ux uy vx vy]
  (Math/atan2 (- (* ux vy) (* uy vx)) (+ (* ux vx) (* uy vy))))

(defn- arc->cubics
  "Segments drawing the SVG arc from [x0 y0] to [x y]: cubics, one per
   quarter turn at most; a line when a radius is zero; nothing when the
   endpoints coincide."
  [x0 y0 rx ry phi-deg large? sweep? x y]
  (let [rx (Math/abs (double rx)) ry (Math/abs (double ry))]
    (cond
      (and (== x0 x) (== y0 y)) []
      (or (zero? rx) (zero? ry)) [[:line x y]]
      :else
      (let [phi  (* (double phi-deg) (/ Math/PI 180.0))
            cp   (Math/cos phi) sp (Math/sin phi)
            dx   (/ (- x0 x) 2.0) dy (/ (- y0 y) 2.0)
            x1p  (+ (* cp dx) (* sp dy))
            y1p  (+ (* (- sp) dx) (* cp dy))
            lam  (+ (/ (* x1p x1p) (* rx rx)) (/ (* y1p y1p) (* ry ry)))
            [rx ry] (if (> lam 1.0) [(* rx (Math/sqrt lam)) (* ry (Math/sqrt lam))] [rx ry])
            num  (- (* rx rx ry ry) (* rx rx y1p y1p) (* ry ry x1p x1p))
            den  (+ (* rx rx y1p y1p) (* ry ry x1p x1p))
            k    (* (if (= large? sweep?) -1.0 1.0) (Math/sqrt (max 0.0 (/ num den))))
            cxp  (* k (/ (* rx y1p) ry))
            cyp  (* k (- (/ (* ry x1p) rx)))
            cx   (+ (* cp cxp) (- (* sp cyp)) (/ (+ x0 x) 2.0))
            cy   (+ (* sp cxp) (* cp cyp) (/ (+ y0 y) 2.0))
            ux   (/ (- x1p cxp) rx) uy (/ (- y1p cyp) ry)
            vx   (/ (- (- x1p) cxp) rx) vy (/ (- (- y1p) cyp) ry)
            th1  (angle 1.0 0.0 ux uy)
            dth  (double (let [d (angle ux uy vx vy)]
                           (cond (and (not sweep?) (pos? d)) (- d (* 2 Math/PI))
                                 (and sweep? (neg? d))       (+ d (* 2 Math/PI))
                                 :else d)))
            n    (max 1 (long (Math/ceil (/ (Math/abs dth) (/ Math/PI 2.0)))))
            step (/ dth n)
            t    (* (/ 4.0 3.0) (Math/tan (/ step 4.0)))
            pt   (fn [th] (let [c (Math/cos th) s (Math/sin th)]
                            [(+ cx (* rx c cp) (- (* ry s sp)))
                             (+ cy (* rx c sp) (* ry s cp))]))
            dpt  (fn [th] (let [c (Math/cos th) s (Math/sin th)]
                            [(- (- (* rx s cp)) (* ry c sp))
                             (+ (- (* rx s sp)) (* ry c cp))]))]
        (mapv (fn [i]
                (let [a (+ th1 (* i step)) b (+ a step)
                      [ax ay] (pt a) [bx by] (pt b)
                      [dax day] (dpt a) [dbx dby] (dpt b)]
                  [:cubic (+ ax (* t dax)) (+ ay (* t day))
                          (- bx (* t dbx)) (- by (* t dby))
                          bx by]))
              (range n))))))

;; ── Normalisation ──────────────────────────────────────────────────────────

(defn- step
  "Fold one command into {:segs :x :y :sx :sy :ctrl}. :ctrl is the last
   control point and the kind of curve that left it (for S and T)."
  [{:keys [x y sx sy ctrl] :as st} [c nums]]
  (let [rel?    (relative? c)
        ax      (fn [v] (if rel? (+ x v) v))
        ay      (fn [v] (if rel? (+ y v) v))
        reflect (fn [kind]
                  (if (= kind (:kind ctrl))
                    [(- (* 2 x) (:x ctrl)) (- (* 2 y) (:y ctrl))]
                    [x y]))
        emit    (fn [seg nx ny ctrl']
                  (-> st (update :segs conj seg) (assoc :x nx :y ny :ctrl ctrl')))]
    (case (upper c)
      "M" (let [[a b] nums nx (ax a) ny (ay b)]
            (-> st (update :segs conj [:move nx ny])
                (assoc :x nx :y ny :sx nx :sy ny :ctrl nil)))
      "L" (let [[a b] nums] (emit [:line (ax a) (ay b)] (ax a) (ay b) nil))
      "H" (let [[a] nums nx (ax a)] (emit [:line nx y] nx y nil))
      "V" (let [[b] nums ny (ay b)] (emit [:line x ny] x ny nil))
      "C" (let [[x1 y1 x2 y2 a b] nums]
            (emit [:cubic (ax x1) (ay y1) (ax x2) (ay y2) (ax a) (ay b)] (ax a) (ay b)
                  {:kind :cubic :x (ax x2) :y (ay y2)}))
      "S" (let [[x2 y2 a b] nums [x1 y1] (reflect :cubic)]
            (emit [:cubic x1 y1 (ax x2) (ay y2) (ax a) (ay b)] (ax a) (ay b)
                  {:kind :cubic :x (ax x2) :y (ay y2)}))
      "Q" (let [[x1 y1 a b] nums]
            (emit [:quad (ax x1) (ay y1) (ax a) (ay b)] (ax a) (ay b)
                  {:kind :quad :x (ax x1) :y (ay y1)}))
      "T" (let [[a b] nums [x1 y1] (reflect :quad)]
            (emit [:quad x1 y1 (ax a) (ay b)] (ax a) (ay b)
                  {:kind :quad :x x1 :y y1}))
      "A" (let [[rx ry phi large sweep a b] nums
                nx (ax a) ny (ay b)]
            (-> st
                (update :segs into (arc->cubics x y rx ry phi (not (zero? large))
                                                (not (zero? sweep)) nx ny))
                (assoc :x nx :y ny :ctrl nil)))
      "Z" (-> st (update :segs conj [:close]) (assoc :x sx :y sy :ctrl nil)))))

(defn segments
  "The absolute segments drawing SVG path string `d` (see the ns doc)."
  [d]
  (:segs (reduce step {:segs [] :x 0.0 :y 0.0 :sx 0.0 :sy 0.0 :ctrl nil}
                 (commands (tokens d)))))
