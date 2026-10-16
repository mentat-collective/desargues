(ns desargues.player.calculus-demos
  "A 3D source for the stage: partial derivatives of z = f(x, y), fully
   interactive and animated.

   A select picks f (desargues.player.calculus-surfaces: Emmy derived f_x and
   f_y symbolically, once per surface); sliders, or dragging P on the
   surface or its shadow Q on the floor, move the point (x0, y0); toggles
   show the tangent plane, the x- and y-slices with their tangent lines, the
   gradient on the floor, the level curves and a wireframe; buttons step
   gradient ascent and reset. A readout shows f, df/dx and df/dy
   symbolically and their values at the point, updating with every change.

   Over time (the tour, `duration` seconds, looped by the session): the
   surface draws in, the level curves appear, the x-slice sweeps across the
   domain to y = y0 and shows its tangent line, then the y-slice, then the
   tangent plane and the gradient; the point travels along a circle or the
   gradient-ascent path with the tangent plane following it; the camera
   flies around (desargues.player.camera-path); with 'morph over time' the
   surface morphs into a second one and back. Every frame is a pure function
   of (params, t): `frame-at`. With the tour off the frame ignores t.

     (require '[desargues.player :as player] '[desargues.player.javafx3d :as fx3]
              '[desargues.player.calculus-demos :as cd])
     (def p (player/open-source! (fx3/host) cd/source-fn
                                 {:title \"partial derivatives\" :controls cd/controls}))

   Host-free .cljc."
  (:require [clojure.string :as str]
            [desargues.player.calculus-scene :as scene]
            [desargues.player.calculus-surfaces :as surfaces]
            [desargues.player.camera-path :as camera-path]
            [desargues.player.controls :as controls]
            [desargues.player.frame :as frame]
            [desargues.player.protocols :as p]))

(def duration
  "Seconds of the tour."
  20.0)

;; ── Timeline: what has appeared by time t (pure) ─────────────────────────

(defn ramp
  "0 before `a`, 1 after `b`, linear between."
  [t a b]
  (max 0.0 (min 1.0 (/ (- (double t) a) (- b a)))))

(defn- smooth [s] (* s s (- 3.0 (* 2.0 s))))

(def windows
  "Tour windows [start end] in seconds, one per reveal key."
  {:axes [0.0 0.6] :surface [0.0 2.5] :levels [2.5 3.5]
   :xsweep [3.5 6.0] :xtan [6.0 6.6] :ysweep [6.6 9.1] :ytan [9.1 9.7]
   :plane [9.7 10.5] :grad [10.2 11.0] :travel [11.0 19.0] :morph [12.0 19.0]})

(defn reveal
  "{key fraction} of every tour window at time `t`; all 1 (and :travel,
   :morph 0) with the tour off."
  [t tour?]
  (if tour?
    (into {} (for [[k [a b]] windows] [k (ramp t a b)]))
    (assoc (zipmap (keys windows) (repeat 1.0)) :travel 0.0 :morph 0.0)))

(defn morph-amount
  "The morph weight m at reveal `rv`: the :morph slider, raised towards 1 and
   back over the morph window when :auto-morph is on."
  [{:keys [morph auto-morph]} rv]
  (let [m    (double (or morph 0.0))
        bump (Math/sin (* Math/PI (double (:morph rv 0.0))))]
    (if auto-morph (+ m (* (- 1.0 m) bump bump)) m)))

(defn surface-at
  "The numeric surface {:f :fx :fy} for `params` at morph weight `m`."
  [{:keys [f morph-to]} m]
  (surfaces/mix (surfaces/surface f) (when (not= f morph-to) (surfaces/surface morph-to)) m))

(defn base-point [{:keys [x0 y0]}] [(double x0) (double y0)])

(defn point-at
  "Where P is at travel fraction `u`: the base point for :still, a circle
   about the origin through it for :circle, the gradient-ascent path from it
   for :ascent (on `surf`)."
  [{:keys [path] :as params} surf u]
  (let [[x y] (base-point params)
        u     (smooth (double u))]
    (case path
      :circle (let [r (max 0.6 (Math/sqrt (+ (* x x) (* y y))))
                    a (+ (Math/atan2 y x) (* 2.0 Math/PI u))]
                [(* r (Math/cos a)) (* r (Math/sin a))])
      :ascent (scene/along (scene/ascent-path surf [x y] {}) u)
      [x y])))

(def base-camera
  {:target [0.0 0.0 0.0] :distance 12.5 :yaw -1.05 :pitch 0.5 :projection :perspective})

(def fly-path
  "The tour's camera: one turn around the surface, rising and closing in."
  (camera-path/fly-around {:t0 0.0 :t1 duration :yaw (:yaw base-camera) :pitch 0.42
                           :distance 12.5 :turns 1.0 :lift 0.3 :zoom 1.5 :n 8}))

(defn camera-at
  "The camera at time `t` for `params`, aimed at the middle of the picture,
   height `zmid`, and backed off a little for a tall one (`height`)."
  [{:keys [fly tour]} t zmid height]
  (let [cam (if (and fly tour) (merge base-camera (camera-path/camera-at fly-path t)) base-camera)]
    (-> cam
        (assoc :target [0.0 0.0 (double zmid)])
        (update :distance + (* 1.2 (max 0.0 (- (double height) 4.5)))))))

;; ── Readout ───────────────────────────────────────────────────────────────

(defn fmt
  "`x` with two decimals, the same text on every host."
  [x]
  (let [c (Math/round (* 100.0 (Math/abs (double x))))
        s (str (quot c 100) "." (let [r (rem c 100)] (if (< r 10) (str "0" r) (str r))))]
    (if (and (neg? (double x)) (pos? c)) (str "-" s) s)))

(defn readout
  "The readout lines {:f :fx :fy :at} for `params`, numeric surface `surf`
   at morph weight `m` and point [x y]: the symbolic f and partials (Emmy's
   infix) with their values at the point."
  [{:keys [f morph-to]} surf m [x y]]
  (let [a     (surfaces/surface f)
        b     (surfaces/surface morph-to)
        mixed (and (pos? m) (not= f morph-to))
        infix (fn [s k] (str/replace (get-in s [:infix k]) "+ -" "- "))
        sym   (fn [k] (if mixed
                        (str "(1-m)(" (infix a k) ") + m(" (infix b k) ")")
                        (infix a k)))
        [gx gy] (scene/gradient surf [x y])]
    {:f  (str "f(x, y) = " (sym :f) (when mixed (str "    m = " (fmt m))))
     :fx (str "df/dx = " (sym :fx) "   = " (fmt gx))
     :fy (str "df/dy = " (sym :fy) "   = " (fmt gy))
     :at (str "at (" (fmt x) ", " (fmt y) "):  f = " (fmt ((:f surf) x y))
              "   |grad f| = " (fmt (Math/sqrt (+ (* gx gx) (* gy gy)))))}))

(defn- text [id s y color size]
  {:id id :node :text :text s :at [-6.95 y] :opacity 1.0 :draw 1.0
   :opts {:font-size size :color color :anchor "start"}})

(defn readout-shapes
  "The readout as 2D text shapes (an overlay on the 3D frame)."
  [lines]
  [(text "readout-f" (:f lines) 3.55 :white 26)
   (text "readout-fx" (:fx lines) 3.1 :red 24)
   (text "readout-fy" (:fy lines) 2.7 :green 24)
   (text "readout-at" (:at lines) 2.3 :grey 22)])

;; ── Frame ─────────────────────────────────────────────────────────────────

(defn statics
  "The part of the picture that does not depend on time, for `params` at
   morph weight `m`: the numeric surface, its top, the floor and the level
   curves. A source computes it once per parameter change (`source-fn`)."
  [params m]
  (let [surf        (surface-at params m)
        [zmin zmax] (scene/z-range surf 24)
        floor       (- zmin 0.6)]
    {:m m :surf surf :zmax zmax :floor floor
     :levels (when (:levels params) (scene/level-lines surf floor))}))

(defn frame-at
  "The demo frame at time `t` for `params` (pure). `st` (optional) is
   `statics` precomputed for the slider's morph weight; it is used whenever
   the frame's morph weight is that one."
  ([params t] (frame-at params t nil))
  ([{:keys [tour] :as params} t st]
   (let [rv     (reveal t tour)
         m      (morph-amount params rv)
         slider (double (or (:morph params) 0.0))
         {:keys [surf zmax floor levels]} (if (and st (== m (:m st))) st (statics params m))
         base   (if (== m slider) surf (surface-at params slider))
         pt     (point-at params base (:travel rv))
         [x y]  pt
         sweep  (fn [k target] (+ -2.0 (* (+ target 2.0) (smooth (double (k rv))))))]
     (assoc (frame/frame
             t
             (concat
              (scene/shapes {:surf surf :point pt :floor floor :zmax zmax :levels levels
                             :show (select-keys params [:tangent :xslice :yslice :gradient :levels :wire])
                             :slices {:x (sweep :xsweep y) :y (sweep :ysweep x)}
                             :reveal rv})
              (readout-shapes (readout params surf m pt))))
            :camera (camera-at params t (/ (+ floor zmax) 2.0) (- zmax floor))))))

(defrecord CalculusDemo [params st]
  p/ISource
  (-duration [_] duration)
  (-frame-at [_ t] (frame-at params t st)))

(defn source
  "The ISource for complete `params`, its statics computed once."
  [params]
  (->CalculusDemo params (statics params (double (or (:morph params) 0.0)))))

;; ── Controls ──────────────────────────────────────────────────────────────

(defn floor-z
  "The floor height for `params` (the surface at its slider morph)."
  [params]
  (- (first (scene/z-range (surface-at params (double (or (:morph params) 0.0))) 24)) 0.6))

(defn p-height
  "The height of P at the base point (x0, y0) for `params`."
  [params]
  (nth (scene/point-on (surface-at params (double (or (:morph params) 0.0))) (base-point params)) 2))

(defn- to-point [ps [x y]]
  (assoc ps :x0 (scene/clamp-domain x) :y0 (scene/clamp-domain y)))

(defn ascend
  "`params` after one gradient-ascent step from (x0, y0) (step 0.3), with the
   point held still so the step shows."
  [params]
  (let [surf    (surface-at params (double (or (:morph params) 0.0)))
        [x y]   (base-point params)
        [gx gy] (scene/gradient surf [x y])]
    (-> (to-point params [(+ x (* 0.3 gx)) (+ y (* 0.3 gy))])
        (assoc :path :still))))

(def reset-params
  "What the reset button restores."
  {:x0 1.0 :y0 0.5 :morph 0.0 :path :circle :auto-morph false})

(def controls
  "Widget and drag specs (desargues.player.controls, desargues.player.drag).
   P is dragged in the horizontal plane through it, Q on the floor; both
   write (x0, y0)."
  [{:id :f :kind :select :label "surface z = f(x, y)" :value :paraboloid
    :options (mapv (fn [k] {:value k :label (:label (surfaces/surface k))}) surfaces/order)}
   {:id :x0 :label "x0" :min -2.0 :max 2.0 :value 1.0}
   {:id :y0 :label "y0" :min -2.0 :max 2.0 :value 0.5}
   {:id :tangent :kind :toggle :label "tangent plane and normal" :value true}
   {:id :xslice :kind :toggle :label "x-slice y = y0 (slope df/dx)" :value true}
   {:id :yslice :kind :toggle :label "y-slice x = x0 (slope df/dy)" :value true}
   {:id :gradient :kind :toggle :label "gradient on the floor" :value true}
   {:id :levels :kind :toggle :label "level curves" :value true}
   {:id :wire :kind :toggle :label "wireframe" :value false}
   {:id :path :kind :select :label "the point travels" :value :circle
    :options [{:value :still :label "still"} {:value :circle :label "around a circle"}
              {:value :ascent :label "up the gradient"}]}
   {:id :morph-to :kind :select :label "morph into" :value :saddle
    :options (mapv (fn [k] {:value k :label (:label (surfaces/surface k))}) surfaces/order)}
   {:id :morph :label "morph m" :min 0.0 :max 1.0 :value 0.0}
   {:id :auto-morph :kind :toggle :label "morph over time" :value false}
   {:id :fly :kind :toggle :label "camera fly-around" :value true}
   {:id :tour :kind :toggle :label "animate (tour)" :value true}
   {:id :ascend :kind :button :label "step gradient ascent" :on-press ascend}
   {:id :reset :kind :button :label "reset" :set reset-params}
   {:id :drag-p :drag {:node "P" :->params (fn [ps p] (to-point ps p))
                       :plane (fn [ps] {:point [0.0 0.0 (p-height ps)] :normal [0.0 0.0 1.0]})}}
   {:id :drag-q :drag {:node "Q" :->params (fn [ps p] (to-point ps p))
                       :plane (fn [ps] {:point [0.0 0.0 (floor-z ps)] :normal [0.0 0.0 1.0]})}}])

(def defaults
  "Every control's starting value."
  (controls/defaults controls))

(defn source-fn
  "(fn [params] ISource) for desargues.player/open-source!."
  [params]
  (source (merge defaults params)))

(defn readout-text
  "The readout of `params` at time `t` as one string (what a check reads)."
  [params t]
  (->> (:shapes (frame-at (merge defaults params) t))
       (filter #(str/starts-with? (str (:id %)) "readout"))
       (map :text)
       (str/join "\n")))
