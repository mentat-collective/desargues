(ns desargues.player.timeline
  "PURE stratum of the player: a recorded scene graph, played at time t.

   Input is the EDN graph the RecordingBackend (desargues.scene.data) yields
   for ANY construct: {:nodes {id node} :steps [{:step :play :anims [...]}
   {:step :hold :seconds s}]}. The same construct that renders an mp4 through
   Manim therefore plays live here: one set of objects, many backends.

   Two pure steps:
     schedule  graph -> {:duration d :anims [{:start :end :anim} ...]}
               (groups and staggers flattened; play steps run back to back)
     frame-at  graph schedule t -> Frame (desargues.player.frame)

   Each animation kind is one `apply-anim` method (OCP): teaching the player a
   new kind is a defmethod, never an edit here. Unknown kinds are ignored, so
   a renderer still shows every object. Host-free .cljc."
  (:require [desargues.player.frame :as frame]))

;; ── Scheduling ──────────────────────────────────────────────────────────────

(defn run-time
  "Seconds `anim` lasts."
  [anim]
  (double (get-in anim [:opts :run-time] frame/default-run-time)))

(defn- lag-ratio [anim] (double (get-in anim [:opts :lag-ratio] 0.0)))

(defn- composite? [anim] (contains? #{:group :stagger} (:anim anim)))

(defn- place
  "Leaf animations of `anim` starting at `t0`, as [{:start :end :anim}]."
  [anim t0]
  (if (composite? anim)
    (let [lag (if (= :stagger (:anim anim))
                (max (lag-ratio anim) 0.05)
                (lag-ratio anim))]
      (into []
            (comp (map-indexed (fn [i child] (place child (+ t0 (* i lag (run-time child))))))
                  cat)
            (:children anim)))
    [{:start t0 :end (+ t0 (run-time anim)) :anim anim}]))

(defn- step-span
  "The leaf placements of one graph `step` starting at `t0`, and its end."
  [step t0]
  (case (:step step)
    :play (let [placed (into [] (mapcat #(place % t0)) (:anims step))]
            [placed (reduce max t0 (map :end placed))])
    :hold [[] (+ t0 (double (:seconds step 1.0)))]
    [[] t0]))

(defn schedule
  "Every leaf animation of `graph` with its [start end] in seconds, in play
   order, and the total duration."
  [graph]
  (loop [steps (:steps graph) t 0.0 acc (transient [])]
    (if-let [step (first steps)]
      (let [[placed t'] (step-span step t)]
        (recur (rest steps) t' (reduce conj! acc placed)))
      {:duration t :anims (persistent! acc)})))

;; ── Interpolation (pure helpers) ────────────────────────────────────────────

(defn progress
  "0..1 progress of a placement at `t` (1 once finished, 0 before start)."
  [{:keys [start end]} t]
  (let [span (- end start)]
    (cond (<= t start) 0.0
          (or (>= t end) (<= span 0.0)) 1.0
          :else (/ (- t start) span))))

(defn smooth
  "Manim's default rate function: smoothstep-like ease in and out."
  [a]
  (* a a (- 3.0 (* 2.0 a))))

(defn lerp [a b alpha] (+ a (* (- b a) alpha)))

(defn lerp-point [p q alpha] (mapv #(lerp (double %1) (double %2) alpha) p q))

;; ── Animation semantics: one method per kind (OCP) ──────────────────────────

(defmulti apply-anim
  "Shapes map {id shape} after `anim` has progressed `alpha` (0..1, eased)."
  (fn [_shapes anim _alpha] (:anim anim)))

(defmethod apply-anim :default [shapes _ _] shapes)

(defn- update-target [shapes anim f & args]
  (if (contains? shapes (:target anim))
    (apply update shapes (:target anim) f args)
    shapes))

(defmethod apply-anim :appear [shapes anim a]
  (update-target shapes anim assoc :opacity a :draw 1.0))

(defmethod apply-anim :draw [shapes anim a]
  (update-target shapes anim assoc :opacity (min 1.0 (* 2.0 a)) :draw a))

(defmethod apply-anim :vanish [shapes anim a]
  (update-target shapes anim assoc :opacity (- 1.0 a)))

(defmethod apply-anim :glide [shapes anim a]
  (update-target shapes anim
                 (fn [s] (assoc s :at (lerp-point (:at s [0 0]) (:to anim) a)))))

(defmethod apply-anim :connect [shapes anim a]
  (update-target shapes anim
                 (fn [s] (assoc s
                                :from (lerp-point (:from s) (:from anim) a)
                                :to   (lerp-point (:to s) (:to anim) a)))))

(defmethod apply-anim :reshape [shapes anim a]
  (update-target shapes anim
                 (fn [s] (let [ps (:points s) qs (:points anim)]
                           (assoc s :points (if (= (count ps) (count qs))
                                              (mapv #(lerp-point %1 %2 a) ps qs)
                                              (if (pos? a) qs ps)))))))

(defmethod apply-anim :count-to [shapes anim a]
  (update-target shapes anim
                 (fn [s] (assoc s :value (lerp (double (:value s 0)) (double (:value anim)) a)))))

(defmethod apply-anim :recolor [shapes anim a]
  (update-target shapes anim
                 (fn [s] (cond-> (assoc s :recolor {:to (:color anim) :alpha a})
                           (>= a 1.0) (-> (assoc-in [:opts :color] (:color anim))
                                          (dissoc :recolor))))))

(defmethod apply-anim :emphasize [shapes anim a]
  (update-target shapes anim assoc :emphasis (Math/sin (* Math/PI a))))

(defmethod apply-anim :morph [shapes anim a]
  (cond-> shapes
    (contains? shapes (:from anim)) (assoc-in [(:from anim) :opacity] (- 1.0 a))
    (contains? shapes (:to anim))   (update (:to anim) assoc :opacity a :draw 1.0)))

;; ── Frames ──────────────────────────────────────────────────────────────────

(defn initial-shapes
  "Every node of `graph` as a hidden shape, keyed by id."
  [graph]
  (into (sorted-map) (map (fn [[id n]] [id (frame/shape n)])) (:nodes graph)))

(defn frame-at
  "The Frame of `graph` at time `t`, given its `sched` (see schedule)."
  [graph sched t]
  (let [t (double t)
        shapes (reduce (fn [shapes placed]
                         (if (< t (:start placed))
                           (reduced shapes)
                           (apply-anim shapes (:anim placed) (smooth (progress placed t)))))
                       (initial-shapes graph)
                       (:anims sched))]
    (frame/frame t (vals shapes))))
