(ns desargues.objects
  "COMPOSITION stratum of the object vocabulary: realize object specs on a
   stage through the desargues.scene facade, so every backend (Manim mp4, the
   RecordingBackend, every player host) gets axes, plots, fields, tangents,
   Riemann rectangles and TeX labels for free.

   An object spec is pure data built by desargues.objects.{axes,plot,field,
   calculus}; its primitives come from the open `parts/parts` multimethod.
   Here we only turn primitives into mobjects and animations:

     (def ax  (objects/add! stage (axes/axes cs)))         ; create + reveal
     (def tan (objects/add! stage (calc/tangent cs f 0.5)))
     (objects/update! stage tan (calc/tangent cs f 1.5))   ; slide it along

   update! matches parts by :key: a line connects, a path reshapes, a dot or
   label glides; parts that appear or disappear fade. A handle is
   {:spec spec :parts {key part} :mobs {key mobject}}."
  (:require [desargues.objects.parts :as parts]
            [desargues.scene :as s]
            ;; each object family registers its parts methods on load
            [desargues.objects.axes]
            [desargues.objects.calculus]
            [desargues.objects.field]
            [desargues.objects.plot]))

;; ── primitive -> mobject, one method per primitive kind (OCP) ─────────────

(defn- apply-fill [mob {:keys [fill fill-opacity]}]
  (if fill (s/fill! mob fill :opacity (or fill-opacity 1.0)) mob))

(defmulti realize
  "A scene mobject for primitive part `p`."
  :prim)

(defmethod realize :line [{:keys [from to style]}]
  (s/line from to :color (:color style :white) :width (:width style 3)))

(defmethod realize :path [{:keys [points closed? style]}]
  (-> (s/path points :color (:color style :white) :width (:width style 3) :closed? closed?)
      (apply-fill style)))

(defmethod realize :dot [{:keys [at style]}]
  (-> (s/dot :color (:color style :white) :radius (:radius style 0.08))
      (s/move-to at)))

(defmethod realize :rectangle [{:keys [at width height style]}]
  (-> (s/rectangle :width width :height height :color (:color style :white))
      (s/move-to at)
      (apply-fill style)))

(defmethod realize :text [{:keys [text at style]}]
  (-> (s/text text :color (:color style :white) :font-size (:font-size style 24))
      (s/move-to at)))

(defmethod realize :tex [{:keys [tex at style]}]
  (-> (s/tex tex :color (:color style :white) :font-size (:font-size style 32))
      (s/move-to at)))

(defn- reveal [p mob opts]
  (if (#{:line :path} (:prim p))
    (s/draw mob :run-time (:run-time opts 1.0))
    (s/appear mob :run-time (:run-time opts 1.0))))

;; ── the REPL / construct surface ──────────────────────────────────────────

(defn parts-of
  "The primitive parts of spec `obj` (pure; see desargues.objects.parts)."
  [obj]
  (parts/parts obj))

(defn create
  "Mobjects for spec `obj`, unplayed: {:handle h :reveal anim}. Use it to
   reveal several objects in one play! step."
  ([obj] (create obj {}))
  ([obj opts]
   (let [ps   (parts/parts obj)
         mobs (into {} (map (fn [p] [(:key p) (realize p)])) ps)]
     {:handle {:spec obj :parts (parts/part-map ps) :mobs mobs}
      :reveal (s/together (mapv #(reveal % (get mobs (:key %)) opts) ps)
                          :lag-ratio (:lag-ratio opts 0.0))})))

(defn add!
  "Create spec `obj` on `stage` and reveal it in one play step.
   opts: :run-time :lag-ratio. Returns the handle."
  [stage obj & {:as opts}]
  (let [{:keys [handle reveal]} (create obj (or opts {}))]
    (s/play! stage reveal)
    handle))

(defn- same-shape?
  "Whether old part `a` can animate into new part `b` without replacing it."
  [a b]
  (and (= (:prim a) (:prim b))
       (case (:prim a)
         :rectangle (and (= (:width a) (:width b)) (= (:height a) (:height b)))
         :text      (= (:text a) (:text b))
         :tex       (= (:tex a) (:tex b))
         true)))

(defn- move [mob a b rt]
  (case (:prim b)
    :line (s/connect mob (:from b) (:to b) :run-time rt)
    :path (s/reshape mob (:points b) :run-time rt)
    (s/glide mob (:at b) :run-time rt)))

(defn transition
  "Animations taking `handle` to spec `obj`, and the new handle:
   {:handle h :anims [...]}. opts: :run-time."
  ([handle obj] (transition handle obj {}))
  ([{old :parts mobs :mobs} obj {:keys [run-time] :or {run-time 1.0}}]
   (let [ps (parts/parts obj)]
     (reduce
      (fn [acc p]
        (let [k (:key p) a (get old k)]
          (if (and a (same-shape? a p))
            (-> acc
                (assoc-in [:handle :mobs k] (get mobs k))
                (update :anims conj (move (get mobs k) a p run-time)))
            (let [m (realize p)]
              (cond-> (-> acc
                          (assoc-in [:handle :mobs k] m)
                          (update :anims conj (s/appear m :run-time run-time)))
                a (update :anims conj (s/vanish (get mobs k) :run-time run-time)))))))
      {:handle {:spec obj :parts (parts/part-map ps) :mobs {}}
       :anims  (into [] (keep (fn [[k m]] (when-not (some #(= k (:key %)) ps)
                                            (s/vanish m :run-time run-time))))
                     mobs)}
      ps))))

(defn update!
  "Animate `handle` into spec `obj` in one play step. Returns the new handle."
  [stage handle obj & {:as opts}]
  (let [{:keys [handle anims]} (transition handle obj (or opts {}))]
    (when (seq anims) (s/play! stage (s/together anims)))
    handle))

(defn sweep!
  "Play `(spec-fn v)` for each v of `values` in turn, one step each: a
   parameter sweep (a tangent sliding along a curve, n growing in a Riemann
   sum). Returns the final handle. opts: :run-time per step."
  [stage handle spec-fn values & {:as opts}]
  (reduce (fn [h v] (update! stage h (spec-fn v) (or opts {}))) handle values))
