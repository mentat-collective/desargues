(ns desargues.scene.data
  "A SECOND scene backend — proof the DIP seam is real.

   RecordingBackend implements the same `desargues.scene.protocols` as the Manim
   backend, but touches NO Python. Instead of drawing, it records every facade
   call as plain EDN: mobjects become {:node ...} maps (colors stay semantic
   keywords), animations become {:anim ...} maps, and a rendered scene becomes
   {:scene name :steps [...]} — a backend-neutral SCENE GRAPH.

   Uses:
   - Testing/inspection: run any presentation's construct fn through this backend
     and assert on the resulting scene graph, with zero render cost or Python.
   - Foundation for a browser/raster backend (M4): the scene graph is exactly the
     data such a renderer would consume.

   Because the SAME consumer code (e.g. goldsmith.cascade, econ.viz.scenario)
   produces a real animation under the Manim backend AND a scene graph under this
   one, the `desargues.scene` facade is proven backend-neutral (OCP/DIP/LSP)."
  (:require [desargues.scene.protocols :as p]
            [clojure.walk :as walk]
            [desargues.layout.core :as core]
            [desargues.layout.measure :as measure]))

;; ============================================================================
;; RecordingBackend — records facade calls as an EDN scene graph
;; ============================================================================

(defn- fresh-id!
  "Monotonic node id from the backend's shared counter."
  [backend]
  (:id (swap! (:state backend) update :id inc)))

(defn- register!
  "Store node `n` under its id in the backend's node registry; returns `n`."
  [backend n]
  (swap! (:state backend) assoc-in [:nodes (:id n)] n)
  n)

(defn- node
  "A fresh mobject node of `kind` with `extra` merged in, registered by id."
  [backend kind extra]
  (register! backend (merge {:node kind :id (fresh-id! backend)} extra)))

(defn- ref-id
  "The stable id of a mobject handle (nodes carry :id)."
  [obj]
  (:id obj))

;; ============================================================================
;; Layout expansion — pure declarative-tree -> EDN scene graph (no python)
;; ============================================================================

(def ^:private manim-default-frame
  "Manim's default 16:9 frame [w h] in scene units; the resolver frame when no
   :frame override is supplied."
  [14.222222 8.0])

(defn- rect-node
  "The bg/border rectangle a decorated node contributes, or nil. Boxes are in
   layout units (top-left origin, y-DOWN); colors stay semantic keywords."
  [id node]
  (let [a (:attrs node)]
    (when (or (:bg a) (:border a))
      (cond-> {:node (if (:corner-radius a) :rounded-rectangle :rectangle)
               :id   id
               :box  (get node core/box-key)}
        (:bg a)            (assoc :fill (cond-> {:color (:bg a)}
                                          (:bg-opacity a) (assoc :opacity (:bg-opacity a))))
        (:corner-radius a) (assoc-in [:opts :corner-radius] (:corner-radius a))
        (:border a)        (assoc :stroke (let [b (:border a)]
                                            (if (vector? b)
                                              {:color (first b) :width (or (second b) (:border-width a) 2)}
                                              {:color b         :width (or (:border-width a) 2)})))))))

(defn- leaf-node
  [id node style]
  {:node    (:type node)
   :id      id
   :content (:content node)
   :box     (get node core/box-key)
   :style   style})

(defn- expand-tree
  "DFS a RESOLVED (boxed) tree into visual nodes in draw order (bg/border rect
   before its content). Threads {:nodes :order :next}."
  [node path styles state]
  (let [state (if-let [r (rect-node (:next state) node)]
                (-> state
                    (update :nodes assoc (:id r) r)
                    (update :order conj (:id r))
                    (update :next inc))
                state)]
    (case (:type node)
      (:text :math :image)
      (let [id (:next state)]
        (-> state
            (update :nodes assoc id (leaf-node id node (get styles path {})))
            (update :order conj id)
            (update :next inc)))
      :spacer state
      (reduce (fn [st [i c]] (expand-tree c (conj path i) styles st))
              state
              (map-indexed vector (:children node))))))

(defn layout->scene-graph
  "Expand a declarative layout `tree` into the same EDN scene-graph shape
   -render! yields (:scene :nodes :steps :node-count), using the pure extent
   estimator. :steps is a whole-scene :appear reveal held for (:hold opts, 3)s.
   opts: {:hold n :margin m :frame [fw fh] :metrics {...}}."
  [scene-name tree {:keys [hold margin frame metrics] :or {margin 0.5}}]
  (let [[fw fh] (or frame manim-default-frame)
        root    {:x 0 :y 0 :w (- fw (* 2 margin)) :h (- fh (* 2 margin))}
        styles  (core/collect-styles tree)
        measure (if metrics (measure/make-measure styles metrics)
                    (measure/make-measure styles))
        placed  (core/resolve-layout tree root measure)
        {:keys [nodes order]} (expand-tree placed [] styles {:nodes {} :order [] :next 1})]
    {:scene      scene-name
     :kind       :layout
     :nodes      nodes
     :steps      [{:step :play :anims [{:anim :appear :target :scene :ids order :opts {}}]}
                  {:step :hold :seconds (or hold 3)}]
     :layout     placed
     :node-count (count nodes)}))

(defrecord RecordingBackend [state]
  p/IStudio
  (-init! [b] b)
  (-configure! [_ opts] {:config opts})

  p/IMobjects
  (-text [b s opts]              (node b :text {:text s :opts opts}))
  (-circle [b opts]              (node b :circle {:opts opts}))
  (-dot [b opts]                 (node b :dot {:opts opts}))
  (-rectangle [b opts]           (node b :rectangle {:opts opts}))
  (-rounded-rectangle [b opts]   (node b :rounded-rectangle {:opts opts}))
  (-decimal [b value opts]       (node b :decimal {:value value :opts opts}))
  (-line [b from to opts]        (node b :line {:from (vec from) :to (vec to) :opts opts}))
  (-place-at [b obj point]       (register! b (assoc obj :at (vec point))))
  (-place-next-to [b obj ref direction opts]
    (register! b (assoc obj :next-to {:ref (ref-id ref) :direction direction :opts opts})))
  (-fill! [b obj color opts]     (register! b (assoc obj :fill (merge {:color color} opts))))
  (-stroke! [b obj opts]         (register! b (assoc obj :stroke opts)))

  p/IAnimations
  (-appear [_ obj opts]          {:anim :appear    :target (ref-id obj) :opts opts})
  (-vanish [_ obj opts]          {:anim :vanish    :target (ref-id obj) :opts opts})
  (-draw [_ obj opts]            {:anim :draw      :target (ref-id obj) :opts opts})
  (-morph [_ obj target opts]    {:anim :morph     :from (ref-id obj) :to (ref-id target) :opts opts})
  (-emphasize [_ obj opts]       {:anim :emphasize :target (ref-id obj) :opts opts})
  (-recolor [_ obj color opts]   {:anim :recolor   :target (ref-id obj) :color color :opts opts})
  (-count-to [_ decimal value opts] {:anim :count-to :target (ref-id decimal) :value value :opts opts})
  (-glide [_ obj point opts]     {:anim :glide     :target (ref-id obj) :to (vec point) :opts opts})
  (-connect [_ line from to opts] {:anim :connect  :target (ref-id line) :from (vec from) :to (vec to) :opts opts})
  (-together [_ anims opts]      {:anim :group     :children (vec anims) :opts opts})
  (-stagger [_ anims opts]       {:anim :stagger   :children (vec anims) :opts opts})

  p/IStage
  (-play! [_ stage anims] (swap! stage update :steps conj {:step :play :anims (vec anims)}) nil)
  (-hold! [_ stage seconds] (swap! stage update :steps conj {:step :hold :seconds seconds}) nil)
  (-render! [b scene-name construct _opts]
    (let [stage (atom {:steps []})]
      (construct stage)
      {:scene scene-name
       :nodes (:nodes @(:state b))
       :steps (:steps @stage)
       :node-count (:id @(:state b))}))
  (-render-layout! [_ scene-name tree opts]
    (layout->scene-graph scene-name tree opts)))

(defn recording-backend
  "Construct a fresh recording backend (its own node-id counter + node registry)."
  []
  (->RecordingBackend (atom {:id 0 :nodes {}})))

;; ============================================================================
;; Scene-graph helpers (analysis over a captured graph)
;; ============================================================================

(defn play-steps  [graph] (filter #(= :play (:step %)) (:steps graph)))
(defn hold-steps  [graph] (filter #(= :hold (:step %)) (:steps graph)))

(defn animation-count
  "Total animations played (flattening group/stagger children one level)."
  [graph]
  (reduce (fn [acc step]
            (+ acc (reduce (fn [a anim]
                             (+ a (if (#{:group :stagger} (:anim anim))
                                    (count (:children anim))
                                    1)))
                           0
                           (:anims step))))
          0
          (play-steps graph)))

(defn colors-used
  "The set of semantic color keywords referenced anywhere in the graph — handy
   to assert consumers never leaked a raw backend color."
  [graph]
  (let [ks (atom #{})]
    (walk/postwalk
     (fn [x]
       (when (map? x)
         (doseq [k [:color :fill]]
           (let [v (get x k)]
             (cond (keyword? v) (swap! ks conj v)
                   (and (map? v) (keyword? (:color v))) (swap! ks conj (:color v))))))
       x)
     graph)
    @ks))