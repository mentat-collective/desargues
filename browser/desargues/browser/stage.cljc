(ns desargues.browser.stage
  "PURE stratum (host-free .cljc): record a construct into the EDN scene graph
   without the JVM-only scene facade, so a browser can rebuild a parametric
   construct on every slider move with no server.

   The graph is the same {:nodes {id node} :steps [...]} the RecordingBackend
   (desargues.scene.data) yields, with the same node and animation shapes, so
   desargues.player.graph-source plays either one (LSP). The vocabulary mirrors
   desargues.objects (add!, update!, sweep!) and desargues.scene (tex, text,
   appear, play!, hold!):

     (record (fn [st]
               (add! st (axes/axes cs))
               (let [h (add! st (calc/tangent cs f 0.0))]
                 (update! st h (calc/tangent cs f 1.0)))
               (hold! st 1)))

   A stage is a local atom a construct mutates; `record` returns plain data.
   Primitive -> node is one `realize` method per primitive kind (OCP)."
  (:require [desargues.objects.parts :as parts]
            ;; each object family registers its parts methods on load
            [desargues.objects.axes]
            [desargues.objects.calculus]
            [desargues.objects.field]
            [desargues.objects.plot]))

;; ── The stage value ───────────────────────────────────────────────────────

(defn stage
  "A fresh, empty recording stage."
  []
  (atom {:id 0 :nodes {} :steps []}))

(defn- node!
  "Register a fresh node of `kind` with `extra` merged in; returns it."
  [st kind extra]
  (let [id (:id (swap! st update :id inc))
        n  (merge {:node kind :id id} extra)]
    (swap! st assoc-in [:nodes id] n)
    n))

(defn graph
  "The EDN scene graph recorded on `st`, named `scene-name`."
  ([st] (graph st "Live"))
  ([st scene-name]
   (let [{:keys [nodes steps id]} @st]
     {:scene scene-name :nodes nodes :steps steps :node-count id})))

(defn record
  "The scene graph of `construct` (fn [stage]) recorded on a fresh stage."
  ([construct] (record "Live" construct))
  ([scene-name construct]
   (let [st (stage)]
     (construct st)
     (graph st scene-name))))

;; ── Mobjects (the facade calls the objects layer makes) ───────────────────

(defn- fill
  "The :fill entry of a primitive style, as RecordingBackend -fill! records it."
  [{:keys [fill fill-opacity]}]
  (when fill {:fill {:color fill :opacity (or fill-opacity 1.0)}}))

(defn text
  "A text node at `at`. opts: :color :font-size."
  [st s at {:keys [color font-size] :or {color :white font-size 24}}]
  (node! st :text {:text s :opts {:color color :font-size font-size} :at (vec at)}))

(defn tex
  "A TeX node at `at`. opts: :color :font-size."
  [st s at {:keys [color font-size] :or {color :white font-size 32}}]
  (node! st :tex {:tex s :opts {:color color :font-size font-size} :at (vec at)}))

(defmulti realize
  "A registered scene node for primitive part `p` (desargues.objects.parts)."
  (fn [_st p] (:prim p)))

(defmethod realize :line [st {:keys [from to style]}]
  (node! st :line {:from (vec from) :to (vec to)
                   :opts {:color (:color style :white) :width (:width style 3)}}))

(defmethod realize :path [st {:keys [points closed? style]}]
  (node! st :path (merge {:points (mapv vec points)
                          :opts {:color (:color style :white) :width (:width style 3)
                                 :closed? closed?}}
                         (fill style))))

(defmethod realize :dot [st {:keys [at style]}]
  (node! st :dot {:opts {:color (:color style :white) :radius (:radius style 0.08)}
                  :at (vec at)}))

(defmethod realize :rectangle [st {:keys [at width height style]}]
  (node! st :rectangle (merge {:opts {:width width :height height
                                      :color (:color style :white)}
                               :at (vec at)}
                              (fill style))))

(defmethod realize :text [st {s :text :keys [at style]}]
  (text st s at {:color (:color style :white) :font-size (:font-size style 24)}))

(defmethod realize :tex [st {s :tex :keys [at style]}]
  (tex st s at {:color (:color style :white) :font-size (:font-size style 32)}))

;; ── Animations (same maps the RecordingBackend records) ───────────────────

(defn- rt [opts] {:run-time (:run-time opts 1.0)})

(defn appear [n & {:as opts}] {:anim :appear :target (:id n) :opts (rt opts)})
(defn draw   [n & {:as opts}] {:anim :draw   :target (:id n) :opts (rt opts)})
(defn vanish [n & {:as opts}] {:anim :vanish :target (:id n) :opts (rt opts)})

(defn together [anims & {:as opts}]
  {:anim :group :children (vec anims) :opts (or opts {})})

(defn stagger [anims & {:as opts}]
  {:anim :stagger :children (vec anims) :opts (or opts {})})

(defn play!
  "Append one play step of `anims` (one animation or several)."
  [st & anims]
  (swap! st update :steps conj {:step :play :anims (vec anims)})
  nil)

(defn hold!
  ([st] (hold! st 1))
  ([st seconds] (swap! st update :steps conj {:step :hold :seconds seconds}) nil))

;; ── Objects (desargues.objects add! / update! / sweep!, recorded) ─────────

(defn- reveal [p n opts]
  (if (#{:line :path} (:prim p)) (draw n :run-time (:run-time opts 1.0))
      (appear n :run-time (:run-time opts 1.0))))

(defn add!
  "Realize object spec `obj` on `st` and reveal it in one play step.
   opts: :run-time :lag-ratio. Returns the handle {:spec :parts :nodes}."
  [st obj & {:as opts}]
  (let [opts  (or opts {})
        ps    (parts/parts obj)
        nodes (into {} (map (fn [p] [(:key p) (realize st p)])) ps)]
    (play! st (together (mapv #(reveal % (get nodes (:key %)) opts) ps)
                        :lag-ratio (:lag-ratio opts 0.0)))
    {:spec obj :parts (parts/part-map ps) :nodes nodes}))

(defn- same-shape? [a b]
  (and (= (:prim a) (:prim b))
       (case (:prim a)
         :rectangle (and (= (:width a) (:width b)) (= (:height a) (:height b)))
         :text      (= (:text a) (:text b))
         :tex       (= (:tex a) (:tex b))
         true)))

(defn- move [n b run-time]
  (let [opts {:run-time run-time}]
    (case (:prim b)
      :line {:anim :connect :target (:id n) :from (vec (:from b)) :to (vec (:to b)) :opts opts}
      :path {:anim :reshape :target (:id n) :points (mapv vec (:points b)) :opts opts}
      {:anim :glide :target (:id n) :to (vec (:at b)) :opts opts})))

(defn update!
  "Animate handle `h` into spec `obj` in one play step; returns the new
   handle. Parts match by :key: same shape moves, otherwise fades."
  [st {old :parts nodes :nodes} obj & {:as opts}]
  (let [run-time (:run-time opts 1.0)
        ps       (parts/parts obj)
        keys-now (set (map :key ps))
        gone     (keep (fn [[k n]] (when-not (keys-now k) (vanish n :run-time run-time)))
                       nodes)
        {:keys [anims new-nodes]}
        (reduce (fn [acc p]
                  (let [k (:key p) a (get old k)]
                    (if (and a (same-shape? a p))
                      (-> acc
                          (assoc-in [:new-nodes k] (get nodes k))
                          (update :anims conj (move (get nodes k) p run-time)))
                      (let [n (realize st p)]
                        (cond-> (-> acc
                                    (assoc-in [:new-nodes k] n)
                                    (update :anims conj (appear n :run-time run-time)))
                          a (update :anims conj (vanish (get nodes k) :run-time run-time)))))))
                {:anims (vec gone) :new-nodes {}}
                ps)]
    (when (seq anims) (play! st (together anims)))
    {:spec obj :parts (parts/part-map ps) :nodes new-nodes}))

(defn sweep!
  "Play `(spec-fn v)` for each v of `values` in turn, one step each."
  [st h spec-fn values & {:as opts}]
  (reduce (fn [h v] (update! st h (spec-fn v) :run-time (:run-time opts 1.0))) h values))
