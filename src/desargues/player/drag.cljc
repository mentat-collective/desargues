(ns desargues.player.drag
  "PURE stratum of the player: draggable parameters.

   A drag spec is a parameter spec carrying :drag,

     {:id :a :drag {:node \"A\"}}

   and says: dragging the shape `:node` writes its new position into
   parameter `:id`. The player then re-records the construct exactly as a
   slider change does, so the picture follows the pointer.

   :node names the shape. A string or keyword matches the node's
   [:opts :name] (`(s/dot :name \"A\")`; Manim's Mobject takes `name` too, so
   the same construct still renders an mp4); an integer is a raw scene-graph
   node id.

   How the position becomes parameters (pure, (fn [params [x y]] params')):
     default        params[:id] = [x y]
     :axis :x | :y  params[:id] = x (or y), a one-dimensional handle
     :->params f    any function, e.g. a point constrained to a circle

   A drag is a small value, {:spec :offset}, threaded through `pointer`:
   :down on a draggable shape starts one (remembering the grab offset so the
   shape does not jump under the cursor), :move yields new params, :up ends it.
   Host-free .cljc.")

(defn drag-spec?
  "A parameter spec that is moved by dragging rather than by a widget."
  [spec]
  (contains? spec :drag))

(defn- named? [x] (or (string? x) (keyword? x) (symbol? x)))

(defn targets?
  "Does drag `spec` name shape `s`?"
  [spec s]
  (let [n (get-in spec [:drag :node])]
    (if (named? n)
      (= (name n) (some-> (get-in s [:opts :name]) name))
      (= n (:id s)))))

(defn spec-for
  "The drag spec among `specs` whose node is shape id `hit` in frame `f`."
  [specs f hit]
  (when (some? hit)
    (when-let [s (some #(when (= hit (:id %)) %) (:shapes f))]
      (some #(when (and (drag-spec? %) (targets? % s)) %) specs))))

(defn ->params
  "Parameters `params` after drag `spec` moved its shape to point `p`."
  [spec params [x y :as p]]
  (let [{:keys [axis] f :->params} (:drag spec)]
    (cond
      f            (f params [x y])
      (= :x axis)  (assoc params (:id spec) (double x))
      (= :y axis)  (assoc params (:id spec) (double y))
      :else        (assoc params (:id spec) [(double x) (double y)]))))

(defn- anchor
  "Where shape `s` sits: its :at, else the midpoint of a line."
  [s]
  (or (some-> (:at s) (subvec 0 2))
      (when-let [[a b] (and (:from s) [(:from s) (:to s)])]
        [(/ (+ (double (first a)) (double (first b))) 2.0)
         (/ (+ (double (second a)) (double (second b))) 2.0)])
      [0.0 0.0]))

(defn- shape [f id] (some #(when (= id (:id %)) %) (:shapes f)))

(defn pointer
  "One pointer `event` ({:type :at :hit}) against drag value `drag` (nil when
   idle), the drag `specs`, the current frame `f` and `params`.
   Returns {:drag drag' :params params'}; params' is nil when unchanged."
  [drag specs f params {:keys [type at hit]}]
  (case type
    :down (if-let [spec (spec-for specs f hit)]
            (let [[sx sy] (anchor (shape f hit))
                  [px py] at]
              {:drag {:spec spec :offset [(- sx px) (- sy py)]} :params nil})
            {:drag nil :params nil})
    :move (if-let [{:keys [spec offset]} drag]
            (let [[ox oy] offset [px py] at]
              {:drag drag :params (->params spec params [(+ px ox) (+ py oy)])})
            {:drag nil :params nil})
    :up   {:drag nil :params nil}
    {:drag drag :params nil}))
