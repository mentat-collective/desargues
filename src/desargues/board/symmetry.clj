(ns desargues.board.symmetry
  "Pure layer of boards: groups acting on a construction, registered on the
   open sets of desargues.board.construction.

   A construction names its finite groups under :groups (forms of
   desargues.board.groups), {:G [:dihedral 4]}, and element i of G is the
   map [:G i] (element 0 is the identity, the rest in breadth-first order
   from the generators).

   Points
     {:id :O :op :orbit :of :A :group :G}     one point per element: :O.0 .. :O.7
     {:id :O :op :orbit :of :A :by :R :k 6}   the powers of a map: A, RA, .. R^5 A
   Draws
     [:images [:OA :OB :OC] {:group :G}]      each element's image of the polygon,
                                              orientation-reversing ones in :flip-color
     [:images [:OA :OB] {:k 6}]               likewise for the powers of a map
     [:cayley :O {:group :G}]                 the Cayley graph on the orbit: an edge
                                              g -> g s for every generator s, one
                                              colour per generator
   Checks
     {:det :R}  {:trace :R}                   of a map: an author's, or [:G i]"
  (:require [desargues.board.construction :as c]
            [desargues.board.linalg :as la]))

(defn- group-of [env gid]
  (or (get-in env [:groups gid])
      (throw (ex-info (str "Unknown group " gid) {:group gid :known (keys (:groups env))}))))

(defmethod c/expand-point :orbit [{:keys [id of group by k]} {:keys [groups]}]
  (let [n (if group
            (count (:elements (or (get groups group)
                                  (throw (ex-info (str "Unknown group " group) {:group group})))))
            k)]
    (for [i (range n)]
      {:id (c/indexed-id id i) :op :map :of of
       :by (if group [[group i]] (vec (repeat i by)))})))

(def ^:private palette [:orange :blue :green :violet :red :yellow])

(defmethod c/draw-layer :cayley [[_ id {:keys [group colors weight]}] env]
  (let [{:keys [table names]} (group-of env group)
        colors (or colors palette)]
    (vec (for [g (range (count names))
               i (range (count table))
               :let [j (get-in table [i g])]
               :when (not= i j)
               ;; an involution's edge goes both ways: draw it once
               :when (or (not= i (get-in table [j g])) (< i j))]
           (c/draw-layer [:segment (c/indexed-id id i) (c/indexed-id id j)
                          {:color (nth colors (mod g (count colors))) :weight (or weight 2)}]
                         env)))))

(defmethod c/draw-layer :images [[_ ids {:keys [group k color flip-color fill-opacity]
                                        :or {color :blue flip-color :pink fill-opacity 0.12}}]
                                 env]
  (let [elements (when group (:elements (group-of env group)))]
    (vec (for [i (range (if group (count elements) k))
               :let [flip? (and group (neg? (la/det (nth elements i))))]]
           (c/draw-layer [:polygon (mapv #(c/indexed-id % i) ids)
                          {:color (if flip? flip-color color) :fill-opacity fill-opacity}]
                         env)))))

(defmethod c/check :det [{m :det} env] (la/det (c/map-of env m)))

(defmethod c/check :trace [{m :trace} env] (la/trace (c/map-of env m)))
