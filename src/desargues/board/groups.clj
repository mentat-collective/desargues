(ns desargues.board.groups
  "Pure layer of boards: group elements as data.

   A MAP FAMILY names a transformation by its parameters, instead of by its
   matrix:

     [:rotation t]            [:rotation t [cx cy]]      [:reflection t]
     [:translation dx dy]     [:scale k] [:scale kx ky]  [:shear k]
     [:boost phi]             [:matrix rows]
     [:rotation-x t] [:rotation-y t] [:rotation-z t]     [:rotation3 [ax ay az] t]
     [:translation3 dx dy dz] [:scale3 k]                [:reflection3 [nx ny nz]]

   `matrix` is OPEN on the family keyword. Its arguments are the author's
   expressions, already read as Emmy by the caller, so one family serves the
   kernel (symbolic in the sliders) and a numeric group table alike. The
   2D families are 3x3, the 3D ones 4x4.

   A FINITE GROUP is named the same way,

     [:cyclic n] [:dihedral n] [:klein4]
     [:tetrahedral] [:octahedral] [:icosahedral]
     [:generated {:generators [family ...] :names [\"a\" ...]}]

   and `finite` closes its generators into every element, each with the
   shortest word that reaches it and its Cayley edges: element i times
   generator g is element (get-in table [i g]). `generators` is OPEN on the
   group keyword."
  (:require [desargues.board.linalg :as la]
            [emmy.env :as e]))

;; ---------------------------------------------------------------------------
;; Map families

(defmulti matrix
  "A family form [kind & args], with args already Emmy -> its matrix."
  (fn [[kind] _] kind))

(defmethod matrix :default [[kind] _]
  (throw (ex-info (str "No map family " kind
                       "; register one with (defmethod desargues.board.groups/matrix " kind " ...)")
                  {:kind kind :known (keys (methods matrix))})))

(defn- read-args
  "The family's arguments as Emmy: `read` applied to every leaf that is not a
   nested vector of numbers-or-forms (an axis or a centre stays a vector)."
  [args read]
  (mapv (fn [a] (if (vector? a) (mapv read a) (read a))) args))

(defn family
  "The matrix of family form f, reading each argument with `read` (an
   author-expression reader; identity for numbers)."
  ([f] (family f identity))
  ([[kind & args] read]
   (matrix (into [kind] (if (= kind :matrix) args (read-args args read))) read)))

(defmethod matrix :matrix [[_ rows] read] (mapv (fn [row] (mapv read row)) rows))

(defmethod matrix :rotation [[_ t [cx cy]] _]
  (let [c (e/cos t) s (e/sin t)
        R [[c (e/- 0 s) 0] [s c 0] [0 0 1]]]
    (if cx
      (la/mat* (la/mat* [[1 0 cx] [0 1 cy] [0 0 1]] R) [[1 0 (e/- 0 cx)] [0 1 (e/- 0 cy)] [0 0 1]])
      R)))

(defmethod matrix :reflection [[_ t] _]
  ;; in the line through the origin at angle t
  (let [c (e/cos (e/* 2 t)) s (e/sin (e/* 2 t))]
    [[c s 0] [s (e/- 0 c) 0] [0 0 1]]))

(defmethod matrix :translation [[_ dx dy] _] [[1 0 dx] [0 1 dy] [0 0 1]])

(defmethod matrix :scale [[_ kx ky] _] [[kx 0 0] [0 (or ky kx) 0] [0 0 1]])

(defmethod matrix :shear [[_ k] _] [[1 k 0] [0 1 0] [0 0 1]])

(defmethod matrix :boost [[_ phi] _]
  (let [c (e/cosh phi) s (e/sinh phi)] [[c s 0] [s c 0] [0 0 1]]))

(defn- embed3
  "A 3x3 linear map as a 4x4 homogeneous one."
  [L]
  (conj (mapv #(conj (vec %) 0) L) [0 0 0 1]))

(defmethod matrix :rotation-x [[_ t] _]
  (let [c (e/cos t) s (e/sin t)] (embed3 [[1 0 0] [0 c (e/- 0 s)] [0 s c]])))

(defmethod matrix :rotation-y [[_ t] _]
  (let [c (e/cos t) s (e/sin t)] (embed3 [[c 0 s] [0 1 0] [(e/- 0 s) 0 c]])))

(defmethod matrix :rotation-z [[_ t] _]
  (let [c (e/cos t) s (e/sin t)] (embed3 [[c (e/- 0 s) 0] [s c 0] [0 0 1]])))

(defmethod matrix :rotation3 [[_ axis t] _]
  ;; Rodrigues: R = cI + s[k]x + (1 - c) k k^T, k the unit axis
  (let [n (la/norm axis)
        [x y z] (mapv #(e/divide % n) axis)
        c (e/cos t) s (e/sin t) C (e/- 1 c)]
    (embed3 [[(e/+ c (e/* C x x)) (e/- (e/* C x y) (e/* s z)) (e/+ (e/* C x z) (e/* s y))]
             [(e/+ (e/* C y x) (e/* s z)) (e/+ c (e/* C y y)) (e/- (e/* C y z) (e/* s x))]
             [(e/- (e/* C z x) (e/* s y)) (e/+ (e/* C z y) (e/* s x)) (e/+ c (e/* C z z))]])))

(defmethod matrix :translation3 [[_ dx dy dz] _]
  [[1 0 0 dx] [0 1 0 dy] [0 0 1 dz] [0 0 0 1]])

(defmethod matrix :scale3 [[_ k] _] (embed3 [[k 0 0] [0 k 0] [0 0 k]]))

(defmethod matrix :reflection3 [[_ normal] _]
  ;; in the plane through the origin with this normal: I - 2 n n^T / n.n
  (let [nn (la/dot normal normal)]
    (embed3 (vec (for [i (range 3)]
                   (vec (for [j (range 3)]
                          (e/- (if (= i j) 1 0)
                               (e/divide (e/* 2 (nth normal i) (nth normal j)) nn)))))))))

;; ---------------------------------------------------------------------------
;; Finite groups, closed numerically from their generators

(defmulti generators
  "A group form -> {:generators [family-form ...] :names [string ...]}."
  (fn [[kind]] kind))

(defmethod generators :default [[kind]]
  (throw (ex-info (str "No finite group " kind
                       "; register one with (defmethod desargues.board.groups/generators " kind " ...)")
                  {:kind kind :known (keys (methods generators))})))

(def ^:private tau (* 2 Math/PI))

(defmethod generators :cyclic [[_ n]]
  {:generators [[:rotation (/ tau n)]] :names ["r"]})

(defmethod generators :dihedral [[_ n]]
  {:generators [[:rotation (/ tau n)] [:reflection 0]] :names ["r" "s"]})

(defmethod generators :klein4 [_]
  {:generators [[:reflection 0] [:reflection (/ Math/PI 2)]] :names ["a" "b"]})

(defmethod generators :tetrahedral [_]
  {:generators [[:rotation3 [1 1 1] (/ tau 3)] [:rotation-z Math/PI]] :names ["a" "b"]})

(defmethod generators :octahedral [_]
  {:generators [[:rotation-z (/ tau 4)] [:rotation3 [1 1 1] (/ tau 3)]] :names ["a" "b"]})

(def phi (/ (+ 1 (Math/sqrt 5)) 2))

(defmethod generators :icosahedral [_]
  ;; a 5-fold turn about the vertex (0, 1, phi) and a 3-fold one about the
  ;; face centre (1, 1, 1) of the icosahedron on the cyclic (0, +-1, +-phi)
  {:generators [[:rotation3 [0 1 phi] (/ tau 5)] [:rotation3 [1 1 1] (/ tau 3)]] :names ["a" "b"]})

(defmethod generators :generated [[_ {:keys [generators names]}]]
  {:generators generators
   :names (or names (mapv #(str (char (+ 97 %))) (range (count generators))))})

(defn- numeric [M] (mapv (fn [row] (mapv #(double (e/freeze %)) row)) M))

(defn- round-key [M] (mapv (fn [row] (mapv #(Math/round (* 1e7 (double %))) row)) M))

(defn finite
  "Close group form g: {:elements [matrix ...] :words [string ...]
   :generators [matrix ...] :names [string ...] :table [[index ...] ...]},
   elements in breadth-first order from the identity, so each word is a
   shortest one. Refuses a group past :max elements (default 240)."
  ([g] (finite g {}))
  ([g {:keys [max] :or {max 240}}]
   (let [{:keys [generators names]} (generators g)
         gens (mapv (comp numeric family) generators)
         I (la/identity-matrix (count (first gens)))
         [elements words index]
         (loop [queue (conj clojure.lang.PersistentQueue/EMPTY 0)
                elements [(numeric I)] words [""] index {(round-key I) 0}]
           (if-let [i (peek queue)]
             (let [[elements words index queue]
                   (reduce (fn [[els ws idx q] [gi G]]
                             (let [M (numeric (la/mat* (nth els i) G))
                                   k (round-key M)]
                               (if (idx k)
                                 [els ws idx q]
                                 (do (when (>= (count els) max)
                                       (throw (ex-info (str "Group " (pr-str g) " has more than " max " elements")
                                                       {:group g :max max})))
                                     [(conj els M) (conj ws (str (nth ws i) (nth names gi)))
                                      (assoc idx k (count els)) (conj q (count els))]))))
                           [elements words index (pop queue)]
                           (map-indexed vector gens))]
               (recur queue elements words index))
             [elements words index]))]
     {:elements elements
      :words (mapv #(if (= "" %) "e" %) words)
      :generators gens
      :names names
      :table (mapv (fn [M] (mapv (fn [G] (index (round-key (la/mat* M G)))) gens)) elements)})))
