(ns desargues.player.cut-test
  "Faces that pass through each other, all pure: polygons are cut along the
   common line, and the flattened view then paints, at every probed point,
   the face nearest the eye on top. Neighbouring faces of a mesh are never
   cut, so frames where nothing interpenetrates flatten as before."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.player.cut :as cut]
            [desargues.player.hit :as hit]
            [desargues.player.protocols :as p]
            [desargues.player.space :as space]
            [desargues.player.space-demos :as demos]))

(defn- shown [s] (assoc s :opacity 1.0 :draw 1.0))

(defn- square
  "A filled square face, given its four corners."
  [id corners]
  (shown {:id id :node :surface3 :vertices corners :faces [[0 1 2 3]] :fill {:color :blue}}))

(def ^:private floor-quad
  (square "floor" [[-2 -2 0] [2 -2 0] [2 2 0] [-2 2 0]]))

(def ^:private wall-quad
  ;; Through the floor along y = 0, with the same centre: whole-face sorting
  ;; cannot order the two, each is in front on one side of their common line.
  (square "wall" [[-1 0 -1] [1 0 -1] [1 0 1] [-1 0 1]]))

(deftest polygon-cutting
  (let [ground {:point [0 0 0] :normal [0 0 1]}
        wall   [[-1 0 -1] [1 0 -1] [1 0 1] [-1 0 1]]
        [below above] (cut/split-polygon wall ground)]
    (is (cut/straddles? wall ground))
    (is (= 4 (count below) (count above)))
    (is (every? #(<= (nth % 2) 0.0) below))
    (is (every? #(>= (nth % 2) 0.0) above))
    (is (not (cut/straddles? [[0 0 0] [1 0 0] [1 1 0]] ground)) "a polygon in the plane does not"))
  (is (cut/faces-intersect? (:vertices floor-quad) (:vertices wall-quad)))
  (is (not (cut/faces-intersect? (:vertices floor-quad) [[-1 0 0.5] [1 0 0.5] [1 0 1] [-1 0 1]]))
      "a wall above the floor does not pass through it")
  (is (not (cut/faces-intersect? [[0 -1 0] [3 -1 0] [3 3 0] [0 3 0]] [[5 0 -1] [6 0 -1] [6 0 1] [5 0 1]]))
      "each straddles the other's plane, but their crossings do not overlap along the line"))

(defn- nearest-face
  "Id of the face (floor or wall) the ray through scene point `at` meets
   first, or nil when it meets neither, or both at nearly the same place (on
   their common line, where either answer is right)."
  [cam at]
  (let [ray  (space/unproject cam at)
        hits (sort-by second
                      (keep (fn [[id plane inside?]]
                              (when-let [w (space/ray-plane ray plane)]
                                (when (inside? w) [id (space/norm (space/v- w (:origin ray)))])))
                            [["floor" {:point [0 0 0] :normal [0 0 1]}
                              (fn [[x y _]] (and (<= -2 x 2) (<= -2 y 2)))]
                             ["wall" {:point [0 0 0] :normal [0 1 0]}
                              (fn [[x _ z]] (and (<= -1 x 1) (<= -1 z 1)))]]))]
    (when (and (seq hits)
               (or (nil? (second hits)) (< 0.05 (- (second (second hits)) (second (first hits))))))
      (ffirst hits))))

(defn- topmost-face
  "Id of the last-painted :path of 2D frame `g` covering scene point `at`."
  [g at]
  (:id (last (filter #(and (= :path (:node %)) (hit/inside-polygon? at (:points %))) (:shapes g)))))

(defn- mismatches
  "{:probed n :wrong [[at want got] ...]}: probed scene points where 2D frame
   `g` paints a face other than the one nearest the eye under `cam`."
  [cam g]
  (let [checked (for [x (range -3.0 3.0 0.25) y (range -2.5 2.5 0.25)
                      :let [at [x y] want (nearest-face cam at)]
                      :when want]
                  [at want (topmost-face g at)])]
    {:probed (count checked) :wrong (remove (fn [[_ want got]] (= want got)) checked)}))

(deftest intersecting-faces-are-cut-and-ordered
  (doseq [cam [{:yaw -0.8 :pitch 0.5 :distance 10.0}
               {:yaw 1.9 :pitch 0.3 :distance 10.0}
               {:yaw -2.6 :pitch -0.4 :distance 10.0 :projection :orthographic}]]
    (let [f     {:t 0 :camera cam :shapes [floor-quad wall-quad]}
          g     (space/->2d f)
          c     (space/camera f)
          b     (space/basis c)
          whole {:shapes (sort-by :depth > (mapcat #(space/primitives c b %) [floor-quad wall-quad]))}
          {:keys [probed wrong]} (mismatches cam g)]
      (testing (str "camera " cam)
        (is (= 4 (count (:shapes g))) "each face was cut in two along their common line")
        (is (< 40 probed))
        (is (seq (:wrong (mismatches cam whole)))
            "sorting the two whole faces paints the wrong one on top somewhere")
        (is (empty? wrong)
            "at every probed point the face nearest the eye is the one painted on top")))))

(deftest faces-that-do-not-interpenetrate-are-left-alone
  (testing "faces that only share an edge are not cut"
    (let [a (square "a" [[0 0 0] [1 0 0] [1 1 0] [0 1 0]])
          b (square "b" [[1 0 0] [2 0 1] [2 1 1] [1 1 0]])]
      (is (= 2 (count (:shapes (space/->2d {:camera {} :shapes [a b]})))))))
  (testing "a mesh that does not pass through itself flattens unchanged"
    (let [f     (p/-frame-at (demos/source-fn {}) 4.0)
          cam   (space/camera f)
          b     (space/basis cam)
          prims (vec (mapcat #(space/primitives cam b %) (filter space/three-d-shape? (:shapes f))))]
      (is (= prims (space/split-intersecting cam b prims))))))
