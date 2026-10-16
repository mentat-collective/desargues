(ns desargues.player.space-extras-test
  "The 3D kinds of desargues.player.space-extras: they validate through
   space/problems, flatten through space/->2d with no edit to space, and their
   geometry is right (an arrow ends at its tip, a plane patch lies in its
   plane, a label lands where its point projects)."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.player.space :as space]
            [desargues.player.space-extras :as sx]))

(defn- close?
  ([a b] (close? a b 1e-9))
  ([a b eps] (every? #(< (Math/abs (double %)) eps) (map - a b))))

(def ^:private cam {:target [0 0 0] :distance 10.0 :yaw -0.8 :pitch 0.5})

(defn- frame-of [& shapes] {:t 0 :camera cam :shapes (vec shapes)})

(defn- flat [& shapes] (:shapes (space/->2d (apply frame-of shapes))))

(defn- problems [s] (map :problem (space/problems (frame-of s))))

(deftest new-kinds-are-three-d
  (doseq [s [(sx/arrow [0 0 0] [1 0 0] {:id "a"})
             (sx/plane [0 0 0] {:id "p" :normal [0 0 1]})
             (sx/contours [[[0 0 0] [1 0 0]]] {:id "c"})
             (sx/label [0 0 0] "L" {:id "l"})]]
    (is (space/three-d-shape? s) (str (:node s) " is a 3D kind"))
    (is (space/three-d? {:t 0 :shapes [s]}))
    (is (space/valid? (frame-of s)) (str (:node s) " builds valid"))))

(deftest arrows
  (let [a (sx/arrow [0 0 0] [2 0 0] {:id "a" :color :orange :head 0.4})
        [shaft cone] (sx/expand a)]
    (testing "a shaft up to the head's base and a cone whose apex is the tip"
      (is (= :curve3 (:node shaft)))
      (is (close? [1.6 0.0 0.0] (second (:points shaft))))
      (is (= :surface3 (:node cone)))
      (is (close? [2.0 0.0 0.0] (first (:vertices cone))))
      (is (every? #(close? [1.6] [(first %)]) (rest (:vertices cone)))
          "the base ring is at the head's base")
      (is (= "a" (:id shaft) (:id cone)) "pieces keep the arrow's id (hit testing)"))
    (testing ":draw grows it from :from"
      (let [[shaft cone] (sx/expand (assoc a :draw 0.5))]
        (is (close? [1.0 0.0 0.0] (first (:vertices cone))))
        (is (close? [0.0 0.0 0.0] (first (:points shaft))))))
    (testing "a zero-length arrow draws nothing and is still valid"
      (let [z (sx/arrow [1 1 1] [1 1 1] {:id "z"})]
        (is (empty? (sx/expand z)))
        (is (empty? (flat z)))
        (is (space/valid? (frame-of z)))))
    (testing "it flattens to lines and filled faces"
      (let [kinds (set (map :node (flat a)))]
        (is (contains? kinds :line))
        (is (contains? kinds :path))))
    (testing "validation"
      (is (= [:arrow3/to] (problems (sx/arrow [0 0 0] [1 0] {:id "a"})))))))

(deftest planes
  (testing "with a normal: a patch perpendicular to it, centred on the point"
    (let [n  [1.0 2.0 2.0]
          p  [0.5 -1.0 0.25]
          pl (sx/plane p {:id "p" :normal n :size 0.7})
          [u v] (sx/plane-basis pl)
          {:keys [grid]} (first (sx/expand pl))]
      (is (close? [0.0 0.0 1.0 1.0] [(space/dot3 u n) (space/dot3 v n) (space/norm u) (space/norm v)]))
      (is (every? #(< (Math/abs (double (space/dot3 (space/v- % p) n))) 1e-9) (apply concat grid))
          "every grid point lies in the plane")
      (is (close? p (get-in grid [(quot (count grid) 2) (quot (count (first grid)) 2)]))
          "the patch is centred on its point")))
  (testing "with spanning vectors: point + s u + t v, |s| <= a, |t| <= b"
    (let [pl  (sx/plane [0 0 1] {:id "p" :u [1 0 0.5] :v [0 1 0] :size [2.0 1.0]})
          pts (apply concat (:grid (first (sx/expand pl))))]
      (is (close? [-2.0 -1.0 0.0] (first pts)))
      (is (close? [2.0 1.0 2.0] (last pts)))))
  (testing "validation"
    (is (= [:plane3/span] (problems (sx/plane [0 0 0] {:id "p" :normal [0 0 0]}))))
    (is (= [:plane3/span] (problems {:id "p" :node :plane3 :point [0 0 0] :u [1 0 0] :v [2 0 0]})))
    (is (= [:plane3/span] (problems {:id "p" :node :plane3 :point [0 0 0]})))
    (is (= [:plane3/point] (problems (sx/plane [0 0] {:id "p" :normal [0 0 1]}))))
    (is (= [:plane3/size] (problems (sx/plane [0 0 0] {:id "p" :normal [0 0 1] :size -1}))))))

(deftest contours-and-labels
  (testing "a :contour3 flattens to one line per segment, and short lines are refused"
    (is (= 3 (count (flat (sx/contours [[[0 0 0] [1 0 0] [1 1 0] [0 1 0]]] {:id "c"})))))
    (is (= [:contour3/lines] (problems (sx/contours [[[0 0 0]]] {:id "c"}))))
    (is (space/valid? (frame-of (sx/contours [] {:id "c"}))) "no level curves is fine"))
  (testing "a :label3 is text where its point projects"
    (let [[t] (flat (sx/label [1 1 1] "P" {:id "l" :color :yellow}))]
      (is (= :text (:node t)))
      (is (= "P" (:text t)))
      (is (close? (:at (space/project cam [1 1 1])) (:at t)))
      (is (= :yellow (get-in t [:opts :color])))))
  (testing "label validation"
    (is (= [:label3/text] (problems {:id "l" :node :label3 :at [0 0 0]})))
    (is (= [:label3/at] (problems {:id "l" :node :label3 :text "x"})))))

(deftest level-curves-sit-at-their-height
  (let [f (fn [x y] (+ (* x x) (* y y)))]
    (testing "on the surface: z equals f along every curve"
      (doseq [line (sx/level-curves f [-2 2] [-2 2] {:n 4})
              [x y z] line]
        (is (< (Math/abs (- (f x y) z)) 0.08))))
    (testing "on the floor: one height"
      (let [ls (sx/level-curves f [-2 2] [-2 2] {:n 3 :z-of (constantly -1.0)})]
        (is (= #{-1.0} (set (map #(nth % 2) (apply concat ls)))))))))

(deftest a-frame-with-every-new-kind-flattens-in-depth-order
  (let [f   (frame-of (space/surface (fn [u v] [u v 0.0]) [-1 1] [-1 1] {:id "s" :nu 4 :nv 4})
                      (sx/plane [0 0 0] {:id "p" :normal [0.3 0 1] :size 0.8})
                      (sx/arrow [0 0 0] [0 0 1.5] {:id "a"})
                      (sx/contours [[[-1 -1 0] [1 1 0]]] {:id "c"})
                      (sx/label [0 0 1.6] "top" {:id "l"}))
        out (space/->2d f)]
    (is (space/valid? f))
    (is (nil? (:camera out)))
    (is (= #{"s" "p" "a" "c" "l"} (set (map :id (:shapes out)))))
    (is (= "l" (:id (last (:shapes out)))) "the label above everything is painted last")))
