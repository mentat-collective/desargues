(ns desargues.board.constructions-test
  "The construction vocabulary beyond the core: groups, Euclidean and
   projective operations, symmetry, solids, and 3D. Each board is compiled
   through raster and read back from its frame, so every number here is the
   kernel's. Needs raster:
   `clojure -M:test:dynamics -n desargues.board.constructions-test`."
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is testing]]
            [desargues.board :as board]
            [desargues.board.groups :as groups]
            [desargues.board.shapes :as sh]
            [desargues.specs.board :as spec]))

(defn- tmp-dir []
  (.getPath (doto (io/file (System/getProperty "java.io.tmpdir") "desargues-constructions-test") .mkdirs)))

(defn- compile! [spec] (board/compile-board! spec {:out-dir (tmp-dir)}))

(defn- v [b k] (first (get-in b [:board/frame k])))

(defn- readouts
  "Every check's value, by label."
  [b]
  (into {} (for [l (:board/layers b) :when (= :value (:layer l))] [(:label l) (v b (:of l))])))

(defn- drawn
  "Every labelled :point layer's position, by label."
  [b]
  (into {} (for [l (:board/layers b) :when (= :point (:layer l))] [(:label l) (mapv #(v b %) (:at l))])))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(deftest finite-groups-close-to-their-orders
  (is (= {[:cyclic 5] 5 [:dihedral 4] 8 [:klein4] 4 [:tetrahedral] 12 [:octahedral] 24 [:icosahedral] 60}
         (into {} (for [g [[:cyclic 5] [:dihedral 4] [:klein4] [:tetrahedral] [:octahedral] [:icosahedral]]]
                    [g (count (:elements (groups/finite g)))]))))
  (testing "words are shortest, breadth-first from the identity; the table is total"
    (let [{:keys [words table]} (groups/finite [:dihedral 4])]
      (is (= ["e" "r" "s" "rr"] (take 4 words)))
      (is (every? #(every? some? %) table))))
  (testing "a group past its bound is refused, not truncated"
    (is (thrown? clojure.lang.ExceptionInfo (groups/finite [:cyclic 300])))))

(deftest euclidean-constructions
  (let [b (compile! {:id :euclid-test :kind :construction
                     :points [{:id :A :at [0 0]} {:id :B :at [2 0]} {:id :C :at [0 2]} {:id :P :at [1 1.5]}
                              {:id :P' :op :reflect :of :P :in [:A :B]}
                              {:id :F :op :foot :of :P :on [:A :B]}
                              {:id :Q :op :rotate :of :B :about :A :by (/ Math/PI 2)}
                              {:id :G :op :centroid :of [:A :B :C]}
                              {:id :K :op :circumcenter :of [:A :B :C]}
                              {:id :X :op :around :center :K :through :A :angle 0.3}]
                     :draw [[:point :P'] [:point :F] [:point :Q] [:point :G] [:point :K]]
                     :checks [{:area [:A :B :C] :label "area"}
                              {:perpendicular [[:A :B] [:A :C]] :label "perp"}
                              {:concyclic [:A :B :C :X] :label "cyc"}
                              {:distance [:K :X] :label "KX"}]})
        p (drawn b) r (readouts b)]
    (is (s/valid? ::spec/board b))
    (is (= [1.0 -1.5] (p "P'")) "P mirrored in AB")
    (is (= [1.0 0.0] (p "F")) "the foot of P on AB")
    (is (every? true? (map close? (p "Q") [0 2])) "B a quarter turn about A")
    (is (every? true? (map close? (p "G") [(/ 2 3) (/ 2 3)])))
    (is (= [1.0 1.0] (p "K")) "the circumcentre of a right triangle is its hypotenuse's midpoint")
    (is (= 2.0 (r "area")))
    (is (close? 0 (r "perp")))
    (is (close? 0 (r "cyc")) "X stays on the circumcircle")
    (is (close? (Math/sqrt 2) (r "KX")))
    (testing "the point on the circle is dragged round it by its angle"
      (let [h (first (filter #(= "X" (:label %)) (:board/layers b)))]
        (is (= :handle (:layer h)))
        (is (= 2 (count (:around h))))
        (is (contains? (:drives h) :angle))))))

(deftest projective-constructions
  (let [pent (for [i (range 5)] (let [a (+ 0.3 (* i 1.1))] [(Math/cos a) (Math/sin a)]))
        b (compile! {:id :projective-test :kind :construction
                     :window {:x [-3 3] :y [-2 2] :n 97}
                     :points (into (vec (map-indexed (fn [i p] {:id (keyword (str "P" i)) :at (vec p)}) pent))
                                   [{:id :X :op :conic :through [:P0 :P1 :P2 :P3 :P4]}
                                    {:id :A :at [-2 -1]} {:id :B :at [2 -1]} {:id :C :at [0.5 -1]}
                                    {:id :D :op :harmonic :of [:A :B :C]}
                                    {:id :O :at [0 1.5]}
                                    {:id :Y :op :project :of :P0 :from :O :onto [:A :B]}])
                     :draw [[:trace :X] [:point :D] [:point :Y]]
                     :checks [{:cross-ratio [:A :B :C :D] :label "cr"}
                              {:collinear [:O :P0 :Y] :label "OPY"}]})
        [kx ky] (:of (first (filter #(= :trace (:layer %)) (:board/layers b))))
        radii (map #(Math/hypot %1 %2) (get-in b [:board/frame kx]) (get-in b [:board/frame ky]))]
    (testing "the conic through five points of the unit circle is the unit circle"
      (is (= 97 (count radii)))
      (is (every? #(close? 1 %) radii)))
    (testing "the harmonic conjugate, and a perspectivity"
      (is (close? -1 (get (readouts b) "cr")))
      (is (= [8.0 -1.0] ((drawn b) "D")))
      (is (close? -1 (second ((drawn b) "Y"))) "Y is on AB")
      (is (close? 0 (get (readouts b) "OPY")) "and on OP"))))

(deftest pascal-holds-for-a-point-riding-the-conic
  (doseq [ph [0.3 1.3 2.2 2.9]]
    (let [b (compile! {:id :pascal-test :kind :construction
                       :params [{:id 'ph :min 0.05 :max 3.09 :init ph :play true}]
                       :points [{:id :A :at [-1.8 -0.6]} {:id :B :at [-0.6 1.3]} {:id :C :at [1.2 1.2]}
                                {:id :D :at [2.0 -0.4]} {:id :E :at [0.3 -1.5]}
                                {:id :F :op :conic :through [:A :B :C :D :E] :angle 'ph}
                                {:id :P :op :meet :lines [[:A :B] [:D :E]]}
                                {:id :Q :op :meet :lines [[:B :C] [:E :F]]}
                                {:id :R :op :meet :lines [[:C :D] [:F :A]]}]
                       :checks [{:collinear [:P :Q :R] :label "PQR"}]})]
      (is (< (Math/abs (double (get (readouts b) "PQR"))) 1e-9) (str "at angle " ph)))))

(deftest groups-act-on-a-figure
  (let [b (compile! {:id :symmetry-test :kind :construction
                     :params [{:id 'th :min 0 :max 6.3 :init 0.7} {:id 'k :min 0.2 :max 3 :init 2}]
                     :groups {:G [:dihedral 4]}
                     :maps {:R [:rotation 'th] :S [:scale 'k] :B [:boost 'th] :M [:reflection 'th]}
                     :points [{:id :A :at [1.5 0.4]} {:id :B :at [2.2 0.5]} {:id :C :at [1.6 0.9]}
                              {:id :OA :op :orbit :of :A :group :G}
                              {:id :OB :op :orbit :of :B :group :G}
                              {:id :OC :op :orbit :of :C :group :G}
                              {:id :W :op :orbit :of :A :by :R :k 5}]
                     :draw [[:images [:OA :OB :OC] {:group :G}] [:cayley :OA {:group :G}]]
                     :checks [{:det :R :label "det R"} {:det :S :label "det S"} {:det :B :label "det B"}
                              {:det :M :label "det M"} {:trace :R :label "tr R"}
                              {:distance [:A :W.3] :label "|A R3A|"}]})
        r (readouts b)
        layers (frequencies (map (juxt :layer :color) (:board/layers b)))]
    (testing "map families, read off their determinants and traces"
      (is (close? 1 (r "det R"))) (is (close? 4 (r "det S"))) (is (close? 1 (r "det B")))
      (is (close? -1 (r "det M")))
      (is (close? (+ 1 (* 2 (Math/cos 0.7))) (r "tr R"))))
    (testing "the powers of a map: |A - R^3 A| = 2 |A| sin(3 th / 2)"
      (is (close? (* 2 (Math/hypot 1.5 0.4) (Math/sin (* 1.5 0.7))) (r "|A R3A|"))))
    (testing "D4's images: four rotations, four reflections in their own colour"
      (is (= 4 (layers [:polygon :blue])))
      (is (= 4 (layers [:polygon :pink]))))
    (testing "D4's Cayley graph: an 8-edge r-cycle pair and 4 s-edges, drawn once each"
      (is (= 8 (layers [:segment :orange])))
      (is (= 4 (layers [:segment :blue]))))))

(deftest solids-are-convex-hulls
  (is (= {:tetrahedron [4 4 6] :cube [8 6 12] :octahedron [6 8 12]
          :icosahedron [12 20 30] :dodecahedron [20 12 30]}
         (into {} (for [k [:tetrahedron :cube :octahedron :icosahedron :dodecahedron]]
                    (let [{:keys [points draw]} (sh/solid :S k)]
                      [k [(count points)
                          (count (filter #(= :polygon (first %)) draw))
                          (count (filter #(= :segment (first %)) draw))]]))))))

(def cube
  (sh/merge-parts
   {:id :cube-test :kind :construction
    :params [{:id 'yaw :min -3.2 :max 3.2 :init 0.6 :control :orbit :axis :yaw}
             {:id 'pitch :min -1.5 :max 1.5 :init 0.4 :control :orbit :axis :pitch}
             {:id 'th :min 0 :max 6.3 :init 0.5}]
    :view {:yaw 'yaw :pitch 'pitch :scale 1.4 :perspective 7}
    :window {:x [-3 3] :y [-2.2 2.2]}
    :maps {:R [:rotation3 [0 0 1] 'th]}
    :points [{:id :Q :op :map :by [:R] :of :C.0}
             {:id :S :op :on :line [:C.0 :C.1] :s 0.25}]
    :draw [[:point :Q]]
    :checks [{:distance [:C.0 :C.1] :label "edge"} {:distance [:C.0 :Q] :label "chord"}
             {:value '(z Q) :label "zQ"}]}
   (sh/solid :C :cube {:size 1.5})
   (sh/axes3 :ax 2)))

(deftest a-3d-construction-hands-its-camera-to-the-page
  (let [b (compile! cube)
        r (readouts b)
        z (/ 1.5 (Math/sqrt 3))]
    (is (s/valid? ::spec/board b))
    (testing "the camera is data: param ids for the orbit, the page projects"
      (is (= {:yaw :yaw :pitch :pitch :scale 1.4 :perspective 7} (:board/view b)))
      (is (= :none (get-in b [:board/window :axes])) "a 3D figure has no plane axes")
      (is (every? #(= 3 (count %)) (:pts (first (filter #(= :polygon (:layer %)) (:board/layers b)))))))
    (testing "measures are in world space"
      (is (close? (* 2 z) (r "edge")))
      (is (close? (* 2 (Math/sqrt 2) z (Math/sin 0.25)) (r "chord")) "a turn about z by th")
      (is (close? z (r "zQ")) "and z is kept"))
    (testing "a point dragged along a 3D edge"
      (let [h (first (filter #(= "S" (:label %)) (:board/layers b)))]
        (is (= 3 (count (:at h))))
        (is (= 2 (count (:along h)))))))
  (testing "sixty images under the icosahedral group compile into one kernel"
    (let [b (compile! (sh/merge-parts
                       {:id :ico-test :kind :construction :view {:yaw 0.6 :pitch 0.4}
                        :groups {:I [:icosahedral]}
                        :points [{:id :P :at [0.3 0.2 1.1]} {:id :OP :op :orbit :of :P :group :I}]
                        :draw (vec (for [i (range 60)] [:point (keyword (str "OP." i)) {:label nil}]))
                        :checks [{:distance [:OP.0 :OP.59] :label "d"}]}
                       (sh/solid :I :icosahedron {:size 1.6})))]
      (is (s/valid? ::spec/board b))
      (is (= 60 (count (filter #(= :point (:layer %)) (:board/layers b))))))))
