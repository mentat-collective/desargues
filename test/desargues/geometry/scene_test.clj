(ns desargues.geometry.scene-test
  "geometry.scene emits exactly the RecordingBackend EDN the facade already
  records: :dot/:line/:text nodes, :play/:hold steps, :glide/:connect anims."
  (:require [clojure.test :refer [deftest testing is]]
            [desargues.geometry.projective :as g]
            [desargues.geometry.scene :as gs]
            [desargues.scene :as s]
            [desargues.scene.data :as rec]))

(def frame-a {:O [0.0 3.0] :P [1.0 -3.1] :Q [-4.25 -3.1]
              :angs [-2.25 -1.57 -1.2] :ra 2.0 :ra' 4.3})
(def frame-b {:O [0.0 3.0] :P [1.0 -2.3] :Q [-3.5 -3.1]
              :angs [-2.25 -1.57 -1.05] :ra 2.0 :ra' 4.3})

(def derive (comp :points g/desargues-configuration))

(def segment-spec
  {:ra {:base [:O :A']}
   :ab {:base [:A :B] :covers [:A :B :P] :pad 0.3}
   :axis {:base [:P :R] :covers [:P :Q :R] :pad 0.3}})

(deftest extend-to-covers-and-pads
  (let [[a b] (gs/extend-to [0 0] [10 0] [[2 5] [7 -3]] 1.0)]
    (is (= [1.0 0.0] a))
    (is (= [8.0 0.0] b))))

(deftest config->segments-extends-covered-lines
  (let [pts (derive frame-a)
        segs (gs/config->segments pts segment-spec)]
    (testing "rays stay the plain base segment"
      (is (= [(:O pts) (:A' pts)] (:ra segs))))
    (testing "the axis segment covers P, Q and R"
      (let [[a b] (:axis segs)
            t (fn [X] (first (mapv - X a)))
            span (mapv - b a)
            proj (fn [X] (/ (reduce + (map * (mapv - X a) span))
                            (reduce + (map * span span))))]
        (doseq [k [:P :Q :R]]
          (is (<= 0.0 (proj (pts k)) 1.0) (str k " on the axis segment")))))))

(deftest config->nodes-emits-recording-backend-edn
  (let [pts (derive frame-a)
        segs (gs/config->segments pts segment-spec)
        g (s/with-backend (rec/recording-backend)
            (s/render! "cfg"
                       (fn [stage]
                         (let [nodes (gs/config->nodes pts segs
                                                       :labels {:O {:text "O" :direction :up}})]
                           (s/play! stage (s/together
                                           (concat (map s/appear (vals (:dots nodes)))
                                                   (map s/draw (vals (:lines nodes))))))))))
        kinds (map :node (vals (:nodes g)))]
    (testing "only known node kinds, no new mobjects"
      (is (every? #{:dot :line :text} kinds))
      (is (= 10 (count (filter #{:dot} kinds))))
      (is (= 3 (count (filter #{:line} kinds))))
      (is (= 1 (count (filter #{:text} kinds)))))
    (testing "a label is placed next-to its dot"
      (let [label (first (filter #(= :text (:node %)) (vals (:nodes g))))]
        (is (some? (:next-to label)))))))

(deftest config->play-steps-records-glide-and-connect
  (let [pts (derive frame-a)
        segs (gs/config->segments pts segment-spec)
        g (s/with-backend (rec/recording-backend)
            (s/render! "move"
                       (fn [stage]
                         (let [nodes (gs/config->nodes pts segs)]
                           (gs/config->play-steps stage nodes derive segment-spec
                                                  [frame-a frame-b]
                                                  :samples 4 :hold 0.2)))))
        plays (rec/play-steps g)
        holds (rec/hold-steps g)
        anims (mapcat :anims plays)
        flat (mapcat #(if (= :group (:anim %)) (:children %) [%]) anims)]
    (testing "one play per sample plus one hold per leg"
      (is (= 4 (count plays)))
      (is (= 1 (count holds))))
    (testing "every sample glides every dot and reconnects every line"
      (is (= 52 (count (filter (comp #{:glide :connect} :anim) flat))))
      (is (= 40 (count (filter (comp #{:glide} :anim) flat))))
      (is (= 12 (count (filter (comp #{:connect} :anim) flat)))))
    (testing "the mid-leg frames are true Desargues configurations"
      (let [mid (derive (g/lerp-params frame-a frame-b 0.5))]
        (is (g/collinear? (:P mid) (:Q mid) (:R mid) 1e-6))))))
