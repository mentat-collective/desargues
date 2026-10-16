(ns desargues.studio.blackboard-test
  "The blackboard (desargues.studio.blackboard): every frame is a valid
   Frame at every instant, steps are chalked in order at the times the plan
   says, the board scrolls to keep the current step in view, and the widgets
   (driven through the windowless recording host) change what is on the
   board and move the clock."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.player :as player]
            [desargues.player.controls :as controls]
            [desargues.player.frame :as frame]
            [desargues.player.recording-host :as rh]
            [desargues.player.timeline :as timeline]
            [desargues.studio.blackboard :as bb]
            [desargues.studio.blackboard-lessons :as bl]))

(defn- graph [ps] (player/record "bb" (bb/blackboard ps)))

(defn- frame-at [g t] (timeline/frame-at g (timeline/schedule g) t))

(defn- finite? [x] (and (number? x) (not (Double/isNaN (double x))) (not (Double/isInfinite (double x)))))

(defn- valid-shape? [s]
  (and (contains? frame/shape-kinds (:node s))
       (<= 0.0 (double (:opacity s)) 1.0)
       (<= 0.0 (double (:draw s 0.0)) 1.0)
       (every? finite? (:at s [0 0]))
       (every? #(every? finite? %) (:points s []))))

(defn- params [& {:as more}] (merge (bb/defaults) more))

(defn- tex-of
  "The shape of the chalk line `key` of plan `p` in frame `f`: matched by its
   TeX, or by its text for a step number."
  [p f key]
  (let [{:keys [tex text]} (first (filter #(= key (:key %)) (:items p)))]
    (first (filter #(if tex (= tex (:tex %)) (and (= :text (:node %)) (= text (:text %))))
                   (:shapes f)))))

(deftest frames-are-valid-at-every-instant
  (doseq [l bl/lessons path [0 1] explain [true false]
          :let [ps (params :problem (:id l) :path path :explain explain)
                g  (graph ps)
                d  (:duration (timeline/schedule g))]]
    (testing (str (name (:id l)) " path " path " explain " explain)
      (is (< (Math/abs (- d (:duration (bb/plan ps)))) 1e-6) "plan and recording agree")
      (doseq [t (concat (range 0.0 d 0.37) [d])]
        (is (every? valid-shape? (:shapes (frame-at g t))) (str "t = " t))))))

(deftest steps-are-written-in-order-at-their-start-times
  (doseq [l bl/lessons
          :let [ps (params :problem (:id l))
                p  (bb/plan ps)
                g  (graph ps)
                n  (count (:starts p))]]
    (testing (name (:id l))
      (is (apply < (:starts p)) "start times increase")
      (doseq [i (range n)
              :let [t0 (nth (:starts p) i)
                    t1 (if (< (inc i) n) (nth (:starts p) (inc i)) (:end p))]]
        (testing (str "step " i)
          (is (not (frame/visible? (tex-of p (frame-at g (- t0 0.05)) [i :eq])))
              "not on the board before its start")
          (let [s (tex-of p (frame-at g (- t1 0.01)) [i :eq])]
            (is (= 1.0 (:draw s)) "fully written before the next step starts")
            (is (get-in s [:opts :write]) "written on like chalk")))))))

(deftest a-step-is-written-left-to-right
  (let [ps (params :problem :by-parts)
        p  (bb/plan ps)
        g  (graph ps)
        t0 (first (:starts p))
        ds (map #(:draw (tex-of p (frame-at g (+ t0 %)) [0 :eq])) [0.2 0.5 0.9])]
    (is (apply < ds))
    (is (every? #(< 0.0 % 1.0) ds))))

(deftest the-board-scrolls-to-keep-the-current-step-in-view
  (doseq [l bl/lessons
          :let [ps (params :problem (:id l))
                p  (bb/plan ps)
                f  (frame-at (graph ps) (:end p))
                last-i (dec (count (:starts p)))
                s  (tex-of p f [last-i :eq])
                {:keys [bottom top]} bb/geom]]
    (testing (name (:id l))
      (is (frame/visible? s))
      (is (<= bottom (second (:at s)) top) "the last line is on the board")
      (testing "lines pushed above the board top are faded out"
        (doseq [{:keys [key gone?]} (:items p) :when gone?]
          (is (not (frame/visible? (tex-of p f key))) (pr-str key)))))))

(deftest step-mode-shows-only-the-steps-asked-for
  (doseq [k [0 1 2]]
    (let [ps (params :auto false :step k)
          p  (bb/plan ps)]
      (is (= k (count (:starts p))))
      (is (= k (bb/shown-steps ps)))))
  (testing "all steps plus the check line once every step is shown"
    (let [n (bb/step-count (params))
          p (bb/plan (params :auto false :step n))]
      (is (= (inc n) (count (:starts p))))
      (is (:check? (peek (:steps p)))))))

(deftest explanations-toggle-adds-and-removes-the-english-lines
  (let [with    (bb/plan (params :explain true))
        without (bb/plan (params :explain false))]
    (is (some #(= :ex (second (:key %))) (:items with)))
    (is (not-any? #(= :ex (second (:key %))) (:items without)))))

(deftest chalk-macros-and-explanations-become-tex
  (is (= "\\textcolor{#FFE27A}{u}" (bb/chalk-tex "\\yel{u}")))
  (is (= "\\textit{Let}\\ u = 1\\ \\textit{now.}" (bb/explain->tex "Let $u = 1$ now.")))
  (is (= "\\textit{50\\% off}" (bb/explain->tex "50% off"))))

(deftest controls-are-well-formed
  (is (= bb/controls (controls/validate bb/controls)))
  (is (= #{:problem :path :explain :auto :step} (set (keys (bb/defaults))))))

;; ── widgets through the recording host ──────────────────────────────────────

(defn- open []
  (player/open! (rh/host) bb/blackboard {:controls bb/controls :autoplay? false}))

(defn- clock [pl] (:t (:session @(:state pl))))

(deftest next-and-previous-step-buttons
  (let [pl (open)]
    (testing "next leaves auto-play, writes step 1 and jumps to where it starts"
      (player/press! pl :next)
      (is (= {:step 1 :auto false} (select-keys (player/params pl) [:step :auto])))
      (is (< (Math/abs (- (clock pl) (bb/step-start (player/params pl) 0))) 1e-9))
      (is (:playing? (:session @(:state pl)))))
    (player/press! pl :next)
    (is (= 2 (:step (player/params pl))))
    (is (< (Math/abs (- (clock pl) (bb/step-start (player/params pl) 1))) 1e-9))
    (testing "the frame then shows step 2 being written, step 1 done"
      (player/seek! pl (+ (clock pl) 0.5))
      (let [p (bb/plan (player/params pl))
            f (player/current-frame pl)]
        (is (= 1.0 (:draw (tex-of p f [0 :eq]))))
        (is (< 0.0 (:draw (tex-of p f [1 :eq])) 1.0))))
    (testing "previous goes back one step and shows it finished"
      (player/press! pl :prev)
      (is (= 1 (:step (player/params pl))))
      (is (< (Math/abs (- (clock pl) (bb/board-end (player/params pl)))) 1e-9)))
    (player/press! pl :prev)
    (player/press! pl :prev)
    (is (= 0 (:step (player/params pl))) "never below 0")))

(deftest show-all-restart-and-auto-play
  (let [pl (open)
        n  (bb/step-count (player/params pl))]
    (player/press! pl :all)
    (is (= n (:step (player/params pl))))
    (is (< (Math/abs (- (clock pl) (bb/board-end (player/params pl)))) 1e-9))
    (let [p (bb/plan (player/params pl))
          f (player/current-frame pl)]
      (is (frame/visible? (tex-of p f [(dec n) :eq])) "the answer is on the board"))
    (player/press! pl :next)
    (is (= n (:step (player/params pl))) "next stops at the last step")
    (player/press! pl :restart)
    (is (= 0 (:step (player/params pl))))
    (is (= 0.0 (clock pl)))
    (player/control-input! pl :auto true)
    (is (true? (:auto (player/params pl))))
    (is (= 0.0 (clock pl)) "auto-play starts the lesson over")))

(deftest problem-and-method-selects
  (let [pl (open)]
    (player/seek! pl 5.0)
    (player/control-input! pl :problem :cyclic)
    (is (= :cyclic (:problem (player/params pl))))
    (is (= 0.0 (clock pl)) "a new problem is written from the start")
    (player/control-input! pl :path 1)
    (is (= "complex exponential" (:label (bb/plan (player/params pl)))))
    (is (some? (:error (player/control-input! pl :problem :nope))) "not an option")))
