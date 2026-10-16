(ns desargues.studio.widget-demos-test
  "The non-slider demos (desargues.studio.widget-demos): their math, and each
   widget kind driven through the windowless recording host the way a user
   would (a press, a choice, typing, a click on a toggle)."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.player :as player]
            [desargues.player.recording-host :as rh]
            [desargues.scene.record :as recorder]
            [desargues.studio.widget-demos :as w]))

(defn- open [k]
  (let [h (rh/host)
        {:keys [construct controls]} (w/demos k)]
    [h (player/open! h construct {:controls controls :autoplay? false})]))

(deftest busy-beaver-halts-after-14-steps-with-six-ones
  (is (= 14 (w/halting-steps w/busy-beaver 1000)))
  (let [m (w/run-machine w/busy-beaver 100)]
    (is (= :halt (:state m)))
    (is (= 6 (count (filter #{1} (vals (:tape m))))))))

(deftest platonic-solids-have-the-right-counts
  (is (= {:tetrahedron [4 6] :cube [8 12] :octahedron [6 12]
          :dodecahedron [20 30] :icosahedron [12 30]}
         (into {} (for [k (keys w/solid-vertices)]
                    [k [(count (w/solid-vertices k)) (count (w/solid-edges k))]])))))

(deftest goedel-numbers
  (testing "Nagel and Newman's example: 0 = 0 is 2^6 3^5 5^6"
    (is (= "243000000" (w/godel-number "0=0"))))
  (is (= "s0=s0" (w/parse-formula " s0 = s0 ")))
  (is (= {:error "not in Goedel's alphabet: q"} (w/parse-formula "0=q")))
  (is (contains? (w/parse-formula "  ") :error)))

(deftest every-demo-records-at-its-defaults
  (doseq [[k {:keys [construct]}] w/demos]
    (testing (name k)
      (is (pos? (count (:nodes (recorder/record (name k) (construct (w/defaults k))))))))))

(deftest turing-buttons-step-run-and-reset
  (let [[h p] (open :turing)]
    (is (= [:button :button :button] (mapv :kind (rh/widgets h))))
    (rh/user! h :step nil)
    (rh/user! h :step nil)
    (is (= 2 (:steps (player/params p))))
    (rh/user! h :run nil)
    (is (= 14 (:steps (player/params p))))
    (rh/user! h :step nil)
    (is (= 14 (:steps (player/params p))) "a halted machine stays halted")
    (rh/user! h :reset nil)
    (is (= 0 (:steps (player/params p))))))

(deftest kepler-select-slider-and-toggle
  (let [[h p] (open :kepler)]
    (is (= [:select nil :toggle] (mapv :kind (rh/widgets h))))
    (rh/user! h :solid :dodecahedron)
    (is (= :dodecahedron (:solid (player/params p))))
    (rh/user! h :solid :sphere)
    (is (= :dodecahedron (:solid (player/params p))) "a value outside the options is refused")
    (is (some? (rh/error h :solid)))
    (rh/user! h :turn 2.0)
    (rh/user! h :spheres false)
    (is (= {:solid :dodecahedron :turn 2.0 :spheres false} (player/params p)))))

(deftest goedel-text-field-refuses-and-accepts
  (let [[h p] (open :goedel)]
    (is (= "0=0" (:formula (player/params p))))
    (rh/user! h :formula "0=q")
    (is (= "0=0" (:formula (player/params p))))
    (is (= "not in Goedel's alphabet: q" (rh/error h :formula)))
    (rh/user! h :formula "(Ex)(x=s0)")
    (is (= "(Ex)(x=s0)" (:formula (player/params p))))
    (is (nil? (rh/error h :formula)))))
