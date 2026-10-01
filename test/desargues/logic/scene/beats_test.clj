(ns desargues.logic.scene.beats-test
  "The shared beat vocabulary: size estimates, captions, footprints."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.logic.scene.beats :as b]))

(def ^:private typeset-pt
  "Widths in TeX points at the 10 pt design size, measured with latex +
   dvisvgm --exact-bbox."
  {"x \\in \\left(\\left(A + B\\right) - C\\right) \\cup \\left(C - \\left(A + B\\right)\\right) \\Leftrightarrow x \\in A + \\left(B + C\\right)" 242.91
   "\\left(A + B\\right) + C = A + \\left(B + C\\right)" 122.23
   "\\lnot \\left(P \\lor Q\\right) \\Leftrightarrow \\lnot P \\land \\lnot Q" 97.23
   "x \\in A \\cap B \\Rightarrow x \\in A" 84.73})

(deftest tex-width-does-not-underestimate
  (doseq [[latex pt] typeset-pt
          :let [real (/ pt 10.0) ; world units at 60 px: one em
                est (b/tex-width latex 60)]]
    (is (<= 0.93 (/ est real) 1.35) (str latex " est " est " real " real))))

(deftest width-is-open-on-kind
  (is (pos? (b/width {:kind :tex :latex "P" :size 30})))
  (is (pos? (b/width {:kind :text :text "P" :size 30})))
  (is (zero? (b/width {:kind :line :from [0 0] :to [1 1]}))))

(deftest caption-fits
  (let [long (apply str (repeat 80 "x"))
        {[it] :items :as beat} (b/caption {:kind :text :text long :at [0 -3.5] :size 24 :color :white} 13.6)]
    (is (= [:caption] (:ids beat)))
    (is (= :caption (:id it)))
    (is (< (:size it) 24))
    (is (<= (b/width it) 13.6))))

(deftest footprints-follow-moves
  (let [bs [{:beat :show :items [{:id :a :kind :text :text "a" :at [0 0] :size 20}]}
            {:beat :move :to {:a [9 0]}}]]
    (is (= [true false] (mapv b/inside? (b/footprints bs))))))

(deftest value-tex
  (is (= "\\mathrm{T}" (b/value-tex true)))
  (is (= "\\mathrm{F}" (b/value-tex false))))
