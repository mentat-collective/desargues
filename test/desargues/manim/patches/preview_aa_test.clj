(ns desargues.manim.patches.preview-aa-test
  (:require [clojure.test :refer [deftest is testing]]
            [libpython-clj2.python :as py]
            [desargues.config :as config]
            [desargues.manim.core :as core]
            [desargues.manim.frames :as fr]
            [desargues.manim.patch-protocols :as pp]
            [desargues.manim.patches :as mp]
            [desargues.manim.patches.preview-aa :as paa]
            [desargues.perf.profile :as perf]
            [tech.v3.datatype :as dt]))

(def manim-env? (delay (config/manim-ready?)))

;; ---------------------------------------------------------------------------
;; Pure: the record and the profile model need no Python and no Manim config
;; ---------------------------------------------------------------------------

(deftest the-patch-record-needs-no-manim
  (let [p (paa/preview-aa)]
    (is (= :preview-aa (pp/-patch-id p)))
    (is (= paa/default-knobs (into {} p))))
  (is (= 0.5 (:tolerance (paa/preview-aa {:tolerance 1/2}))) "numbers are coerced to doubles")
  (is (= :fast (:antialias (paa/preview-aa {:antialias :fast :aa-max-alpha 0.25}))))
  (is (thrown? clojure.lang.ExceptionInfo (paa/preview-aa {:antialias :blurry})))
  (is (thrown? clojure.lang.ExceptionInfo (paa/preview-aa {:tolerance 0})))
  (is (thrown? clojure.lang.ExceptionInfo (paa/preview-aa {:aa-max-alpha 2.0}))))

(deftest preview-is-fast-plus-the-lossy-patch
  (let [fast (perf/profile :fast) prev (perf/profile :preview)]
    (is (perf/valid? prev))
    (is (true? (:lossy-ok? prev)))
    (is (false? (:exact-only? prev)))
    (is (= (conj (:patches fast) :preview-aa) (:patches prev)))
    (is (= (:live fast) (:live prev)))
    (is (= (:backend fast) (:backend prev)))))

(deftest exact-profiles-never-hold-the-lossy-patch
  (doseq [k [:stock :fast]]
    (let [p (perf/profile k)]
      (is (true? (:exact-only? p)) (str k))
      (is (false? (:lossy-ok? p)) (str k))
      (is (not-any? #{:preview-aa} (:patches p)) (str k))))
  (is (false? (:exact? (perf/patch-entry :preview-aa))))
  (is (= :preview (:id (perf/selected "Preview")))))

;; ---------------------------------------------------------------------------
;; Against Manim (needs the Manim env)
;; ---------------------------------------------------------------------------

(defn- frames [patches]
  (let [M  (core/init!)
        Sc (core/create-scene-class
            "PreviewAATestScene"
            (fn [self]
              (let [c (py/call-attr M "Circle")]
                (py/call-attr-kw c "set_fill" [] {"opacity" 0.4})
                (core/play! self (py/call-attr M "FadeIn" c)))
              (core/play! self (py/call-attr M "Create" (py/call-attr M "Square")))))
        out (atom [])]
    (mp/with-patches* patches
      #(fr/render-live! (fn [] (Sc)) (fn [t _ _ _] (swap! out conj (dt/->byte-array t)))
                        {:crossing :tensor}))
    @out))

(defn- psnr [^bytes a ^bytes b]
  (let [n (alength a)
        se (loop [i 0 se 0.0]
             (if (< i n)
               (let [d (- (bit-and (aget a i) 0xff) (bit-and (aget b i) 0xff))]
                 (recur (inc i) (+ se (* d d))))
               se))]
    (if (zero? se) 99.0 (* 10 (Math/log10 (/ (* 255.0 255.0) (/ se n)))))))

(deftest preview-aa-installs-and-stays-close-to-manim
  (if-not @manim-env?
    (println "skip: no Manim env")
    (let [p     (paa/preview-aa)
          exact (frames [])]
      (mp/uninstall! p)
      (testing "install / uninstall"
        (is (true? (mp/install! p)))
        (is (true? (mp/install! p)) "idempotent")
        (is (mp/installed? p))
        (is (true? (mp/uninstall! p)))
        (is (false? (mp/uninstall! p)))
        (is (not (mp/installed? p))))
      (testing "skipping invisible VMobjects alone is exact"
        (is (= (map seq exact)
               (map seq (frames [(paa/preview-aa {:tolerance nil :skip-invisible? true})])))))
      (testing "the shipped knobs keep every frame at or above 40 dB"
        (let [lossy (frames [p])]
          (is (= (count exact) (count lossy)))
          (is (every? #(>= % 40.0) (map psnr exact lossy)))))
      (testing "uninstall restores Manim's frames"
        (is (= (map seq exact) (map seq (frames [])))))
      (is (pos? (:batches (paa/stats)))))))
