(ns desargues.board-test
  "Boards, layer by layer: the pure plan, the compiler port, the Board value,
   and the acceptance test of the design, that a new board kind is a
   registration and never an edit.
   Needs raster: `clojure -M:test:dynamics -n desargues.board-test`."
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is testing]]
            [desargues.board :as board]
            [desargues.board.kernel :as kernel]
            [desargues.specs.board :as spec]))

(def wave
  {:id :wave-test :kind :calculus
   :f '(+ (* a (sin (* b x))) (* c (* x x))) :var 'x
   :params [{:id 'a :min -2 :max 2 :step 0.01 :init 1}
            {:id 'b :min 0.1 :max 4 :step 0.01 :init 1}
            {:id 'c :min -0.5 :max 0.5 :step 0.005 :init 0}]})

(defn- tmp-dir [] (doto (io/file (System/getProperty "java.io.tmpdir") "desargues-board-test") .mkdirs))

(defn- max-err [actual f xs]
  (reduce max (map #(Math/abs (- (double %1) (f %2))) actual xs)))

(deftest pure-plan
  (testing "raster differentiates f symbolically"
    (is (= '(+ (* a (* (cos (* b x)) b)) (* c (+ x x))) (kernel/derivative wave))))
  (testing "the :calculus plan is data: outputs, layers, probes, and a form builder"
    (let [p (kernel/plan wave)]
      (is (= [:xs :ys :dys :iys] (:outputs p)))
      (is (every? :layer (:layers p)))
      (is (= 'raster.core/deftm (first ((:form p) 'k!))))))
  (testing "an unknown kind says how to register one"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"register one"
                          (kernel/plan (assoc wave :kind :no-such-kind))))))

(deftest invalid-specs-are-refused-at-the-boundary
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid board spec"
                        (board/compile-board! (assoc-in wave [:params 0 :init] 99)
                                              {:out-dir (.getPath (tmp-dir))}))))

(deftest compile-board-writes-wasm-and-samples-the-frame
  (let [dir (tmp-dir)
        b (board/compile-board! wave {:out-dir (.getPath dir) :url-base "./b/"})
        {:keys [xs ys dys iys]} (:board/frame b)
        wasm (io/file dir "wave-test.wasm")]
    (is (s/valid? ::spec/board b) "the value honours the Board contract")
    (testing "the module is on disk and is WebAssembly"
      (is (= [0 97 115 109] (with-open [in (io/input-stream wasm)] (vec (repeatedly 4 #(.read in)))))))
    (testing "the board names its kernel, outputs and layers"
      (is (= {:wasm "./b/wave-test.wasm" :export "wave-test"} (:board/kernel b)))
      (is (= [:xs :ys :dys :iys] (:board/outputs b)))
      (is (= [:a :b :c] (mapv :id (:board/params b)))))
    (testing "the frame is the kernel run on the JVM at :init (a=1 b=1 c=0: f = sin)"
      (is (= 401 (count xs)))
      (is (< (max-err ys #(Math/sin %) xs) 1e-12))
      (is (< (max-err dys #(Math/cos %) xs) 1e-12))
      (is (< (max-err iys #(- (Math/cos -4.0) (Math/cos %)) xs) 1e-4)))))

;; ---- OCP acceptance: a new kind is a defmethod, nothing else ---------------

(defmethod kernel/plan ::parabola-test [spec]
  {:outputs [:xs :ys]
   :form (fn [kname]
           (kernel/sweep-form kname spec [:xs :ys]
                              [['y '(* k (* x x))]]
                              ['x 'y]
                              []))
   :layers [{:layer :curve :of :ys}]
   :probes {}
   :labels {:f "k·x²"}})

(deftest a-new-kind-is-a-registration
  (let [b (board/compile-board! {:id :parabola-test :kind ::parabola-test
                                 :params [{:id 'k :min 0 :max 2 :init 0.5}]
                                 :window {:x [-2 2] :y [0 3] :n 5}}
                                {:out-dir (.getPath (tmp-dir))})]
    (is (s/valid? ::spec/board b))
    (is (= [:xs :ys] (:board/outputs b)))
    (is (= [2.0 0.5 0.0 0.5 2.0] (get-in b [:board/frame :ys])) "k x² at k = 0.5")))
