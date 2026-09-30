(ns desargues.board-test
  "desargues.board compiles a board spec into the value plato renders.
   Needs raster: run with `clojure -M:test:dynamics -n desargues.board-test`."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [desargues.board :as board]))

(def wave
  {:id :wave-test
   :f '(+ (* a (sin (* b x))) (* c (* x x))) :var 'x
   :params [{:id 'a :min -2 :max 2 :step 0.01 :init 1}
            {:id 'b :min 0.1 :max 4 :step 0.01 :init 1}
            {:id 'c :min -0.5 :max 0.5 :step 0.005 :init 0}]})

(defn- max-err [actual f xs]
  (reduce max (map #(Math/abs (- (double %1) (f %2))) actual xs)))

(deftest derivative-is-raster-symbolic
  (is (= '(+ (* a (* (cos (* b x)) b)) (* c (+ x x)))
         (board/derivative wave))))

(deftest compile-board-writes-wasm-and-samples-the-frame
  (let [dir (doto (io/file (System/getProperty "java.io.tmpdir") "desargues-board-test") .mkdirs)
        b (board/compile-board! wave {:out-dir (.getPath dir) :url-base "./b/"})
        {:keys [xs ys dys iys]} (:board/frame b)
        wasm (io/file dir "wave-test.wasm")]
    (testing "the module is on disk and is WebAssembly"
      (is (.exists wasm))
      (is (= [0 97 115 109]                               ; \0asm
             (with-open [in (io/input-stream wasm)] (vec (repeatedly 4 #(.read in)))))))
    (testing "the board value names the kernel the page fetches"
      (is (= {:wasm "./b/wave-test.wasm" :export "wave-test"} (:board/kernel b)))
      (is (= [:a :b :c] (mapv :id (:board/params b)))))
    (testing "the frame is the kernel run on the JVM at :init (a=1 b=1 c=0: f = sin)"
      (is (= 401 (count xs)))
      (is (< (max-err ys #(Math/sin %) xs) 1e-12))
      (is (< (max-err dys #(Math/cos %) xs) 1e-12))
      (is (< (max-err iys #(- (Math/cos -4.0) (Math/cos %)) xs) 1e-4)
          "running trapezoid integral of sin from -4"))))
