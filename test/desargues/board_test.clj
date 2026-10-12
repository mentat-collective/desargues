(ns desargues.board-test
  "Boards, layer by layer: Emmy's algebra and its realisations, the pure plan,
   the compiler port, the Board value, and the acceptance test of the design,
   that a new board kind is a registration and never an edit.
   Needs raster: `clojure -M:test:dynamics -n desargues.board-test`."
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is testing]]
            [desargues.board :as board]
            [desargues.board.algebra :as algebra]
            [desargues.board.kernel :as kernel]
            [desargues.specs.board :as spec]))

(def wave
  {:id :wave-test :kind :calculus
   :f '(+ (* a (sin (* b x))) (* c (expt x 2))) :var 'x
   :params [{:id 'a :min -2 :max 2 :step 0.01 :init 1}
            {:id 'b :min 0.1 :max 4 :step 0.01 :init 1}
            {:id 'c :min -0.5 :max 0.5 :step 0.005 :init 0}]})

(defn- tmp-dir [] (doto (io/file (System/getProperty "java.io.tmpdir") "desargues-board-test") .mkdirs))

(defn- max-err [actual f xs]
  (reduce max (map #(Math/abs (- (double %1) (f %2))) actual xs)))

(deftest emmy-does-the-algebra
  (testing "f' is Emmy's D, simplified"
    (is (= '(+ (* a b (cos (* b x))) (* 2 c x))
           (algebra/realize :sexp (algebra/derivative wave)))))
  (testing "the same expression, realised for TeX"
    (is (= "a\\,b\\,\\cos\\left(b\\,x\\right) + 2\\,c\\,x"
           (algebra/realize :tex (algebra/derivative wave))))))

(deftest raster-realisation
  (testing "binary ops, raster.math transcendentals, double literals"
    (is (= '(+ (* (* a b) (raster.math/cos (* b x))) (* (* 2.0 c) x))
           (algebra/realize :raster (algebra/derivative wave)))))
  (testing "integer powers become products; raster's wasm pow is undefined for x < 0"
    (is (= '(* (* x x) x)
           (algebra/realize :raster (algebra/expression {:f '(expt x 3) :var 'x :params []}))))
    (is (= '(/ 1.0 (* x x))
           (algebra/realize :raster (algebra/expression {:f '(expt x -2) :var 'x :params []}))))))

(deftest pure-plan
  (let [p (kernel/plan wave)]
    (is (= [:xs :ys :dys :iys] (:outputs p)))
    (is (every? :layer (:layers p)))
    (is (= 'raster.core/deftm (first ((:form p) 'k!))))
    (is (string? (get-in p [:labels :df-tex]))))
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
    (testing "the math as TeX display lines, Emmy's f and f'"
      (is (= ["f(x) = c\\,{x}^{2} + a\\,\\sin\\left(b\\,x\\right)"
              "f'(x) = a\\,b\\,\\cos\\left(b\\,x\\right) + 2\\,c\\,x"]
             (:board/math b))))
    (testing "the frame is raster's kernel on the JVM at :init (a=1 b=1 c=0: f = sin)"
      (is (= 401 (count xs)))
      (is (< (max-err ys #(Math/sin %) xs) 1e-12))
      (is (< (max-err dys #(Math/cos %) xs) 1e-12))
      (is (< (max-err iys #(- (Math/cos -4.0) (Math/cos %)) xs) 1e-4)))))

;; ---- OCP acceptance: a new kind is a defmethod, nothing else ---------------

(defmethod kernel/plan ::parabola-test [spec]
  (let [f (algebra/expression spec)]
    {:outputs [:xs :ys]
     :form (fn [kname]
             (kernel/sweep-form kname spec [:xs :ys]
                                [['y (algebra/realize :raster f)]]
                                ['x 'y]
                                []))
     :layers [{:layer :curve :of :ys}]
     :probes {}
     :labels {:f-tex (algebra/realize :tex f)}}))

(deftest a-new-kind-is-a-registration
  (let [b (board/compile-board! {:id :parabola-test :kind ::parabola-test
                                 :f '(* k (expt x 2)) :var 'x
                                 :params [{:id 'k :min 0 :max 2 :init 0.5}]
                                 :window {:x [-2 2] :y [0 3] :n 5}}
                                {:out-dir (.getPath (tmp-dir))})]
    (is (s/valid? ::spec/board b))
    (is (= [:xs :ys] (:board/outputs b)))
    (is (= [2.0 0.5 0.0 0.5 2.0] (get-in b [:board/frame :ys])) "k x² at k = 0.5")))

;; ---- :construction: geometry as data -------------------------------------

(def figure
  {:id :figure-test :kind :construction
   :params [{:id 't :min 0 :max 1 :init 0.25 :label "turn"}]
   :maps {:R '[[(cos t) (- (sin t)) 0] [(sin t) (cos t) 0] [0 0 1]]}
   :points [{:id :A :at [0 0]} {:id :B :at [1 1]} {:id :C :at [0 1]} {:id :D :at [1 0] :fixed? true}
            {:id :P :op :meet :lines [[:A :B] [:C :D]]}
            {:id :M :op :mid :of [:A :B]}
            {:id :X :op :on :line [:A :B] :s 0.25}
            {:id :B' :op :map :by [:R] :of :B}]
   :draw [[:segment :A :B] [:line :C :D] [:point :P]]
   :checks [{:distance [:A :B] :label "|AB|"} {:collinear [:A :M :B]}
            {:distance [:A :B'] :label "|AB'|"} {:angle [:A :C :B]}]})

(defn- at-point [b id]
  (let [{[kx ky] :at} (first (filter #(and (= :handle (:layer %)) (= (name id) (:label %)))
                                     (:board/layers b)))]
    [(first (get-in b [:board/frame kx])) (first (get-in b [:board/frame ky]))]))

(deftest a-construction-is-one-kernel-call
  (let [b (board/compile-board! figure {:out-dir (.getPath (tmp-dir))})
        v (fn [k] (first (get-in b [:board/frame k])))]
    (is (s/valid? ::spec/board b))
    (is (= 1 (get-in b [:board/window :n])) "one configuration per call")
    (testing "the meet of y = x and y = 1 - x, a midpoint, a point on AB"
      (is (= [0.5 0.5] [(v :o-pt4x) (v :o-pt4y)]))
      (is (= [0.5 0.5] [(v :o-pt5x) (v :o-pt5y)]))
      (is (= [0.25 0.25] (at-point b :X))))
    (testing "the map R(t) acts; distance survives it, and the checks read out"
      (is (< (Math/abs (- (v :o-pt7x) (- (Math/cos 0.25) (Math/sin 0.25)))) 1e-12))
      (is (< (Math/abs (- (v :o-ck0) (Math/sqrt 2))) 1e-12))
      (is (< (Math/abs (- (v :o-ck2) (Math/sqrt 2))) 1e-12))
      (is (== 0.0 (v :o-ck1)))
      (is (< (Math/abs (- (v :o-ck3) 90.0)) 1e-9)))
    (testing "free points own their coordinates as params; a pinned one has no handle"
      (is (= [:t :inpt0x :inpt0y] (take 3 (map :id (:board/params b)))))
      (is (= :point (:control (second (:board/params b)))))
      (is (= #{"A" "B" "C" "X"} (set (keep #(when (= :handle (:layer %)) (:label %)) (:board/layers b))))))))

;; OCP acceptance: a new point operation is a defmethod, nothing else.
(defmethod desargues.board.construction/point ::reflect-test [{:keys [of]} env]
  (let [{:keys [x y]} (get-in env [:points of])] {:xy [(list '- x) y]}))

(deftest a-new-point-op-is-a-registration
  (let [b (board/compile-board! {:id :reflect-test :kind :construction
                                 :points [{:id :A :at [2 1]} {:id :A* :op ::reflect-test :of :A}]}
                                {:out-dir (.getPath (tmp-dir))})]
    (is (= [-2.0 1.0] [(first (get-in b [:board/frame :o-pt1x])) (first (get-in b [:board/frame :o-pt1y]))]))))
