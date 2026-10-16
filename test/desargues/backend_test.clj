(ns desargues.backend-test
  "The backend registry and the ports that dispatch on a backend key. Pure:
   a stub backend stands in for an engine, so nothing here needs raster. The
   :raster checks at the end run only when raster is on the classpath
   (`clojure -M:test:dynamics`)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [desargues.backend :as backend]
            [desargues.board.compiler :as compiler]
            [desargues.geometry.kernel :as gk]
            [desargues.kernel :as kernel]
            [desargues.physics :as phys]))

(defrecord RecordingSolver [calls]
  phys/TrajectorySolver
  (solve-trajectory [_ system opts]
    (swap! calls conj opts)
    [{:time 0.0 :state (phys/get-state system)}]))

(defn- with-stub-backend [f]
  (backend/register-backend! ::stub {:namespaces [] :requires "nothing"})
  (backend/register-backend! ::absent {:namespaces '[desargues.no-such-provider] :requires "a library nobody ships"})
  (defmethod phys/solver ::stub
    ([_] (->RecordingSolver (atom [])))
    ([_ {:keys [calls]}] (->RecordingSolver calls)))
  (try
    (f)
    (finally
      (remove-method phys/solver ::stub))))

(use-fixtures :each with-stub-backend)

(def pendulum (phys/make-pendulum {:length 3.0 :damping 0.1 :initial-theta 0.3}))

(deftest registry
  (testing "the shipped backends"
    (is (contains? (backend/backends) :jvm))
    (is (contains? (backend/backends) :raster))
    (is (backend/available? :jvm)))
  (testing "an unknown backend names the known ones"
    (is (false? (backend/available? ::nobody)))
    (let [e (is (thrown? clojure.lang.ExceptionInfo (backend/load! ::nobody)))]
      (is (= :unknown (:reason (ex-data e))))
      (is (contains? (:known (ex-data e)) :jvm))))
  (testing "a backend whose provider is not on the classpath is absent, and says what it needs"
    (is (false? (backend/available? ::absent)))
    (let [e (is (thrown-with-msg? clojure.lang.ExceptionInfo #"a library nobody ships"
                                  (backend/load! ::absent)))]
      (is (= :absent (:reason (ex-data e)))))))

(deftest a-port-dispatches-on-the-backend-key
  (testing ":jvm is the built-in stepper"
    (is (= (phys/stepper-solver) (phys/solver :jvm)))
    (is (= (phys/stepper-solver phys/euler) (phys/solver :jvm {:integrator phys/euler}))))
  (testing "a registered backend answers through its own method"
    (let [calls (atom [])]
      (phys/evolve pendulum {:solver [::stub {:calls calls}] :dt 0.5 :duration 1.0})
      (is (= [{:dt 0.5 :duration 1.0}] @calls))))
  (testing "a backend without a method for a port says which port"
    (let [e (is (thrown-with-msg? clojure.lang.ExceptionInfo #"geometry kernel"
                                  (gk/provider ::stub)))]
      (is (= :no-method (:reason (ex-data e)))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"board kernel compiler"
                          (compiler/kernel-compiler ::stub))))
  (testing "an unknown or absent backend fails at the port, never silently"
    (is (thrown? clojure.lang.ExceptionInfo (phys/solver ::nobody)))
    (is (thrown? clojure.lang.ExceptionInfo (phys/solver ::absent)))))

(deftest evolve-takes-a-backend-key
  (let [default (phys/evolve pendulum {:dt 0.01 :duration 0.5})]
    (is (= default (phys/evolve pendulum {:solver :jvm :dt 0.01 :duration 0.5})))
    (is (= default (phys/evolve pendulum {:solver (phys/solver :jvm) :dt 0.01 :duration 0.5})))
    (is (= (phys/evolve pendulum {:integrator phys/euler :dt 0.01 :duration 0.5})
           (phys/evolve pendulum {:solver [:jvm {:integrator phys/euler}] :dt 0.01 :duration 0.5})))
    (is (= [default default]
           (phys/evolve-batch [pendulum pendulum] {:solver :jvm :dt 0.01 :duration 0.5})))))

(deftest geometry-kernel-by-key
  (is (identical? gk/array-kernel (gk/provider :jvm)))
  (is (identical? gk/array-kernel (gk/select-kernel :jvm)))
  (is (identical? gk/array-kernel (gk/select-kernel :auto)))
  (is (identical? gk/array-kernel (gk/select-kernel :array))))

(deftest realize-is-open-by-target
  (testing "an unknown target says how to register one"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"defmethod desargues.kernel/realize"
                          (kernel/realize ::glsl 'x))))
  (testing "a new target is a registration"
    (defmethod kernel/realize ::shout [_ expr] (list 'shout expr))
    (try
      (is (= '(shout x) (kernel/realize ::shout 'x)))
      (finally (remove-method kernel/realize ::shout)))))

(def ^:private source
  "An Emmy :primitive kernel source by hand: out[0] = 1, out[1] = a chain of
   12 bindings over in[1] and in[2]."
  (let [in (with-meta 'in {:tag 'doubles})
        out (with-meta 'out {:tag 'doubles})
        ps (with-meta 'ps {:tag 'doubles})
        chain (mapcat (fn [i]
                        [(symbol (str "b" i))
                         (list 'clojure.core/+ (list 'clojure.core/* (symbol (str "b" (dec i))) 'y)
                               (list 'Math/sin (symbol (str "b" (max 0 (- i 3))))))])
                      (range 1 13))]
    (list 'fn [in out ps]
          (list 'clojure.core/let (vec (concat ['x (list 'clojure.core/aget in 1)
                                                'y (list 'clojure.core/aget in 2)
                                                'b0 (list 'clojure.core/* 'x 'y)]
                                               chain))
                (list 'clojure.core/doto out
                      (list 'clojure.core/aset 0 1.0)
                      (list 'clojure.core/aset 1 'b12)
                      (list 'clojure.core/aset 2 (list 'clojure.core/+ 'b5 'b11)))))))

(defn- run-kernel [f]
  (let [out (double-array 3)]
    (f (double-array [0.0 0.7 -1.3]) out (double-array 0))
    (vec out)))

(deftest emmy-kernels-as-data
  (let [ir (kernel/source->ir source)]
    (testing "the ir names the arrays, the bindings in order and what is stored"
      (is (= '[in out ps] ((juxt :in :out :params) ir)))
      (is (= 15 (count (:bindings ir))))
      (is (= '[x (clojure.core/aget in 1)] (first (:bindings ir))))
      (is (= [0 1 2] (mapv first (kernel/stores ir))))
      (is (= 'b12 (second (second (kernel/stores ir))))))
    (testing "chunks read back only earlier bindings, in slot order"
      (let [{:keys [slots chunks tail-needs]} (kernel/chunk (:bindings ir) (:body ir) 4)]
        (is (= 4 (count chunks)))
        (is (= (mapv first (:bindings ir)) (mapcat #(map first (:bindings %)) chunks)))
        (is (empty? (:needs (first chunks))))
        (is (every? (fn [{:keys [needs bindings]}]
                      (and (= needs (sort-by slots needs))
                           (every? #(< (slots %) (slots (ffirst bindings))) needs)))
                    chunks))
        (is (= '[b5 b11 b12] tail-needs))))
    (testing "the chunked JVM kernel is bit-identical to the unsplit one"
      (let [whole (run-kernel (eval (kernel/jvm-form ir 1000)))]
        (is (= 1.0 (first whole)))
        (doseq [size [1 2 3 4 7 14 15]]
          (is (= whole (run-kernel (eval (kernel/jvm-form ir size))))
              (str "chunk size " size)))
        (is (= whole (run-kernel (kernel/compile-jvm source))))))))

(deftest raster-backend-when-present
  (if (backend/available? :raster)
    (let [stepper (phys/evolve pendulum {:dt 0.01 :duration 1.0})]
      (testing "the whole-loop kernel answers the stepper's trajectory, bit for bit"
        (is (= stepper (phys/evolve pendulum {:solver [:raster {:kernel? true}] :dt 0.01 :duration 1.0}))))
      (testing "raster's own solvers by :alg"
        (is (= (count stepper)
               (count (phys/evolve pendulum {:solver [:raster {:alg :tsit5}] :dt 0.01 :duration 1.0})))))
      (testing "the geometry kernel and the board compiler come from the same key"
        (is (satisfies? gk/GeometryKernel (gk/provider :raster)))
        (is (some? (gk/select-kernel :raster)))
        (is (satisfies? compiler/KernelCompiler (compiler/kernel-compiler :raster)))))
    (do (println "[backend-test] raster not on classpath (use -A:dynamics); skipping")
        (is (nil? (gk/select-kernel :raster))))))
