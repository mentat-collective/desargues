(ns desargues.bench.geometry
  "Solve-time benchmarks of the geometry layer: the homography fit, the
   disposition solver, the trajectory check and the raw H-over-cloud kernel,
   once per available GeometryKernel provider (array; raster under :dynamics).

     clojure -M:bench:dynamics -m desargues.bench.geometry [--out DIR]

   Writes DIR/results/geometry.edn (DIR defaults to bench). Runs against a
   tree without desargues.geometry.kernel too (one :baseline provider), which
   is how before/after numbers are taken."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [desargues.bench :as b]
            [desargues.geometry.disposition :as d]
            [desargues.geometry.fit :as fit]
            [desargues.geometry.projective :as g]
            [emmy.env :as e]))

(defn- say [& xs] (println (apply str xs)) (flush))

(defn- kernel-fn
  "The var named by sym, or nil when its namespace is not on the classpath
   (the :baseline tree has no desargues.geometry.kernel; that absence is the
   answer, not an error)."
  [sym]
  (try (requiring-resolve sym)
       (catch java.io.FileNotFoundException _ns-absent nil)))

(defn providers
  "[label kernel-or-nil binder]; binder runs a thunk with that kernel bound.
   Without desargues.geometry.kernel on the classpath: one :baseline row."
  []
  (if-let [select (kernel-fn 'desargues.geometry.kernel/select-kernel)]
    (let [kvar (kernel-fn 'desargues.geometry.kernel/*kernel*)]
      (for [[label k] [["array" (select :array)] ["raster" (select :raster)]]
            :when k]
        [label k (fn [f] (with-bindings {kvar k} (f)))]))
    [["baseline" nil (fn [f] (f))]]))

(def figure
  (let [base (:points (g/desargues-configuration
                       {:O [0.0 3.0] :P [1.0 -3.1] :Q [-4.25 -3.1]
                        :angs [-2.25 -1.57 -1.2] :ra 2.0 :ra' 4.3}))
        H (e/* (g/translate-h [16.0 2.0]) (g/scale-h 4.0))]
    (into {} (map (fn [[k p]] [k (mapv double (g/apply-h H p))])) base)))

(def figure-2
  (:points (g/desargues-configuration
            {:O [0.0 3.0] :P [1.0 -2.3] :Q [-3.5 -3.1]
             :angs [-2.25 -1.57 -1.05] :ra 2.0 :ra' 4.3})))

(def construction (d/desargues-disposition))

(def slow {:warmup-ms 3000 :runs 5 :target-ms 1})

(defn- cloud-rows
  "Raw H over an n-point cloud: Emmy per point (what the fit used before)
   and each kernel's transform-cloud!."
  [providers]
  (let [H (g/similarity-h [0.1 0.25 -4.0 -0.5])
        h->array (kernel-fn 'desargues.geometry.kernel/h->array)
        transform! (kernel-fn 'desargues.geometry.kernel/transform-cloud!)]
    (into []
          cat
          (for [n [10 100 1000 10000]]
            (let [rnd (java.util.Random. 7)
                  xs (double-array (repeatedly n #(.nextDouble rnd)))
                  ys (double-array (repeatedly n #(.nextDouble rnd)))
                  pts (mapv vector xs ys)]
              (say "  n = " n)
              (into (if (<= n 1000)
                      [(merge {:group :h-cloud :id "emmy apply-h" :n n}
                              (b/sample #(mapv (fn [p] (g/apply-h H p)) pts)))]
                      [])
                    (for [[label k _] providers :when k]
                      (let [h (h->array H)
                            ox (double-array n) oy (double-array n) ow (double-array n)]
                        (merge {:group :h-cloud :id (str label " kernel") :n n}
                               (b/sample #(transform! k h xs ys ox oy ow)))))))))))

(defn- solve-rows [providers]
  (let [frames (d/solve-keyframes construction 3)
        subjects [["fit-homography :similarity" slow #(fit/fit-homography figure :family :similarity)]
                  ["fit-homography :projective" slow #(fit/fit-homography figure :family :projective)]
                  ["fit-keyframes x2 :similarity" slow
                   #(fit/fit-keyframes [figure figure-2] :family :similarity)]
                  ["solve-keyframes 3" slow #(d/solve-keyframes construction 3)]
                  ["valid-trajectory? 3 frames x 24" {} #(d/valid-trajectory? construction frames 24)]]]
    (vec (for [[id opts f] subjects
               [label _ bind] providers]
           (do (say "  " id " / " label)
               (merge {:group :geometry :id id :provider label}
                      (bind #(b/sample f opts))))))))

(defn -main [& args]
  (let [out (or (second (drop-while #(not= "--out" %) args)) "bench")
        ps (vec (providers))
        _ (say "providers: " (mapv first ps))
        _ (say "h-cloud")
        cloud (cloud-rows ps)
        _ (say "geometry solve")
        solve (solve-rows ps)
        rep (assoc (b/system-info) :results (into cloud solve))
        f (io/file out "results" "geometry.edn")]
    (.mkdirs (.getParentFile f))
    (spit f (with-out-str (pp/pprint rep)))
    (pp/print-table [:group :id :provider :n :mean-ms :std-ms :batch] (:results rep))
    (say "results: " f)
    (shutdown-agents)
    (System/exit 0)))
