(ns desargues.bench.live-frames
  "DESARGUES-PERF-LIVE-1 bench, warm process, 480p (low_quality, 854x480 @15):
   - edit -> first frame at the IFrameSink (desargues.manim.frames/render-live!),
     interleaved run-for-run against devx preview! (render-preview!: the last
     frame saved as PNG). An 'edit' is a fresh Scene class whose construct
     differs (circle radius) each run.
   - per-frame Python -> JVM crossing of one 854x480 RGBA frame: zero-copy
     tensor view vs a JVM-heap clone, measured from the Python side.
   Needs the Manim env and the dev classpath (devx). Run: (run!)"
  (:require [desargues.bench.sample :as smp]
            [desargues.manim.core :as mc]
            [desargues.manim.frames :as fr]
            [libpython-clj2.python :as py]))

(defn- construct [M i]
  (fn [self]
    (let [c (py/call-attr-kw M "Circle" [] {"radius" (+ 1.0 (* 0.01 (mod i 50)))})]
      (mc/play! self (py/call-attr M "Create" c))
      (mc/wait! self 0.5))))

(defn first-frame-run
  "One edit -> live render. {:first-ms :total-ms :frames}."
  [M i]
  (let [t0   (System/nanoTime)
        sink (fr/collecting-sink)
        Sc   (mc/create-scene-class (str "LiveEdit" i) (construct M i))]
    (fr/render-live! #(Sc) sink {:quality "low_quality"})
    (let [c (fr/collected sink)]
      {:first-ms (/ (- (:first-ns c) t0) 1e6)
       :total-ms (/ (- (System/nanoTime) t0) 1e6)
       :frames   (:count c)})))

(defn preview-run
  "One edit -> devx preview! (last frame, PNG), ms."
  [M i]
  (let [segment     (requiring-resolve 'desargues.devx.core/segment)
        render-prev (requiring-resolve 'desargues.devx.preview/render-preview!)
        seg (segment (keyword (str "pv" i)) (construct M i))
        out (str (System/getProperty "java.io.tmpdir") "/desargues-bench-preview-" i ".png")
        t0  (System/nanoTime)]
    (binding [*out* (java.io.StringWriter.)]
      (when-not (render-prev seg :quality :low :output-path out)
        (throw (ex-info "preview! failed" {:i i}))))
    (/ (- (System/nanoTime) t0) 1e6)))

(defn crossing
  "ns per frame to cross one 854x480 RGBA numpy frame into a NullSink, k frames."
  [k copy?]
  (let [np    (py/import-module "numpy")
        frame (py/call-attr-kw np "zeros" [(py/->py-tuple [480 854 4])] {:dtype "uint8"})]
    (fr/crossing-ns frame (fr/null-sink) 200 {:copy? copy?}) ; warm
    (/ (double (fr/crossing-ns frame (fr/null-sink) k {:copy? copy?})) k)))

(defn run!
  ([] (run! {}))
  ([{:keys [warmup runs] :or {warmup 5 runs 30}}]
   (let [M (mc/manim)]
     (py/call-attr (py/get-attr M "logger") "setLevel" "ERROR")
     (dotimes [i warmup] (first-frame-run M i) (preview-run M i))
     (let [rs (vec (for [i (range warmup (+ warmup runs))]
                     (assoc (first-frame-run M i) :preview-ms (preview-run M i))))]
       {:frames        (:frames (first rs))
        :first-frame   (smp/summarize (map :first-ms rs))
        :live-total    (smp/summarize (map :total-ms rs))
        :preview       (smp/summarize (map :preview-ms rs))
        :crossing-ns   {:zero-copy (crossing 2000 false)
                        :copy      (crossing 2000 true)}}))))
