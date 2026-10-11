(ns desargues.bench.sample
  "The sampler every desargues bench shares, and the system stamp of a report.
   A sample is a vector of per-call times; its summary carries the mean and
   standard deviation for the charts and the median and median absolute
   deviation (MAD) for the regression check.")

(defn now-ms [] (/ (System/nanoTime) 1e6))

(defn time-once
  "Wall time of one call of f, in ms."
  [f]
  (let [t0 (System/nanoTime)] (f) (/ (- (System/nanoTime) t0) 1e6)))

(defn median
  "Median of a non-empty seq of numbers."
  [xs]
  (let [v (vec (sort xs)) n (count v) h (quot n 2)]
    (if (odd? n) (double (v h)) (/ (+ (v (dec h)) (v h)) 2.0))))

(defn summarize
  "Per-call times xs (ms) -> {:mean-ms :std-ms :min-ms :median-ms :mad-ms :runs}."
  [xs]
  (let [n    (count xs)
        mean (/ (reduce + xs) n)
        var  (/ (reduce + (map (fn [x] (let [d (- x mean)] (* d d))) xs)) n)
        med  (median xs)]
    {:mean-ms mean :std-ms (Math/sqrt var) :min-ms (reduce min xs)
     :median-ms med :mad-ms (median (map #(Math/abs (double (- % med))) xs))
     :runs n}))

(defn sample
  "Time thunk f: warm up for `warmup-ms`, then take `runs` samples, each a batch
   of calls sized so one sample lasts about `target-ms`. Returns the per-call
   summary (see summarize) plus :batch."
  ([f] (sample f {}))
  ([f {:keys [warmup-ms runs target-ms] :or {warmup-ms 400 runs 30 target-ms 40}}]
   (let [deadline (+ (now-ms) warmup-ms)]
     (while (< (now-ms) deadline) (f)))
   (let [t1    (max 1e-6 (time-once f))
         batch (long (max 1 (Math/ceil (/ target-ms t1))))
         one   (fn [] (/ (time-once (fn [] (dotimes [_ batch] (f)))) batch))
         _     (System/gc)
         xs    (vec (repeatedly runs one))]
     (assoc (summarize xs) :batch batch))))

(defn system-info []
  {:generated (str (java.time.Instant/now))
   :jvm       (System/getProperty "java.version")
   :clojure   (clojure-version)
   :os        (str (System/getProperty "os.name") " " (System/getProperty "os.arch"))
   :cpu       (str (.availableProcessors (Runtime/getRuntime)) " cores")})

(defn say [& xs] (println (apply str xs)) (flush))
