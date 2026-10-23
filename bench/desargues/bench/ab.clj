(ns desargues.bench.ab
  "Interleaved A/B benchmarking for slow subjects (renders, process starts),
   where the micro-sampler (desargues.bench.sample) does not apply: one call
   takes seconds, the host drifts while it runs, and part of the cost may be
   spent off the calling thread or in another process.

   A Variant is a map:

     {:name     \"frames-only\"          ; unique within its arm
      :data     {...}                    ; optional, handed to :setup
      :setup    (fn [data] ctx)          ; optional, untimed; default: ctx = data
      :run      (fn [ctx] ...)           ; the timed call
      :teardown (fn [ctx] ...)}          ; optional, untimed, runs even if :run throws

   An Arm is a map:

     {:id \"warm render 480p15\" :variants [A B ...]
      :rounds 5 :warmup 1                ; warmup rounds are run but not counted
      :alternate? false                  ; true: ABAB, then BABA on odd rounds
      :setup (fn [] ...) :teardown (fn [] ...)}   ; optional, once per arm

   run-arm executes the schedule round by round (A B A B ..., never A A A B B B,
   so drift hits every variant alike) and records per sample the wall time,
   the PROCESS CPU-seconds (every thread of this JVM, embedded Python
   included) and the host load average taken just before the call. When :run
   returns {::measured {:wall-ms w :cpu-s c ...}} that replaces the outer
   measurement: the hook for subjects that run in a child process.

   Per variant the summary has median, min, p90 (and mean) of wall-ms and cpu-s,
   the load seen, and the ratio of its medians to the first variant's (the
   baseline). arm->rows flattens an arm into report rows that
   desargues.bench.check can compare against a baseline."
  (:require [clojure.string :as str]
            [desargues.bench.sample :as smp])
  (:import (java.lang.management ManagementFactory)))

;; ── pure: schedule ───────────────────────────────────────────────────────────

(defn schedule
  "The run order of an arm: a vector of {:variant name :round r :warmup? bool}.
   Rounds 0..warmup-1 are warmup, then `rounds` measured rounds; each round
   runs every variant once, in the given order (reversed on odd rounds when
   alternate?)."
  [names {:keys [rounds warmup alternate?] :or {rounds 5 warmup 1}}]
  (let [names (vec names)]
    (vec (for [r (range (+ warmup rounds))
               v (if (and alternate? (odd? r)) (rseq names) names)]
           {:variant v :round r :warmup? (< r warmup)}))))

;; ── pure: stats ──────────────────────────────────────────────────────────────

(defn percentile
  "Nearest-rank p-th percentile (0 < p <= 100) of a non-empty seq."
  [p xs]
  (let [v (vec (sort xs))
        k (max 0 (dec (long (Math/ceil (* (/ p 100.0) (count v))))))]
    (double (v (min k (dec (count v)))))))

(defn stats
  "Non-empty seq of numbers -> {:median :min :p90 :max :mean :n}."
  [xs]
  {:median (smp/median xs)
   :min    (double (reduce min xs))
   :p90    (percentile 90 xs)
   :max    (double (reduce max xs))
   :mean   (/ (double (reduce + xs)) (count xs))
   :n      (count xs)})

(defn- ratio [a b] (when (and a b (pos? b)) (/ (double a) b)))

(defn summarize
  "Measured samples (warmups dropped by the caller) and the variant order ->
   one summary per variant, with :ratio of its medians to the first variant's."
  [samples names]
  (let [by   (group-by :variant samples)
        one  (fn [nm]
               (let [ss (by nm)]
                 {:variant nm
                  :wall-ms (stats (map :wall-ms ss))
                  :cpu-s   (stats (map :cpu-s ss))
                  :load1   (stats (keep (comp :load1 :load) ss))}))
        rows (mapv one (filter by names))
        base (first rows)]
    (mapv (fn [r]
            (assoc r :ratio {:wall (ratio (get-in r [:wall-ms :median]) (get-in base [:wall-ms :median]))
                             :cpu  (ratio (get-in r [:cpu-s :median]) (get-in base [:cpu-s :median]))}))
          rows)))

(defn parse-loadavg
  "Text of /proc/loadavg -> {:load1 :load5 :load15}, or nil when unparseable."
  [s]
  (let [[a b c] (some-> s str/trim (str/split #"\s+"))
        [x y z] (map #(some-> % parse-double) [a b c])]
    (when (and x y z) {:load1 x :load5 y :load15 z})))

(defn arm->rows
  "An arm result -> report rows {:group :render :id arm :provider variant ...}
   carrying desargues.bench.sample/summarize keys on wall time, so
   desargues.bench.check compares render runs like any other suite."
  [{:keys [arm samples variants]}]
  (let [by (group-by :variant (remove :warmup? samples))]
    (mapv (fn [{:keys [variant cpu-s ratio]}]
            (merge {:group :render :id arm :provider variant}
                   (smp/summarize (map :wall-ms (by variant)))
                   {:cpu-s-median (:median cpu-s) :ratio-wall (:wall ratio)}))
          variants)))

;; ── impure: measurement ──────────────────────────────────────────────────────

(defn process-cpu-s
  "CPU-seconds used so far by this process, all threads."
  []
  (/ (.getProcessCpuTime ^com.sun.management.OperatingSystemMXBean
                         (ManagementFactory/getOperatingSystemMXBean))
     1e9))

(defn host-load
  "Host load average now: /proc/loadavg when readable, else the JVM's 1-min figure."
  []
  (or (try (parse-loadavg (slurp "/proc/loadavg")) (catch Exception _ nil))
      {:load1 (.getSystemLoadAverage (ManagementFactory/getOperatingSystemMXBean))}))

(defn measure
  "Call f once -> {:wall-ms :cpu-s :value}."
  [f]
  (let [c0 (process-cpu-s) t0 (System/nanoTime)
        v  (f)
        t1 (System/nanoTime) c1 (process-cpu-s)]
    {:wall-ms (/ (- t1 t0) 1e6) :cpu-s (- c1 c0) :value v}))

(defn run-once
  "One sample of a variant: setup, timed run, teardown -> {:wall-ms :cpu-s :load ...}."
  [{:keys [data setup run teardown]}]
  (let [ctx  (if setup (setup data) data)
        load (host-load)]
    (try
      (let [{:keys [value] :as m} (measure #(run ctx))
            inner (when (map? value) (::measured value))]
        (merge {:wall-ms (:wall-ms m) :cpu-s (:cpu-s m)} inner {:load load}))
      (finally (when teardown (teardown ctx))))))

(defn run-arm
  "Run an arm (see the ns doc) -> {:arm :rounds :warmup :alternate? :load-start
   :load-end :samples [...] :variants [summary ...]}. `log` gets one line per sample."
  ([arm] (run-arm arm {}))
  ([{:keys [id variants rounds warmup alternate? setup teardown]
     :or {rounds 5 warmup 1} :as arm}
    {:keys [log] :or {log smp/say}}]
   (let [names   (mapv :name variants)
         _       (assert (apply distinct? names) "variant names must be unique")
         by-name (zipmap names variants)
         sched   (schedule names arm)
         start   (host-load)]
     (when setup (setup))
     (try
       (let [samples (mapv (fn [{:keys [variant round warmup?] :as slot}]
                             (let [s (merge slot (run-once (by-name variant)))]
                               (log (format "  %s r%d%s %-14s %9.1f ms  %7.2f cpu-s  load %.1f"
                                            id round (if warmup? "w" " ") variant
                                            (double (:wall-ms s)) (double (:cpu-s s))
                                            (double (or (get-in s [:load :load1]) -1))))
                               s))
                           sched)]
         {:arm id :rounds rounds :warmup warmup :alternate? (boolean alternate?)
          :load-start start :load-end (host-load)
          :samples samples
          :variants (summarize (remove :warmup? samples) names)})
       (finally (when teardown (teardown)))))))
