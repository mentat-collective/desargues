(ns desargues.doctor
  "Environment check: `clojure -M:doctor` (or `bb doctor`).

   Reports one row per requirement as ok / missing with the fix. The pure rows
   (JVM, recording backend) decide the exit code; the Manim rows (conda prefix,
   `import manim`, latex, ffmpeg) are advisory, since the library works without
   them. `checks` is pure over its inputs; only -main prints and exits."
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [desargues.config :as config]
            [desargues.layout.core :as l]
            [desargues.scene.data :as rec])
  (:gen-class))

(defn- on-path [bin]
  (let [{:keys [exit out]} (sh/sh "sh" "-c" (str "command -v " bin))]
    (when (zero? exit) (str/trim out))))

(defn- ok  [id detail] {:id id :ok? true :detail detail})
(defn- bad [id detail fix] {:id id :ok? false :detail detail :fix fix})

(defn jvm-check []
  (let [v     (System/getProperty "java.version")
        major (Long/parseLong (re-find #"^\d+" v))]
    (if (>= major 11)
      (ok :jvm (str "java " v))
      (bad :jvm (str "java " v) "install JDK 11 or newer"))))

(defn recording-backend-check
  "Realize a two-leaf layout through the recording backend: no Python involved."
  []
  (try
    (let [g (rec/layout->scene-graph "doctor"
                                     (l/column {:padding 0.5 :spacing 0.3}
                                       (l/text "desargues")
                                       (l/math "\\int f"))
                                     {})]
      (ok :recording-backend (str (:node-count g) " nodes realized, no Python")))
    (catch Throwable t
      (bad :recording-backend (ex-message t) "the pure path is broken; run clojure -M:test"))))

(defn manim-config-check []
  (try
    (let [{:keys [python-exe library-path] :as cfg} (config/manim-config)]
      (assoc (ok :manim-config (str python-exe " · " library-path)) :config cfg))
    (catch Throwable t
      (bad :manim-config
           (or (some-> (ex-data t) :conda-prefix (as-> p (str "no Manim under " p)))
               "no conda env detected")
           "conda activate manim, or set DESARGUES_CONDA_PREFIX=<env root>"))))

(defn manim-import-check []
  (let [{:keys [ok? version error python-exe]} (config/probe-manim-import)]
    (if ok?
      (ok :manim (str "Manim Community v" version " (" python-exe ")"))
      (bad :manim (or error "import failed")
           (if python-exe
             "conda install -c conda-forge manim  (in that env)"
             "fix manim-config first")))))

(defn tool-check [id bin fix]
  (if-let [p (on-path bin)]
    (ok id p)
    (bad id (str bin " not on PATH") fix)))

(defn checks
  "Run every check. Returns rows [{:id :ok? :detail :fix?}] in report order."
  []
  [(jvm-check)
   (recording-backend-check)
   (manim-config-check)
   (manim-import-check)
   (tool-check :latex "latex" "install a TeX distribution (texlive) for math rendering")
   (tool-check :ffmpeg "ffmpeg" "install ffmpeg (Manim writes video through it)")])

(def pure-ids #{:jvm :recording-backend})

(defn report
  "Rows -> {:lines [...] :exit 0|1}. Exit is 1 only when a pure row fails."
  [rows]
  (let [line (fn [{:keys [id ok? detail fix]}]
               (format "  %-8s %-18s %s%s" (if ok? "ok" "MISSING") (name id) detail
                       (if (and (not ok?) fix) (str "\n           fix: " fix) "")))
        pure-ok? (every? :ok? (filter (comp pure-ids :id) rows))
        manim-ok? (every? :ok? (remove (comp pure-ids :id) rows))]
    {:exit (if pure-ok? 0 1)
     :lines (concat ["desargues doctor"]
                    (map line rows)
                    [""
                     (cond
                       (not pure-ok?) "The pure path is broken."
                       manim-ok?      "Everything is in place: the recording backend and the Manim backend both work."
                       :else          "The pure path works (recording backend, layout, Emmy). Fix the MISSING rows to render video with Manim.")])}))

(defn -main [& _]
  (let [{:keys [lines exit]} (report (checks))]
    (doseq [l lines] (println l))
    (shutdown-agents)
    (System/exit exit)))
