(ns desargues.config-check
  "Smoke check: hive-di loads and ManimConfig resolves. `lein run -m
   desargues.config-check`."
  (:require [desargues.config :as config]))

(defn -main [& _]
  (println "hive-di loaded; ManimConfig resolves to:")
  (doseq [[k v] (sort (config/manim-config))]
    (println (format "  %-16s %s" (name k) v)))
  (println "CONFIG-OK")
  (System/exit 0))
