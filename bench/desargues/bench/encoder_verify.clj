(ns desargues.bench.encoder-verify
  "One real render per encoder profile THROUGH the JVM path (ManimBackend
   -init!/-configure!), with codec and x264 preset read back from the file.

   clojure -M:test -m desargues.bench.encoder-verify"
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [desargues.scene.manim :as sm]
            [desargues.scene.protocols :as p]
            [desargues.manim.core :as core]
            [desargues.manim.encoder :as enc]))

(defn- x264-fingerprint
  "codec / pix_fmt from ffprobe; subme/me/ref from x264's SEI option string."
  [path]
  (let [probe (:out (sh/sh "ffprobe" "-v" "error" "-select_streams" "v:0"
                           "-show_entries" "stream=codec_name,pix_fmt" "-of" "csv=p=0" path))
        sei   (->> (str/split-lines (:out (sh/sh "strings" "-n" "20" path)))
                   (filter #(str/includes? % "x264 - core")) first)
        opt   (fn [k] (second (re-find (re-pattern (str " " k "=([^ ]+)")) (or sei ""))))]
    {:probe (str/trim probe) :subme (opt "subme") :me (opt "me") :ref (opt "ref")}))

(defn -main [& _]
  (let [b (sm/manim-backend)]
    (p/-init! b)
    (doseq [k [:final :preview]]
      (p/-configure! b {:encoder k :quality "low_quality"})
      (let [scene (str "EncoderVerify_" (name k))]
        (p/-render! b scene
                    (fn [self]
                      (let [c ((requiring-resolve 'desargues.manim.mobjects/circle))]
                        (core/play! self ((requiring-resolve 'desargues.manim.animations/create) c))))
                    {})
        (let [path (str (core/get-attr (core/get-attr (core/manim) "config") "output_file"))
              f    (->> (file-seq (java.io.File. "media/videos"))
                        (filter #(= (str scene ".mp4") (.getName ^java.io.File %)))
                        (sort-by #(.lastModified ^java.io.File %)) last)]
          (println k (:options (enc/encoder-profile k)) "->" (str f) path
                   (x264-fingerprint (str f))))))
    (shutdown-agents)
    (System/exit 0)))
