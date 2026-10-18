(ns desargues.lesson.export
  "BOUNDARY stratum (JVM): a lesson bundle (desargues.lesson.schema) exported
   as reproducible files, plus a manifest that proves it.

     (build!  lesson-or-path {:out dir})  ;; => the manifest (also dir/manifest.edn)
     (verify! dir)                        ;; => {:reproducible? bool :mismatches [...]}

   build! validates the lesson, then writes into `dir`:

     lesson.edn     canonical EDN (desargues.scene.serialize/->canonical-edn)
     lesson.json    lossless JSON, see `->json-data` for the convention
     notebook.clj   a GENERATED Clerk notebook, one section per slide; open it
                    with the :notebooks alias (it requires desargues.calc-emmy)
     media/<block-id>.<png|gif|mp4>
                    one file per :media block, painted by the headless PNG host
                    (desargues.player.headless, the `bb showcase` path); GIF and
                    MP4 go through ffmpeg. A block the host cannot render is
                    SKIPPED, warned about on stderr and recorded under the
                    manifest's :skipped; it never fails silently.
     manifest.edn   canonical EDN:
                    {:lesson/id :generated-by \"bb lesson:build <path>\"
                     :deps {lib {:git/sha ..} | {:mvn/version ..}}   ; deps.edn :deps
                     :notebook-deps {...}                            ; :notebooks alias
                     :files {relpath sha256-hex}
                     :skipped [{:block :format :from :reason ...}]}

   Nothing written carries a timestamp or an absolute path, so building the
   same lesson twice yields byte-identical files. verify! re-reads
   dir/lesson.edn, rebuilds it in a temp dir and compares the sha256 maps (and
   the files on disk against the manifest).

   Media hashes depend on the renderer host (JDK font rasterisation, ffmpeg
   version): reproducible on one host, not promised across hosts."
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [desargues.lesson.schema :as ls]
            [desargues.objects.calc.figure :as figure]
            [desargues.player.graph-source :as gs]
            [desargues.player.headless :as hl]
            [desargues.player.protocols :as pp]
            [desargues.scene.serialize :as ser])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.security MessageDigest)
           (java.util LinkedHashMap)))

;; ============================================================================
;; JSON convention (lossless)
;; ============================================================================
;;
;; JSON has strings, numbers, booleans, null, arrays and objects; EDN has more.
;; The encoding (transit-like, `~` is the escape character):
;;
;;   nil, true, false          null, true, false
;;   long / int                JSON integer
;;   double                    JSON number (always printed with . or E, so it
;;                             reads back as a double)
;;   string "s"                "s"; a string that STARTS with ~ gets one more:
;;                             "~x" -> "~~x"
;;   keyword :ns/name          "~:ns/name"
;;   symbol  ns/name           "~$ns/name"
;;   char    \c                "~cc"
;;   bigint 3N                 "~n3"
;;   ratio 16/3                "~r16/3"
;;   bigdec 1.5M               "~m1.5"
;;   vector [a b]              [a b]
;;   list (a b)                {"~list": [a b]}
;;   set #{a b}                {"~set": [a b]}      (canonical order)
;;   map whose keys are all strings, keywords, symbols or chars
;;                             {"k": v ...}         keys encoded as above,
;;                                                  in canonical key order
;;   any other map             {"~map": [[k v] ...]}
;;
;; A JSON object whose ONLY key is "~list", "~set" or "~map" is a tag. A user
;; key can never collide with one: a user string key starting with ~ is
;; escaped to ~~, and keyword/symbol/char keys encode as ~: ~$ ~c. Output is
;; indented two spaces, objects keep the canonical key order
;; (desargues.scene.serialize/compare-canonical), and the file ends with a
;; newline, so it is as deterministic as lesson.edn.

(defn- encode-scalar [x]
  (cond
    (string? x)  (if (str/starts-with? x "~") (str "~" x) x)
    (keyword? x) (str "~:" (subs (str x) 1))
    (symbol? x)  (str "~$" x)
    (char? x)    (str "~c" x)
    (instance? clojure.lang.BigInt x)  (str "~n" x)
    (instance? java.math.BigInteger x) (str "~n" x)
    (ratio? x)   (str "~r" x)
    (decimal? x) (str "~m" x)
    (float? x)   (double x)
    :else        x))

(defn- string-key? [k] (or (string? k) (keyword? k) (symbol? k) (char? k)))

(defn- linked [entries]
  (let [m (LinkedHashMap.)]
    (doseq [[k v] entries] (.put m k v))
    m))

(defn ->json-data
  "The JSON-ready value of EDN `x` under the convention above (Java maps keep
   the canonical key order)."
  [x]
  (cond
    (map? x)
    (let [es (sort-by key ser/compare-canonical (seq x))]
      (if (every? (comp string-key? key) es)
        (linked (map (fn [[k v]] [(encode-scalar k) (->json-data v)]) es))
        (linked [["~map" (mapv (fn [[k v]] [(->json-data k) (->json-data v)]) es)]])))
    (set? x)    (linked [["~set" (mapv ->json-data (sort ser/compare-canonical x))]])
    (vector? x) (mapv ->json-data x)
    (seq? x)    (linked [["~list" (mapv ->json-data x)]])
    :else       (encode-scalar x)))

(defn- decode-string [^String s]
  (if (or (not (str/starts-with? s "~")) (< (count s) 2))
    s
    (let [body (subs s 2)]
      (case (.charAt s 1)
        \~ (subs s 1)
        \: (keyword body)
        \$ (symbol body)
        \c (.charAt s 2)
        \n (bigint (BigInteger. body))
        \r (let [[n d] (str/split body #"/")]
             (/ (bigint (BigInteger. ^String n)) (bigint (BigInteger. ^String d))))
        \m (bigdec body)
        (throw (ex-info (str "lesson.json: unknown string tag in " (pr-str s)) {:value s}))))))

(defn <-json-data
  "The EDN value of JSON-read `x` (string keys, as json/read-str gives them)."
  [x]
  (cond
    (map? x)
    (let [[[k v] & more] (seq x)]
      (if (and (nil? more) (contains? #{"~list" "~set" "~map"} k))
        (case k
          "~list" (apply list (map <-json-data v))
          "~set"  (into #{} (map <-json-data) v)
          "~map"  (into {} (map (fn [[k v]] [(<-json-data k) (<-json-data v)])) v))
        (into {} (map (fn [[k v]] [(decode-string k) (<-json-data v)])) x)))
    (vector? x) (mapv <-json-data x)
    (string? x) (decode-string x)
    :else       x))

(defn ->json
  "Lossless, deterministic JSON text of EDN `x` (trailing newline)."
  [x]
  (str (json/write-str (->json-data x) :indent true :escape-slash false
                       :escape-unicode false :escape-js-separators false)
       "\n"))

(defn <-json
  "EDN back from `->json` text."
  [s]
  (<-json-data (json/read-str s)))

;; ============================================================================
;; Clerk notebook source
;; ============================================================================

(defn- slug [x]
  (-> (if (keyword? x) (subs (str x) 1) (str x))
      (str/replace #"/" ".")
      str/lower-case
      (str/replace #"[^a-z0-9.\-]+" "-")))

(defn notebook-ns
  "The namespace of `lesson`'s generated notebook."
  [lesson]
  (symbol (str "lesson." (slug (:lesson/id lesson)))))

(defn- block-sym [b] (symbol (str/replace (slug (:block/id b)) #"\." "-")))

(defn- comment-lines [s]
  (str/join "\n" (map #(if (str/blank? %) ";;" (str ";; " %)) (str/split-lines s))))

(defn- indent [s n]
  (let [pad (apply str (repeat n \space))]
    (str/join "\n" (map-indexed (fn [i l] (if (or (zero? i) (str/blank? l)) l (str pad l)))
                                (str/split-lines s)))))

(defn- quoted
  "'<canonical edn of v>' laid out from column `col`."
  [v col]
  (str "'" (indent (ser/->canonical-edn v) (inc col))))

(defn- quoted-def [sym v] (str "(def " sym "\n  " (quoted v 2) ")"))

(defn- media-path [b] (str "media/" (slug (:block/id b)) "." (name (:format b))))

(defmulti block->clj
  "Notebook source of block `b`; ctx {:skipped #{media-block-id}}."
  (fn [b _ctx] (:block/kind b)))

(defmethod block->clj :prose [b _] (comment-lines (:md b)))

(defmethod block->clj :tex [b _]
  (str "(clerk/tex (tex/->TeX " (quoted (:expr b) 17) "))"))

(defmethod block->clj :figure [b _]
  (let [s      (block-sym b)
        target (if (= :space (get-in b [:figure :view :view]))
                 "calc-emmy/figure->mathbox"
                 "calc-emmy/figure->mafs")]
    (str ";; figure " (:block/id b) ", through the ->emmy-viewers target (desargues.calc-emmy)\n"
         "^{:nextjournal.clerk/visibility {:result :hide}}\n"
         (quoted-def s (:figure b)) "\n\n"
         "(" target " " s ")")))

(defmethod block->clj :viewer [b _]
  (str "(ev/fragment\n " (quoted (:fragment b) 1) ")"))

(defmethod block->clj :construction [b _]
  (str ";; construction " (:block/id b) " (desargues.board.construction EDN), shown as data.\n"
       ";; TODO(lesson-export): no notebook render path for constructions yet; drawing one\n"
       ";; compiles a board kernel (desargues.board/compile-board!), which needs the kernel toolchain.\n"
       (quoted-def (block-sym b) (:construction b)) "\n\n"
       (block-sym b)))

(defmethod block->clj :scene [b _]
  (let [s (block-sym b)]
    (str ";; scene " (:block/id b) " (desargues.scene.data recording), as data, then its last\n"
         ";; frame painted by the headless player host (desargues.player.headless)\n"
         (quoted-def s (:scene b)) "\n\n"
         "(scene-image " s ")")))

(defmethod block->clj :media [b {:keys [skipped]}]
  (if (contains? skipped (:block/id b))
    (str ";; media " (:block/id b) " (" (name (:format b)) " of " (:from b)
         ") was SKIPPED at build time; see manifest.edn :skipped")
    (str ";; media " (:block/id b) ": " (name (:format b)) " of " (:from b) "\n"
         (if (= :png (:format b))
           (str "(clerk/image (media-url \"" (media-path b) "\"))")
           (str "(clerk/html [:" (if (= :gif (:format b)) "img" "video")
                " {:src (media-data-uri \"" (media-path b) "\" \""
                (if (= :gif (:format b)) "image/gif" "video/mp4") "\")"
                (when (= :mp4 (:format b)) " :controls true") "}])")))))

(defn- header [lesson]
  (str ";; # " (:lesson/title lesson) "\n"
       ";;\n"
       ";; Lesson " (:lesson/id lesson) ", version " (:lesson/version lesson) ".\n"
       ";; GENERATED by desargues.lesson.export from lesson.edn: do not edit by hand,\n"
       ";; edit the lesson and rebuild (bb lesson:build). Open it with the :notebooks\n"
       ";; alias: (nextjournal.clerk/serve! {}) then (nextjournal.clerk/show! \"<path>/notebook.clj\").\n\n"
       "^{:nextjournal.clerk/visibility {:code :hide}}\n"
       "(ns " (notebook-ns lesson) "\n"
       "  {:nextjournal.clerk/toc true}\n"
       "  (:require [clojure.java.io :as io]\n"
       "            [desargues.calc-emmy :as calc-emmy]\n"
       "            [desargues.lesson.tex :as tex]\n"
       "            [desargues.player.graph-source :as gs]\n"
       "            [desargues.player.headless :as hl]\n"
       "            [desargues.player.protocols :as pp]\n"
       "            [emmy.clerk :as ec]\n"
       "            [emmy.viewer :as ev]\n"
       "            [nextjournal.clerk :as clerk])\n"
       "  (:import (java.nio.file Files)\n"
       "           (java.util Base64)))\n\n"
       "^{:nextjournal.clerk/visibility {:code :hide :result :hide}}\n"
       "(ec/install!)\n\n"
       ";; The bundle directory (the one holding this file), resolved when the notebook\n"
       ";; is loaded from *file*: a filesystem path under load-file and clerk/show!, else\n"
       ";; a classpath resource. Media paths are relative to it, so the notebook works\n"
       ";; from any working directory.\n"
       "^{:nextjournal.clerk/visibility {:code :hide :result :hide} :nextjournal.clerk/no-cache true}\n"
       "(def bundle-dir\n"
       "  (let [f (io/file *file*)]\n"
       "    (if (.isFile f)\n"
       "      (.getParentFile (.getAbsoluteFile f))\n"
       "      (some-> (io/resource *file*) io/file .getParentFile))))\n\n"
       "^{:nextjournal.clerk/visibility {:code :hide :result :hide}}\n"
       "(defn media-file\n"
       "  \"The file at `rel` (e.g. \\\"media/x.png\\\") below the bundle directory.\"\n"
       "  [rel]\n"
       "  (io/file bundle-dir rel))\n\n"
       "^{:nextjournal.clerk/visibility {:code :hide :result :hide}}\n"
       "(defn media-url\n"
       "  \"file: URL of media file `rel`, for clerk/image.\"\n"
       "  [rel]\n"
       "  (io/as-url (media-file rel)))\n\n"
       "^{:nextjournal.clerk/visibility {:code :hide :result :hide}}\n"
       "(defn media-data-uri\n"
       "  \"data: URI of media file `rel`, so the browser needs no file server.\"\n"
       "  [rel mime]\n"
       "  (str \"data:\" mime \";base64,\"\n"
       "       (.encodeToString (Base64/getEncoder) (Files/readAllBytes (.toPath (media-file rel))))))\n\n"
       "^{:nextjournal.clerk/visibility {:code :hide :result :hide}}\n"
       "(defn scene-image\n"
       "  \"The last frame of recorded scene `graph`, painted headless.\"\n"
       "  [graph]\n"
       "  (let [src (gs/graph-source graph)]\n"
       "    (hl/frame->image (pp/-frame-at src (pp/-duration src)) 720 405)))"))

(defn notebook-source
  "The generated Clerk notebook text of `lesson`, one section per slide;
   `skipped` is the set of :media block ids that were not rendered."
  ([lesson] (notebook-source lesson #{}))
  ([lesson skipped]
   (let [ctx    {:skipped skipped}
         slides (for [sl (:lesson/slides lesson)]
                  (str/join "\n\n" (cons (str ";; ## " (:slide/title sl))
                                         (map #(block->clj % ctx) (:slide/blocks sl)))))]
     (str (str/join "\n\n" (cons (header lesson) slides)) "\n"))))

;; ============================================================================
;; Media (headless player host + ffmpeg)
;; ============================================================================

(def media-size
  "Pixel size of rendered media. Part of the format: changing it changes bytes."
  [960 540])

(def media-fps 12)

(defn- block-source
  "ISource of block `b` for the headless host, or nil when its kind has none."
  [b]
  (case (:block/kind b)
    :figure ((figure/source-fn (:figure b)) {})
    :scene  (gs/graph-source (:scene b))
    nil))

(defn- ffmpeg? []
  (try (zero? (:exit (sh/sh "ffmpeg" "-version"))) (catch Exception _ false)))

(defn- tmp-dir ^java.io.File [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-tree! [f]
  (doseq [x (reverse (file-seq (io/file f)))] (io/delete-file x true)))

(defn- ffmpeg! [& args]
  (let [{:keys [exit err]} (apply sh/sh "ffmpeg" args)]
    (when-not (zero? exit)
      (throw (ex-info "ffmpeg failed" {:reason :ffmpeg-failed :err err})))))

(defn- render-media!
  "Write media block `b` (rendered from block `src-b`) to `file`."
  [b src-b file]
  (let [source (block-source src-b)
        [w h]  media-size
        dur    (double (pp/-duration source))]
    (io/make-parents file)
    (if (= :png (:format b))
      (hl/render-png source dur file {:w w :h h})
      (let [tmp (tmp-dir "lesson-media")]
        (try
          (hl/render-sequence source media-fps tmp {:w w :h h})
          (apply ffmpeg! "-y" "-loglevel" "error" "-framerate" (str media-fps)
                 "-i" (str tmp "/frame-%05d.png")
                 (concat
                  (if (= :gif (:format b))
                    ["-vf" "split[a][b];[a]palettegen=max_colors=96[p];[b][p]paletteuse=dither=bayer:bayer_scale=4"
                     "-loop" "0"]
                    ["-c:v" "libx264" "-pix_fmt" "yuv420p" "-threads" "1" "-flags:v" "+bitexact"])
                  ["-map_metadata" "-1" "-fflags" "+bitexact" (str file)]))
          (finally (delete-tree! tmp)))))))

(defn- headless-capable?
  "Can this JVM paint offscreen images (java.awt)? False when AWT is absent
   or broken (e.g. a stripped JRE)."
  []
  (try (some? (java.awt.image.BufferedImage. 1 1 java.awt.image.BufferedImage/TYPE_INT_ARGB))
       (catch Throwable _ false)))

(defn- media!
  "Render every :media block of `lesson` under `out`; returns {:files [relpath]
   :skipped [{:block :format :from :reason ...}]}."
  [lesson out {:keys [media?] :or {media? true}}]
  (let [bs    (mapcat :slide/blocks (:lesson/slides lesson))
        by-id (into {} (map (juxt :block/id identity)) bs)]
    (reduce
     (fn [acc b]
       (let [src-b (by-id (:from b))
             skip  (fn [reason & {:as more}]
                     (binding [*out* *err*]
                       (println "lesson:build WARNING: skipped media" (:block/id b) (name (:format b))
                                "-" (name reason) (or (:error more) "")))
                     (update acc :skipped conj
                             (merge {:block (:block/id b) :format (:format b) :from (:from b)
                                     :reason reason}
                                    more)))]
         (cond
           (not media?)                 (skip :disabled)
           (nil? (block-source src-b))  (skip :no-render-path :kind (:block/kind src-b))
           (not (headless-capable?))    (skip :no-awt)
           (and (not= :png (:format b)) (not (ffmpeg?))) (skip :ffmpeg-missing)
           :else
           (let [rel (media-path b)]
             (try
               (render-media! b src-b (io/file out rel))
               (update acc :files conj rel)
               (catch Throwable t
                 (io/delete-file (io/file out rel) true)
                 (skip (or (:reason (ex-data t)) :render-failed)
                       :error (.getName (class t)))))))))
     {:files [] :skipped []}
     (filter #(= :media (:block/kind %)) bs))))

;; ============================================================================
;; Manifest
;; ============================================================================

(defn sha256-hex
  "Lower-case hex SHA-256 of file `f`'s bytes."
  [f]
  (let [d (MessageDigest/getInstance "SHA-256")]
    (apply str (map #(format "%02x" (bit-and % 0xff))
                    (.digest d (Files/readAllBytes (.toPath (io/file f))))))))

(defn- pin [coord]
  (cond
    (:git/sha coord)     {:git/sha (:git/sha coord)}
    (:mvn/version coord) {:mvn/version (:mvn/version coord)}
    :else                {:unpinned true}))

(defn deps-pins
  "{:deps {lib pin} :notebook-deps {lib pin}} from the deps.edn at `path`:
   top-level :deps and the :notebooks alias's :extra-deps."
  [path]
  (let [f (io/file path)]
    (if-not (.exists f)
      {:deps {} :notebook-deps {}}
      (let [d (edn/read-string (slurp f))]
        {:deps          (update-vals (or (:deps d) {}) pin)
         :notebook-deps (update-vals (or (get-in d [:aliases :notebooks :extra-deps]) {}) pin)}))))

(defn- relative-source
  "A path for :generated-by with no absolute part: relative to the cwd when
   below it, else its file name."
  [p]
  (let [f (io/file p)]
    (if-not (.isAbsolute f)
      (str/replace (str p) "\\" "/")
      (let [cwd (.toPath (.getCanonicalFile (io/file ".")))
            fp  (.toPath (.getCanonicalFile f))]
        (if (.startsWith fp cwd) (str (.relativize cwd fp)) (.getName f))))))

;; ============================================================================
;; API
;; ============================================================================

(defn read-lesson
  "The lesson EDN at `path` (tag-free EDN, as lesson.edn is written)."
  [path]
  (ser/<-edn (slurp (io/file path) :encoding "UTF-8")))

(defn- write! [out rel s]
  (let [f (io/file out rel)]
    (io/make-parents f)
    (spit f s :encoding "UTF-8")
    rel))

(defn default-out
  "dist/lessons/<id>, the default bundle directory of `lesson`."
  [lesson]
  (str "dist/lessons/" (slug (:lesson/id lesson))))

(defn build!
  "Validate `lesson-or-path` (a lesson map or a path to its EDN) with
   desargues.lesson.schema/validate and write the bundle into (:out opts).
   Returns the manifest, also written as manifest.edn. opts: :out (required),
   :deps-file (default \"deps.edn\"), :source (the path :generated-by names;
   default the given path, else \"lesson.edn\"), :media? (default true; false
   skips every :media block, recorded as :disabled)."
  [lesson-or-path {:keys [out deps-file source] :or {deps-file "deps.edn"} :as opts}]
  (when-not out (throw (ex-info "build! needs {:out dir}" {:opts opts})))
  (let [path?    (not (map? lesson-or-path))
        lesson   (ls/validate (if path? (read-lesson lesson-or-path) lesson-or-path))
        out      (io/file out)
        _        (.mkdirs out)
        {media-files :files skipped :skipped} (media! lesson out opts)
        files    (into [(write! out "lesson.edn" (ser/->canonical-edn lesson))
                        (write! out "lesson.json" (->json lesson))
                        (write! out "notebook.clj"
                                (notebook-source lesson (set (map :block skipped))))]
                       media-files)
        src      (relative-source (or source (when path? lesson-or-path) "lesson.edn"))
        manifest (merge {:lesson/id    (:lesson/id lesson)
                         :generated-by (str "bb lesson:build " src)
                         :files        (into (sorted-map)
                                             (map (juxt identity #(sha256-hex (io/file out %))))
                                             files)
                         :skipped      skipped}
                        (deps-pins deps-file))]
    (write! out "manifest.edn" (ser/->canonical-edn manifest))
    manifest))

(defn read-manifest
  "dir/manifest.edn as data."
  [dir]
  (ser/<-edn (slurp (io/file dir "manifest.edn") :encoding "UTF-8")))

(defn verify!
  "Rebuild the lesson in `dir` (from dir/lesson.edn) in a temp dir and compare
   the sha256 maps. A file mismatches when the manifest's hash differs from
   the rebuilt one or from the file on disk. Returns {:reproducible? bool
   :mismatches [{:file :expected :actual :on-disk}]}: :expected from the
   manifest, :actual from the rebuild, :on-disk from dir (nil = missing)."
  ([dir] (verify! dir {}))
  ([dir opts]
   (let [expected (:files (read-manifest dir))
         tmp      (tmp-dir "lesson-verify")]
     (try
       (let [actual     (:files (build! (str (io/file dir "lesson.edn"))
                                        (assoc opts :out (str tmp) :source "lesson.edn")))
             on-disk    (fn [rel] (let [f (io/file dir rel)] (when (.exists f) (sha256-hex f))))
             mismatches (vec (for [rel (sort (into (set (keys expected)) (keys actual)))
                                   :let [e (get expected rel) a (get actual rel) d (on-disk rel)]
                                   :when (or (not= e a) (not= e d))]
                               {:file rel :expected e :actual a :on-disk d}))]
         {:reproducible? (empty? mismatches) :mismatches mismatches})
       (finally (delete-tree! tmp))))))

(defn -main
  "clojure -M -m desargues.lesson.export build <lesson.edn> [out-dir]
   clojure -M -m desargues.lesson.export verify <dir>"
  [& [cmd a b]]
  (let [code (case cmd
               "build"  (let [out (or b (default-out (read-lesson a)))
                              m   (build! a {:out out})]
                          (println "lesson:build" (:lesson/id m) "->" out)
                          (doseq [[f h] (:files m)] (println " " h f))
                          (doseq [s (:skipped m)] (println "  SKIPPED" (pr-str s)))
                          0)
               "verify" (let [{:keys [reproducible? mismatches]} (verify! a)]
                          (println "lesson:verify" a (if reproducible? "REPRODUCIBLE" "MISMATCH"))
                          (doseq [m mismatches] (println " " (pr-str m)))
                          (if reproducible? 0 1))
               (do (println "usage: build <lesson.edn> [out-dir] | verify <dir>") 2))]
    (shutdown-agents)
    (System/exit code)))
