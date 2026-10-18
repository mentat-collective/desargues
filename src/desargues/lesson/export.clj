(ns desargues.lesson.export
  "BOUNDARY stratum (JVM): a lesson bundle (desargues.lesson.schema) exported
   as reproducible files, plus a manifest that proves it.

     (build!  lesson-or-path {:out dir})  ;; => the manifest (also dir/manifest.edn)
     (verify! dir)                        ;; => {:reproducible? bool :mismatches [...]}

   build! validates the lesson, then writes into `dir`:

     lesson.edn     canonical EDN (desargues.scene.serialize/->canonical-edn)
     lesson.json    lossless JSON, see desargues.lesson.json for the convention
     notebook.clj   a GENERATED Clerk notebook, one section per slide; open it
                    with the :notebooks alias (it requires desargues.calc-emmy).
                    Rendered media is EMBEDDED in it as data URIs, so it reads
                    no file at run time and works from any working directory.
     media/<name>.<png|gif|mp4>
                    one file per :media block, painted by the headless PNG host
                    (desargues.player.headless, the `bb showcase` path); GIF and
                    MP4 go through ffmpeg. <name> is the block id made safe and
                    checked unique (see `block-names`). A block the host cannot
                    render is SKIPPED, warned about on stderr and recorded under
                    the manifest's :skipped; it never fails silently.
     manifest.edn   canonical EDN:
                    {:lesson/id :generated-by \"bb lesson:build <path>\"
                     :deps {lib {:git/sha ..} | {:mvn/version ..}}   ; deps.edn :deps
                     :notebook-deps {...}                            ; :notebooks alias
                     :files {relpath sha256-hex}
                     :skipped [{:block :format :from :reason ...}]}

   build! only writes into an empty directory or a previous bundle (one with a
   manifest.edn), and afterwards removes every file below it that the new
   manifest does not list. It refuses the working directory and its ancestors.

   Nothing written carries a timestamp or an absolute path, so building the
   same lesson twice yields byte-identical files. verify! re-reads
   dir/lesson.edn, rebuilds it in a temp dir and compares the sha256 maps, the
   :deps / :notebook-deps / :skipped entries, and the files on disk against
   the manifest (missing, changed and extra files).

   Media hashes depend on the renderer host (JDK font rasterisation, ffmpeg
   version): reproducible on one host, not promised across hosts."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [desargues.lesson.json :as lj]
            [desargues.lesson.schema :as ls]
            [desargues.objects.calc.figure :as figure]
            [desargues.player.graph-source :as gs]
            [desargues.player.headless :as hl]
            [desargues.player.protocols :as pp]
            [desargues.scene.serialize :as ser])
  (:import (java.nio.file Files LinkOption Path)
           (java.nio.file.attribute FileAttribute)
           (java.security MessageDigest)
           (java.util Base64)))

;; lesson.json is the emmy-viewers fragment JSON encoding, v1 (desargues.lesson.json).

(defn ->json
  "lesson.json text of `lesson` (desargues.lesson.json/lesson->json)."
  [lesson]
  (lj/lesson->json lesson))

(defn <-json
  "The lesson back from lesson.json text."
  [s]
  (lj/json->lesson s))

;; ============================================================================
;; Generated names
;; ============================================================================
;;
;; A block id becomes a notebook var `block-<name>` and, for a :media block, a
;; file `media/<name>.<format>`. <name> lower-cases the id (namespace and name
;; alike) and turns every run of characters outside [a-z0-9] into one `-`, so
;; it is safe as a symbol and as a file name on any filesystem, including
;; case-insensitive ones. The mapping is lossy (:fig/a, :fig.a and :Fig-A all
;; give fig-a), so `block-names` checks the whole lesson and throws, naming
;; both ids, when two blocks map to the same name. The `block-` prefix keeps
;; the vars clear of the notebook's own helpers (scene-image), of clojure.core
;; and of names starting with a digit.

(defn- slug [x]
  (-> (if (keyword? x) (subs (str x) 1) (str x))
      (str/replace #"/" ".")
      str/lower-case
      (str/replace #"[^a-z0-9.\-]+" "-")))

(defn notebook-ns
  "The namespace of `lesson`'s generated notebook."
  [lesson]
  (symbol (str "lesson." (slug (:lesson/id lesson)))))

(defn- block-name [id]
  (str/replace (str/lower-case (if (keyword? id) (subs (str id) 1) (str id)))
               #"[^a-z0-9]+" "-"))

(defn block-names
  "{block-id {:name s :var sym :media relpath-or-nil}} for every block of
   `lesson`. Throws ex-info {:reason :lesson/name-collision :ids [a b]} when
   two block ids map to the same generated name (and so the same notebook var
   and media file name)."
  [lesson]
  (reduce
   (fn [acc b]
     (let [id (:block/id b) nm (block-name id)]
       (when-let [other (some (fn [[oid v]] (when (= nm (:name v)) oid)) acc)]
         (throw (ex-info (str "lesson: block ids " other " and " id " both map to the generated name "
                              (pr-str nm) " (notebook var block-" nm ", media file media/" nm ".*);"
                              " rename one of them")
                         {:reason :lesson/name-collision :ids [other id] :name nm})))
       (assoc acc id {:name  nm
                      :var   (symbol (str "block-" nm))
                      :media (when (= :media (:block/kind b))
                               (str "media/" nm "." (name (:format b))))})))
   {}
   (mapcat :slide/blocks (:lesson/slides lesson))))

;; ============================================================================
;; Clerk notebook source
;; ============================================================================

(def ^:private line-break #"\r\n|\r|\n|\u0085|\u2028|\u2029")

(defn- one-line
  "`s` with every line break and control character collapsed into one space,
   so it cannot end the `;;` comment it is written into (the Clojure reader
   ends a comment at \\n or \\r)."
  [s]
  (str/replace (str s) #"[\p{Cntrl}\u0085\u2028\u2029]+" " "))

(defn- comment-lines
  "One `;;` line per line of `s`."
  [s]
  (str/join "\n" (map #(if (str/blank? %) ";;" (str ";; " (one-line %)))
                      (str/split (str s) line-break))))

(defn- indent [s n]
  (let [pad (apply str (repeat n \space))]
    (str/join "\n" (map-indexed (fn [i l] (if (or (zero? i) (str/blank? l)) l (str pad l)))
                                (str/split-lines s)))))

(defn- quoted
  "'<canonical edn of v>' laid out from column `col`."
  [v col]
  (str "'" (indent (ser/->canonical-edn v) (inc col))))

(defn- quoted-def [sym v] (str "(def " sym "\n  " (quoted v 2) ")"))

(def ^:private data-uri-chunk
  "Base64 characters per string literal of an embedded data URI. One literal
   must stay below the JVM's 65535-byte limit on a class-file string
   constant, which evaluating the cell would otherwise hit."
  32768)

(def ^:private mime {:png "image/png" :gif "image/gif" :mp4 "video/mp4"})

(defn- data-uri-expr
  "(str \"data:<mime>;base64,\" \"<chunk>\" ...) of `bytes`, one literal per
   line, laid out from column `col`."
  [mime-type ^bytes bytes col]
  (let [b64 (.encodeToString (Base64/getEncoder) bytes)
        pad (apply str (repeat (+ col 5) \space))]
    (str "(str \"data:" mime-type ";base64,\""
         (apply str (map #(str "\n" pad "\"" (apply str %) "\"")
                         (partition-all data-uri-chunk b64)))
         ")")))

(defmulti block->clj
  "Notebook source of block `b`; ctx {:names (block-names lesson)
   :skipped #{media-block-id} :media {media-block-id bytes}}."
  (fn [b _ctx] (:block/kind b)))

(defn- var-of [b ctx] (get-in ctx [:names (:block/id b) :var]))

(defmethod block->clj :prose [b _] (comment-lines (:md b)))

(defmethod block->clj :tex [b _]
  (str "(clerk/tex (tex/->TeX " (quoted (:expr b) 17) "))"))

(defmethod block->clj :figure [b ctx]
  (let [s      (var-of b ctx)
        target (if (= :space (get-in b [:figure :view :view]))
                 "calc-emmy/figure->mathbox"
                 "calc-emmy/figure->mafs")]
    (str ";; figure " (one-line (:block/id b)) ", through the ->emmy-viewers target (desargues.calc-emmy)\n"
         "^{:nextjournal.clerk/visibility {:result :hide}}\n"
         (quoted-def s (:figure b)) "\n\n"
         "(" target " " s ")")))

(defmethod block->clj :viewer [b _]
  (str "(ev/fragment\n " (quoted (:fragment b) 1) ")"))

(defmethod block->clj :construction [b ctx]
  (let [s (var-of b ctx)]
    (str ";; construction " (one-line (:block/id b)) " (desargues.board.construction EDN), shown as data.\n"
         ";; TODO(lesson-export): no notebook render path for constructions yet; drawing one\n"
         ";; compiles a board kernel (desargues.board/compile-board!), which needs the kernel toolchain.\n"
         (quoted-def s (:construction b)) "\n\n"
         s)))

(defmethod block->clj :scene [b ctx]
  (let [s (var-of b ctx)]
    (str ";; scene " (one-line (:block/id b)) " (desargues.scene.data recording), as data, then its last\n"
         ";; frame painted by the headless player host (desargues.player.headless)\n"
         (quoted-def s (:scene b)) "\n\n"
         "(scene-image " s ")")))

;; A rendered :media block is embedded as a data URI. The cell therefore holds
;; the media bytes themselves: no file is read at run time (Clerk binds no
;; *file*, and the cwd is arbitrary), and since Clerk caches a cell by the
;; hash of its form, changed media changes the form and can never be served
;; from a stale cache entry.
(defmethod block->clj :media [b {:keys [skipped media names]}]
  (let [id   (:block/id b)
        fmt  (:format b)
        what (str (one-line id) " (" (name fmt) " of " (one-line (:from b)) ")")]
    (cond
      (contains? skipped id)
      (str ";; media " what " was SKIPPED at build time; see manifest.edn :skipped")

      (nil? (get media id))
      (throw (ex-info (str "notebook-source: no rendered bytes for media block " id
                           "; pass them in the media map or list the block as skipped")
                      {:reason :lesson/media-missing :block id}))

      :else
      (str ";; media " what ", the bundle's " (get-in names [id :media])
           ", embedded as a data URI\n"
           "^{:nextjournal.clerk/visibility {:code :hide}}\n"
           "(clerk/html\n"
           " [" (if (= :mp4 fmt) ":video" ":img") "\n"
           "  {:style {:max-width \"100%\"}" (when (= :mp4 fmt) " :controls true") "\n"
           "   :src " (data-uri-expr (mime fmt) (get media id) 3) "}])"))))

(defn- header [lesson]
  (str ";; # " (one-line (:lesson/title lesson)) "\n"
       ";;\n"
       ";; Lesson " (one-line (:lesson/id lesson)) ", version " (one-line (:lesson/version lesson)) ".\n"
       ";; GENERATED by desargues.lesson.export from lesson.edn: do not edit by hand,\n"
       ";; edit the lesson and rebuild (bb lesson:build). Open it with the :notebooks\n"
       ";; alias: (nextjournal.clerk/serve! {}) then (nextjournal.clerk/show! \"<path>/notebook.clj\").\n"
       ";; Rendered media is embedded as data URIs: the notebook reads no file at run\n"
       ";; time, so it works from any working directory.\n\n"
       "^{:nextjournal.clerk/visibility {:code :hide}}\n"
       "(ns " (notebook-ns lesson) "\n"
       "  {:nextjournal.clerk/toc true}\n"
       "  (:require [desargues.calc-emmy :as calc-emmy]\n"
       "            [desargues.lesson.tex :as tex]\n"
       "            [desargues.player.graph-source :as gs]\n"
       "            [desargues.player.headless :as hl]\n"
       "            [desargues.player.protocols :as pp]\n"
       "            [emmy.clerk :as ec]\n"
       "            [emmy.viewer :as ev]\n"
       "            [nextjournal.clerk :as clerk]))\n\n"
       "^{:nextjournal.clerk/visibility {:code :hide :result :hide}}\n"
       "(ec/install!)\n\n"
       "^{:nextjournal.clerk/visibility {:code :hide :result :hide}}\n"
       "(defn scene-image\n"
       "  \"The last frame of recorded scene `graph`, painted headless.\"\n"
       "  [graph]\n"
       "  (let [src (gs/graph-source graph)]\n"
       "    (hl/frame->image (pp/-frame-at src (pp/-duration src)) 720 405)))"))

(defn notebook-source
  "The generated Clerk notebook text of `lesson`, one section per slide.
   `skipped` is the set of :media block ids that were not rendered; `media`
   maps every other :media block id to its rendered bytes, which the notebook
   embeds as a data URI (a :media block in neither throws)."
  ([lesson] (notebook-source lesson #{} {}))
  ([lesson skipped] (notebook-source lesson skipped {}))
  ([lesson skipped media]
   (let [ctx    {:names (block-names lesson) :skipped (set skipped) :media media}
         slides (for [sl (:lesson/slides lesson)]
                  (str/join "\n\n" (cons (str ";; ## " (one-line (:slide/title sl)))
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
  "Render every :media block of `lesson` under `out` (file names from
   `names`, see `block-names`); returns {:files [relpath] :bytes {block-id
   bytes} :skipped [{:block :format :from :reason ...}]}."
  [lesson names out {:keys [media?] :or {media? true}}]
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
           (let [rel (get-in names [(:block/id b) :media])
                 f   (io/file out rel)]
             (try
               (render-media! b src-b f)
               (-> acc
                   (update :files conj rel)
                   (assoc-in [:bytes (:block/id b)] (Files/readAllBytes (.toPath f))))
               (catch Throwable t
                 (io/delete-file f true)
                 (skip (or (:reason (ex-data t)) :render-failed)
                       :error (.getName (class t)))))))))
     {:files [] :bytes {} :skipped []}
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
;; Bundle directory
;; ============================================================================

(def ^:private no-follow (make-array LinkOption 0))

(defn- bundle-files
  "Relative paths (forward slashes, sorted) of every non-directory entry below
   `dir`. Symlinks are listed, never followed."
  [dir]
  (let [root (.toPath (io/file dir))]
    (if-not (Files/isDirectory root no-follow)
      []
      (with-open [s (Files/walk root (make-array java.nio.file.FileVisitOption 0))]
        (->> (iterator-seq (.iterator s))
             (remove #(Files/isDirectory ^Path % (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])))
             (map #(str/replace (str (.relativize root ^Path %)) "\\" "/"))
             sort
             vec)))))

(defn- check-out-dir!
  "Refuse an out dir build! must not write into: the working directory or one
   of its ancestors, a git work tree root, or a non-empty directory that is
   not a previous bundle (has no manifest.edn)."
  [out]
  (let [o   (.toPath (.getCanonicalFile (io/file out)))
        cwd (.toPath (.getCanonicalFile (io/file ".")))
        bad (fn [why] (throw (ex-info (str "lesson:build refuses out dir " out ": " why)
                                      {:reason :lesson/unsafe-out-dir :out (str out) :why why})))]
    (cond
      (.startsWith cwd o)                 (bad "it is the working directory or an ancestor of it")
      (.exists (io/file out ".git"))      (bad "it is a git work tree root")
      (and (seq (bundle-files out))
           (not (.exists (io/file out "manifest.edn"))))
      (bad "it is not empty and holds no manifest.edn (not a lesson bundle)"))))

(defn- prune!
  "Delete every file below `out` not in `keep` (relpaths), then every
   directory below `out` left empty. Never touches anything outside `out`."
  [out keep]
  (let [root (.getCanonicalFile (io/file out))
        keep (set keep)]
    (doseq [rel (bundle-files root) :when (not (keep rel))
            :let [p (.toPath (io/file root rel))]]
      (when (.startsWith (.normalize (.toAbsolutePath p)) (.toPath root))
        (Files/deleteIfExists p)))
    (doseq [d (reverse (sort-by #(count (str %)) (filter #(.isDirectory ^java.io.File %) (file-seq root))))
            :when (and (not= d root) (empty? (.list ^java.io.File d)))]
      (.delete ^java.io.File d))))

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
   Returns the manifest, also written as manifest.edn. The out dir must be
   empty, absent or a previous bundle; files the new manifest does not list
   are removed from it (see `check-out-dir!`). opts: :out (required),
   :deps-file (default \"deps.edn\"), :source (the path :generated-by names;
   default the given path, else \"lesson.edn\"), :media? (default true; false
   skips every :media block, recorded as :disabled)."
  [lesson-or-path {:keys [out deps-file source] :or {deps-file "deps.edn"} :as opts}]
  (when-not out (throw (ex-info "build! needs {:out dir}" {:opts opts})))
  (let [path?    (not (map? lesson-or-path))
        lesson   (ls/validate (if path? (read-lesson lesson-or-path) lesson-or-path))
        names    (block-names lesson)
        _        (check-out-dir! out)
        out      (io/file out)
        _        (.mkdirs out)
        {media-files :files media-bytes :bytes skipped :skipped} (media! lesson names out opts)
        files    (into [(write! out "lesson.edn" (ser/->canonical-edn lesson))
                        (write! out "lesson.json" (->json lesson))
                        (write! out "notebook.clj"
                                (notebook-source lesson (set (map :block skipped)) media-bytes))]
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
    (prune! out (conj files "manifest.edn"))
    manifest))

(defn read-manifest
  "dir/manifest.edn as data."
  [dir]
  (ser/<-edn (slurp (io/file dir "manifest.edn") :encoding "UTF-8")))

(def ^:private compared-keys
  "Manifest entries verify! compares with the rebuilt manifest, besides :files."
  [:lesson/id :deps :notebook-deps :skipped])

(defn verify!
  "Rebuild the lesson in `dir` (from dir/lesson.edn) in a temp dir and compare
   it with dir/manifest.edn. Returns {:reproducible? bool :mismatches [...]},
   one mismatch per difference:

     {:file rel :expected :actual :on-disk}   a listed or rebuilt file whose
                                              manifest hash (:expected) differs
                                              from the rebuild (:actual) or from
                                              the file in dir (:on-disk, nil =
                                              missing)
     {:file rel :on-disk h :problem :not-in-manifest}
                                              a file in dir the manifest does
                                              not list
     {:key k :expected :actual}               a manifest entry (:lesson/id,
                                              :deps, :notebook-deps, :skipped)
                                              that differs from the rebuild"
  ([dir] (verify! dir {}))
  ([dir opts]
   (let [manifest (read-manifest dir)
         expected (:files manifest)
         tmp      (tmp-dir "lesson-verify")]
     (try
       (let [rebuilt    (build! (str (io/file dir "lesson.edn"))
                                (assoc opts :out (str tmp) :source "lesson.edn"))
             actual     (:files rebuilt)
             on-disk    (fn [rel] (let [f (io/file dir rel)] (when (.exists f) (sha256-hex f))))
             files      (for [rel (sort (into (set (keys expected)) (keys actual)))
                              :let [e (get expected rel) a (get actual rel) d (on-disk rel)]
                              :when (or (not= e a) (not= e d))]
                          {:file rel :expected e :actual a :on-disk d})
             extra      (for [rel (bundle-files dir)
                              :when (and (not= rel "manifest.edn") (not (contains? expected rel)))]
                          {:file rel :on-disk (sha256-hex (io/file dir rel)) :problem :not-in-manifest})
             entries    (for [k compared-keys
                              :let [e (get manifest k) a (get rebuilt k)]
                              :when (not= e a)]
                          {:key k :expected e :actual a})
             mismatches (vec (concat files extra entries))]
         {:reproducible? (empty? mismatches) :mismatches mismatches})
       (finally (delete-tree! tmp))))))

(def usage
  "usage: clojure -M -m desargues.lesson.export build <lesson.edn> [out-dir]
       clojure -M -m desargues.lesson.export verify <dir>")

(defn run
  "The CLI without the exit: returns the exit code (2 = usage error)."
  [& [cmd a b :as args]]
  (cond
    (and (= cmd "build") a (<= (count args) 3))
    (let [out (or b (default-out (read-lesson a)))
          m   (build! a {:out out})]
      (println "lesson:build" (:lesson/id m) "->" out)
      (doseq [[f h] (:files m)] (println " " h f))
      (doseq [s (:skipped m)] (println "  SKIPPED" (pr-str s)))
      0)

    (and (= cmd "verify") a (= 2 (count args)))
    (let [{:keys [reproducible? mismatches]} (verify! a)]
      (println "lesson:verify" a (if reproducible? "REPRODUCIBLE" "MISMATCH"))
      (doseq [m mismatches] (println " " (pr-str m)))
      (if reproducible? 0 1))

    :else
    (do (binding [*out* *err*] (println usage)) 2)))

(defn -main
  "clojure -M -m desargues.lesson.export build <lesson.edn> [out-dir]
   clojure -M -m desargues.lesson.export verify <dir>"
  [& args]
  (let [code (apply run args)]
    (shutdown-agents)
    (System/exit code)))
