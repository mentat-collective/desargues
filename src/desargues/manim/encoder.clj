(ns desargues.manim.encoder
  "Encoder profiles for Manim's movie writer.

   Manim CE hard-codes libx264 / yuv420p / crf 23 (x264 preset medium) for
   every partial movie file, and the final movie is a packet remux of those,
   so the partial-file codec IS the output codec.

   Collect   — DESARGUES_ENCODER (desargues.config :encoder), default \"final\".
   Promote   — `encoder-profile` turns the name into an EncoderProfile value
               (pure, open for extension via defmethod).
   Boundary  — IEncoderInstaller; the Manim adapter hands the profile to
               py/desargues_encoder.py, which patches
               SceneFileWriter.open_partial_movie_stream.

   :final is Manim's own settings. :preview trades file size (~4x) for
   encoder CPU: worth it on a contended box, neutral on an idle one (the
   encoder runs in a writer thread beside the rasterizer)."
  (:require [clojure.string :as str]
            [malli.core :as m]
            [desargues.config :as config]))

;; ============================================================================
;; Value
;; ============================================================================

(defrecord EncoderProfile [codec pix-fmt options])

(def EncoderProfileSchema
  "codec and pix-fmt are ffmpeg names; options are ffmpeg codec options
   (string -> string), passed to libav as-is."
  (m/schema
   [:map
    [:codec [:string {:min 1}]]
    [:pix-fmt [:string {:min 1}]]
    [:options [:map-of :string :string]]]))

(defn valid-profile? [p] (m/validate EncoderProfileSchema p))

;; ============================================================================
;; Profiles — (encoder-profile :final) etc.
;; ============================================================================

(defmulti encoder-profile
  "Profile keyword -> EncoderProfile."
  identity)

(defmethod encoder-profile :final [_]
  ;; Manim's own settings, with the implicit x264 preset spelled out.
  (->EncoderProfile "libx264" "yuv420p" {"crf" "23" "preset" "medium"}))

(defmethod encoder-profile :preview [_]
  ;; ultrafast: no CABAC, no B-frames, dia ME — a fraction of the encoder CPU.
  ;; zerolatency: no lookahead / frame threads, so the writer thread does not
  ;; buffer frames; yuv420p keeps every player happy.
  (->EncoderProfile "libx264" "yuv420p"
                    {"crf" "23" "preset" "ultrafast" "tune" "zerolatency"}))

(defn known-profiles []
  (sort (remove #{:default} (keys (methods encoder-profile)))))

(defmethod encoder-profile :default [k]
  (throw (ex-info (str "Unknown encoder profile " (pr-str k)
                       "; known: " (pr-str (known-profiles)))
                  {:profile k})))

(defn profile-key
  "Config value (\"final\", \" Preview \", :preview) -> profile keyword; blank/nil -> :final."
  [s]
  (let [n (some-> s name str/trim str/lower-case)]
    (if (str/blank? n) :final (keyword n))))

(defn selected-profile
  "The profile selected by config (DESARGUES_ENCODER), or by an explicit name."
  ([] (selected-profile (:encoder (config/manim-config))))
  ([name] (encoder-profile (profile-key name))))

;; ============================================================================
;; Boundary
;; ============================================================================

(defprotocol IEncoderInstaller
  (-install! [installer profile] "Make subsequent renders encode with profile; returns profile.")
  (-uninstall! [installer] "Restore the backend's own encoder."))

(defn- py-fn [sym] (requiring-resolve (symbol "libpython-clj2.python" (name sym))))

(defn- shim-module
  "py/desargues_encoder, imported after manim is (so its libs are preloaded)."
  []
  ((requiring-resolve 'desargues.manim.core/manim))
  (config/add-project-to-syspath!)
  ((py-fn 'import-module) "desargues_encoder"))

(defrecord ManimEncoderInstaller []
  IEncoderInstaller
  (-install! [_ {:keys [codec pix-fmt options] :as profile}]
    (when-not (valid-profile? profile)
      (throw (ex-info "Invalid encoder profile"
                      {:profile profile :explain (m/explain EncoderProfileSchema profile)})))
    ((py-fn 'call-attr) (shim-module) "install" codec pix-fmt ((py-fn '->py-dict) options))
    profile)
  (-uninstall! [_]
    ((py-fn 'call-attr) (shim-module) "uninstall")
    nil))

(defn manim-installer [] (->ManimEncoderInstaller))

(defn ->profile
  "An EncoderProfile as given, or the one a name/keyword selects."
  [profile-or-name]
  (if (instance? EncoderProfile profile-or-name)
    profile-or-name
    (selected-profile profile-or-name)))

(defn install!
  "Install the config-selected profile (or the named / given one) into Manim."
  ([] (install! (selected-profile)))
  ([profile-or-name]
   (-install! (manim-installer) (->profile profile-or-name))))
