(ns desargues.perf.profile
  "Named perf profiles: which proven Manim patches and options the dev and
   live loops run with, as data.

   Collect  - DESARGUES_PERF_PROFILE (PerfConfig below), default \"fast\";
              \"stock\" is Manim as shipped.
   Promote  - `profile` turns a name into a PerfProfile value: the profile's
              base data (`profile-base`, open) plus every registered patch
              entry (`patch-entry`, open) that names the profile.
   Boundary - `install!` puts a profile's render patches in place (and takes
              the other registered ones out); `live-opts` is what play-live!
              runs with; `mobject-cache` is the cache a Manim backend reuses
              built mobjects through.

   A patch joins a profile by registering an entry, without editing this
   namespace's data:

     (defmethod profile/patch-entry :my-patch [_]
       {:id :my-patch :rank 60 :profiles #{:fast} :exact? true
        :make (fn [] my-patch-record)})

   Only entries marked :exact? (frames proven SHA-256 identical to stock)
   may join a profile marked :exact-only?, as :fast is. Entries whose
   artifact is missing (the Zig library) still belong to the profile; their
   -install! answers false and Manim keeps its own code.

   Building a profile reads no Manim config and imports no Python."
  (:require [clojure.string :as str]
            [hive-di.core :refer [defconfig env]]
            [malli.core :as m]
            [desargues.manim.patch-protocols :as pp]
            [desargues.manim.patches :as mp]
            [desargues.manim.patches.native-display :as nd]
            [desargues.manim.patches.background-cache :as bc]
            [desargues.manim.mobject-cache :as mcache]
            [desargues.manim.patches.play-setup :as ps]))

;; ============================================================================
;; Schemas and values
;; ============================================================================

(def PatchEntry
  "A patch a profile may install: :make builds its IManimPatch record."
  (m/schema
   [:map
    [:id :keyword]
    [:rank int?]
    [:profiles [:set :keyword]]
    [:exact? :boolean]
    [:make fn?]]))

(def LiveOpts
  "play-live! defaults a profile sets (callers' options win)."
  (m/schema [:map
             [:surface-reuse? {:optional true} :boolean]
             [:native? {:optional true} :boolean]]))

(def BackendOpts
  (m/schema [:map [:mobject-cache? :boolean]]))

(def ProfileBase
  (m/schema [:map
             [:doc :string]
             [:exact-only? :boolean]
             [:live LiveOpts]
             [:backend BackendOpts]]))

(def PerfProfileSchema
  (m/schema [:map
             [:id :keyword]
             [:doc :string]
             [:exact-only? :boolean]
             [:patches [:vector :keyword]]
             [:live LiveOpts]
             [:backend BackendOpts]]))

(defrecord PerfProfile [id doc exact-only? patches live backend])

;; ============================================================================
;; Registries (open)
;; ============================================================================

(defmulti profile-base
  "Profile keyword -> its base data (ProfileBase): everything but the patch
   list, which the registered patch entries supply."
  identity)

(defmethod profile-base :stock [_]
  {:doc "Manim as shipped: no patches, no reuse."
   :exact-only? true
   :live {:surface-reuse? false}
   :backend {:mobject-cache? false}})

(defmethod profile-base :fast [_]
  {:doc "Every patch whose frames are SHA-256 identical to stock, plus
         surface reuse on the live path and an LRU of built mobjects."
   :exact-only? true
   :live {:surface-reuse? true}
   :backend {:mobject-cache? true}})

(defmulti patch-entry
  "Patch id -> PatchEntry. Lower :rank installs first."
  identity)

(defmethod patch-entry :svg-path-shared-copy [_]
  {:id :svg-path-shared-copy :rank 10 :profiles #{:fast} :exact? true
   :make (constantly mp/svg-path-shared-copy)})

(defmethod patch-entry :glyph-cache [_]
  {:id :glyph-cache :rank 20 :profiles #{:fast} :exact? true
   :make mp/glyph-cache})

(defmethod patch-entry :native-cairo-path [_]
  {:id :native-cairo-path :rank 30 :profiles #{:fast} :exact? true
   :make mp/native-cairo-path})

(defmethod patch-entry :native-display [_]
  {:id :native-display :rank 40 :profiles #{:fast} :exact? true
   :make nd/native-display})

(defmethod patch-entry :background-cache [_]
  {:id :background-cache :rank 50 :profiles #{:fast} :exact? true
   :make bc/background-cache})

(defmethod patch-entry :play-setup [_]
  {:id :play-setup :rank 60 :profiles #{:fast} :exact? true
   :make ps/play-setup})

(defn- entry
  "The validated PatchEntry registered under `id`."
  [id]
  (let [e (patch-entry id)]
    (when-not (m/validate PatchEntry e)
      (throw (ex-info (str "Invalid patch entry " id)
                      {:id id :explain (m/explain PatchEntry e)})))
    e))

(defn entries
  "Every registered patch entry, by :rank."
  []
  (->> (keys (methods patch-entry))
       (filter keyword?)
       (map entry)
       (sort-by (juxt :rank :id))
       vec))

(defn known-profiles []
  (sort (remove #{:default} (filter keyword? (keys (methods profile-base))))))

(defmethod profile-base :default [k]
  (throw (ex-info (str "Unknown perf profile " (pr-str k)
                       "; known: " (pr-str (known-profiles)))
                  {:profile k})))

;; ============================================================================
;; Promote: name -> PerfProfile
;; ============================================================================

(defn profile
  "The PerfProfile named `k`: its base data plus the ids of every entry
   that lists it, by rank. Throws when an :exact-only? profile would hold an
   entry that is not :exact?."
  [k]
  (let [base (profile-base k)
        _    (when-not (m/validate ProfileBase base)
               (throw (ex-info (str "Invalid perf profile " k)
                               {:profile k :explain (m/explain ProfileBase base)})))
        mine (filterv #(contains? (:profiles %) k) (entries))
        lossy (remove :exact? mine)]
    (when (and (:exact-only? base) (seq lossy))
      (throw (ex-info (str "Perf profile " k " admits only exact patches")
                      {:profile k :not-exact (mapv :id lossy)})))
    (map->PerfProfile (assoc base :id k :patches (mapv :id mine)))))

(defn valid? [p] (m/validate PerfProfileSchema p))

(defn profile-key
  "Config value (\"fast\", \" Stock \", :fast) -> keyword; blank/nil -> :fast."
  [s]
  (let [n (some-> s name str/trim str/lower-case)]
    (if (str/blank? n) :fast (keyword n))))

(defconfig PerfConfig
  ;; desargues.perf.profile: "fast" (default) or "stock" (Manim as shipped).
  :profile (env "DESARGUES_PERF_PROFILE" :default "fast" :type :string))

(defn configured-name
  "The profile name DESARGUES_PERF_PROFILE selects (\"fast\" when unset)."
  []
  (get-in (resolve-PerfConfig) [:ok :profile] "fast"))

(defn selected
  "The profile selected by DESARGUES_PERF_PROFILE, or by an explicit name."
  ([] (selected (configured-name)))
  ([nm] (profile (profile-key nm))))

(defn ->profile
  "A PerfProfile as given, the one a name selects, or (nil) the configured one."
  [p]
  (cond (instance? PerfProfile p) p
        (nil? p) (selected)
        :else (selected p)))

;; ============================================================================
;; Boundary
;; ============================================================================

(defn render-patches
  "The IManimPatch records of profile `p`, in install order."
  [p]
  (mapv #((:make (entry %))) (:patches (->profile p))))

(defn install!
  "Put profile `p`'s render patches in place and take out every other
   registered patch that is installed. Returns {:profile id :installed [ids]
   :unavailable [ids]}: a patch whose -install! is false (no native library)
   is unavailable and Manim keeps its own code there."
  [p]
  (let [p     (->profile p)
        want  (set (:patches p))
        other (into [] (comp (remove (comp want :id)) (map #((:make %)))) (entries))]
    (run! #(when (mp/installed? %) (mp/uninstall! %)) (rseq other))
    (let [res (mapv (fn [patch] [(pp/-patch-id patch) (boolean (mp/install! patch))])
                    (render-patches p))]
      {:profile (:id p)
       :installed (into [] (comp (filter second) (map first)) res)
       :unavailable (into [] (comp (remove second) (map first)) res)})))

(defn live-opts
  "play-live! options under profile `p`: the profile's :live defaults under
   the caller's `opts`, and :patches = the profile's render patches followed
   by the caller's."
  [p opts]
  (let [p (->profile p)]
    (assoc (merge (:live p) opts)
           :patches (into (render-patches p) (:patches opts)))))

(defn mobject-cache
  "The IMobjectCache a Manim backend under profile `p` reuses built mobjects
   through: an LRU when the profile asks for one, else the NullCache."
  [p]
  (if (get-in (->profile p) [:backend :mobject-cache?])
    (mcache/lru-cache)
    (mcache/null-cache)))
