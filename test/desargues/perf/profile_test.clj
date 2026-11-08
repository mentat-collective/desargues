(ns desargues.perf.profile-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.config :as config]
            [desargues.manim.core :as core]
            [desargues.manim.mobject-cache :as mcache]
            [desargues.manim.patch-protocols :as pp]
            [desargues.manim.patches :as mp]
            [desargues.manim.patches.surface-reuse :as sr]
            [desargues.manim.raster :as mr]
            [desargues.perf.profile :as perf]
            [desargues.scene.manim :as sm]))

(def manim-env? (delay (config/manim-ready?)))

;; ---------------------------------------------------------------------------
;; Pure: profiles are data and need no Manim
;; ---------------------------------------------------------------------------

(deftest profiles-are-valid-values
  (doseq [k (perf/known-profiles)]
    (is (perf/valid? (perf/profile k)) (str k)))
  (is (every? (set (perf/known-profiles)) [:fast :stock]))
  (is (not-any? #{:default} (perf/known-profiles))))

(deftest stock-is-manim-as-shipped
  (let [p (perf/profile :stock)]
    (is (= [] (:patches p)))
    (is (false? (get-in p [:live :surface-reuse?])))
    (is (false? (get-in p [:backend :mobject-cache?])))
    (is (= [] (perf/render-patches p)))))

(deftest fast-holds-the-exact-patches-in-rank-order
  (let [p (perf/profile :fast)]
    (is (= [:svg-path-shared-copy :glyph-cache :native-cairo-path :native-display
            :background-cache :play-setup]
           (:patches p)))
    (is (= (:patches p) (mapv pp/-patch-id (perf/render-patches p)))
        "every entry builds its record without Manim")
    (is (true? (get-in p [:live :surface-reuse?])))
    (is (true? (get-in p [:backend :mobject-cache?])))))

(deftest names-are-normalised
  (is (= :fast (perf/profile-key nil)))
  (is (= :fast (perf/profile-key "")))
  (is (= :stock (perf/profile-key " Stock ")))
  (is (= :fast (perf/profile-key :fast)))
  (is (= :stock (:id (perf/selected "stock"))))
  (is (thrown? clojure.lang.ExceptionInfo (perf/selected "turbo"))))

(deftest the-default-profile-is-fast-unless-configured
  (is (= (perf/profile-key (System/getenv "DESARGUES_PERF_PROFILE"))
         (:id (perf/->profile nil))))
  (let [p (perf/profile :stock)]
    (is (identical? p (perf/->profile p)))))

;; A registered entry joins the profiles it names; a lossy one is refused by
;; an :exact-only? profile. Test-only profile keys keep :fast untouched.

(defmethod perf/profile-base ::trial [_]
  {:doc "test profile" :exact-only? true
   :live {} :backend {:mobject-cache? false}})

(defmethod perf/profile-base ::lossy-ok [_]
  {:doc "test profile admitting lossy patches" :exact-only? false
   :live {} :backend {:mobject-cache? false}})

(defrecord NoopPatch [id]
  pp/IManimPatch
  (-patch-id [_] id)
  (-install! [_] true)
  (-uninstall! [_] false)
  (-installed? [_] false))

(defmethod perf/patch-entry ::exact-trial [_]
  {:id ::exact-trial :rank 1000 :profiles #{::trial ::lossy-ok} :exact? true
   :make #(->NoopPatch ::exact-trial)})

(deftest registered-entries-join-their-profiles
  (is (= [::exact-trial] (:patches (perf/profile ::trial))))
  (is (not-any? #{::exact-trial} (:patches (perf/profile :fast)))))

(deftest exact-only-profiles-refuse-lossy-patches
  (defmethod perf/patch-entry ::lossy-trial [_]
    {:id ::lossy-trial :rank 1001 :profiles #{::trial ::lossy-ok} :exact? false
     :make #(->NoopPatch ::lossy-trial)})
  (try
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"only exact"
                          (perf/profile ::trial)))
    (is (= [::exact-trial ::lossy-trial] (:patches (perf/profile ::lossy-ok))))
    (finally (remove-method perf/patch-entry ::lossy-trial))))

(deftest invalid-entries-are-refused
  (defmethod perf/patch-entry ::broken [_] {:id ::broken :profiles #{::trial}})
  (try
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid patch entry"
                          (perf/profile ::trial)))
    (finally (remove-method perf/patch-entry ::broken))))

(deftest live-opts-put-the-callers-options-over-the-profile
  (let [o (perf/live-opts :fast {:paced? false})]
    (is (true? (:surface-reuse? o)))
    (is (false? (:paced? o)))
    (is (= (:patches (perf/profile :fast)) (mapv pp/-patch-id (:patches o)))))
  (let [extra (->NoopPatch ::extra)
        o     (perf/live-opts :fast {:surface-reuse? false :patches [extra]})]
    (is (false? (:surface-reuse? o)))
    (is (= ::extra (pp/-patch-id (peek (:patches o)))) "the caller's patches go last"))
  (is (= {:surface-reuse? false :patches []} (perf/live-opts :stock {}))))

(deftest live-plan-follows-the-profile
  (let [stock (mr/live-plan :bgra-pre {:profile :stock})
        fast  (mr/live-plan :bgra-pre {:profile :fast})]
    (is (= :tensor (:crossing stock)))
    (is (= :address (:crossing fast)))
    (is (not-any? #{:surface-reuse} (map pp/-patch-id (:patches stock))))
    (is (some #{:surface-reuse} (map pp/-patch-id (:patches fast))))
    (is (= (:src-layout stock) (:src-layout fast)) "the pixel layout does not depend on the profile")
    (is (= 15 (:fps fast)))))

(deftest backends-take-their-cache-from-the-profile
  (is (instance? desargues.manim.mobject_cache.NullCache
                 (:mobject-cache (sm/manim-backend {:perf-profile :stock}))))
  (is (mcache/active? (:mobject-cache (sm/manim-backend {:perf-profile :fast}))))
  (let [c (mcache/null-cache)]
    (is (identical? c (:mobject-cache (sm/manim-backend {:perf-profile :fast :mobject-cache c})))
        "an explicit cache wins"))
  (is (= :stock (:id (:perf-profile (sm/manim-backend {:perf-profile "stock"}))))))

;; ---------------------------------------------------------------------------
;; Against Manim (needs the Manim env)
;; ---------------------------------------------------------------------------

(deftest install-switches-between-profiles
  (if-not @manim-env?
    (is true "no Manim env; skipped")
    (do
      (core/init!)
      (try
        (let [fast (perf/install! :fast)]
          (is (= :fast (:profile fast)))
          (is (= (set (:patches (perf/profile :fast)))
                 (into (set (:installed fast)) (:unavailable fast)))
              "every patch is either installed or reported unavailable")
          (is (every? #{:native-cairo-path :native-display} (:unavailable fast))
              "only the native patches can be missing (no Zig library)")
          (is (every? mp/installed? (filter (comp (set (:installed fast)) pp/-patch-id)
                                            (perf/render-patches :fast)))))
        (let [stock (perf/install! :stock)]
          (is (= {:profile :stock :installed [] :unavailable []} stock))
          (is (not-any? mp/installed? (perf/render-patches :fast))
              "stock takes every registered patch out"))
        (is (not (mp/installed? sr/surface-reuse)))
        (finally (perf/install! :stock))))))
