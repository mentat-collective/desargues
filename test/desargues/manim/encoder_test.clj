(ns desargues.manim.encoder-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.config :as config]
            [desargues.manim.encoder :as enc]))

(deftest profiles-are-valid-values
  (doseq [k (enc/known-profiles)]
    (testing (str k)
      (is (instance? desargues.manim.encoder.EncoderProfile (enc/encoder-profile k)))
      (is (enc/valid-profile? (enc/encoder-profile k))))))

(deftest final-is-manims-own-settings
  (let [{:keys [codec pix-fmt options]} (enc/encoder-profile :final)]
    (is (= "libx264" codec))
    (is (= "yuv420p" pix-fmt))
    (is (= {"crf" "23" "preset" "medium"} options))))

(deftest preview-is-ultrafast-x264
  (let [{:keys [codec pix-fmt options]} (enc/encoder-profile :preview)]
    (is (= "libx264" codec))
    (is (= "yuv420p" pix-fmt))
    (is (= "ultrafast" (options "preset")))
    (is (= "23" (options "crf")))))

(deftest profile-key-normalizes-config-strings
  (is (= :final (enc/profile-key nil)))
  (is (= :final (enc/profile-key "")))
  (is (= :final (enc/profile-key "  ")))
  (is (= :preview (enc/profile-key " Preview ")))
  (is (= :preview (enc/profile-key :preview)))
  (is (= :final (enc/profile-key "final"))))

(deftest selection-by-name
  (is (= (enc/encoder-profile :preview) (enc/selected-profile "preview")))
  (is (= (enc/encoder-profile :final) (enc/selected-profile nil)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown encoder profile :bogus"
                        (enc/selected-profile "bogus"))))

(deftest ->profile-passes-values-through
  (let [p (enc/->EncoderProfile "mjpeg" "yuvj420p" {"q:v" "5"})]
    (is (identical? p (enc/->profile p)))
    (is (= (enc/encoder-profile :preview) (enc/->profile :preview)))))

(deftest schema-rejects-malformed-profiles
  (is (not (enc/valid-profile? (enc/->EncoderProfile "" "yuv420p" {}))))
  (is (not (enc/valid-profile? (enc/->EncoderProfile "libx264" "yuv420p" {"crf" 23}))))
  (is (not (enc/valid-profile? {:codec "libx264"}))))

(deftest config-defaults-to-final
  (testing "DESARGUES_ENCODER unset -> \"final\" (an env-set value is still a string)"
    (let [v (get-in (config/resolve-ManimConfig) [:ok :encoder])]
      (when (map? (:ok (config/resolve-ManimConfig)))
        (is (string? v))
        (when-not (System/getenv "DESARGUES_ENCODER")
          (is (= "final" v)))))))

(deftest installer-rejects-invalid-profile-before-python
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid encoder profile"
                        (enc/-install! (enc/manim-installer)
                                       (enc/->EncoderProfile "" "" {})))))
