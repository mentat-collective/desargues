(ns desargues.scene.landing-golden-test
  "The landing page's recorded scene graph is byte-identical to the one
   recorded before the primitive-kernel Lagrangian path (main at 8468f75):
   same evolve, same positions, same EDN."
  (:require [clojure.test :refer [deftest is]]
            [desargues.scene :as s]
            [desargues.scene.data :as rec]))

(def ^:private golden-sha256
  "SHA-256 of (pr-str graph) recorded on main 8468f75, 530336 bytes."
  "7e56a956cc61c268c38e5158876bd1656c5661bab3757ffb3a73482eb51e48ef")

(defn- sha256 [^String s]
  (let [d (.digest (java.security.MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) d))))

(deftest landing-scene-edn-is-byte-identical
  (let [construct @(requiring-resolve 'desargues.site.pendulum3d/construct)
        edn (pr-str (s/with-backend (rec/recording-backend)
                      (s/render! "desargues-double-pendulum" construct)))]
    (is (= 530336 (count edn)))
    (is (= golden-sha256 (sha256 edn)))))
