(ns desargues.wallpaper-luxcore-test
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [desargues.backend :as backend]
            [desargues.wallpaper.luxcore :as lx]
            [desargues.wallpaper.render :as r]))

(def ^:private tiny
  {:camera {:orig [0 -5 4] :target [0 0 0] :fov 45}
   :materials {:floor {:type "matte" :kd [0.05 0.12 0.23]}
               :lamp {:type "matte" :kd [0 0 0] :emission [30 26 20]}}
   :objects {:floor {:material :floor :vertices [[-4 -4 0] [4 -4 0] [4 4 0] [-4 4 0]] :faces [[0 1 2] [0 2 3]]}
             :lamp {:material :lamp :vertices [[-1 -1 3] [1 -1 3] [1 1 3] [-1 1 3]] :faces [[0 2 1] [0 3 2]]}}
   :spp 4})

(deftest scene-text-is-luxcore-properties
  (let [lines (set (str/split-lines (lx/props->text (lx/scene-props tiny))))]
    (is (lines "scene.camera.lookat.orig = 0 -5 4"))
    (is (lines "scene.materials.lamp.emission = 30 26 20"))
    (is (lines "scene.objects.floor.material = floor"))
    (is (lines "scene.objects.floor.faces = 0 1 2 0 2 3"))))

(deftest extra-object-keys-pass-through
  (let [s (assoc-in tiny [:objects :lamp :camerainvisible] true)
        pairs (into {} (lx/scene-props s))]
    (is (= 1 (pairs "scene.objects.lamp.camerainvisible")))
    (is (not (contains? pairs "scene.objects.floor.camerainvisible")))))

(deftest config-names-the-film
  (let [cfg (into {} (lx/config-props tiny 64 36 "/tmp/f.png"))]
    (is (= 64 (cfg "film.width")))
    (is (= 4 (cfg "batch.haltspp")))
    (is (= "/tmp/f.png" (cfg "film.outputs.1.filename")))))

(deftest prism-is-closed
  (let [{:keys [vertices faces]} (lx/prism [[0 0] [1 0] [1 1] [0 1]] 0.0 0.1)]
    (is (= 9 (count vertices)))
    (testing "4 top fan triangles + 4 sides x 2"
      (is (= 12 (count faces))))
    (is (every? #(< -1 % (count vertices)) (flatten faces)))))

(deftest ribbons-merge-with-offsets
  (let [{:keys [vertices faces]} (lx/ribbons [[[0 0] [1 0] [1 1]] [[2 2] [3 2]]] 0.1 0.0 0.05)]
    (is (= (* 3 9) (count vertices)))
    (is (= (* 3 12) (count faces)))
    (is (= (dec (count vertices)) (apply max (flatten faces))))))

(deftest luxcore-is-a-registered-engine
  (is (contains? (backend/backends) :luxcore)))

(deftest a-scene-renders-through-luxcoreconsole
  (when (zero? (:exit (sh/sh "sh" "-c" (str "command -v " (lx/console)))))
    (let [img (r/render-frame {:size [64 36] :scene (constantly tiny)} 0.0)]
      (is (= 64 (.getWidth img)))
      (is (some #(not= 0 (bit-and % 0xFFFFFF))
                (.getRGB img 0 0 64 36 nil 0 64))))))
