(ns desargues.player.native-test
  "The native (cljrs) host: the pendulum demo recorded on the JVM is played by
   the cljrs runtime in native/ (the pure player strata on cljrs, tiny-skia
   painting), and its frame at t = 3 matches the headless golden image. The
   JVM-side bridge (desargues.player.native) drives the same four ports
   offscreen through desargues.player/open!.

   Skipped (one passing assertion saying so) when the binary is not built:
   cargo build --release in native/."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [desargues.player :as player]
            [desargues.player.headless :as hl]
            [desargues.player.native :as native]
            [desargues.player.protocols :as p])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private golden-path "test/resources/desargues/player/golden/pendulum-t3.png")

(defn- tmp-dir []
  (str (Files/createTempDirectory "desargues-native" (make-array FileAttribute 0))))

(defn- pendulum-fn []
  (load-file "studio/desargues/studio/demos.clj")
  (resolve 'desargues.studio.demos/pendulum))

(defn- run-native [& args]
  (let [{:keys [exit out err]} (apply sh/sh (.getPath (native/binary)) "desargues.native.main"
                                      (concat args [:env (assoc (into {} (System/getenv))
                                                                "DESARGUES_ROOT"
                                                                (.getAbsolutePath (io/file ".")))]))]
    {:exit exit :err err
     :reply (some->> (str/split-lines out) (remove str/blank?) last edn/read-string)}))

(deftest native-player-plays-the-recorded-pendulum
  (if-not (native/available?)
    (is true "desargues-native not built; skipped")
    (let [graph (player/record "Damped pendulum" ((pendulum-fn) {}))
          dir   (tmp-dir)
          gfile (str dir "/pendulum.edn")
          png   (str dir "/native-t3.png")]
      (spit gfile (pr-str graph))
      (let [{:keys [exit reply err]} (run-native "snapshot" gfile "3.0" "640" "360" png)]
        (is (zero? exit) err)
        (testing "cljrs replays the RecordingBackend graph step for step"
          (is (= (count (:steps graph)) (:steps reply))))
        (testing "the native frame at t = 3 matches the headless golden"
          (let [res (hl/golden-check (hl/read-png png) golden-path
                                     {:actual-out "target/golden-actual/native-pendulum-t3.png"})]
            (is (:pass? res) (str "native pendulum at t=3 vs " golden-path ": " res))))))))

(deftest native-host-implements-the-ports
  (if-not (native/available?)
    (is true "desargues-native not built; skipped")
    (let [h   (native/host {:offscreen? true})
          png (str (tmp-dir) "/bridge-t3.png")
          pl  (player/open! h (pendulum-fn)
                            {:title "native test" :w 640 :h 360 :autoplay? false
                             :scene-name "Damped pendulum"
                             :controls [{:id :damping :label "damping" :min 0.0 :max 1.0 :value 0.15}]})]
      (try
        (is (every? #(satisfies? % h) [p/ISurface p/IWindow p/IControls p/IClock]))
        (is (= [640 360] (p/-size h)))
        (player/seek! pl 3.0)
        (is (= png (player/snapshot! pl png)))
        (let [res (hl/golden-check (hl/read-png png) golden-path {})]
          (is (:pass? res) (str "bridged snapshot vs golden: " res)))
        (testing "a REPL param change re-records and moves the native slider"
          (player/set-param! pl :damping 0.5)
          (is (= 0.5 (:damping (player/params pl)))))
        (finally (player/close! pl))))))

(defn- eventually
  "Poll (pred) for up to `ms`; its last value."
  [ms pred]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (let [v (pred)]
        (if (or v (> (System/currentTimeMillis) deadline)) v (do (Thread/sleep 20) (recur)))))))

(deftest native-input-reaches-the-player-through-serve
  (if-not (native/available?)
    (is true "desargues-native not built; skipped")
    (let [h  (native/host {:offscreen? true})
          pl (player/open! h (pendulum-fn)
                           {:title "native input" :w 640 :h 360 :autoplay? false
                            :scene-name "Damped pendulum"})]
      (try
        (is (every? #(satisfies? % h) [p/IPointer p/IKeyboard]))
        (testing "a wheel notch over the stage zooms the 2D view about the pointer"
          (native/feed! h {:type :wheel :px [320.0 180.0] :lines 1.0 :mods []})
          (is (eventually 5000 #(> (:zoom (player/view pl)) 1.0))))
        (testing "the 0 key resets the view"
          (native/feed! h {:type :key :key "0" :text "0" :mods []})
          (is (eventually 5000 #(nil? (:view (player/view pl))))))
        (testing "a shift drag on the stage pans it; a press over the slider panel does not reach the player"
          (doseq [e [{:type :down :px [300.0 180.0] :button :primary :mods [:shift] :ms 0.0}
                     {:type :move :px [380.0 180.0] :mods [:shift]}
                     {:type :up :px [380.0 180.0] :button :primary :mods [:shift] :ms 90.0}]]
            (native/feed! h e))
          (is (eventually 5000 #(some-> (player/view pl) :view :center first neg?))))
        (testing "a double click resets"
          (doseq [ms [1000.0 1100.0]]
            (native/feed! h {:type :down :px [100.0 100.0] :button :primary :mods [] :ms ms})
            (native/feed! h {:type :up :px [100.0 100.0] :button :primary :mods [] :ms (+ ms 20)}))
          (is (eventually 5000 #(nil? (:view (player/view pl))))))
        (is (nil? (:pointer-error @(:state h))))
        (finally (player/close! pl))))))

(defn- raster-on-classpath? []
  (try (some? (requiring-resolve 'desargues.infrastructure.raster-adapter/raster-solver))
       (catch Throwable _ false)))

(deftest pendulum-raster-row
  (let [pendulum (pendulum-fn)
        controls @(resolve 'desargues.studio.demos/pendulum-controls)
        solver-for @(resolve 'desargues.studio.demos/solver-for)]
    (testing "the demo controls carry a raster row"
      (is (some #(= :raster (:id %)) controls)))
    (testing "the row selects the solver; an explicit :solver wins"
      (is (= :jvm (solver-for {})))
      (is (= :jvm (solver-for {:raster 0.0})))
      (is (= :raster (solver-for {:raster 1.0})))
      (is (= :raster (solver-for {:raster true})))
      (is (= :jvm (solver-for {:raster 1.0 :solver :jvm}))))
    (if-not (raster-on-classpath?)
      (is true "raster not on the classpath (run with :dynamics); skipped")
      (testing "{:raster 1} integrates on raster and records the same steps as the JVM"
        (let [jvm (player/record "Damped pendulum" (pendulum {}))
              ras (player/record "Damped pendulum" (pendulum {:raster 1.0}))]
          (is (= (count (:steps jvm)) (count (:steps ras))))
          (is (pos? (count (:steps ras)))))))))
