(ns desargues.devaot-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [desargues.bench.render :as render]
            [desargues.devaot :as devaot]
            [desargues.devaot.launch :as launch]))

(deftest ns-paths-strip-clojure-extensions
  (is (= #{"desargues/scene" "desargues/scene/data" "user"}
         (devaot/ns-paths ["desargues/scene.cljc" "desargues/scene/data.clj" "user.clj"
                           "desargues/manim/deferred_imports.py"]))))

(deftest local-class-covers-init-fns-and-types
  (let [nses #{"desargues/scene" "desargues/player/view"}]
    (testing "a namespace owns its __init, fn and nested type classes"
      (is (devaot/local-class? nses "desargues/scene__init.class"))
      (is (devaot/local-class? nses "desargues/scene$render_layout_BANG_.class"))
      (is (devaot/local-class? nses "desargues/player/view$fn__123$fn__124.class"))
      (is (devaot/local-class? nses "desargues/scene/SomeRecord.class")))
    (testing "libraries and non-class files are kept"
      (is (not (devaot/local-class? nses "emmy/env__init.class")))
      (is (not (devaot/local-class? nses "desargues/pyaot/PythonLibrary.class")))
      (is (not (devaot/local-class? nses "desargues/devaot-stamp.edn"))))))

(deftest local-dirs-keep-project-dirs-only
  (let [root (.getCanonicalPath (io/file "."))
        cp   (str "src:test:target/devaot:" (System/getProperty "java.home") ":nope.jar")]
    (is (= [(str root "/src") (str root "/test")]
           (devaot/local-dirs cp "." ["target"])))))

(deftest source-loaded-skips-local-and-compiled
  (let [local? #(= 'desargues.scene %)]
    (is (= ['no.such.lib] (devaot/source-loaded ['desargues.scene 'clojure.core 'no.such.lib]
                                                "target/no-such-dir" local?)))))

(deftest split-classpath-separates-jars
  (is (= {:jars ["a.jar" "b/c.jar"] :dirs ["src" "/x/y"]}
         (launch/split-classpath "a.jar:src::b/c.jar:/x/y")))
  (is (= {:jars [] :dirs []} (launch/split-classpath nil))))

(deftest cds-opts-train-then-use
  (is (= "-XX:AOTCacheOutput=x.aot" (first (render/cds-opts "x.aot" (constantly false)))))
  (is (= "-XX:AOTCache=x.aot" (first (render/cds-opts "x.aot" (constantly true))))))

(deftest jar-only-child-cmd-hands-dirs-to-the-launcher
  (let [argv (render/child-cmd "py" {:cp-prefix ["target/devaot.jar"] :jar-only? true})
        cp   (second (drop-while #(not= "-cp" %) argv))
        dirs (some #(when (.startsWith ^String % "-Ddesargues.devaot.dirs=") %) argv)]
    (is (every? #(.endsWith ^String % ".jar") (.split ^String cp ":")))
    (is (.startsWith ^String cp (.getAbsolutePath (io/file "target/devaot.jar"))))
    (is (some? dirs))
    (is (= ["clojure.main" "-m" "desargues.devaot.launch" "desargues.bench.render" "--child" "py"]
           (take-last 6 argv)))))
