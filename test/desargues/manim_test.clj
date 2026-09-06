(ns desargues.manim-test
  "Unit tests for Manim integration"
  (:require [clojure.test :refer :all]
            [libpython-clj2.python :as py]
            [desargues.manim-quickstart :as mq]
            [desargues.config :as config]))

(def ^:dynamic *python-initialized* false)

(def manim-env?
  "True when the resolved python can import manim (desargues.config/manim-ready?)."
  (delay (config/manim-ready?)))

(defn init-python-once! []
  "Initialize Python once for all tests (config-driven; override via
   DESARGUES_MANIM_* env vars)."
  (when-not *python-initialized*
    (mq/init!)
    (alter-var-root #'*python-initialized* (constantly true))))

(use-fixtures :once
  (fn [f]
    (if @manim-env?
      (do (init-python-once!) (f))
      (println "[manim-test] no Manim env (set CONDA_PREFIX/DESARGUES_CONDA_PREFIX); skipping"))))

(deftest test-python-initialization
  (testing "Python should be initialized"
    (is *python-initialized*
        "Python runtime should be initialized")))

(deftest test-python-version
  (testing "Python version should be 3.12.x"
    (let [sys (py/import-module "sys")
          version-info (py/get-attr sys "version_info")
          major (py/get-item version-info 0)
          minor (py/get-item version-info 1)]
      (is (= 3 major) "Python major version should be 3")
      (is (= 12 minor) "Python minor version should be 12"))))

(deftest test-manim-import
  (testing "Manim module should be importable"
    (let [manim (py/import-module "manim")]
      (is (some? manim) "Manim module should not be nil")
      (is (py/has-attr? manim "__version__") "Manim should have __version__ attribute"))))

(deftest test-manim-version
  (testing "Manim version should be accessible"
    (let [manim (py/import-module "manim")
          version (py/get-attr manim "__version__")]
      (is (string? version) "Manim version should be a string")
      (is (re-matches #"\d+\.\d+\.\d+" version)
          "Version should match pattern X.Y.Z"))))

(deftest test-create-circle
  (testing "Should be able to create a Circle object"
    (let [manim (py/import-module "manim")
          Circle (py/get-attr manim "Circle")
          circle (Circle)]
      (is (some? circle) "Circle object should not be nil")
      (is (= :pyobject (type circle)) "Circle should be a Python object"))))

(deftest test-circle-methods
  (testing "Circle should have expected methods"
    (let [manim (py/import-module "manim")
          Circle (py/get-attr manim "Circle")
          circle (Circle)]
      (is (py/has-attr? circle "set_fill") "Circle should have set_fill method")
      (is (py/has-attr? circle "set_stroke") "Circle should have set_stroke method")
      (is (py/has-attr? circle "rotate") "Circle should have rotate method"))))

(deftest test-set-fill-color
  (testing "Should be able to set circle fill color"
    (let [manim (py/import-module "manim")
          Circle (py/get-attr manim "Circle")
          PINK (py/get-attr manim "PINK")
          circle (Circle)]
      ;; Call set_fill with proper keyword arguments
      (py/call-attr-kw circle "set_fill" [PINK] {:opacity 0.5})
      ;; If we got here without exception, the test passes
      (is true "set_fill should execute without error"))))

(deftest test-scene-class
  (testing "Should be able to access Scene class"
    (let [manim (py/import-module "manim")
          Scene (py/get-attr manim "Scene")]
      (is (some? Scene) "Scene class should not be nil")
      (is (py/has-attr? Scene "__init__") "Scene should be a class with __init__"))))

(deftest test-create-animation
  (testing "Should be able to access Create animation"
    (let [manim (py/import-module "manim")
          Create (py/get-attr manim "Create")]
      (is (some? Create) "Create animation should not be nil"))))

(deftest test-colors-available
  (testing "Common colors should be available"
    (let [manim (py/import-module "manim")
          colors [:RED :BLUE :GREEN :PINK :YELLOW :ORANGE :PURPLE :WHITE :BLACK]]
      (doseq [color colors]
        (is (py/has-attr? manim (name color))
            (str (name color) " color should be available"))))))

(deftest test-create-scene-instance
  (testing "Should be able to import a Scene from Python module"
    (let [;; Put the desargues project root (where manim_examples.py lives) on
          ;; sys.path via the injected config, then import the example module.
          py-module (do (config/add-project-to-syspath!)
                        (py/run-simple-string "
import manim_examples
manim_examples
"))
          ;; The return value should have our examples
          ]
      ;; Just verify we can import without error
      (is true "Python module should import successfully"))))

(deftest test-basic-shapes
  (testing "Basic shapes should be available"
    (let [manim (py/import-module "manim")
          shapes [:Circle :Square :Triangle :Rectangle :Polygon]]
      (doseq [shape shapes]
        (is (py/has-attr? manim (name shape))
            (str (name shape) " should be available"))))))

(deftest test-basic-animations
  (testing "Basic animations should be available"
    (let [manim (py/import-module "manim")
          animations [:Create :Transform :FadeIn :FadeOut :Write]]
      (doseq [animation animations]
        (is (py/has-attr? manim (name animation))
            (str (name animation) " animation should be available"))))))

(deftest test-math-constants
  (testing "Mathematical constants should be available"
    (let [manim (py/import-module "manim")
          PI (py/get-attr manim "PI")
          TAU (py/get-attr manim "TAU")]
      (is (number? PI) "PI should be a number")
      (is (number? TAU) "TAU should be a number")
      (is (< 3.14 PI 3.15) "PI should be approximately 3.14159")
      (is (< 6.28 TAU 6.29) "TAU should be approximately 6.28318"))))
