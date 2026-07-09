(ns desargues.manim-test
  "Simple test to verify manim integration works"
  (:require [libpython-clj2.python :as py]
            [desargues.config :as config]))

(defn test-python-init []
  "Test that Python initializes correctly"
  (try
    (let [{:keys [python-exe library-path]} (config/manim-config)]
      (py/initialize!
       :python-executable python-exe
       :library-path library-path))
    (println "✓ Python initialized successfully")
    (println "Python version:" (py/run-simple-string "import sys; print(sys.version)"))
    true
    (catch Exception e
      (println "✗ Failed to initialize Python:" (.getMessage e))
      false)))

(defn test-manim-import []
  "Test that manim can be imported"
  (try
    (let [manim (py/import-module "manim")]
      (println "✓ Manim imported successfully")
      (println "Manim version:" (py/get-attr manim "__version__"))
      true)
    (catch Exception e
      (println "✗ Failed to import manim:" (.getMessage e))
      false)))

(defn test-basic-object-creation []
  "Test creating a basic manim object"
  (try
    (let [manim (py/import-module "manim")
          Circle (py/get-attr manim "Circle")
          circle (Circle)]
      (println "✓ Created Circle object successfully")
      (println "Circle type:" (type circle))
      true)
    (catch Exception e
      (println "✗ Failed to create Circle:" (.getMessage e))
      false)))

(defn run-all-tests []
  "Run all tests in sequence"
  (println "\n=== Testing Manim Integration ===\n")
  (let [init-ok? (test-python-init)
        import-ok? (when init-ok? (test-manim-import))
        create-ok? (when import-ok? (test-basic-object-creation))]
    (println "\n=== Test Results ===")
    (println "Python Init:" (if init-ok? "✓ PASS" "✗ FAIL"))
    (println "Manim Import:" (if import-ok? "✓ PASS" "✗ FAIL"))
    (println "Object Creation:" (if create-ok? "✓ PASS" "✗ FAIL"))
    (println "\n" (if (and init-ok? import-ok? create-ok?)
                    "All tests passed! 🎉"
                    "Some tests failed. Check the output above."))
    (and init-ok? import-ok? create-ok?)))

(comment
  ;; Run this in the REPL to test your setup:
  (run-all-tests))
