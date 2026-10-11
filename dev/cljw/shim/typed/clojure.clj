(ns typed.clojure
  "Probe-only stub: the real runtime reaches into clojure.lang.RT. Every
  annotation macro desargues uses expands to nil.")

(defmacro ann [& _] nil)
(defmacro ann-protocol [& _] nil)
(defmacro ann-record [& _] nil)
(defmacro ann-datatype [& _] nil)
(defmacro defalias [& _] nil)
