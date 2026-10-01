(ns desargues.logic.verify.ansatz
  "The Lean 4 kernel as a desargues.logic.verify/Checker, through ansatz.

   A tautology's proof term (desargues.logic.lean) is checked by the kernel
   with Lean's Init environment only. Needs the :ansatz alias (ansatz on the
   classpath, Java 21+, --enable-native-access=ALL-UNNAMED)."
  (:require [ansatz.core :as a]
            [desargues.logic.lean :as lean]
            [desargues.logic.verify :as v]))

(defonce ^:private init (delay (a/load-init!)))

(defonce ^:private counter (atom 0))

(defn- theorem-name []
  (symbol (str "desargues_" (swap! counter inc))))

(defn kernel
  "A Checker that proves a formula in the Lean kernel. Its certificate is the
   theorem it checked: {:name :params :statement :tactics}. A formula that is
   not a tautology is :refuted without calling the kernel; a kernel rejection
   is :unknown with the kernel's message."
  []
  (reify v/Checker
    (-id [_] :lean-kernel)
    (-check [_ formula]
      (if-let [{:keys [name params statement tactics] :as thm}
               (lean/theorem (theorem-name) formula)]
        (try
          @init
          (a/prove-theorem name params statement tactics)
          {:verdict :proved :certificate thm}
          (catch Throwable e
            {:verdict :unknown :error (.getMessage e) :certificate thm}))
        {:verdict :refuted}))))
