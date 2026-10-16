(ns desargues.backend
  "Numeric backends, open by key.

   A backend is a keyword naming who runs the numbers behind a port: :jvm
   (pure Clojure, always present), :raster (io.github.replikativ/raster,
   under the :dynamics alias), or any key another library registers.

   Each port owns a multimethod dispatching on the backend key:

     desargues.physics.solver/solver            key, opts -> TrajectorySolver
     desargues.geometry.kernel/provider         key       -> GeometryKernel
     desargues.board.compiler/kernel-compiler   key, opts -> KernelCompiler
     desargues.kernel/compile-kernel            key, plan -> compiled kernel
     desargues.kernel/realize                   target, expression -> form

   A backend's methods live in provider namespaces. This namespace is the
   registry backend key -> provider namespaces, and loads them the first time
   a port is asked for that backend, so an optional engine stays off the
   classpath until someone names it. A new backend is a `register-backend!`
   plus `defmethod`s in its own namespace; no port is edited."
  (:import (java.io FileNotFoundException)))

(defonce ^:private registry (atom {}))

(defonce ^:private loaded (atom #{}))

(defn register-backend!
  "Declare backend `k` as {:namespaces [sym ..] :requires str}. :namespaces
   carry the backend's port methods and are required by `load!`; :requires
   says what puts the engine on the classpath (shown when it is absent).
   Returns k."
  [k spec]
  (swap! registry assoc k (update spec :namespaces vec))
  (swap! loaded disj k)
  k)

(defn backends
  "The registered backend keys."
  []
  (set (keys @registry)))

(defn describe
  "The registered spec of backend `k`, or nil."
  [k]
  (get @registry k))

(defn- root-cause ^Throwable [^Throwable t]
  (if-let [c (.getCause t)] (recur c) t))

(defn load!
  "Require the provider namespaces of backend `k`; returns k. Throws ex-info
   with :reason :unknown (no such backend) or :absent (a namespace or one of
   its libraries is not on the classpath: a namespace file, or a Java class a
   provider imports). Any other load failure is rethrown as it is: a provider
   that fails to compile must not read as absent."
  [k]
  (let [{:keys [namespaces requires] :as spec} (get @registry k)]
    (when-not spec
      (throw (ex-info (str "Unknown backend " (pr-str k) "; known: " (pr-str (sort (backends)))
                           ". Register one with desargues.backend/register-backend!")
                      {:backend k :reason :unknown :known (backends)})))
    (when-not (contains? @loaded k)
      (try
        (doseq [n namespaces] (require n))
        (catch Throwable t
          (let [root (root-cause t)]
            (if (or (instance? FileNotFoundException root) (instance? ClassNotFoundException root))
              (throw (ex-info (str "Backend " (pr-str k) " is not on the classpath"
                                   (when requires (str "; it needs " requires))
                                   " (" (.getMessage root) ")")
                              {:backend k :reason :absent :requires requires} t))
              (throw t)))))
      (swap! loaded conj k))
    k))

(defn available?
  "True when backend `k` is registered and its providers load."
  [k]
  (try
    (load! k)
    true
    (catch clojure.lang.ExceptionInfo e
      (if (#{:unknown :absent} (:reason (ex-data e))) false (throw e)))))

(defn dispatch-default
  "The body of a port multimethod's :default method: load backend `k`'s
   providers, then call the method they registered on `multi` with k and
   `args`. `port` names the port in the error raised when the backend is
   unknown, absent, or provides no method for it."
  [multi port k args]
  (load! k)
  (if-let [m (get (methods multi) k)]
    (apply m k args)
    (throw (ex-info (str "Backend " (pr-str k) " provides no " port)
                    {:backend k :reason :no-method :port port
                     :provided (disj (set (keys (methods multi))) :default)}))))

(register-backend! :jvm {:namespaces [] :requires nil})

(register-backend! :raster
                   {:namespaces '[desargues.infrastructure.raster-adapter
                                  desargues.infrastructure.raster-board]
                    :requires "the :dynamics alias (io.github.replikativ/raster, JDK 24+)"})

;; Emmy-derived trajectories as WebAssembly (raster's wasm emitter), run on the
;; JVM by Chicory; the same module runs in a browser.
(register-backend! :wasm
                   {:namespaces '[desargues.infrastructure.wasm-runtime]
                    :requires "the :dynamics alias (raster + com.dylibso.chicory/runtime, JDK 24+)"})

;; Batched RK4 on a GPU: raster.par kernels compiled to OpenCL / Level Zero.
(register-backend! :gpu
                   {:namespaces '[desargues.infrastructure.gpu-solver]
                    :requires "the :dynamics alias (raster, JDK 24+) and an OpenCL ICD or Level Zero driver"})
