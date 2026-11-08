(ns desargues.manim.mobject-cache
  "IMobjectCache and IKeptStore adapters.

   NullCache keeps nothing: every fetch builds, which is the behaviour of a
   desargues without reuse. LruMobjectCache keeps at most `max-entries`
   builds, least recently used out first; its keys and metas are JVM values,
   the built objects live in an IKeptStore under an integer slot.

   Stores: `python-store` keeps Manim mobjects in a Python dict and copies
   them with Mobject.copy (a deep copy), so the kept build outlives the
   Python callback it was built in. `value-store` keeps JVM objects with an
   injected copy fn (tests, non-Python backends). Building any record needs
   no Python and no Manim config: the Python store resolves its shim on
   first use."
  (:require [desargues.manim.mobject-cache-protocols :as p]
            [malli.core :as m])
  (:import (java.util LinkedHashMap Map$Entry)))

;; ============================================================================
;; Values
;; ============================================================================

(def CacheStatsSchema
  (m/schema [:map
             [:hits nat-int?] [:misses nat-int?]
             [:size nat-int?] [:max nat-int?]]))

(def BuiltSchema
  "What a build fn returns: the object and the meta to keep with it."
  (m/schema [:map [:mobject some?] [:meta {:optional true} any?]]))

;; ============================================================================
;; Stores
;; ============================================================================

(defrecord ValueStore [copy objects]
  p/IKeptStore
  (-keep! [_ slot obj] (swap! objects assoc slot (copy obj)) nil)
  (-copy-of [_ slot] (copy (get @objects slot)))
  (-drop! [_ slot] (swap! objects dissoc slot) nil)
  (-drop-all! [_] (reset! objects {}) nil))

(defn value-store
  "A store of JVM objects, copied with `copy` (fn [obj] independent-copy)."
  [copy]
  (->ValueStore copy (atom {})))

(def ^:private py-store-source
  "_DMC = {}

def desargues_mc_keep(store, slot, obj):
    _DMC.setdefault(store, {})[slot] = obj.copy()
    return None

def desargues_mc_copy(store, slot):
    return _DMC[store][slot].copy()

def desargues_mc_drop(store, slot):
    _DMC.get(store, {}).pop(slot, None)
    return None

def desargues_mc_drop_all(store):
    _DMC.pop(store, None)
    return None

def desargues_mc_size(store):
    return len(_DMC.get(store, {}))
")

(def ^:private py-module "desargues_mobject_cache")

(defn- ensure-py-module!
  "Define the store's Python module (in sys.modules) once. Nothing JVM-side
   holds on to it: a handle made inside a Python callback would be released
   when the callback returns."
  []
  (let [run (requiring-resolve 'libpython-clj2.python/run-simple-string)]
    (run (str "import sys as _s, types as _t\n"
              "if '" py-module "' not in _s.modules:\n"
              "    _m = _t.ModuleType('" py-module "')\n"
              "    exec(" (pr-str py-store-source) ", _m.__dict__)\n"
              "    _s.modules['" py-module "'] = _m\n"))
    nil))

(defn- py-call
  "Call `fname` of the store module with `args`."
  [fname & args]
  (let [import (requiring-resolve 'libpython-clj2.python/import-module)
        call   (requiring-resolve 'libpython-clj2.python/call-attr)
        m      (or (try (import py-module) (catch Exception _ nil))
                   (do (ensure-py-module!) (import py-module)))]
    (apply call m fname args)))

(defrecord PythonStore [id]
  p/IKeptStore
  (-keep! [_ slot obj] (py-call "desargues_mc_keep" id slot obj) nil)
  (-copy-of [_ slot] (py-call "desargues_mc_copy" id slot))
  (-drop! [_ slot] (py-call "desargues_mc_drop" id slot) nil)
  (-drop-all! [_] (py-call "desargues_mc_drop_all" id) nil))

(defn python-store
  "A store of Manim mobjects held by Python (a dict per store in the module
   desargues_mobject_cache), copied with Mobject.copy."
  []
  (->PythonStore (str (random-uuid))))

;; ============================================================================
;; NullCache
;; ============================================================================

(defrecord NullCache []
  p/IMobjectCache
  (-active? [_] false)
  (-fetch! [_ _ build] (assoc (build) :hit? false))
  (-meta [_ _] nil)
  (-stats [_] {:hits 0 :misses 0 :size 0 :max 0})
  (-clear! [_] nil))

(defn null-cache
  "A cache that keeps nothing."
  []
  (->NullCache))

;; ============================================================================
;; LruMobjectCache
;; ============================================================================

(defn- evict-eldest!
  "Drop entries past `max-entries`, least recently used first."
  [^LinkedHashMap index store max-entries]
  (while (> (.size index) (long max-entries))
    (let [it (.iterator (.entrySet index))
          ^Map$Entry e (.next it)]
      (.remove it)
      (p/-drop! store (:slot (.getValue e))))))

(defrecord LruMobjectCache [max-entries store ^LinkedHashMap index state]
  p/IMobjectCache
  (-active? [_] true)
  (-fetch! [_ key build]
    (if-let [{:keys [slot meta]} (locking index (.get index key))]
      (do (swap! state update :hits inc)
          {:mobject (p/-copy-of store slot) :meta meta :hit? true})
      (let [{:keys [mobject meta] :as built} (build)]
        (when-not (m/validate BuiltSchema built)
          (throw (ex-info "mobject build fn must return {:mobject m :meta x}"
                          {:key key})))
        (let [slot (:next-slot (swap! state #(-> % (update :misses inc) (update :next-slot inc))))]
          (p/-keep! store slot mobject)
          (locking index
            (.put index key {:slot slot :meta meta})
            (evict-eldest! index store max-entries)))
        {:mobject mobject :meta meta :hit? false})))
  (-meta [_ key]
    (:meta (locking index (.get index key))))
  (-stats [_]
    (-> (select-keys @state [:hits :misses])
        (assoc :size (locking index (.size index)) :max max-entries)))
  (-clear! [_]
    (locking index
      (.clear index)
      (p/-drop-all! store))
    (swap! state assoc :hits 0 :misses 0)
    nil))

(defn lru-cache
  "A bounded LRU cache of built mobjects. opts: :max-entries (default 512),
   :store (an IKeptStore), default `python-store`."
  ([] (lru-cache {}))
  ([{:keys [max-entries store] :or {max-entries 512}}]
   {:pre [(pos-int? max-entries)]}
   (->LruMobjectCache max-entries (or store (python-store))
                      (LinkedHashMap. 16 0.75 true)
                      (atom {:hits 0 :misses 0 :next-slot 0}))))

;; ============================================================================
;; Port calls
;; ============================================================================

(defn active? [cache] (p/-active? cache))
(defn fetch! [cache key build] (p/-fetch! cache key build))
(defn kept-meta [cache key] (p/-meta cache key))

(defn stats
  "{:hits :misses :size :max}."
  [cache]
  (p/-stats cache))

(defn clear! [cache] (p/-clear! cache))
