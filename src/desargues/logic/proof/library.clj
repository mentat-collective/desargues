(ns desargues.logic.proof.library
  "The results a proof may use: the port, and a map-backed adapter.

   A deck builds its library cumulatively in book order, so an exercise can
   use only what was proved before it.")

(defprotocol ResultLibrary
  (statement [lib id] "The statement of proved result id, or nil."))

(defrecord MapLibrary [results]
  ResultLibrary
  (statement [_ id] (get results id)))

(defn library
  "A library over a map of result id -> statement."
  [results]
  (->MapLibrary results))

(def empty-library (library {}))
