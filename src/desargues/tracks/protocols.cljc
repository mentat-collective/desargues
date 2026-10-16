(ns desargues.tracks.protocols
  "PROTOCOL stratum of the tracks context: ITracks, the one seam a consumer
   (a player source, a future MathBox / glTF / Blender exporter) reads tracks
   through. Kept to one method on purpose (ISP): sampling is a pure function
   over the value (desargues.tracks.sample), not a capability of the holder.

   A scene graph map carries its tracks under :tracks; any other holder (a
   TrackSource, a simulation) implements the port itself. Host-free .cljc.")

(defprotocol ITracks
  "Anything that carries time tracks."
  (-tracks [holder]
    "The holder's Tracks value {node-id Track} (desargues.tracks.core); {} when
     it has none."))

(extend-protocol ITracks
  #?(:clj clojure.lang.IPersistentMap :cljs cljs.core/PersistentArrayMap)
  (-tracks [m] (get m :tracks {}))
  #?@(:cljs [cljs.core/PersistentHashMap (-tracks [m] (get m :tracks {}))])
  nil
  (-tracks [_] {}))
