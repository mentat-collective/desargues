(ns desargues.player.native
  "BOUNDARY stratum: the native (cljrs) host for the player, seen from the JVM.

   Implements every port of desargues.player.protocols (ISurface, IWindow,
   IControls, IClock) by driving a `desargues-native serve` process: the
   cljrs runtime in native/ that paints Frames with tiny-skia into a winit +
   softbuffer window (native/cljrs/desargues/native/host.cljrs implements the
   same ports on that side). Frames cross as EDN, one map per line; ticks,
   slider changes and snapshots come back the same way. Nothing above the
   ports changes (LSP): (player/open! (native/host) construct opts).

   Build the binary once: `cargo build --release` in native/ (or point
   DESARGUES_NATIVE at a built one). `available?` says whether it exists.
   Pass :offscreen? true in the open! opts to run without a window."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [desargues.player.protocols :as p]
            [desargues.player.native-input :as native-input])
  (:import [java.io BufferedReader BufferedWriter File]
           [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(defn binary
  "Path of the desargues-native executable: $DESARGUES_NATIVE, else
   native/target/release/desargues-native under the working directory."
  ^File []
  (io/file (or (System/getenv "DESARGUES_NATIVE")
               "native/target/release/desargues-native")))

(defn available?
  "True when the native binary has been built."
  []
  (let [f (binary)] (and (.isFile f) (.canExecute f))))

(defn- send! [{:keys [state]} m]
  (let [^BufferedWriter w (:out @state)]
    (locking w
      (.write w (pr-str m))
      (.newLine w)
      (.flush w))))

(defn- input!
  "Translate raw native input `raw` (desargues.player.native-input) against
   the frame last presented and hand the events to the :on-pointer and
   :on-key fns, when set. Errors are kept in the state as :pointer-error."
  [state raw]
  (let [{:keys [w h frame on-pointer on-key]} @state
        [st' evs] (native-input/translate (:input @state (native-input/initial)) w h frame raw)]
    (swap! state assoc :input st')
    (doseq [ev evs]
      (when-let [f (if (= :key (:type ev)) on-key on-pointer)]
        (try (f ev) (catch Throwable t (swap! state assoc :pointer-error t)))))))

(defn- handle! [state {:keys [ev] :as m}]
  (case ev
    :tick   (when-let [f (:tick @state)]
              (try (f (double (:dt m)))
                   (catch Throwable t (swap! state assoc :tick-error t))))
    :change (when-let [f (get-in @state [:on-change (:id m)])]
              (f (:id m) (:value m)))
    :input  (input! state (:event m))
    (.put ^LinkedBlockingQueue (:replies @state) m)))

(defn- start-reader! [state ^BufferedReader r]
  (doto (Thread. ^Runnable
         (fn []
           (loop []
             (when-let [line (.readLine r)]
               (when-let [m (try (edn/read-string line) (catch Exception _ nil))]
                 (when (map? m) (handle! state m)))
               (recur)))
           (.put ^LinkedBlockingQueue (:replies @state) {:ev :closed})))
    (.setDaemon true)
    (.setName "desargues-native-reader")
    (.start)))

(defn- await-reply
  "The next reply whose :ev is in `evs`, or nil after `ms`."
  [state evs ms]
  (let [^LinkedBlockingQueue q (:replies @state)
        deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (let [left (- deadline (System/currentTimeMillis))]
        (when (pos? left)
          (let [m (.poll q left TimeUnit/MILLISECONDS)]
            (if (or (nil? m) (contains? evs (:ev m))) m (recur))))))))

(defn- launch! [state]
  (let [pb (doto (ProcessBuilder. ^java.util.List [(.getPath (binary))
                                                   "desargues.native.main" "serve"])
             (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT))
        _  (.put (.environment pb) "DESARGUES_ROOT" (.getAbsolutePath (io/file ".")))
        pr (.start pb)]
    (swap! state assoc :proc pr
           :out (io/writer (.getOutputStream pr))
           :replies (LinkedBlockingQueue.))
    (start-reader! state (io/reader (.getInputStream pr)))))

(defrecord NativeHost [state]
  p/ISurface
  (-present! [this f]
    (swap! state assoc :frame f)
    (send! this {:op :present :frame f})
    this)
  (-size [_] [(:w @state) (:h @state)])

  p/IWindow
  (-show! [this {:keys [w h] :as opts}]
    (launch! state)
    (swap! state assoc :w w :h h)
    (send! this (assoc (select-keys opts [:title :w :h])
                       :op :show :offscreen? (boolean (:offscreen? @state))))
    (swap! state assoc :window? (:window? (await-reply state #{:ready :closed} 20000)))
    this)
  (-close! [this]
    (when-let [^Process pr (:proc @state)]
      (try (send! this {:op :close}) (catch Exception _ nil))
      (await-reply state #{:closed} 5000)
      (.destroy pr))
    this)
  (-snapshot! [this path]
    (let [path (.getAbsolutePath (io/file (str path)))]
      (some-> (io/file path) .getParentFile .mkdirs)
      (send! this {:op :snapshot :path path})
      (:path (await-reply state #{:snapshot} 20000)))))

(extend-type NativeHost
  p/IControls
  (-control! [this spec on-change]
    (swap! (:state this) assoc-in [:on-change (:id spec)] on-change)
    (send! this {:op :control :spec (dissoc spec :on-change)})
    this)
  (-set-control! [this id v]
    (send! this {:op :set-control :id id :value v})
    this)

  p/IClock
  (-on-tick! [this f]
    (swap! (:state this) assoc :tick f)
    (send! this {:op :ticks :on? true})
    this)
  (-stop-ticks! [this]
    (swap! (:state this) assoc :tick nil)
    (when (:proc @(:state this))
      (try (send! this {:op :ticks :on? false}) (catch Exception _ nil)))
    this))

(extend-type NativeHost
  p/IPointer
  (-on-pointer! [this f]
    (swap! (:state this) assoc :on-pointer f)
    this)

  p/IKeyboard
  (-on-key! [this f]
    (swap! (:state this) assoc :on-key f)
    this))

(defn feed!
  "Hand raw window event `raw` (see desargues.player.native-input) to the
   native side as if its window had sent it; it comes back as {:ev :input}
   and reaches the IPointer / IKeyboard fns like real input. For tests and
   displayless runs."
  [h raw]
  (send! h {:op :input :event raw})
  h)

(defn host
  "A fresh, unopened native host. opts: {:offscreen? bool} (no window; every
   port still works, snapshots included). Hand it to desargues.player/open!."
  ([] (host {}))
  ([{:keys [offscreen?]}]
   (->NativeHost (atom {:offscreen? (boolean offscreen?)}))))

(defn window?
  "True when the native side opened a real window (false offscreen or headless)."
  [h]
  (boolean (:window? @(:state h))))
