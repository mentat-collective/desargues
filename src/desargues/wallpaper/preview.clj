(ns desargues.wallpaper.preview
  "A live preview window for wallpaper loops: one mpv window that plays the
   current loop on repeat. `show!` renders a spec at preview size in the
   background and swaps it into the window when it is ready; a newer `show!`
   wins over an older one still rendering.

     (open!)                               ; the window (mpv, $DESARGUES_MPV)
     (show! spec)                          ; render at 640x360, then play it
     (show! spec {:size [960 540] :seconds 4})
     (status)                              ; {:generation :rendering :done :total :file}
     (close!)"
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [desargues.wallpaper.render :as r])
  (:import (java.net StandardProtocolFamily UnixDomainSocketAddress)
           (java.nio ByteBuffer)
           (java.nio.channels SocketChannel)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defonce ^:private state (atom {:generation 0}))

(defn- dir []
  (or (:dir @state)
      (let [d (str (Files/createTempDirectory "desargues-preview" (make-array FileAttribute 0)))]
        (swap! state assoc :dir d)
        d)))

(defn- socket-path [] (str (dir) "/mpv.sock"))

(defn- mpv [] (or (System/getenv "DESARGUES_MPV") "mpv"))

(defn- send!
  "Send one mpv IPC command (a vector) to the open window."
  [cmd]
  (with-open [ch (SocketChannel/open StandardProtocolFamily/UNIX)]
    (.connect ch (UnixDomainSocketAddress/of ^String (socket-path)))
    (.write ch (ByteBuffer/wrap (.getBytes (str (json/write-str {:command cmd}) "\n")
                                           StandardCharsets/UTF_8)))))

(defn open?
  "True while the preview window's process runs."
  []
  (boolean (some-> ^Process (:proc @state) .isAlive)))

(defn open!
  "Open the preview window (idempotent). opts: {:geometry \"960x540\" :title}.
   mpv's own output is discarded: an unread pipe would fill and freeze it."
  ([] (open! {}))
  ([{:keys [geometry title] :or {geometry "960x540" title "desargues wallpaper"}}]
   (when-not (open?)
     (let [sock (socket-path)
           _ (io/delete-file sock true)
           proc (.start (doto (ProcessBuilder.
                               ^"[Ljava.lang.String;"
                               (into-array String [(mpv) "--idle=yes" "--force-window=yes" "--keep-open=yes"
                                                   "--loop-file=inf" (str "--geometry=" geometry)
                                                   (str "--title=" title) "--osd-level=0"
                                                   "--really-quiet" "--no-terminal"
                                                   (str "--input-ipc-server=" sock)]))
                          (.redirectOutput java.lang.ProcessBuilder$Redirect/DISCARD)
                          (.redirectError java.lang.ProcessBuilder$Redirect/DISCARD)))]
       (swap! state assoc :proc proc)
       (loop [i 0]
         (when (and (< i 100) (not (.exists (io/file sock))))
           (Thread/sleep 50)
           (recur (inc i))))))
   (open?)))

(defn close!
  "Close the preview window."
  []
  (when-let [^Process p (:proc @state)]
    (.destroy p))
  (swap! state dissoc :proc)
  nil)

(defn status
  "Where the preview is: the current generation, whether it is still
   rendering, frames done of total, and the last file shown."
  []
  (select-keys @state [:generation :rendering :done :total :file :error]))

(defn show!
  "Render loop `spec` at preview size in the background, then play it in the
   window (opened on demand). opts: {:size [w h] :fps :seconds}, default
   640x360 at the spec's fps. Returns the generation number at once."
  ([spec] (show! spec {}))
  ([spec {:keys [size fps seconds] :or {size [640 360]}}]
   (open!)
   (let [gen (:generation (swap! state update :generation inc))
         out (str (dir) "/loop-" gen ".mp4")
         opts (cond-> {:size size :crf 26
                       :progress (fn [done total]
                                   (when (= gen (:generation @state))
                                     (swap! state assoc :done done :total total)))}
                fps (assoc :fps fps)
                seconds (assoc :seconds seconds))]
     (swap! state assoc :rendering true :done 0 :total (r/frames spec opts) :error nil)
     (future
       (try
         (r/encode! :mp4 spec out opts)
         (when (= gen (:generation @state))
           (send! ["loadfile" out "replace"])
           (swap! state assoc :rendering false :file out))
         (catch Throwable t
           (swap! state assoc :rendering false :error (.getMessage t)))))
     gen)))
