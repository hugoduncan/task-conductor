(ns task-conductor.dev-env.socket
  "UNIX domain socket client for dev-env communication.

   Provides bidirectional JSON message exchange over a UNIX domain socket.
   Messages are newline-delimited JSON (one message per line)."
  (:require
   [clojure.data.json :as json])
  (:import
   [java.net StandardProtocolFamily UnixDomainSocketAddress]
   [java.nio ByteBuffer]
   [java.nio.channels SocketChannel]
   [java.nio.charset StandardCharsets]))

(def default-socket-path
  "Default socket path for dev-env communication."
  "/tmp/task-conductor-dev-env.sock")

(defn connect!
  "Connect to a UNIX domain socket.

   path - socket file path (defaults to default-socket-path)

   Returns the SocketChannel on success, nil on failure."
  ([]
   (connect! default-socket-path))
  ([path]
   (try
     (let [address (UnixDomainSocketAddress/of ^String path)
           channel (SocketChannel/open StandardProtocolFamily/UNIX)]
       (.connect channel address)
       channel)
     (catch Exception _
       nil))))

(defn disconnect!
  "Close a socket channel.

   Returns true on success, false on failure."
  [^SocketChannel channel]
  (try
    (when channel
      (.close channel))
    true
    (catch Exception _
      false)))

(defn send-message!
  "Send a message as newline-delimited JSON.

   channel - the SocketChannel to write to
   message - a map to serialize as JSON

   Returns true on success, nil on failure."
  [^SocketChannel channel message]
  (try
    (let [json-str (str (json/write-str message) "\n")
          bytes (.getBytes json-str StandardCharsets/UTF_8)
          buffer (ByteBuffer/wrap bytes)]
      (while (.hasRemaining buffer)
        (.write channel buffer))
      true)
    (catch Exception _
      nil)))

(defn receive-message!
  "Receive a newline-delimited JSON message (blocking).

   channel - the SocketChannel to read from

   Returns the parsed message map on success, or:
   - nil if the connection was closed
   - {:error :parse-error, :raw <string>} if JSON parsing fails"
  [^SocketChannel channel]
  (try
    (let [buffer (ByteBuffer/allocate 4096)
          sb (StringBuilder.)]
      (loop []
        (let [bytes-read (.read channel buffer)]
          (cond
            ;; Connection closed
            (neg? bytes-read)
            nil

            ;; No bytes read, keep waiting
            (zero? bytes-read)
            (recur)

            :else
            (do
              (.flip buffer)
              (let [bytes (byte-array (.remaining buffer))]
                (.get buffer bytes)
                (.append sb (String. bytes StandardCharsets/UTF_8)))
              (.clear buffer)
              (let [content (.toString sb)
                    newline-idx (.indexOf content "\n")]
                (if (neg? newline-idx)
                  (recur)
                  (let [json-str (.substring content 0 newline-idx)]
                    (try
                      (json/read-str json-str :key-fn keyword)
                      (catch Exception _
                        {:error :parse-error :raw json-str}))))))))))
    (catch Exception _
      nil)))
