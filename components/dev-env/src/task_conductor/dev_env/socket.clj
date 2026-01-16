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

(defn- parse-first-message
  "Parse the first complete JSON message from content string.
   Returns [parsed-message remaining-content] or nil if no complete message."
  [content]
  (let [newline-idx (.indexOf ^String content "\n")]
    (when-not (neg? newline-idx)
      (let [json-str (.substring ^String content 0 newline-idx)
            remaining (.substring ^String content (inc newline-idx))
            parsed (try
                     (json/read-str json-str :key-fn keyword)
                     (catch Exception _
                       {:error :parse-error :raw json-str}))]
        [parsed remaining]))))

(defn make-message-reader
  "Create a stateful message reader for a socket channel.
   Returns a function that when called, reads and returns the next message.
   Buffers any content after a newline for subsequent calls.

   The returned function returns:
   - parsed message map on success
   - nil if the connection was closed
   - {:error :parse-error, :raw <string>} if JSON parsing fails"
  [^SocketChannel channel]
  (let [buffer-atom (atom "")
        byte-buffer (ByteBuffer/allocate 4096)]
    (fn []
      (try
        ;; First check if we have a complete message buffered
        (if-let [[msg remaining] (parse-first-message @buffer-atom)]
          (do
            (reset! buffer-atom remaining)
            msg)
          ;; Need to read more data
          (loop []
            (let [bytes-read (.read channel byte-buffer)]
              (cond
                ;; Connection closed
                (neg? bytes-read)
                nil

                ;; No bytes read, keep waiting
                (zero? bytes-read)
                (recur)

                :else
                (do
                  (.flip byte-buffer)
                  (let [bytes (byte-array (.remaining byte-buffer))]
                    (.get byte-buffer bytes)
                    (swap! buffer-atom str (String. bytes StandardCharsets/UTF_8)))
                  (.clear byte-buffer)
                  (if-let [[msg remaining] (parse-first-message @buffer-atom)]
                    (do
                      (reset! buffer-atom remaining)
                      msg)
                    (recur)))))))
        (catch Exception _
          nil)))))

(defn receive-message!
  "Receive a newline-delimited JSON message (blocking).
   NOTE: Does not buffer content after newlines - use make-message-reader
   for scenarios where multiple messages may arrive together.

   channel - the SocketChannel to read from

   Returns the parsed message map on success, or:
   - nil if the connection was closed
   - {:error :parse-error, :raw <string>} if JSON parsing fails"
  [^SocketChannel channel]
  ((make-message-reader channel)))
