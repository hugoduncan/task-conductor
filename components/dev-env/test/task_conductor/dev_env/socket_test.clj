(ns task-conductor.dev-env.socket-test
  ;; Tests for UNIX domain socket client.
  ;; Verifies connection, message exchange, and disconnection using
  ;; a temp socket with a test server.
  (:require
   [clojure.data.json :as json]
   [clojure.test :refer [deftest is testing]]
   [task-conductor.dev-env.socket :as socket])
  (:import
   [java.net StandardProtocolFamily UnixDomainSocketAddress]
   [java.nio ByteBuffer]
   [java.nio.channels ServerSocketChannel SocketChannel]
   [java.nio.charset StandardCharsets]
   [java.nio.file Files]))

(defn- temp-socket-path
  "Create a temporary socket file path."
  []
  (let [temp-dir (Files/createTempDirectory "socket-test" (into-array java.nio.file.attribute.FileAttribute []))
        socket-file (.resolve temp-dir "test.sock")]
    (.toString socket-file)))

(defn- start-echo-server
  "Start a server that echoes back JSON messages with an :echo key added.
   Returns {:server server-channel :path socket-path :ready-promise :stop-fn (fn [])}."
  [socket-path]
  (let [address (UnixDomainSocketAddress/of socket-path)
        server (doto (ServerSocketChannel/open StandardProtocolFamily/UNIX)
                 (.bind address))
        running (atom true)
        ready-promise (promise)
        thread (Thread.
                (fn []
                  (try
                    (deliver ready-promise true)
                    (while @running
                      (when-let [client (.accept server)]
                        (try
                          (let [buffer (ByteBuffer/allocate socket/buffer-size)
                                sb (StringBuilder.)]
                            (loop []
                              (let [bytes-read (.read client buffer)]
                                (when (pos? bytes-read)
                                  (.flip buffer)
                                  (let [bytes (byte-array (.remaining buffer))]
                                    (.get buffer bytes)
                                    (.append sb (String. bytes StandardCharsets/UTF_8)))
                                  (.clear buffer)
                                  (let [content (.toString sb)
                                        newline-idx (.indexOf content "\n")]
                                    (if (neg? newline-idx)
                                      (recur)
                                      (let [json-str (.substring content 0 newline-idx)
                                            msg (json/read-str json-str :key-fn keyword)
                                            response (assoc msg :echo true)
                                            response-str (str (json/write-str response) "\n")
                                            response-buf (ByteBuffer/wrap (.getBytes response-str StandardCharsets/UTF_8))]
                                        (while (.hasRemaining response-buf)
                                          (.write client response-buf)))))))))
                          (finally
                            (.close client)))))
                    (catch Exception _))))]
    (.start thread)
    {:server server
     :path socket-path
     :ready-promise ready-promise
     :stop-fn (fn []
                (reset! running false)
                (.close server)
                (.interrupt thread)
                (Files/deleteIfExists (java.nio.file.Path/of socket-path (into-array String []))))}))

(defmacro with-echo-server
  "Execute body with an echo server running on a temp socket.
   Binds socket-path to the path of the socket."
  [[socket-path-sym] & body]
  `(let [path# (temp-socket-path)
         {:keys [~'stop-fn ~'ready-promise]} (start-echo-server path#)
         ~socket-path-sym path#]
     (try
       (deref ~'ready-promise 1000 nil)
       ~@body
       (finally
         (~'stop-fn)))))

(deftest connect!-test
  (testing "connect!"
    (testing "returns nil for non-existent socket"
      (is (nil? (socket/connect! "/tmp/non-existent-socket-12345.sock"))))

    (testing "returns channel for valid socket"
      (with-echo-server [socket-path]
        (let [channel (socket/connect! socket-path)]
          (is (instance? SocketChannel channel))
          (is (.isConnected channel))
          (socket/disconnect! channel))))))

(deftest disconnect!-test
  (testing "disconnect!"
    (testing "returns true and closes channel"
      (with-echo-server [socket-path]
        (let [channel (socket/connect! socket-path)]
          (is (true? (socket/disconnect! channel)))
          (is (not (.isOpen channel))))))

    (testing "returns true for nil channel"
      (is (true? (socket/disconnect! nil))))))

(deftest send-and-receive-test
  (testing "send-message! and receive-message!"
    (testing "round-trip a simple message"
      (with-echo-server [socket-path]
        (let [channel (socket/connect! socket-path)
              msg {:type "test" :data "hello"}]
          (is (true? (socket/send-message! channel msg)))
          (let [response (socket/receive-message! channel)]
            (is (= "test" (:type response)))
            (is (= "hello" (:data response)))
            (is (true? (:echo response))))
          (socket/disconnect! channel))))

    (testing "handles nested data structures"
      (with-echo-server [socket-path]
        (let [channel (socket/connect! socket-path)
              msg {:type "complex"
                   :nested {:foo "bar"}
                   :list [1 2 3]}]
          (is (true? (socket/send-message! channel msg)))
          (let [response (socket/receive-message! channel)]
            (is (= {:foo "bar"} (:nested response)))
            (is (= [1 2 3] (:list response))))
          (socket/disconnect! channel))))))

(deftest error-handling-test
  (testing "error handling"
    (testing "send-message! returns nil on closed channel"
      (with-echo-server [socket-path]
        (let [channel (socket/connect! socket-path)]
          (socket/disconnect! channel)
          (is (nil? (socket/send-message! channel {:test true}))))))

    (testing "receive-message! returns nil on closed channel"
      (with-echo-server [socket-path]
        (let [channel (socket/connect! socket-path)]
          (socket/disconnect! channel)
          (is (nil? (socket/receive-message! channel))))))))

(defn- start-multi-message-server
  "Start a server that sends multiple messages in a single write.
   Returns {:server server-channel :path socket-path :ready-promise :stop-fn (fn [])}."
  [socket-path messages]
  (let [address (UnixDomainSocketAddress/of socket-path)
        server (doto (ServerSocketChannel/open StandardProtocolFamily/UNIX)
                 (.bind address))
        running (atom true)
        ready-promise (promise)
        thread (Thread.
                (fn []
                  (try
                    (deliver ready-promise true)
                    (while @running
                      (when-let [client (.accept server)]
                        (try
                          ;; Write all messages in a single buffer
                          (let [combined (apply str (map #(str (json/write-str %) "\n") messages))
                                response-buf (ByteBuffer/wrap (.getBytes combined StandardCharsets/UTF_8))]
                            (while (.hasRemaining response-buf)
                              (.write client response-buf)))
                          (finally
                            (.close client)))))
                    (catch Exception _))))]
    (.start thread)
    {:server server
     :path socket-path
     :ready-promise ready-promise
     :stop-fn (fn []
                (reset! running false)
                (.close server)
                (.interrupt thread)
                (Files/deleteIfExists (java.nio.file.Path/of socket-path (into-array String []))))}))

(defmacro with-multi-message-server
  "Execute body with a server that sends multiple messages at once."
  [[socket-path-sym messages] & body]
  `(let [path# (temp-socket-path)
         {:keys [~'stop-fn ~'ready-promise]} (start-multi-message-server path# ~messages)
         ~socket-path-sym path#]
     (try
       (deref ~'ready-promise 1000 nil)
       ~@body
       (finally
         (~'stop-fn)))))

(deftest make-message-reader-test
  ;; Tests for stateful message reader that buffers content across calls.
  ;; Verifies that multiple messages received in a single read are
  ;; correctly parsed and returned on subsequent calls.
  (testing "make-message-reader"
    (testing "returns messages one at a time when multiple arrive together"
      (let [messages [{:type "first" :n 1}
                      {:type "second" :n 2}
                      {:type "third" :n 3}]]
        (with-multi-message-server [socket-path messages]
          (let [channel (socket/connect! socket-path)
                read-message! (socket/make-message-reader channel)]
            ;; Blocking read waits for server to send
            (is (= {:type "first" :n 1} (read-message!)))
            (is (= {:type "second" :n 2} (read-message!)))
            (is (= {:type "third" :n 3} (read-message!)))
            (socket/disconnect! channel)))))

    (testing "returns nil after all buffered messages consumed and connection closed"
      (let [messages [{:type "only" :n 1}]]
        (with-multi-message-server [socket-path messages]
          (let [channel (socket/connect! socket-path)
                read-message! (socket/make-message-reader channel)]
            ;; Blocking read waits for server to send
            (is (= {:type "only" :n 1} (read-message!)))
            ;; Server closed after sending - next read should return nil
            (is (nil? (read-message!)))
            (socket/disconnect! channel)))))))
