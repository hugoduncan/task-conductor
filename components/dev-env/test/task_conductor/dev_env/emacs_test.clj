(ns task-conductor.dev-env.emacs-test
  ;; Tests for EmacsDevEnv implementation.
  ;; Verifies: callback registration and invocation, message sending format,
  ;; handling of session-complete and error responses, connection failure handling.
  (:require
   [clojure.data.json :as json]
   [clojure.test :refer [deftest is testing]]
   [task-conductor.dev-env.emacs :as emacs]
   [task-conductor.dev-env.interface :as dev-env])
  (:import
   [java.net StandardProtocolFamily UnixDomainSocketAddress]
   [java.nio ByteBuffer]
   [java.nio.channels ServerSocketChannel]
   [java.nio.charset StandardCharsets]
   [java.nio.file Files]))

;;; Test server infrastructure

(defn- temp-socket-path
  "Create a temporary socket file path."
  []
  (let [temp-dir (Files/createTempDirectory
                  "emacs-test"
                  (into-array java.nio.file.attribute.FileAttribute []))
        socket-file (.resolve temp-dir "test.sock")]
    (.toString socket-file)))

(defn- read-json-message
  "Read a newline-delimited JSON message from a channel."
  [channel]
  (let [buffer (ByteBuffer/allocate 4096)
        sb (StringBuilder.)]
    (loop []
      (let [bytes-read (.read channel buffer)]
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
              (let [json-str (.substring content 0 newline-idx)]
                (json/read-str json-str :key-fn keyword)))))))))

(defn- send-json-message
  "Send a JSON message to a channel."
  [channel msg]
  (let [json-str (str (json/write-str msg) "\n")
        bytes (.getBytes json-str StandardCharsets/UTF_8)
        buffer (ByteBuffer/wrap bytes)]
    (while (.hasRemaining buffer)
      (.write channel buffer))))

(defn- start-test-server
  "Start a test server that accepts one connection and provides
   control over responses. Returns map with:
   :path - socket path
   :client-promise - delivers the client channel when connected
   :stop-fn - cleanup function"
  [socket-path]
  (let [address (UnixDomainSocketAddress/of socket-path)
        server (doto (ServerSocketChannel/open StandardProtocolFamily/UNIX)
                 (.bind address))
        client-promise (promise)
        thread (Thread.
                (fn []
                  (try
                    (when-let [client (.accept server)]
                      (deliver client-promise client))
                    (catch Exception _))))]
    (.start thread)
    {:path socket-path
     :client-promise client-promise
     :stop-fn (fn []
                (.close server)
                (.interrupt thread)
                (when (realized? client-promise)
                  (try (.close @client-promise) (catch Exception _)))
                (Files/deleteIfExists
                 (java.nio.file.Path/of socket-path (into-array String []))))}))

(defmacro with-test-server
  "Execute body with a test server. Binds server-map symbol to the server."
  [[server-sym] & body]
  `(let [path# (temp-socket-path)
         ~server-sym (start-test-server path#)]
     (try
       (Thread/sleep 50)
       ~@body
       (finally
         ((:stop-fn ~server-sym))))))

;;; Tests

(deftest create-emacs-dev-env-test
  (testing "create-emacs-dev-env"
    (testing "returns nil when socket does not exist"
      (is (nil? (emacs/create-emacs-dev-env "/tmp/nonexistent-12345.sock"))))

    (testing "returns EmacsDevEnv when socket exists"
      (with-test-server [server]
        (let [env (emacs/create-emacs-dev-env (:path server))]
          (is (some? env))
          (is (satisfies? dev-env/DevEnv env))
          (emacs/stop! env))))))

(deftest open-cli-session-test
  (testing "open-cli-session"
    (testing "when session-id is missing"
      (testing "returns error status and invokes callback"
        (with-test-server [server]
          (let [env (emacs/create-emacs-dev-env (:path server))
                result-promise (promise)
                result (dev-env/open-cli-session
                        env
                        {:prompt "hello"
                         :working-dir "/tmp"}
                        (fn [r] (deliver result-promise r)))
                callback-result (deref result-promise 100 :timeout)]
            (is (= {:status :error} result))
            (is (not= :timeout callback-result))
            (is (nil? (:session-id callback-result)))
            (is (= :error (:status callback-result)))
            (is (= "Missing required :session-id in opts"
                   (:message callback-result)))
            (emacs/stop! env)))))

    (testing "returns {:status :requested}"
      (with-test-server [server]
        (let [env (emacs/create-emacs-dev-env (:path server))
              result (dev-env/open-cli-session
                      env
                      {:session-id "test-1"
                       :prompt "hello"
                       :working-dir "/tmp"}
                      identity)]
          (is (= {:status :requested} result))
          (emacs/stop! env))))

    (testing "sends correct message format"
      (with-test-server [server]
        (let [env (emacs/create-emacs-dev-env (:path server))
              client (deref (:client-promise server) 1000 nil)]
          (is (some? client))
          (dev-env/open-cli-session
           env
           {:session-id "sess-123"
            :prompt "test prompt"
            :working-dir "/home/user"}
           identity)
          (let [msg (read-json-message client)]
            (is (= "open-session" (:type msg)))
            (is (= "sess-123" (:session-id msg)))
            (is (= "test prompt" (:prompt msg)))
            (is (= "/home/user" (:working-dir msg))))
          (emacs/stop! env))))

    (testing "invokes callback on session-complete"
      (with-test-server [server]
        (let [env (emacs/create-emacs-dev-env (:path server))
              client (deref (:client-promise server) 1000 nil)
              result-promise (promise)]
          (dev-env/open-cli-session
           env
           {:session-id "complete-test"}
           (fn [r] (deliver result-promise r)))
          ;; Read the open-session message
          (read-json-message client)
          ;; Send session-complete response
          (send-json-message client
                             {:type "session-complete"
                              :session-id "complete-test"
                              :status "completed"
                              :hook-status {:state "idle"}
                              :exit-code 0})
          (let [result (deref result-promise 1000 :timeout)]
            (is (not= :timeout result))
            (is (= "complete-test" (:session-id result)))
            (is (= :completed (:status result)))
            (is (= {:state "idle"} (:hook-status result)))
            (is (= 0 (:exit-code result))))
          (emacs/stop! env))))

    (testing "invokes callback on error response"
      (with-test-server [server]
        (let [env (emacs/create-emacs-dev-env (:path server))
              client (deref (:client-promise server) 1000 nil)
              result-promise (promise)]
          (dev-env/open-cli-session
           env
           {:session-id "error-test"}
           (fn [r] (deliver result-promise r)))
          (read-json-message client)
          (send-json-message client
                             {:type "error"
                              :session-id "error-test"
                              :message "Connection lost"})
          (let [result (deref result-promise 1000 :timeout)]
            (is (not= :timeout result))
            (is (= "error-test" (:session-id result)))
            (is (= :error (:status result)))
            (is (= "Connection lost" (:message result))))
          (emacs/stop! env))))))

(deftest close-session-test
  (testing "close-session"
    (testing "returns {:status :requested}"
      (with-test-server [server]
        (let [env (emacs/create-emacs-dev-env (:path server))]
          (is (= {:status :requested}
                 (dev-env/close-session env "any-session")))
          (emacs/stop! env))))

    (testing "sends correct message format"
      (with-test-server [server]
        (let [env (emacs/create-emacs-dev-env (:path server))
              client (deref (:client-promise server) 1000 nil)]
          (dev-env/close-session env "close-me")
          (let [msg (read-json-message client)]
            (is (= "close-session" (:type msg)))
            (is (= "close-me" (:session-id msg))))
          (emacs/stop! env))))))

(deftest stop!-test
  (testing "stop!"
    (testing "notifies pending callbacks with error"
      (with-test-server [server]
        (let [env (emacs/create-emacs-dev-env (:path server))
              result-promise (promise)]
          (dev-env/open-cli-session
           env
           {:session-id "pending"}
           (fn [r] (deliver result-promise r)))
          ;; Don't send response, just stop
          (emacs/stop! env)
          (let [result (deref result-promise 1000 :timeout)]
            (is (not= :timeout result))
            (is (= :error (:status result)))
            (is (= "DevEnv shutdown" (:message result)))))))))

