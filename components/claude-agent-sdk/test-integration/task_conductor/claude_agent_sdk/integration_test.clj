(ns task-conductor.claude-agent-sdk.integration-test
  ;; Integration tests for Claude Agent SDK with live API.
  ;; Requires Claude authentication (API key or subscription).
  ;; Run with: clj -M:test --focus :integration
  ;;
  ;; Initializes libpython-clj with venv Python at load time.
  ;; Tests skip gracefully when authentication is unavailable.
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [libpython-clj2.python :as py]))

(def ^:private venv-path
  "Relative path to venv from project root."
  "components/claude-agent-sdk/.venv")

(defn- abs-venv-python
  "Get absolute path to venv Python executable."
  []
  (str (.getAbsolutePath (io/file venv-path)) "/bin/python"))

(defn- skip-integration-tests?
  "Check if integration tests should be skipped.
   Set SKIP_INTEGRATION_TESTS=1 to skip explicitly."
  []
  (some? (System/getenv "SKIP_INTEGRATION_TESTS")))

;; Initialize libpython-clj with venv Python before requiring SDK.
(py/initialize! :python-executable (abs-venv-python))

;; Now require SDK interface - it will use the already-initialized Python.
(require '[task-conductor.claude-agent-sdk.interface :as sdk])

;; Initialize SDK module imports.
((resolve 'task-conductor.claude-agent-sdk.interface/initialize!)
 {:venv-path venv-path})

(defn- auth-error?
  "Check if exception is an authentication error."
  [e]
  (let [msg (str e)]
    (or (re-find #"(?i)auth" msg)
        (re-find #"(?i)api.key" msg)
        (re-find #"(?i)unauthorized" msg)
        (re-find #"(?i)not.logged.in" msg))))

(defmacro with-auth-skip
  "Execute body, skipping if SKIP_INTEGRATION_TESTS is set or auth fails."
  [& body]
  `(if (skip-integration-tests?)
     (testing "skipped (SKIP_INTEGRATION_TESTS set)"
       (is true))
     (try
       ~@body
       (catch Exception e#
         (if (auth-error? e#)
           (testing "skipped (Claude authentication unavailable)"
             (is true))
           (throw e#))))))

(deftest live-query-test
  ;; Tests full query lifecycle with live Claude API.
  ;; Sends a simple prompt and verifies response structure.
  (with-auth-skip
    (testing "live query"
      (let [client (sdk/create-client {:max-turns 1
                                       :permission-mode "bypassPermissions"})]
        (try
          (sdk/connect client)

          (testing "returns response with messages and session-id"
            (let [result (sdk/query client "Reply with just: OK")]
              (is (map? result)
                  "should return a map")
              (is (vector? (:messages result))
                  "should have :messages vector")
              (is (seq (:messages result))
                  "should have at least one message")
              (is (string? (:session-id result))
                  "should have session-id string")))

          (finally
            (sdk/disconnect client)))))))

(deftest live-with-session-test
  ;; Tests with-session macro with live Claude API.
  (with-auth-skip
    (testing "with-session"
      (testing "manages session lifecycle and captures session-id"
        (let [result (sdk/with-session [client {:max-turns 1
                                                :permission-mode "bypassPermissions"}]
                       (sdk/session-query client "Reply: OK"))]
          (is (map? result)
              "should return a map")
          (is (some? (:result result))
              "should have :result")
          (is (string? (:session-id result))
              "should capture session-id"))))))

(deftest live-message-parsing-test
  ;; Tests that live responses are correctly parsed into Clojure data.
  (with-auth-skip
    (testing "message parsing"
      (let [client (sdk/create-client {:max-turns 1
                                       :permission-mode "bypassPermissions"})]
        (try
          (sdk/connect client)
          (let [result (sdk/query client "Reply: test")
                messages (:messages result)]

            (testing "includes user message"
              (let [user-msgs (filter #(= :user-message (:type %)) messages)]
                (is (seq user-msgs)
                    "should have user message")))

            (testing "includes assistant message with content"
              (let [assistant-msgs (filter #(= :assistant-message (:type %))
                                           messages)]
                (is (seq assistant-msgs)
                    "should have assistant message")
                (when-let [msg (first assistant-msgs)]
                  (is (some? (:content msg))
                      "assistant message should have content")
                  (is (some? (:model msg))
                      "assistant message should have model"))))

            (testing "includes result message"
              (let [result-msgs (filter #(= :result-message (:type %)) messages)]
                (is (seq result-msgs)
                    "should have result message")
                (when-let [msg (first result-msgs)]
                  (is (some? (:session-id msg))
                      "result message should have session-id")
                  (is (number? (:duration-ms msg))
                      "result message should have duration-ms")))))
          (finally
            (sdk/disconnect client)))))))
