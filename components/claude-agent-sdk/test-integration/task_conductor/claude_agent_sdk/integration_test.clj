(ns task-conductor.claude-agent-sdk.integration-test
  ;; Integration tests for Claude Agent SDK with live API.
  ;; Requires Claude authentication (API key or subscription).
  ;; Run with: clj -M:test --focus :integration
  ;;
  ;; Uses lazy initialization to gracefully skip when venv is unavailable.
  ;; Tests skip gracefully when authentication is unavailable.
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.python-venv.interface :as venv]))

(def ^:private venv-path
  "Relative path to venv from project root."
  "components/claude-agent-sdk/.venv")

(def ^:private requirements-path
  "Relative path to requirements.txt from project root."
  "components/claude-agent-sdk/requirements.txt")

(defn- skip-integration-tests?
  "Check if integration tests should be skipped.
   Set SKIP_INTEGRATION_TESTS=1 to skip explicitly."
  []
  (some? (System/getenv "SKIP_INTEGRATION_TESTS")))

;; Track whether SDK initialization succeeded.
(def ^:private sdk-initialized?
  (delay
    (when (not (skip-integration-tests?))
      (when (venv/ensure! venv-path requirements-path)
        (try
          (require 'libpython-clj2.python)
          ((resolve 'libpython-clj2.python/initialize!)
           :python-executable (venv/python-path venv-path))
          (require '[task-conductor.claude-agent-sdk.interface :as sdk])
          ((resolve 'task-conductor.claude-agent-sdk.interface/initialize!)
           {:venv-path venv-path})
          true
          (catch Exception e
            (println "SDK initialization failed:" (.getMessage e))
            false))))))

(defn- sdk-available?
  "Check if the SDK is initialized and available."
  []
  @sdk-initialized?)

(defn- auth-error?
  "Check if exception is an authentication error."
  [e]
  (let [msg (str e)]
    (or (re-find #"(?i)auth" msg)
        (re-find #"(?i)api.key" msg)
        (re-find #"(?i)unauthorized" msg)
        (re-find #"(?i)not.logged.in" msg))))

(defmacro with-auth-skip
  "Execute body, skipping if SKIP_INTEGRATION_TESTS is set, SDK unavailable, or auth fails."
  [& body]
  `(cond
     (skip-integration-tests?)
     (testing "skipped (SKIP_INTEGRATION_TESTS set)"
       (is true))

     (not (sdk-available?))
     (testing "skipped (Python venv not available)"
       (is true))

     :else
     (try
       ~@body
       (catch Exception e#
         (if (auth-error? e#)
           (testing "skipped (Claude authentication unavailable)"
             (is true))
           (throw e#))))))

;;; SDK API Wrappers
;;
;; These functions wrap the dynamically-loaded SDK namespace.

(defn- sdk-create-client [opts]
  ((resolve 'task-conductor.claude-agent-sdk.interface/create-client) opts))

(defn- sdk-connect [client]
  ((resolve 'task-conductor.claude-agent-sdk.interface/connect) client))

(defn- sdk-disconnect [client]
  ((resolve 'task-conductor.claude-agent-sdk.interface/disconnect) client))

(defn- sdk-query [client prompt]
  ((resolve 'task-conductor.claude-agent-sdk.interface/query) client prompt))

(defn- sdk-resume-client [session-id opts]
  ((resolve 'task-conductor.claude-agent-sdk.interface/resume-client) session-id opts))

(defn- sdk-fork-client [session-id opts]
  ((resolve 'task-conductor.claude-agent-sdk.interface/fork-client) session-id opts))

(defn- sdk-session-query [client prompt]
  ((resolve 'task-conductor.claude-agent-sdk.interface/session-query) client prompt))

(defn- sdk-make-tracked-client [client]
  ((resolve 'task-conductor.claude-agent-sdk.interface/make-tracked-client) client))

(defn- sdk-get-session-id [tracked-client]
  ((resolve 'task-conductor.claude-agent-sdk.interface/get-session-id) tracked-client))

;;; Tests

(deftest live-query-test
  ;; Tests full query lifecycle with live Claude API.
  ;; Sends a simple prompt and verifies response structure.
  (with-auth-skip
    (testing "live query"
      (let [client (sdk-create-client {:max-turns 1
                                       :permission-mode "bypassPermissions"})]
        (try
          (sdk-connect client)

          (testing "returns response with messages and session-id"
            (let [result (sdk-query client "Reply with just: OK")]
              (is (map? result)
                  "should return a map")
              (is (vector? (:messages result))
                  "should have :messages vector")
              (is (seq (:messages result))
                  "should have at least one message")
              (is (string? (:session-id result))
                  "should have session-id string")))

          (finally
            (sdk-disconnect client)))))))

(deftest live-with-session-test
  ;; Tests with-session macro with live Claude API.
  ;; Uses tracked client directly since macro can't be resolved dynamically.
  (with-auth-skip
    (testing "with-session"
      (testing "manages session lifecycle and captures session-id"
        (let [client (sdk-create-client {:max-turns 1
                                         :permission-mode "bypassPermissions"})
              tracked (sdk-make-tracked-client client)]
          (try
            (sdk-connect client)
            (let [result (sdk-session-query tracked "Reply: OK")
                  session-id (sdk-get-session-id tracked)]
              (is (map? result)
                  "should return a map")
              (is (string? session-id)
                  "should capture session-id"))
            (finally
              (sdk-disconnect client))))))))

(deftest live-message-parsing-test
  ;; Tests that live responses are correctly parsed into Clojure data.
  ;; Note: SDK returns received messages (system, assistant, result),
  ;; not the sent user message.
  (with-auth-skip
    (testing "message parsing"
      (let [client (sdk-create-client {:max-turns 1
                                       :permission-mode "bypassPermissions"})]
        (try
          (sdk-connect client)
          (let [result (sdk-query client "Reply: test")
                messages (:messages result)]

            (testing "includes system message (init)"
              (let [sys-msgs (filter #(= :system-message (:type %)) messages)]
                (is (seq sys-msgs)
                    "should have system message")))

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
            (sdk-disconnect client)))))))

(deftest live-session-resumption-test
  ;; Tests that resume-client can continue a previous session.
  ;; Establishes a session, captures its ID, then resumes it.
  (with-auth-skip
    (testing "session resumption"
      (let [client (sdk-create-client {:max-turns 1
                                       :permission-mode "bypassPermissions"})
            tracked (sdk-make-tracked-client client)
            _ (sdk-connect client)
            _ (sdk-session-query tracked "Remember: CODE=42")
            session-id (sdk-get-session-id tracked)
            _ (sdk-disconnect client)]

        (testing "captures session-id from initial session"
          (is (string? session-id)
              "should have session-id"))

        (testing "resumed client can query"
          (let [resumed-client (sdk-resume-client session-id
                                                  {:max-turns 1
                                                   :permission-mode "bypassPermissions"})]
            (try
              (sdk-connect resumed-client)
              (let [result (sdk-query resumed-client "What was CODE?")
                    messages (:messages result)]
                (is (vector? messages)
                    "should return messages")
                (is (seq messages)
                    "should have at least one message"))
              (finally
                (sdk-disconnect resumed-client)))))))))

(deftest live-session-fork-test
  ;; Tests that fork-client creates a new session branching from an existing one.
  (with-auth-skip
    (testing "session forking"
      (let [client (sdk-create-client {:max-turns 1
                                       :permission-mode "bypassPermissions"})
            tracked (sdk-make-tracked-client client)
            _ (sdk-connect client)
            _ (sdk-session-query tracked "Remember: SECRET=xyz")
            original-session-id (sdk-get-session-id tracked)
            _ (sdk-disconnect client)]

        (testing "captures session-id from original session"
          (is (string? original-session-id)
              "should have session-id"))

        (testing "forked client can query"
          (let [forked-client (sdk-fork-client original-session-id
                                               {:max-turns 1
                                                :permission-mode "bypassPermissions"})]
            (try
              (sdk-connect forked-client)
              (let [result (sdk-query forked-client "What was SECRET?")
                    new-session-id (:session-id result)]
                (is (vector? (:messages result))
                    "should return messages")
                (is (string? new-session-id)
                    "forked session should have its own session-id")
                (is (not= original-session-id new-session-id)
                    "forked session-id should differ from original"))
              (finally
                (sdk-disconnect forked-client)))))))))
