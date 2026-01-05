(ns task-conductor.claude-agent-sdk.integration-test
  ;; Integration tests for Claude Agent SDK with live API.
  ;; These tests require ANTHROPIC_API_KEY to be set.
  ;; Run with: clj -M:test --focus :integration
  ;;
  ;; IMPORTANT: Initialize libpython-clj with venv BEFORE requiring SDK.
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(def ^:private venv-path
  "Relative path to venv from project root."
  "components/claude-agent-sdk/.venv")

(defn- abs-venv-python
  "Get absolute path to venv Python executable."
  []
  (str (.getAbsolutePath (io/file venv-path)) "/bin/python"))

(defn- api-key-available?
  "Check if ANTHROPIC_API_KEY is set."
  []
  (some? (System/getenv "ANTHROPIC_API_KEY")))

;; Conditional initialization - only if API key is available
(when (api-key-available?)
  (require '[libpython-clj2.python :as py])
  ((resolve 'py/initialize!) :python-executable (abs-venv-python))
  (require '[task-conductor.claude-agent-sdk.interface :as sdk])
  ((resolve 'task-conductor.claude-agent-sdk.interface/initialize!)
   {:venv-path venv-path}))

(defmacro when-api-key
  "Execute body only if ANTHROPIC_API_KEY is available.
   Skips with a passing assertion when API key is missing."
  [& body]
  `(if (api-key-available?)
     (do ~@body)
     (testing "skipped (ANTHROPIC_API_KEY not set)"
       (is true))))

(deftest live-query-test
  ;; Tests full query lifecycle with live Claude API.
  ;; Sends a simple prompt and verifies response structure.
  (when-api-key
   (testing "live query"
     (let [sdk (requiring-resolve 'task-conductor.claude-agent-sdk.interface/create-client)
           connect (requiring-resolve 'task-conductor.claude-agent-sdk.interface/connect)
           query (requiring-resolve 'task-conductor.claude-agent-sdk.interface/query)
           disconnect (requiring-resolve 'task-conductor.claude-agent-sdk.interface/disconnect)
           client (sdk {:max-turns 1
                        :permission-mode "bypassPermissions"})]
       (try
         (connect client)

         (testing "returns response with messages and session-id"
           (let [result (query client "Reply with just: OK")]
             (is (map? result)
                 "should return a map")
             (is (vector? (:messages result))
                 "should have :messages vector")
             (is (seq (:messages result))
                 "should have at least one message")
             (is (string? (:session-id result))
                 "should have session-id string")))

         (finally
           (disconnect client)))))))

(deftest live-with-session-test
  ;; Tests with-session macro with live Claude API.
  (when-api-key
   (testing "with-session"
     (let [with-session-macro (requiring-resolve
                               'task-conductor.claude-agent-sdk.interface/with-session)
           session-query (requiring-resolve
                          'task-conductor.claude-agent-sdk.interface/session-query)]
       (testing "manages session lifecycle and captures session-id"
         (let [result (eval `(~with-session-macro
                              [client# {:max-turns 1
                                        :permission-mode "bypassPermissions"}]
                              (~session-query client# "Reply: OK")))]
           (is (map? result)
               "should return a map")
           (is (some? (:result result))
               "should have :result")
           (is (string? (:session-id result))
               "should capture session-id")))))))

(deftest live-message-parsing-test
  ;; Tests that live responses are correctly parsed into Clojure data.
  (when-api-key
   (testing "message parsing"
     (let [sdk (requiring-resolve 'task-conductor.claude-agent-sdk.interface/create-client)
           connect (requiring-resolve 'task-conductor.claude-agent-sdk.interface/connect)
           query (requiring-resolve 'task-conductor.claude-agent-sdk.interface/query)
           disconnect (requiring-resolve 'task-conductor.claude-agent-sdk.interface/disconnect)
           client (sdk {:max-turns 1
                        :permission-mode "bypassPermissions"})]
       (try
         (connect client)
         (let [result (query client "Reply: test")
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
           (disconnect client)))))))
