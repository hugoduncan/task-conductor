(ns task-conductor.claude-agent-sdk.core-test
  ;; Tests Python interop initialization and type conversion.
  ;; Verifies libpython-clj2 integration with claude-agent-sdk.
  ;;
  ;; IMPORTANT: We initialize libpython-clj with the venv BEFORE
  ;; requiring our SDK to prevent auto-initialization with system Python.
  ;; Do NOT add SDK requires here - they must be loaded after py/initialize!
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [libpython-clj2.python :as py :refer [py.]]))

(def ^:private venv-path
  "Relative path to venv from project root."
  "components/claude-agent-sdk/.venv")

(defn- abs-venv-python
  "Get absolute path to venv Python executable."
  []
  (str (.getAbsolutePath (io/file venv-path)) "/bin/python"))

;; Initialize Python with venv BEFORE loading our SDK
;; The result is unused but the side effect is required
(defonce ^:private _python-init
  (py/initialize! :python-executable (abs-venv-python)))

;; Now require the SDK (which will find Python already initialized)
(require '[task-conductor.claude-agent-sdk.interface :as sdk])

;; Complete our initialization (imports modules)
(defonce ^:private init-result
  (sdk/initialize! {:venv-path venv-path}))

(deftest initialize!-test
  ;; Verifies Python interpreter initialization with venv support
  ;; and claude_agent_sdk module import.
  ;; Note: initialization happens at namespace load via init-result defonce.
  (testing "initialize!"
    (testing "returns true on first initialization (checked via init-result)"
      (is (true? init-result)
          "should return true on first initialization"))

    (testing "returns nil on subsequent calls (already initialized)"
      (is (nil? (sdk/initialize! {:venv-path venv-path}))
          "should return nil when already initialized"))

    (testing "reports initialized state"
      (is (true? (sdk/initialized?))
          "should report as initialized"))))

(deftest get-sdk-module-test
  ;; Verifies SDK module is accessible after initialization.
  (testing "get-sdk-module"
    (testing "returns the claude_agent_sdk module"
      (let [sdk-mod (sdk/get-sdk-module)]
        (is (some? sdk-mod)
            "should return a non-nil module")))))

(deftest py->clj-test
  ;; Verifies Python to Clojure type conversion.
  (testing "py->clj"
    (testing "converts nil to nil"
      (is (nil? (sdk/py->clj nil))))

    (testing "passes through JVM primitives"
      (is (= "hello" (sdk/py->clj "hello")))
      (is (= 42 (sdk/py->clj 42)))
      (is (= true (sdk/py->clj true))))))

(deftest clj->py-test
  ;; Verifies Clojure to Python type conversion.
  (testing "clj->py"
    (testing "converts keywords to strings"
      (is (= "foo" (sdk/clj->py :foo))))))

(deftest make-options-test
  ;; Verifies ClaudeAgentOptions construction from Clojure maps.
  (testing "make-options"
    (testing "creates options with allowed-tools"
      (let [opts (sdk/make-options {:allowed-tools ["Read" "Write"]})]
        (is (some? opts)
            "should create a non-nil options object")))

    (testing "creates options with disallowed-tools"
      (let [opts (sdk/make-options {:disallowed-tools ["Bash"]})]
        (is (some? opts)
            "should create options with disallowed-tools")))

    (testing "creates options with cwd"
      (let [opts (sdk/make-options {:cwd "/tmp"})]
        (is (some? opts)
            "should create options with cwd set")))

    (testing "creates options with permission-mode"
      (let [opts (sdk/make-options {:permission-mode "bypassPermissions"})]
        (is (some? opts)
            "should create options with permission-mode")))

    (testing "creates options with env"
      (let [opts (sdk/make-options {:env {"MY_VAR" "value"}})]
        (is (some? opts)
            "should create options with env map")))

    (testing "creates options with multiple settings"
      (let [opts (sdk/make-options {:allowed-tools ["Bash"]
                                    :permission-mode "acceptEdits"
                                    :max-turns 10})]
        (is (some? opts)
            "should create options with multiple settings")))

    (testing "creates options with comprehensive tool configuration"
      (let [opts (sdk/make-options {:allowed-tools ["Read" "Write" "Edit"]
                                    :disallowed-tools ["Bash"]
                                    :permission-mode "plan"
                                    :cwd "/home/user/project"
                                    :max-turns 5})]
        (is (some? opts)
            "should create options with full tool configuration")))

    (testing "throws on unknown option keys"
      (let [ex (try
                 (sdk/make-options {:allowed-tools ["Read"]
                                    :unkown-key "typo"
                                    :another-bad-key 123})
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
        (is (some? ex)
            "should throw an exception")
        (is (= :invalid-options (:type (ex-data ex)))
            "should have :invalid-options type")
        (is (= #{:unkown-key :another-bad-key} (:unknown-keys (ex-data ex)))
            "should include unknown keys in ex-data")))))

(deftest create-client-test
  ;; Verifies ClaudeSDKClient instantiation.
  ;; Note: connect/disconnect tests require network access and are in integration tests.
  (testing "create-client"
    (testing "creates client without options"
      (let [client (sdk/create-client)]
        (is (some? client)
            "should create a non-nil client")))

    (testing "creates client with options map"
      (let [client (sdk/create-client {:allowed-tools ["Read"]})]
        (is (some? client)
            "should create client with options")))

    (testing "creates client with cwd option"
      (let [client (sdk/create-client {:cwd "/tmp"
                                       :permission-mode "default"})]
        (is (some? client)
            "should create client with cwd and permission-mode")))))

(deftest parse-content-block-test
  ;; Verifies ContentBlock parsing handles nil and unknown types.
  ;; Full parsing tests require live Python objects from SDK responses.
  (testing "parse-content-block"
    (testing "returns nil for nil input"
      (is (nil? (sdk/parse-content-block nil))))))

(deftest parse-message-test
  ;; Verifies Message parsing handles nil.
  ;; Full parsing tests require live Python objects from SDK responses.
  (testing "parse-message"
    (testing "returns nil for nil input"
      (is (nil? (sdk/parse-message nil))))))

(deftest resume-client-test
  ;; Verifies client creation with resume option.
  (testing "resume-client"
    (testing "creates client with session-id only"
      (let [client (sdk/resume-client "test-session-123")]
        (is (some? client)
            "should create a non-nil client")))

    (testing "creates client with session-id and additional options"
      (let [client (sdk/resume-client "test-session-123"
                                      {:allowed-tools ["Read"]})]
        (is (some? client)
            "should create client with resume and options")))))

(deftest fork-client-test
  ;; Verifies client creation with fork-session option.
  (testing "fork-client"
    (testing "creates client with session-id only"
      (let [client (sdk/fork-client "test-session-123")]
        (is (some? client)
            "should create a non-nil client")))

    (testing "creates client with session-id and additional options"
      (let [client (sdk/fork-client "test-session-123"
                                    {:max-turns 5})]
        (is (some? client)
            "should create client with fork and options")))))

;;; Mocked Unit Tests
;;
;; These tests use with-redefs to mock Python layer interactions,
;; allowing us to test error handling and response parsing logic
;; without live Python/SDK calls.

(deftest connect-mocked-test
  ;; Tests connect error handling with mocked session runner.
  (testing "connect"
    (testing "when connection succeeds"
      (testing "returns the client"
        (let [connect-called (atom false)]
          ;; Mock session runner creation to return nil, forcing legacy path
          (with-redefs [task-conductor.claude-agent-sdk.core/create-session-runner
                        (fn [_] nil)
                        task-conductor.claude-agent-sdk.core/run-async
                        (fn [_coro]
                          (reset! connect-called true)
                          nil)]
            (let [client (sdk/create-client)
                  result (sdk/connect client)]
              (is (= client result)
                  "should return the same client")
              (is @connect-called
                  "should call run-async"))))))

    (testing "when connection fails"
      (testing "throws ex-info with :connection-error type"
        (with-redefs [task-conductor.claude-agent-sdk.core/create-session-runner
                      (fn [_] nil)
                      task-conductor.claude-agent-sdk.core/run-async
                      (fn [_coro]
                        (throw (Exception. "Connection refused")))]
          (let [client (sdk/create-client)
                ex (try
                     (sdk/connect client "hello")
                     nil
                     (catch clojure.lang.ExceptionInfo e e))]
            (is (some? ex)
                "should throw an exception")
            (is (= :connection-error (:type (ex-data ex)))
                "should have :connection-error type")
            (is (= "hello" (:prompt (ex-data ex)))
                "should include prompt in ex-data")))))))

(deftest disconnect-mocked-test
  ;; Tests disconnect error handling with mocked session runner.
  (testing "disconnect"
    (testing "when disconnection succeeds"
      (testing "returns nil"
        (with-redefs [task-conductor.claude-agent-sdk.core/create-session-runner
                      (fn [_] nil)
                      task-conductor.claude-agent-sdk.core/run-async
                      (fn [_coro] nil)]
          (let [client (sdk/create-client)]
            (is (nil? (sdk/disconnect client))
                "should return nil")))))

    (testing "when disconnection fails"
      (testing "throws ex-info with :disconnection-error type"
        (with-redefs [task-conductor.claude-agent-sdk.core/create-session-runner
                      (fn [_] nil)
                      task-conductor.claude-agent-sdk.core/run-async
                      (fn [_coro]
                        (throw (Exception. "Disconnect failed")))]
          (let [client (sdk/create-client)
                ex (try
                     (sdk/disconnect client)
                     nil
                     (catch clojure.lang.ExceptionInfo e e))]
            (is (some? ex)
                "should throw an exception")
            (is (= :disconnection-error (:type (ex-data ex)))
                "should have :disconnection-error type")))))))

(deftest query-mocked-test
  ;; Tests query function with mocked async operations.
  (testing "query"
    (testing "when query fails"
      (testing "throws ex-info with :query-error type"
        (with-redefs [task-conductor.claude-agent-sdk.core/create-session-runner
                      (fn [_] nil)
                      task-conductor.claude-agent-sdk.core/run-async
                      (fn [_coro]
                        (throw (Exception. "Query timeout")))]
          (let [client (sdk/create-client)
                ex (try
                     (sdk/query client "test prompt")
                     nil
                     (catch clojure.lang.ExceptionInfo e e))]
            (is (some? ex)
                "should throw an exception")
            (is (= :query-error (:type (ex-data ex)))
                "should have :query-error type")
            (is (= "test prompt" (:prompt (ex-data ex)))
                "should include prompt in ex-data")))))))

(deftest with-session-mocked-test
  ;; Tests with-session macro flow with mocked operations.
  (testing "with-session"
    (testing "when session succeeds"
      (testing "returns result and session-id"
        (let [connect-called (atom false)
              disconnect-called (atom false)]
          (with-redefs [task-conductor.claude-agent-sdk.core/connect
                        (fn [client & _args]
                          (reset! connect-called true)
                          client)
                        task-conductor.claude-agent-sdk.core/disconnect
                        (fn [_client]
                          (reset! disconnect-called true)
                          nil)]
            (let [result (sdk/with-session [client {}]
                           ;; Manually update session-id atom
                           (reset! (:session-id-atom client) "mock-session")
                           {:value 42})]
              (is (= {:value 42} (:result result))
                  "should return body result")
              (is (= "mock-session" (:session-id result))
                  "should return session-id")
              (is @connect-called
                  "should call connect")
              (is @disconnect-called
                  "should call disconnect"))))))

    (testing "when body throws exception"
      (testing "includes session-id in ex-data and disconnects"
        (let [disconnect-called (atom false)]
          (with-redefs [task-conductor.claude-agent-sdk.core/connect
                        (fn [client & _args] client)
                        task-conductor.claude-agent-sdk.core/disconnect
                        (fn [_client]
                          (reset! disconnect-called true)
                          nil)]
            (let [ex (try
                       (sdk/with-session [client {}]
                         (reset! (:session-id-atom client) "error-session")
                         (throw (Exception. "Body error")))
                       nil
                       (catch clojure.lang.ExceptionInfo e e))]
              (is (some? ex)
                  "should throw an exception")
              (is (= :session-error (:type (ex-data ex)))
                  "should have :session-error type")
              (is (= "error-session" (:session-id (ex-data ex)))
                  "should include session-id in ex-data")
              (is @disconnect-called
                  "should disconnect even on error"))))))))

(deftest session-query-mocked-test
  ;; Tests session-query updates session-id atom.
  (testing "session-query"
    (testing "updates session-id atom from result"
      (let [raw-client (sdk/create-client)
            session-atom (atom nil)
            tracked ((resolve 'task-conductor.claude-agent-sdk.core/->TrackedClient)
                     raw-client session-atom)]
        (with-redefs [task-conductor.claude-agent-sdk.core/query
                      (fn [_client _prompt]
                        {:messages [{:type :result-message
                                     :session-id "captured-id"}]
                         :session-id "captured-id"})]
          (let [result (sdk/session-query tracked "test")]
            (is (= "captured-id" (:session-id result))
                "should return session-id in result")
            (is (= "captured-id" @session-atom)
                "should update session-id atom")))))

    (testing "returns query result unchanged"
      (let [raw-client (sdk/create-client)
            session-atom (atom nil)
            tracked ((resolve 'task-conductor.claude-agent-sdk.core/->TrackedClient)
                     raw-client session-atom)
            expected-result {:messages [{:type :assistant-message
                                         :content "Hello!"}]
                             :session-id nil}]
        (with-redefs [task-conductor.claude-agent-sdk.core/query
                      (fn [_client _prompt] expected-result)]
          (let [result (sdk/session-query tracked "test")]
            (is (= expected-result result)
                "should return full query result")))))))

(deftest tracked-client-test
  ;; Verifies TrackedClient record and accessors.
  (testing "TrackedClient"
    (let [managed-client (sdk/create-client)
          py-client (sdk/get-py-client managed-client)
          session-atom (atom nil)
          ;; Using resolved var since core is loaded after py/initialize!
          tracked ((resolve 'task-conductor.claude-agent-sdk.core/->TrackedClient)
                   managed-client session-atom)]

      (testing "get-raw-client returns the underlying Python client"
        (is (= py-client (sdk/get-raw-client tracked))
            "should return the raw Python client"))

      (testing "get-session-id returns nil initially"
        (is (nil? (sdk/get-session-id tracked))
            "should return nil when no session-id set"))

      (testing "get-session-id returns value after atom update"
        (reset! session-atom "test-session-456")
        (is (= "test-session-456" (sdk/get-session-id tracked))
            "should return the session-id from atom")))))

(deftest run-async-error-handling-test
  ;; Tests run-async propagates exceptions when coroutine fails.
  ;; The function uses asyncio.run() to execute a coroutine synchronously;
  ;; if the coroutine throws, the exception should propagate to the caller.
  (testing "run-async"
    (testing "when coroutine succeeds"
      (testing "returns the coroutine result"
        (let [asyncio (sdk/get-asyncio-module)
              ;; Create a simple coroutine that returns a value
              coro (py. asyncio sleep 0)]
          ;; asyncio.sleep(0) returns None, just verify no exception
          (is (nil? (sdk/run-async coro))
              "should return nil for sleep(0)"))))

    (testing "when coroutine raises exception"
      (testing "propagates the exception to caller"
        ;; Create a coroutine that fails using Python's asyncio
        ;; We'll use a coroutine that raises an exception
        (py/run-simple-string
         "
async def _test_failing_coro():
    raise RuntimeError('test coroutine failure')
")
        (let [main-mod (py/import-module "__main__")
              failing-coro-fn (py/get-attr main-mod "_test_failing_coro")
              coro (failing-coro-fn)]
          (is (thrown-with-msg? Exception #"test coroutine failure"
                                (sdk/run-async coro))
              "should propagate exception from failed coroutine"))))))

(deftest collect-async-iterator-test
  ;; Tests collect-async-iterator with mocked async operations.
  ;; Verifies that the function collects items from an async iterator,
  ;; runs the collector coroutine, and converts the result to Clojure.
  (testing "collect-async-iterator"
    (testing "collects items from mock async iterator"
      (let [mock-iter :mock-async-iter
            collector-called (atom nil)
            run-async-called (atom nil)
            py->clj-called (atom nil)]
        (with-redefs [task-conductor.claude-agent-sdk.core/make-collector-coroutine
                      (fn [async-iter]
                        (reset! collector-called async-iter)
                        :mock-coroutine)
                      task-conductor.claude-agent-sdk.core/run-async
                      (fn [coro]
                        (reset! run-async-called coro)
                        :raw-py-result)
                      task-conductor.claude-agent-sdk.core/py->clj
                      (fn [obj]
                        (reset! py->clj-called obj)
                        ["item1" "item2" "item3"])]
          (let [result (sdk/collect-async-iterator mock-iter)]
            (is (= mock-iter @collector-called)
                "should pass async-iter to make-collector-coroutine")
            (is (= :mock-coroutine @run-async-called)
                "should pass coroutine to run-async")
            (is (= :raw-py-result @py->clj-called)
                "should pass run-async result to py->clj")
            (is (= ["item1" "item2" "item3"] result)
                "should return py->clj converted result")))))

    (testing "returns empty vector for empty iterator"
      (with-redefs [task-conductor.claude-agent-sdk.core/make-collector-coroutine
                    (fn [_] :empty-coroutine)
                    task-conductor.claude-agent-sdk.core/run-async
                    (fn [_] :empty-py-list)
                    task-conductor.claude-agent-sdk.core/py->clj
                    (fn [_] [])]
        (let [result (sdk/collect-async-iterator :empty-iter)]
          (is (= [] result)
              "should return empty vector"))))

    (testing "applies py->clj conversion to results"
      (let [conversion-input (atom nil)]
        (with-redefs [task-conductor.claude-agent-sdk.core/make-collector-coroutine
                      (fn [_] :coro)
                      task-conductor.claude-agent-sdk.core/run-async
                      (fn [_] :py-list-of-numbers)
                      task-conductor.claude-agent-sdk.core/py->clj
                      (fn [obj]
                        (reset! conversion-input obj)
                        [1 2 3])]
          (let [result (sdk/collect-async-iterator :iter)]
            (is (= :py-list-of-numbers @conversion-input)
                "should convert run-async result")
            (is (= [1 2 3] result)
                "should return converted result")
            (is (vector? result)
                "result should be a vector")))))))
