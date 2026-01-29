(ns task-conductor.claude-agent-sdk.core-test
  ;; Tests Python interop initialization and type conversion.
  ;; Verifies libpython-clj2 integration with claude-agent-sdk.
  ;;
  ;; Python initialization is lazy - it only happens when tests run,
  ;; not at namespace load time. This allows other tests to run even
  ;; when the Python venv is unavailable.
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

(defn- venv-exists?
  "Check if the Python venv exists."
  []
  (.exists (io/file (abs-venv-python))))

;;; Lazy Initialization
;;
;; Python and SDK are initialized on first use via delay.
;; This prevents namespace load failures when venv is missing.

(def ^:private initialization
  "Delay that initializes Python and SDK on first deref.
   Returns {:init-result <bool> :sdk <ns> :core <ns> :py <ns>} on success,
   or throws if venv is missing."
  (delay
    (when-not (venv-exists?)
      (throw (ex-info "Python venv not found - skipping SDK tests"
                      {:type :venv-missing
                       :path (abs-venv-python)})))
    ;; Initialize Python with venv
    (require 'libpython-clj2.python)
    ((resolve 'libpython-clj2.python/initialize!)
     :python-executable (abs-venv-python))
    ;; Now require SDK (which will find Python already initialized)
    (require '[task-conductor.claude-agent-sdk.interface :as sdk])
    (require '[task-conductor.claude-agent-sdk.core :as core])
    ;; Complete initialization (imports modules)
    (let [init-fn (resolve 'task-conductor.claude-agent-sdk.interface/initialize!)
          init-result (init-fn {:venv-path venv-path})]
      {:init-result init-result
       :sdk (find-ns 'task-conductor.claude-agent-sdk.interface)
       :core (find-ns 'task-conductor.claude-agent-sdk.core)
       :py (find-ns 'libpython-clj2.python)})))

(defn ensure-initialized!
  "Ensure Python and SDK are initialized. Call at start of each test.
   Returns the initialization map or throws if venv missing."
  []
  @initialization)

(defn- sdk
  "Get a var from the SDK interface namespace."
  [sym]
  (ensure-initialized!)
  (var-get (resolve (symbol "task-conductor.claude-agent-sdk.interface" (name sym)))))

(defn- core
  "Get a var from the SDK core namespace."
  [sym]
  (ensure-initialized!)
  (var-get (resolve (symbol "task-conductor.claude-agent-sdk.core" (name sym)))))

(defn- py
  "Get a var from libpython-clj2.python namespace."
  [sym]
  (ensure-initialized!)
  (var-get (resolve (symbol "libpython-clj2.python" (name sym)))))

(defn- py-attr
  "Get Python object attribute (equivalent to py.-)."
  [obj attr]
  ((py 'get-attr) obj (name attr)))

(defn- py-call
  "Call Python method (equivalent to py.)."
  [obj method & args]
  (apply (py 'call-attr) obj (name method) args))

(defn- sdk-unavailable-error?
  "Check if exception indicates SDK is unavailable (venv missing or import failed)."
  [e]
  (or (= :venv-missing (:type (ex-data e)))
      (and (instance? Exception e)
           (re-find #"ModuleNotFoundError" (str e)))))

(defmacro with-sdk
  "Execute body with SDK initialized, or skip test if SDK unavailable.
   Skips on venv missing or Python module import failure."
  [& body]
  `(try
     (ensure-initialized!)
     ~@body
     (catch Exception e#
       (if (sdk-unavailable-error? e#)
         (testing "skipped (SDK not available)"
           (is true))
         (throw e#)))))

(defmacro with-dynamic-redefs
  "Like with-redefs but resolves vars at runtime instead of compile time.
   Takes a vector of [sym value] pairs where sym is a fully-qualified symbol.
   This allows redefining vars from namespaces that may not exist at compile time."
  [bindings & body]
  (let [binding-pairs (partition 2 bindings)
        var-syms (mapv first binding-pairs)
        values (mapv second binding-pairs)]
    `(let [vars# (mapv resolve '~var-syms)
           originals# (mapv deref vars#)
           new-vals# ~(vec values)]
       (try
         (doseq [[v# new-val#] (map vector vars# new-vals#)]
           (alter-var-root v# (constantly new-val#)))
         ~@body
         (finally
           (doseq [[v# orig#] (map vector vars# originals#)]
             (alter-var-root v# (constantly orig#))))))))

(deftest initialize!-test
  ;; Verifies Python interpreter initialization with venv support
  ;; and claude_agent_sdk module import.
  ;; Note: The specific return value of initialize! depends on whether
  ;; Python was already initialized in this JVM, so we verify behavior
  ;; rather than specific return values from the test fixture.
  (with-sdk
    (testing "initialize!"
      (testing "initializes successfully via test fixture"
        (ensure-initialized!)
        (is (true? ((sdk 'initialized?)))
            "SDK should report as initialized after ensure-initialized!"))

      (testing "returns nil on subsequent calls (already initialized)"
        (is (nil? ((sdk 'initialize!) {:venv-path venv-path}))
            "should return nil when already initialized"))

      (testing "reports initialized state"
        (is (true? ((sdk 'initialized?)))
            "should report as initialized")))))

(deftest get-sdk-module-test
  ;; Verifies SDK module is accessible after initialization.
  (with-sdk
    (testing "get-sdk-module"
      (testing "returns the claude_agent_sdk module with expected classes"
        (let [sdk-mod ((core 'get-sdk-module))]
          (is (some? sdk-mod)
              "should return a non-nil module")
          (is (some? ((py 'get-attr) sdk-mod "ClaudeSDKClient"))
              "should have ClaudeSDKClient class")
          (is (some? ((py 'get-attr) sdk-mod "ClaudeAgentOptions"))
              "should have ClaudeAgentOptions class"))))))

(deftest py->clj-test
  ;; Verifies Python to Clojure type conversion.
  (with-sdk
    (testing "py->clj"
      (testing "converts nil to nil"
        (is (nil? ((core 'py->clj) nil))))

      (testing "passes through JVM primitives"
        (is (= "hello" ((core 'py->clj) "hello")))
        (is (= 42 ((core 'py->clj) 42)))
        (is (= true ((core 'py->clj) true)))))))

(deftest clj->py-test
  ;; Verifies Clojure to Python type conversion.
  (with-sdk
    (testing "clj->py"
      (testing "converts keywords to strings"
        (is (= "foo" ((core 'clj->py) :foo)))))))

(deftest make-options-test
  ;; Verifies ClaudeAgentOptions construction from Clojure maps.
  (with-sdk
    (testing "make-options"
      (testing "creates options with allowed-tools"
        (let [opts ((sdk 'make-options) {:allowed-tools ["Read" "Write"]})]
          (is (some? opts)
              "should create a non-nil options object")
          (is (= ["Read" "Write"] (vec (py-attr opts :allowed_tools)))
              "should set allowed_tools attribute")))

      (testing "creates options with disallowed-tools"
        (let [opts ((sdk 'make-options) {:disallowed-tools ["Bash"]})]
          (is (= ["Bash"] (vec (py-attr opts :disallowed_tools)))
              "should set disallowed_tools attribute")))

      (testing "creates options with cwd"
        (let [opts ((sdk 'make-options) {:cwd "/tmp"})]
          (is (= "/tmp" (py-attr opts :cwd))
              "should set cwd attribute")))

      (testing "creates options with permission-mode"
        (let [opts ((sdk 'make-options) {:permission-mode "bypassPermissions"})]
          (is (= "bypassPermissions" (py-attr opts :permission_mode))
              "should set permission_mode attribute")))

      (testing "creates options with env"
        (let [opts ((sdk 'make-options) {:env {"MY_VAR" "value"}})]
          (is (= {"MY_VAR" "value"} (into {} (py-attr opts :env)))
              "should set env attribute as dict")))

      (testing "creates options with multiple settings"
        (let [opts ((sdk 'make-options) {:allowed-tools ["Bash"]
                                         :permission-mode "acceptEdits"
                                         :max-turns 10})]
          (is (= ["Bash"] (vec (py-attr opts :allowed_tools)))
              "should set allowed_tools")
          (is (= "acceptEdits" (py-attr opts :permission_mode))
              "should set permission_mode")
          (is (= 10 (py-attr opts :max_turns))
              "should set max_turns")))

      (testing "creates options with comprehensive tool configuration"
        (let [opts ((sdk 'make-options) {:allowed-tools ["Read" "Write" "Edit"]
                                         :disallowed-tools ["Bash"]
                                         :permission-mode "plan"
                                         :cwd "/home/user/project"
                                         :max-turns 5})]
          (is (= ["Read" "Write" "Edit"] (vec (py-attr opts :allowed_tools)))
              "should set allowed_tools")
          (is (= ["Bash"] (vec (py-attr opts :disallowed_tools)))
              "should set disallowed_tools")
          (is (= "plan" (py-attr opts :permission_mode))
              "should set permission_mode")
          (is (= "/home/user/project" (py-attr opts :cwd))
              "should set cwd")
          (is (= 5 (py-attr opts :max_turns))
              "should set max_turns")))

      (testing "throws on unknown option keys"
        (let [ex (try
                   ((sdk 'make-options) {:allowed-tools ["Read"]
                                         :unkown-key "typo"
                                         :another-bad-key 123})
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex)
              "should throw an exception")
          (is (= :invalid-options (:type (ex-data ex)))
              "should have :invalid-options type")
          (is (= #{:unkown-key :another-bad-key} (:unknown-keys (ex-data ex)))
              "should include unknown keys in ex-data"))))))

(deftest create-client-test
  ;; Verifies ClaudeSDKClient instantiation.
  ;; Note: connect/disconnect tests require network access and are in integration tests.
  (with-sdk
    (testing "create-client"
      (testing "creates client without options"
        (let [client ((sdk 'create-client))]
          (is (some? client)
              "should create a non-nil client")
          (is ((core 'managed-client?) client)
              "should return a ManagedClient")
          (is (some? ((sdk 'get-py-client) client))
              "should have underlying Python client")))

      (testing "creates client with options map"
        (let [client ((sdk 'create-client) {:allowed-tools ["Read"]})]
          (is ((core 'managed-client?) client)
              "should return a ManagedClient")
          (let [py-client ((sdk 'get-py-client) client)
                options (py-attr py-client :options)]
            (is (= ["Read"] (vec (py-attr options :allowed_tools)))
                "should pass allowed_tools to Python client"))))

      (testing "creates client with cwd option"
        (let [client ((sdk 'create-client) {:cwd "/tmp"
                                            :permission-mode "default"})
              py-client ((sdk 'get-py-client) client)
              options (py-attr py-client :options)]
          (is (= "/tmp" (py-attr options :cwd))
              "should set cwd option")
          (is (= "default" (py-attr options :permission_mode))
              "should set permission_mode option"))))))

(deftest parse-content-block-test
  ;; Verifies ContentBlock parsing handles nil and unknown types.
  ;; Full parsing tests require live Python objects from SDK responses.
  (with-sdk
    (testing "parse-content-block"
      (testing "returns nil for nil input"
        (is (nil? ((sdk 'parse-content-block) nil)))))))

(deftest parse-message-test
  ;; Verifies Message parsing handles nil.
  ;; Full parsing tests require live Python objects from SDK responses.
  (with-sdk
    (testing "parse-message"
      (testing "returns nil for nil input"
        (is (nil? ((sdk 'parse-message) nil)))))))

(deftest resume-client-test
  ;; Verifies client creation with resume option.
  (with-sdk
    (testing "resume-client"
      (testing "creates client with session-id only"
        (let [client ((sdk 'resume-client) "test-session-123")]
          (is ((core 'managed-client?) client)
              "should return a ManagedClient")
          (let [py-client ((sdk 'get-py-client) client)
                options (py-attr py-client :options)]
            (is (= "test-session-123" (py-attr options :resume))
                "should set resume option to session-id"))))

      (testing "creates client with session-id and additional options"
        (let [client ((sdk 'resume-client) "test-session-123"
                                           {:allowed-tools ["Read"]})
              py-client ((sdk 'get-py-client) client)
              options (py-attr py-client :options)]
          (is (= "test-session-123" (py-attr options :resume))
              "should set resume option")
          (is (= ["Read"] (vec (py-attr options :allowed_tools)))
              "should set allowed_tools option"))))))

(deftest fork-client-test
  ;; Verifies client creation with fork-session option.
  (with-sdk
    (testing "fork-client"
      (testing "creates client with session-id only"
        (let [client ((sdk 'fork-client) "test-session-123")]
          (is ((core 'managed-client?) client)
              "should return a ManagedClient")
          (let [py-client ((sdk 'get-py-client) client)
                options (py-attr py-client :options)]
            (is (= "test-session-123" (py-attr options :resume))
                "should set resume option to session-id")
            (is (true? (py-attr options :fork_session))
                "should set fork_session to true"))))

      (testing "creates client with session-id and additional options"
        (let [client ((sdk 'fork-client) "test-session-123"
                                         {:max-turns 5})
              py-client ((sdk 'get-py-client) client)
              options (py-attr py-client :options)]
          (is (= "test-session-123" (py-attr options :resume))
              "should set resume option")
          (is (true? (py-attr options :fork_session))
              "should set fork_session to true")
          (is (= 5 (py-attr options :max_turns))
              "should set max_turns option"))))))

;;; Mocked Unit Tests
;;
;; These tests use with-redefs to mock Python layer interactions,
;; allowing us to test error handling and response parsing logic
;; without live Python/SDK calls.

(deftest connect-mocked-test
  ;; Tests connect error handling with mocked session runner.
  (with-sdk
    (testing "connect"
      (testing "when connection succeeds"
        (testing "returns the client"
          (let [connect-called (atom false)]
            ;; Mock session runner creation to return nil, forcing legacy path
            (with-dynamic-redefs [task-conductor.claude-agent-sdk.core/create-session-runner
                                  (fn [_ _] nil)
                                  task-conductor.claude-agent-sdk.core/run-async
                                  (fn [_coro]
                                    (reset! connect-called true)
                                    nil)]
              (let [client ((sdk 'create-client))
                    result ((sdk 'connect) client)]
                (is (= client result)
                    "should return the same client")
                (is @connect-called
                    "should call run-async"))))))

      (testing "when connection fails"
        (testing "throws ex-info with :connection-error type"
          (with-dynamic-redefs [task-conductor.claude-agent-sdk.core/create-session-runner
                                (fn [_ _] nil)
                                task-conductor.claude-agent-sdk.core/run-async
                                (fn [_coro]
                                  (throw (Exception. "Connection refused")))]
            (let [client ((sdk 'create-client))
                  ex (try
                       ((sdk 'connect) client "hello")
                       nil
                       (catch clojure.lang.ExceptionInfo e e))]
              (is (some? ex)
                  "should throw an exception")
              (is (= :connection-error (:type (ex-data ex)))
                  "should have :connection-error type")
              (is (= "hello" (:prompt (ex-data ex)))
                  "should include prompt in ex-data"))))

        (testing "cleans up session runner on failure"
          (let [close-called (atom false)]
            (with-dynamic-redefs [task-conductor.claude-agent-sdk.core/create-session-runner
                                  (fn [_ _] nil)
                                  task-conductor.claude-agent-sdk.core/close-client
                                  (fn [_client]
                                    (reset! close-called true)
                                    nil)
                                  task-conductor.claude-agent-sdk.core/run-async
                                  (fn [_coro]
                                    (throw (Exception. "Connection refused")))]
              (let [client ((sdk 'create-client))]
                (try
                  ((sdk 'connect) client)
                  (catch Exception _))
                (is @close-called
                    "should call close-client on connection failure")))))))))

(deftest close-client-test
  ;; Tests close-client function for cleaning up session runners.
  (with-sdk
    (testing "close-client"
      (testing "returns nil when client has no session runner"
        (with-dynamic-redefs [task-conductor.claude-agent-sdk.core/create-session-runner
                              (fn [_ _] nil)]
          (let [client ((sdk 'create-client))]
            (is (nil? ((sdk 'close-client) client))
                "should return nil")))))))

(deftest disconnect-mocked-test
  ;; Tests disconnect error handling with mocked session runner.
  (with-sdk
    (testing "disconnect"
      (testing "when disconnection succeeds"
        (testing "returns nil"
          (with-dynamic-redefs [task-conductor.claude-agent-sdk.core/create-session-runner
                                (fn [_ _] nil)
                                task-conductor.claude-agent-sdk.core/run-async
                                (fn [_coro] nil)]
            (let [client ((sdk 'create-client))]
              (is (nil? ((sdk 'disconnect) client))
                  "should return nil")))))

      (testing "when disconnection fails"
        (testing "throws ex-info with :disconnection-error type"
          (with-dynamic-redefs [task-conductor.claude-agent-sdk.core/create-session-runner
                                (fn [_ _] nil)
                                task-conductor.claude-agent-sdk.core/run-async
                                (fn [_coro]
                                  (throw (Exception. "Disconnect failed")))]
            (let [client ((sdk 'create-client))
                  ex (try
                       ((sdk 'disconnect) client)
                       nil
                       (catch clojure.lang.ExceptionInfo e e))]
              (is (some? ex)
                  "should throw an exception")
              (is (= :disconnection-error (:type (ex-data ex)))
                  "should have :disconnection-error type"))))))))

(deftest query-mocked-test
  ;; Tests query function with mocked async operations.
  (with-sdk
    (testing "query"
      (testing "when query fails"
        (testing "throws ex-info with :query-error type"
          (with-dynamic-redefs [task-conductor.claude-agent-sdk.core/create-session-runner
                                (fn [_ _] nil)
                                task-conductor.claude-agent-sdk.core/run-async
                                (fn [_coro]
                                  (throw (Exception. "Query timeout")))]
            (let [client ((sdk 'create-client))
                  ex (try
                       ((sdk 'query) client "test prompt")
                       nil
                       (catch clojure.lang.ExceptionInfo e e))]
              (is (some? ex)
                  "should throw an exception")
              (is (= :query-error (:type (ex-data ex)))
                  "should have :query-error type")
              (is (= "test prompt" (:prompt (ex-data ex)))
                  "should include prompt in ex-data"))))))))

(deftest with-session-mocked-test
  ;; Tests with-session macro flow with mocked operations.
  ;; Note: with-session is a macro, so we use eval to invoke it dynamically.
  (with-sdk
    (testing "with-session"
      (testing "when session succeeds"
        (testing "returns result and session-id"
          (let [connect-called (atom false)
                disconnect-called (atom false)]
            (with-dynamic-redefs [task-conductor.claude-agent-sdk.core/connect
                                  (fn [client & _args]
                                    (reset! connect-called true)
                                    client)
                                  task-conductor.claude-agent-sdk.core/disconnect
                                  (fn [_client]
                                    (reset! disconnect-called true)
                                    nil)]
              (let [result (eval '(task-conductor.claude-agent-sdk.interface/with-session
                                    [client {}]
                                    (reset! (:session-id-atom client) "mock-session")
                                    {:value 42}))]
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
            (with-dynamic-redefs [task-conductor.claude-agent-sdk.core/connect
                                  (fn [client & _args] client)
                                  task-conductor.claude-agent-sdk.core/disconnect
                                  (fn [_client]
                                    (reset! disconnect-called true)
                                    nil)]
              (let [ex (try
                         (eval '(task-conductor.claude-agent-sdk.interface/with-session
                                  [client {}]
                                  (reset! (:session-id-atom client) "error-session")
                                  (throw (Exception. "Body error"))))
                         nil
                         (catch clojure.lang.ExceptionInfo e e))]
                (is (some? ex)
                    "should throw an exception")
                (is (= :session-error (:type (ex-data ex)))
                    "should have :session-error type")
                (is (= "error-session" (:session-id (ex-data ex)))
                    "should include session-id in ex-data")
                (is @disconnect-called
                    "should disconnect even on error")))))))))

(deftest session-query-mocked-test
  ;; Tests session-query updates session-id atom.
  (with-sdk
    (testing "session-query"
      (testing "updates session-id atom from result"
        (let [raw-client ((sdk 'create-client))
              session-atom (atom nil)
              tracked ((resolve 'task-conductor.claude-agent-sdk.core/->TrackedClient)
                       raw-client session-atom)]
          (with-dynamic-redefs [task-conductor.claude-agent-sdk.core/query
                                (fn [_client _prompt _opts]
                                  {:messages [{:type :result-message
                                               :session-id "captured-id"}]
                                   :session-id "captured-id"})]
            (let [result ((sdk 'session-query) tracked "test")]
              (is (= "captured-id" (:session-id result))
                  "should return session-id in result")
              (is (= "captured-id" @session-atom)
                  "should update session-id atom")))))

      (testing "returns query result unchanged"
        (let [raw-client ((sdk 'create-client))
              session-atom (atom nil)
              tracked ((resolve 'task-conductor.claude-agent-sdk.core/->TrackedClient)
                       raw-client session-atom)
              expected-result {:messages [{:type :assistant-message
                                           :content "Hello!"}]
                               :session-id nil}]
          (with-dynamic-redefs [task-conductor.claude-agent-sdk.core/query
                                (fn [_client _prompt _opts] expected-result)]
            (let [result ((sdk 'session-query) tracked "test")]
              (is (= expected-result result)
                  "should return full query result"))))))))

(deftest tracked-client-test
  ;; Verifies TrackedClient record and accessors.
  (with-sdk
    (testing "TrackedClient"
      (let [managed-client ((sdk 'create-client))
            py-client ((sdk 'get-py-client) managed-client)
            session-atom (atom nil)
            tracked ((resolve 'task-conductor.claude-agent-sdk.core/->TrackedClient)
                     managed-client session-atom)]

        (testing "get-raw-client returns the underlying Python client"
          (is (= py-client ((sdk 'get-raw-client) tracked))
              "should return the raw Python client"))

        (testing "get-session-id returns nil initially"
          (is (nil? ((sdk 'get-session-id) tracked))
              "should return nil when no session-id set"))

        (testing "get-session-id returns value after atom update"
          (reset! session-atom "test-session-456")
          (is (= "test-session-456" ((sdk 'get-session-id) tracked))
              "should return the session-id from atom"))))))

(deftest run-async-error-handling-test
  ;; Tests run-async propagates exceptions when coroutine fails.
  ;; The function uses asyncio.run() to execute a coroutine synchronously;
  ;; if the coroutine throws, the exception should propagate to the caller.
  (with-sdk
    (testing "run-async"
      (testing "when coroutine succeeds"
        (testing "returns the coroutine result"
          (let [asyncio ((core 'get-asyncio-module))
                ;; Create a simple coroutine that returns a value
                coro (py-call asyncio :sleep 0)]
            ;; asyncio.sleep(0) returns None, just verify no exception
            (is (nil? ((core 'run-async) coro))
                "should return nil for sleep(0)"))))

      (testing "when coroutine raises exception"
        (testing "propagates the exception to caller"
          ;; Create a coroutine that fails using Python's asyncio
          ;; We'll use a coroutine that raises an exception
          ((py 'run-simple-string)
           "
async def _test_failing_coro():
    raise RuntimeError('test coroutine failure')
")
          (let [main-mod ((py 'import-module) "__main__")
                failing-coro-fn ((py 'get-attr) main-mod "_test_failing_coro")
                coro (failing-coro-fn)]
            (is (thrown-with-msg? Exception #"test coroutine failure"
                                  ((core 'run-async) coro))
                "should propagate exception from failed coroutine")))))))

(deftest collect-async-iterator-test
  ;; Tests collect-async-iterator with mocked async operations.
  ;; Verifies that the function collects items from an async iterator,
  ;; runs the collector coroutine, and converts the result to Clojure.
  (with-sdk
    (testing "collect-async-iterator"
      (testing "collects items from mock async iterator"
        (let [mock-iter :mock-async-iter
              collector-called (atom nil)
              run-async-called (atom nil)
              py->clj-called (atom nil)]
          (with-dynamic-redefs [task-conductor.claude-agent-sdk.core/make-collector-coroutine
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
            (let [result ((core 'collect-async-iterator) mock-iter)]
              (is (= mock-iter @collector-called)
                  "should pass async-iter to make-collector-coroutine")
              (is (= :mock-coroutine @run-async-called)
                  "should pass coroutine to run-async")
              (is (= :raw-py-result @py->clj-called)
                  "should pass run-async result to py->clj")
              (is (= ["item1" "item2" "item3"] result)
                  "should return py->clj converted result")))))

      (testing "returns empty vector for empty iterator"
        (with-dynamic-redefs [task-conductor.claude-agent-sdk.core/make-collector-coroutine
                              (fn [_] :empty-coroutine)
                              task-conductor.claude-agent-sdk.core/run-async
                              (fn [_] :empty-py-list)
                              task-conductor.claude-agent-sdk.core/py->clj
                              (fn [_] [])]
          (let [result ((core 'collect-async-iterator) :empty-iter)]
            (is (= [] result)
                "should return empty vector"))))

      (testing "applies py->clj conversion to results"
        (let [conversion-input (atom nil)]
          (with-dynamic-redefs [task-conductor.claude-agent-sdk.core/make-collector-coroutine
                                (fn [_] :coro)
                                task-conductor.claude-agent-sdk.core/run-async
                                (fn [_] :py-list-of-numbers)
                                task-conductor.claude-agent-sdk.core/py->clj
                                (fn [obj]
                                  (reset! conversion-input obj)
                                  [1 2 3])]
            (let [result ((core 'collect-async-iterator) :iter)]
              (is (= :py-list-of-numbers @conversion-input)
                  "should convert run-async result")
              (is (= [1 2 3] result)
                  "should return converted result")
              (is (vector? result)
                  "result should be a vector"))))))))

;;; Timeout Tests

(deftest connect-timeout-test
  ;; Tests connect timeout parameter handling.
  (with-sdk
    (testing "connect with timeout"
      (testing "when timeout is not specified"
        (testing "passes nil timeout to Python"
          (let [timeout-passed (atom :not-called)]
            (with-dynamic-redefs [task-conductor.claude-agent-sdk.core/create-session-runner
                                  (fn [_ _] nil)
                                  task-conductor.claude-agent-sdk.core/run-async
                                  (fn [_coro]
                                    (reset! timeout-passed nil)
                                    nil)]
              (let [client ((sdk 'create-client))]
                ((sdk 'connect) client)
                (is (nil? @timeout-passed)))))))

      (testing "when timeout-ms is provided"
        (testing "throws ex-info with :timeout type on timeout"
          (with-dynamic-redefs [task-conductor.claude-agent-sdk.core/create-session-runner
                                (fn [_ _] nil)
                                task-conductor.claude-agent-sdk.core/run-async
                                (fn [_coro]
                                  (throw (Exception. "connect operation timed out after 5.0s")))]
            (let [client ((sdk 'create-client))
                  ex (try
                       ((sdk 'connect) client nil {:timeout-ms 5000})
                       nil
                       (catch clojure.lang.ExceptionInfo e e))]
              (is (some? ex)
                  "should throw an exception")
              (is (= :timeout (:type (ex-data ex)))
                  "should have :timeout type")
              (is (= :connect (:operation (ex-data ex)))
                  "should have :connect operation")
              (is (= 5000 (:timeout-ms (ex-data ex)))
                  "should include timeout-ms in ex-data"))))))))

(deftest disconnect-timeout-test
  ;; Tests disconnect timeout parameter handling.
  (with-sdk
    (testing "disconnect with timeout"
      (testing "when timeout-ms is provided"
        (testing "throws ex-info with :timeout type on timeout"
          (with-dynamic-redefs [task-conductor.claude-agent-sdk.core/create-session-runner
                                (fn [_ _] nil)
                                task-conductor.claude-agent-sdk.core/run-async
                                (fn [_coro]
                                  (throw (Exception. "disconnect operation timed out after 3.0s")))]
            (let [client ((sdk 'create-client))
                  ex (try
                       ((sdk 'disconnect) client {:timeout-ms 3000})
                       nil
                       (catch clojure.lang.ExceptionInfo e e))]
              (is (some? ex)
                  "should throw an exception")
              (is (= :timeout (:type (ex-data ex)))
                  "should have :timeout type")
              (is (= :disconnect (:operation (ex-data ex)))
                  "should have :disconnect operation")
              (is (= 3000 (:timeout-ms (ex-data ex)))
                  "should include timeout-ms in ex-data"))))))))

(deftest query-timeout-test
  ;; Tests query timeout parameter handling.
  (with-sdk
    (testing "query with timeout"
      (testing "when timeout-ms is provided"
        (testing "throws ex-info with :timeout type on timeout"
          (with-dynamic-redefs [task-conductor.claude-agent-sdk.core/create-session-runner
                                (fn [_ _] nil)
                                task-conductor.claude-agent-sdk.core/run-async
                                (fn [_coro]
                                  (throw (Exception. "query operation timed out after 60.0s")))]
            (let [client ((sdk 'create-client))
                  ex (try
                       ((sdk 'query) client "test" {:timeout-ms 60000})
                       nil
                       (catch clojure.lang.ExceptionInfo e e))]
              (is (some? ex)
                  "should throw an exception")
              (is (= :timeout (:type (ex-data ex)))
                  "should have :timeout type")
              (is (= :query (:operation (ex-data ex)))
                  "should have :query operation")
              (is (= 60000 (:timeout-ms (ex-data ex)))
                  "should include timeout-ms in ex-data")
              (is (= "test" (:prompt (ex-data ex)))
                  "should include prompt in ex-data"))))))))

(deftest session-query-timeout-test
  ;; Tests session-query timeout parameter handling.
  (with-sdk
    (testing "session-query with timeout"
      (testing "passes timeout to underlying query"
        (let [opts-passed (atom nil)
              raw-client ((sdk 'create-client))
              session-atom (atom nil)
              tracked ((resolve 'task-conductor.claude-agent-sdk.core/->TrackedClient)
                       raw-client session-atom)]
          (with-dynamic-redefs [task-conductor.claude-agent-sdk.core/query
                                (fn [_client _prompt opts]
                                  (reset! opts-passed opts)
                                  {:messages [] :session-id nil})]
            ((sdk 'session-query) tracked "test" {:timeout-ms 10000})
            (is (= {:timeout-ms 10000} @opts-passed)
                "should pass opts to query")))))))
