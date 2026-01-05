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
   [libpython-clj2.python :as py]))

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
      (sdk/initialize! {:venv-path venv-path})
      (let [sdk-mod (sdk/get-sdk-module)]
        (is (some? sdk-mod)
            "should return a non-nil module")))))

(deftest py->clj-test
  ;; Verifies Python to Clojure type conversion.
  (testing "py->clj"
    (sdk/initialize! {:venv-path venv-path})

    (testing "converts nil to nil"
      (is (nil? (sdk/py->clj nil))))

    (testing "passes through JVM primitives"
      (is (= "hello" (sdk/py->clj "hello")))
      (is (= 42 (sdk/py->clj 42)))
      (is (= true (sdk/py->clj true))))))

(deftest clj->py-test
  ;; Verifies Clojure to Python type conversion.
  (testing "clj->py"
    (sdk/initialize! {:venv-path venv-path})

    (testing "converts keywords to strings"
      (is (= "foo" (sdk/clj->py :foo))))))

(deftest make-options-test
  ;; Verifies ClaudeAgentOptions construction from Clojure maps.
  (testing "make-options"
    (sdk/initialize! {:venv-path venv-path})

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
            "should create options with full tool configuration")))))

(deftest create-client-test
  ;; Verifies ClaudeSDKClient instantiation.
  ;; Note: connect/disconnect tests require network access and are in integration tests.
  (testing "create-client"
    (sdk/initialize! {:venv-path venv-path})

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
    (sdk/initialize! {:venv-path venv-path})

    (testing "returns nil for nil input"
      (is (nil? (sdk/parse-content-block nil))))))

(deftest parse-message-test
  ;; Verifies Message parsing handles nil.
  ;; Full parsing tests require live Python objects from SDK responses.
  (testing "parse-message"
    (sdk/initialize! {:venv-path venv-path})

    (testing "returns nil for nil input"
      (is (nil? (sdk/parse-message nil))))))

(deftest resume-client-test
  ;; Verifies client creation with resume option.
  (testing "resume-client"
    (sdk/initialize! {:venv-path venv-path})

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
    (sdk/initialize! {:venv-path venv-path})

    (testing "creates client with session-id only"
      (let [client (sdk/fork-client "test-session-123")]
        (is (some? client)
            "should create a non-nil client")))

    (testing "creates client with session-id and additional options"
      (let [client (sdk/fork-client "test-session-123"
                                    {:max-turns 5})]
        (is (some? client)
            "should create client with fork and options")))))

(deftest tracked-client-test
  ;; Verifies TrackedClient record and accessors.
  (testing "TrackedClient"
    (sdk/initialize! {:venv-path venv-path})
    (let [raw-client (sdk/create-client)
          session-atom (atom nil)
          ;; Using resolved var since core is loaded after py/initialize!
          tracked ((resolve 'task-conductor.claude-agent-sdk.core/->TrackedClient)
                   raw-client session-atom)]

      (testing "get-raw-client returns the underlying client"
        (is (= raw-client (sdk/get-raw-client tracked))
            "should return the raw Python client"))

      (testing "get-session-id returns nil initially"
        (is (nil? (sdk/get-session-id tracked))
            "should return nil when no session-id set"))

      (testing "get-session-id returns value after atom update"
        (reset! session-atom "test-session-456")
        (is (= "test-session-456" (sdk/get-session-id tracked))
            "should return the session-id from atom")))))
