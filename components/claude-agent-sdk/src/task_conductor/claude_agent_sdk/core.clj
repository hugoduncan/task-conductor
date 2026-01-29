(ns task-conductor.claude-agent-sdk.core
  "Core implementation for Claude Agent SDK integration via libpython-clj.

   Provides functionality to create Claude SDK clients, manage sessions,
   send queries, and handle responses."
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]
   [libpython-clj2.python :as py :refer [py. py.-]]
   [libpython-clj2.require :refer [require-python]]))

;;; State

(defonce ^:private state
  (atom {:initialized? false
         :venv-path nil}))

;;; Python Module References

(defonce ^:private modules
  (atom {}))

;;; Initialization

(defn initialized?
  "Returns true if Python and the Claude SDK have been initialized."
  []
  (:initialized? @state))

(defn- abs-path
  "Convert a path to absolute, resolving relative paths against cwd."
  [path]
  (when path
    (.getAbsolutePath (io/file path))))

(defn- add-venv-to-path!
  "Add venv's site-packages to Python's sys.path if not already present."
  [abs-venv]
  (when abs-venv
    (let [sys-module (py/import-module "sys")
          sys-path #_{:clj-kondo/ignore [:unresolved-symbol]}
          (py/py.- sys-module path)
          venv-lib (io/file abs-venv "lib")
          site-packages (when (.exists venv-lib)
                          (->> (.listFiles venv-lib)
                               (filter #(.isDirectory %))
                               (map #(io/file % "site-packages"))
                               (filter #(.exists %))
                               first))
          site-packages-path (when site-packages (.getAbsolutePath site-packages))
          current-paths (set (py/as-list sys-path))]
      (when (and site-packages-path
                 (not (current-paths site-packages-path)))
        #_{:clj-kondo/ignore [:unresolved-symbol]}
        (py/py. sys-path insert 0 site-packages-path)))))

(defn initialize!
  "Initialize Python interpreter and import Claude Agent SDK.

   Options:
   - :venv-path - Path to Python venv directory (e.g., \".venv\")
   - :python-executable - Explicit path to Python executable

   If Python is already initialized by libpython-clj, adds the venv's
   site-packages to sys.path instead of attempting to reinitialize.

   Returns true on success, throws on failure."
  ([]
   (initialize! {}))
  ([{:keys [venv-path python-executable]}]
   (when-not (initialized?)
     (let [abs-venv (abs-path venv-path)
           py-exe (or python-executable
                      (when abs-venv
                        (str abs-venv "/bin/python")))
           ;; Check if libpython-clj is already initialized
           py-already-initialized? (try
                                     (py/import-module "sys")
                                     true
                                     (catch Exception _ false))]
       (if py-already-initialized?
         ;; Python already initialized - add venv to path instead
         (add-venv-to-path! abs-venv)
         ;; Initialize with specified Python
         (if py-exe
           (py/initialize! :python-executable py-exe)
           (py/initialize!)))

       ;; Import required modules
       (require-python 'asyncio)
       (require-python 'claude_agent_sdk)

       ;; Store module references for later use
       (reset! modules
               {:asyncio (py/import-module "asyncio")
                :sdk (py/import-module "claude_agent_sdk")
                :inspect (py/import-module "inspect")
                :dataclasses (py/import-module "dataclasses")})

       ;; Load Python helper classes from resource file
       (let [helper-code (slurp (io/resource "client_session_runner.py"))]
         (py/run-simple-string helper-code))
       (let [main-module (py/import-module "__main__")]
         (swap! modules assoc
                :session-runner-class (py/get-attr main-module "ClientSessionRunner")
                :collect-async-iter (py/get-attr main-module "_collect_async_iter")
                :query-and-receive (py/get-attr main-module "_query_and_receive")))

       (swap! state assoc
              :initialized? true
              :venv-path abs-venv)
       true))))

(defn- ensure-initialized!
  "Throws if Python is not initialized."
  []
  (when-not (initialized?)
    (throw (ex-info "Python not initialized. Call initialize! first."
                    {:type :not-initialized}))))

;;; Type Conversion Helpers

(defn- py-dict?
  "Returns true if obj is a Python dict."
  [obj]
  (= :dict (py/python-type obj)))

(defn- py-list?
  "Returns true if obj is a Python list or tuple."
  [obj]
  (#{:list :tuple} (py/python-type obj)))

(defn- py-none?
  "Returns true if obj is Python None."
  [obj]
  (= :none-type (py/python-type obj)))

(defn- dataclass?
  "Returns true if obj is a Python dataclass instance."
  [obj]
  (when obj
    (let [dataclasses (:dataclasses @modules)]
      (py. dataclasses is_dataclass obj))))

(defn- dataclass->map
  "Convert a Python dataclass instance to a Clojure map."
  [obj]
  (let [dataclasses (:dataclasses @modules)
        fields (py. dataclasses fields obj)]
    (into {}
          (for [field fields]
            (let [field-name (py.- field name)
                  value (py/get-attr obj field-name)]
              [(keyword field-name) value])))))

(defn py->clj
  "Recursively convert Python objects to Clojure data structures.

   - dict -> map (with keyword keys)
   - list/tuple -> vector
   - None -> nil
   - dataclass -> map
   - str/int/float/bool -> unchanged (auto-converted)
   - other -> unchanged"
  [obj]
  (cond
    (nil? obj) nil
    (py-none? obj) nil
    (py-dict? obj) (into {}
                         (map (fn [[k v]]
                                [(if (string? k) (keyword k) k)
                                 (py->clj v)]))
                         (py/as-map obj))
    (py-list? obj) (mapv py->clj (py/as-list obj))
    (dataclass? obj) (into {}
                           (map (fn [[k v]] [k (py->clj v)]))
                           (dataclass->map obj))
    :else obj))

(defn clj->py
  "Convert Clojure data structures to Python objects.

   - map -> dict
   - vector/seq -> list
   - keyword -> string
   - nil -> None
   - other -> unchanged"
  [obj]
  (cond
    (nil? obj) (py/->python nil)
    (map? obj) (py/->python
                (into {}
                      (map (fn [[k v]]
                             [(if (keyword? k) (name k) k)
                              (clj->py v)]))
                      obj))
    (or (vector? obj) (seq? obj)) (py/->python (mapv clj->py obj))
    (keyword? obj) (name obj)
    :else (py/->python obj)))

;;; Timeout Helpers

(defn- ms->s
  "Convert milliseconds to seconds for Python timeout parameter."
  [timeout-ms]
  (when timeout-ms
    (/ timeout-ms 1000.0)))

;;; Async Bridge

(defn run-async
  "Run a Python coroutine synchronously, blocking until complete.

   Uses asyncio.run() to execute the coroutine in a new event loop.
   Returns the result of the coroutine.

   Note: For client operations, use the session runner instead to maintain
   a consistent async task across connect/query/disconnect calls."
  [coroutine]
  (ensure-initialized!)
  (let [asyncio (:asyncio @modules)]
    (py. asyncio run coroutine)))

(defn- create-session-runner
  "Create a ClientSessionRunner for the given Python client.

   Options:
   - :event-callback - Optional callback function called for each message
                       received during query. Receives the raw Python message."
  ([py-client]
   (create-session-runner py-client nil))
  ([py-client opts]
   (ensure-initialized!)
   (let [runner-class (:session-runner-class @modules)
         event-callback (:event-callback opts)]
     (if event-callback
       (runner-class py-client :event_callback event-callback)
       (runner-class py-client)))))

;;; Managed Client

(defrecord
 ^{:doc "Wraps a Python ClaudeSDKClient with its ClientSessionRunner.

   ManagedClient maintains async task context across connect/query/disconnect
   operations. The session-runner executes all async operations in a single
   background thread, avoiding event loop issues that occur with separate
   asyncio.run() calls.

   Use create-client to construct a ManagedClient (the default).

   Fields:
   - py-client: The raw Python ClaudeSDKClient instance
   - session-runner: A ClientSessionRunner managing the async execution context

   See also: TrackedClient for session ID tracking across queries."}
 ManagedClient
 [py-client session-runner])

(defn managed-client?
  "Returns true if x is a ManagedClient."
  [x]
  (instance? ManagedClient x))

(defn get-py-client
  "Get the underlying Python client from a ManagedClient or return as-is."
  [client]
  (if (managed-client? client)
    (:py-client client)
    client))

(defn get-session-runner
  "Get the session runner from a ManagedClient."
  [client]
  (when (managed-client? client)
    (:session-runner client)))

(defn close-client
  "Close the session runner without disconnecting the client session.

   Call this to clean up resources if create-client succeeded but connect
   failed. If connect succeeded, use disconnect instead."
  [client]
  (when-let [runner (get-session-runner client)]
    (py. runner close))
  nil)

(defn- make-collector-coroutine
  "Create a Python coroutine that collects all items from an async iterator."
  [async-iter]
  (let [collector (:collect-async-iter @modules)]
    (collector async-iter)))

(defn collect-async-iterator
  "Collect all items from a Python AsyncIterator into a Clojure vector.

   Blocks until the iterator is exhausted."
  [async-iter]
  (ensure-initialized!)
  (let [result (run-async (make-collector-coroutine async-iter))]
    (py->clj result)))

;;; ClaudeAgentOptions Construction

(def ^:private option-keys
  "Valid keys for ClaudeAgentOptions.

   See https://github.com/anthropics/claude-agent-sdk-python for full docs.

   Tool configuration:
     :tools :allowed-tools :disallowed-tools :mcp-servers

   Permission and security:
     :permission-mode :can-use-tool :permission-prompt-tool-name :sandbox

   Working environment:
     :cwd :env :add-dirs :settings :setting-sources

   Prompt and model:
     :system-prompt :model :fallback-model :betas :max-thinking-tokens

   Session control:
     :max-turns :max-budget-usd :resume :continue-conversation :fork-session

   Output and streaming:
     :include-partial-messages :output-format :stderr :max-buffer-size

   CLI and infrastructure:
     :cli-path :extra-args :hooks :user :agents :plugins
     :enable-file-checkpointing"
  #{:tools :allowed-tools :system-prompt :mcp-servers :permission-mode
    :continue-conversation :resume :max-turns :max-budget-usd
    :disallowed-tools :model :fallback-model :betas
    :permission-prompt-tool-name :cwd :cli-path
    :settings :add-dirs :env :extra-args :max-buffer-size
    :stderr :can-use-tool :hooks :user :include-partial-messages
    :fork-session :agents :setting-sources :sandbox :plugins
    :max-thinking-tokens :output-format :enable-file-checkpointing})

(defn- clj-key->py-key
  "Convert Clojure keyword to Python parameter name.

   :allowed-tools -> allowed_tools"
  [k]
  (-> (name k)
      (str/replace "-" "_")))

(defn- validate-option-keys
  "Validate that all keys in opts are recognized option keys.
   Throws ex-info if unknown keys are found."
  [opts]
  (let [provided-keys (set (keys opts))
        unknown-keys (set/difference provided-keys option-keys)]
    (when (seq unknown-keys)
      (throw (ex-info (str "Unknown option keys: " (pr-str unknown-keys))
                      {:type :invalid-options
                       :unknown-keys unknown-keys
                       :valid-keys option-keys})))))

(defn make-options
  "Create a ClaudeAgentOptions instance from a Clojure map.

   Supported keys (use kebab-case, will be converted to snake_case):

   Tool configuration:
   - :tools - preset map {:type \"preset\" :preset \"claude_code\"} or list
   - :allowed-tools - vector of allowed tool name strings
   - :disallowed-tools - vector of disallowed tool name strings

   Permission and security:
   - :permission-mode - \"default\", \"acceptEdits\", \"plan\", \"bypassPermissions\"
   - :can-use-tool - callback for custom tool permissions

   Working environment:
   - :cwd - working directory path (string or Path)
   - :env - map of environment variables
   - :add-dirs - vector of additional directory paths

   Prompt and model:
   - :system-prompt - string or preset map
   - :model - model name string
   - :fallback-model - fallback model name

   Session control:
   - :max-turns - max conversation turns
   - :max-budget-usd - max budget in USD
   - :resume - session ID to resume
   - :continue-conversation - boolean
   - :fork-session - boolean

   See ClaudeAgentOptions Python class for complete documentation.

   Throws ex-info if unknown keys are provided.

   Returns a Python ClaudeAgentOptions instance."
  [opts]
  (ensure-initialized!)
  (validate-option-keys opts)
  (let [sdk (:sdk @modules)
        options-class (py/get-attr sdk "ClaudeAgentOptions")
        ;; Convert Clojure map to keyword args (libpython-clj uses :kw val pairs)
        kw-args (mapcat (fn [[k v]]
                          [(keyword (clj-key->py-key k)) (clj->py v)])
                        opts)]
    ;; libpython-clj recognizes keyword-value pairs as Python kwargs
    (apply options-class kw-args)))

;;; Module Access

(defn get-sdk-module
  "Get the claude_agent_sdk Python module.

   Useful for accessing SDK classes directly."
  []
  (ensure-initialized!)
  (:sdk @modules))

(defn get-asyncio-module
  "Get the asyncio Python module."
  []
  (ensure-initialized!)
  (:asyncio @modules))

;;; Client Lifecycle

(defn create-client
  "Create a ClaudeSDKClient instance wrapped in a ManagedClient.

   Options map is passed to make-options to create ClaudeAgentOptions.
   Returns a ManagedClient containing the Python ClaudeSDKClient and
   a session runner for executing async operations in a single task context.

   Additional options (not passed to SDK):
   - :event-callback - Optional callback function called for each message
                       received during query. Receives raw Python message.
                       Use for real-time event capture and monitoring."
  ([]
   (create-client nil))
  ([opts]
   (println "[SDK create-client] Creating client with opts:" (pr-str opts))
   (ensure-initialized!)
   (let [;; Extract our options before passing to SDK
         event-callback (:event-callback opts)
         sdk-opts (dissoc opts :event-callback)
         sdk (:sdk @modules)
         client-class (py/get-attr sdk "ClaudeSDKClient")
         _ (println "[SDK create-client] Got client-class, creating py-client")
         py-client (if (seq sdk-opts)
                     (client-class :options (make-options sdk-opts))
                     (client-class))
         _ (println "[SDK create-client] py-client created, creating session-runner")
         session-runner (create-session-runner py-client {:event-callback event-callback})]
     (println "[SDK create-client] session-runner created, returning ManagedClient")
     (->ManagedClient py-client session-runner))))

(defn connect
  "Connect the client to establish a session.

   Optionally accepts an initial prompt string and/or options map.
   Blocks until connection is established or timeout expires.

   Options:
   - :timeout-ms - timeout in milliseconds (nil = no timeout)

   On connection failure, the session runner is automatically closed to
   prevent resource leaks.

   Returns the client for chaining.
   Throws ex-info with :type :timeout if timeout expires."
  ([client]
   (connect client nil nil))
  ([client prompt-or-opts]
   (if (map? prompt-or-opts)
     (connect client nil prompt-or-opts)
     (connect client prompt-or-opts nil)))
  ([client prompt opts]
   (println "[SDK connect] Connecting client, prompt:" (some-> prompt (subs 0 (min 50 (count prompt)))) "...")
   (ensure-initialized!)
   (let [runner (get-session-runner client)
         timeout-s (ms->s (:timeout-ms opts))]
     (println "[SDK connect] Got runner:" (boolean runner) "timeout-s:" timeout-s)
     (try
       (if runner
         ;; Use the session runner (keeps operations in same async task)
         (do
           (println "[SDK connect] Calling runner.connect")
           (py. runner connect prompt :timeout timeout-s))
         ;; Fallback for raw Python clients (legacy)
         (let [py-client (get-py-client client)]
           (println "[SDK connect] Using legacy raw client connect")
           (run-async (if prompt
                        (py. py-client connect prompt)
                        (py. py-client connect)))))
       (println "[SDK connect] Connection successful")
       client
       (catch Exception e
         (println "[SDK connect] EXCEPTION:" (type e) (ex-message e))
         (.printStackTrace e)
         ;; Clean up session runner to prevent thread leak
         (close-client client)
         (let [msg (ex-message e)]
           (if (and msg (.contains msg "timed out"))
             (throw (ex-info "Connect operation timed out"
                             {:type :timeout
                              :operation :connect
                              :timeout-ms (:timeout-ms opts)
                              :prompt prompt}
                             e))
             (throw (ex-info "Failed to connect client"
                             {:type :connection-error
                              :prompt prompt}
                             e)))))))))

(defn disconnect
  "Disconnect the client and close the session.

   Blocks until disconnection is complete or timeout expires.

   Options:
   - :timeout-ms - timeout in milliseconds (nil = no timeout)

   Returns nil.
   Throws ex-info with :type :timeout if timeout expires."
  ([client]
   (disconnect client nil))
  ([client opts]
   (println "[SDK disconnect] Disconnecting client")
   (ensure-initialized!)
   (let [runner (get-session-runner client)
         timeout-s (ms->s (:timeout-ms opts))]
     (println "[SDK disconnect] Got runner:" (boolean runner) "timeout-s:" timeout-s)
     (try
       (if runner
         ;; Use the session runner (this also stops its background thread)
         (do
           (println "[SDK disconnect] Calling runner.disconnect")
           (py. runner disconnect :timeout timeout-s))
         ;; Fallback for raw Python clients (legacy)
         (let [py-client (get-py-client client)]
           (println "[SDK disconnect] Using legacy raw client disconnect")
           (run-async (py. py-client disconnect))))
       (println "[SDK disconnect] Disconnect successful")
       nil
       (catch Exception e
         (println "[SDK disconnect] EXCEPTION:" (type e) (ex-message e))
         (.printStackTrace e)
         (let [msg (ex-message e)]
           (if (and msg (.contains msg "timed out"))
             (throw (ex-info "Disconnect operation timed out"
                             {:type :timeout
                              :operation :disconnect
                              :timeout-ms (:timeout-ms opts)}
                             e))
             (throw (ex-info "Failed to disconnect client"
                             {:type :disconnection-error}
                             e)))))))))

;;; Data-driven Field Extraction

(defn- get-class-name
  "Get the class name of a Python object.

   Returns nil if the object is nil or if the type cannot be determined."
  [obj]
  (when obj
    (try
      ;; Use obj.__class__.__name__ since py/python-type returns a keyword
      (py.- (py.- obj __class__) __name__)
      (catch Exception _
        nil))))

(defn- extract-field
  "Extract a single field from a Python object based on a field spec.

   Field spec keys:
   - :key - Clojure keyword for the result map
   - :py - Python attribute name (string)
   - :xf - Optional transform function (default: identity)
   - :optional? - If true, skip field when Python value is None (default: false)

   Returns [key value] pair, or nil if optional and None."
  [py-obj {:keys [key py xf optional?] :or {xf identity}}]
  (let [v (py/get-attr py-obj py)]
    (cond
      (and optional? (py-none? v)) nil
      (py-none? v) [key nil]
      :else [key (xf v)])))

(defn- extract-fields
  "Extract multiple fields from a Python object into a Clojure map."
  [py-obj fields]
  (into {}
        (keep #(extract-field py-obj %))
        fields))

(defn- parse-with-spec
  "Parse a Python object using a type spec map.

   spec-map is a map from class name strings to specs.
   Each spec has :type and :fields keys.

   Returns parsed map with :type, or unknown fallback if class not in spec-map."
  [py-obj spec-map unknown-type]
  (when py-obj
    (let [class-name (get-class-name py-obj)]
      (if-let [spec (get spec-map class-name)]
        (assoc (extract-fields py-obj (:fields spec))
               :type (:type spec))
        {:type unknown-type
         :class-name class-name
         :data (py->clj py-obj)}))))

;;; ContentBlock Parsing

(def ^:private content-block-specs
  "Specs for parsing Python ContentBlock types to Clojure maps."
  {"TextBlock"
   {:type :text-block
    :fields [{:key :text :py "text"}]}

   "ThinkingBlock"
   {:type :thinking-block
    :fields [{:key :thinking :py "thinking"}
             {:key :signature :py "signature"}]}

   "ToolUseBlock"
   {:type :tool-use-block
    :fields [{:key :id :py "id"}
             {:key :name :py "name"}
             {:key :input :py "input" :xf py->clj}]}

   "ToolResultBlock"
   {:type :tool-result-block
    :fields [{:key :tool-use-id :py "tool_use_id"}
             {:key :content :py "content" :xf py->clj}
             {:key :is-error :py "is_error" :optional? true}]}})

(defn parse-content-block
  "Parse a Python ContentBlock into a Clojure map with :type discriminator.

   Supported types:
   - TextBlock -> {:type :text-block :text \"...\"}
   - ThinkingBlock -> {:type :thinking-block :thinking \"...\" :signature \"...\"}
   - ToolUseBlock -> {:type :tool-use-block :id \"...\" :name \"...\" :input {...}}
   - ToolResultBlock -> {:type :tool-result-block :tool-use-id \"...\" ...}"
  [block]
  (parse-with-spec block content-block-specs :unknown-block))

(defn- parse-content
  "Parse message content field, handling both string and list of ContentBlocks."
  [content]
  (cond
    (nil? content) nil
    (py-none? content) nil
    (string? content) content
    (py-list? content) (mapv parse-content-block (py/as-list content))
    :else (py->clj content)))

;;; Message Parsing

(def ^:private message-specs
  "Specs for parsing Python Message types to Clojure maps."
  {"UserMessage"
   {:type :user-message
    :fields [{:key :content :py "content" :xf parse-content}
             {:key :uuid :py "uuid" :optional? true}
             {:key :parent-tool-use-id :py "parent_tool_use_id" :optional? true}]}

   "AssistantMessage"
   {:type :assistant-message
    :fields [{:key :content :py "content" :xf parse-content}
             {:key :model :py "model"}
             {:key :parent-tool-use-id :py "parent_tool_use_id" :optional? true}
             {:key :error :py "error" :optional? true}]}

   "SystemMessage"
   {:type :system-message
    :fields [{:key :subtype :py "subtype"}
             {:key :data :py "data" :xf py->clj}]}

   "ResultMessage"
   {:type :result-message
    :fields [{:key :subtype :py "subtype"}
             {:key :duration-ms :py "duration_ms"}
             {:key :duration-api-ms :py "duration_api_ms"}
             {:key :is-error :py "is_error"}
             {:key :num-turns :py "num_turns"}
             {:key :session-id :py "session_id"}
             {:key :total-cost-usd :py "total_cost_usd" :optional? true}
             {:key :usage :py "usage" :optional? true :xf py->clj}
             {:key :result :py "result" :optional? true}
             {:key :structured-output :py "structured_output" :optional? true :xf py->clj}]}

   "StreamEvent"
   {:type :stream-event
    :fields [{:key :uuid :py "uuid"}
             {:key :session-id :py "session_id"}
             {:key :event :py "event" :xf py->clj}
             {:key :parent-tool-use-id :py "parent_tool_use_id" :optional? true}]}})

(defn parse-message
  "Parse a Python Message into a Clojure map with :type discriminator.

   Supported types:
   - UserMessage -> {:type :user-message :content ...}
   - AssistantMessage -> {:type :assistant-message :content [...] :model \"...\"}
   - SystemMessage -> {:type :system-message :subtype \"...\" :data {...}}
   - ResultMessage -> {:type :result-message :session-id \"...\" ...}
   - StreamEvent -> {:type :stream-event ...}"
  [msg]
  (parse-with-spec msg message-specs :unknown-message))

;;; Query

(defn- make-query-and-receive-coroutine
  "Create a Python coroutine that sends a query and collects all response messages."
  [client prompt]
  (let [query-fn (:query-and-receive @modules)]
    (query-fn client prompt)))

(defn query
  "Send a prompt to a connected client and collect response messages.

   Returns a map with:
   - :messages - vector of parsed Message maps
   - :session-id - session ID from the ResultMessage (nil if not found)

   Options:
   - :timeout-ms - timeout in milliseconds (nil = no timeout)

   The client must be connected before calling query.
   Throws ex-info with :type :timeout if timeout expires."
  ([client prompt]
   (query client prompt nil))
  ([client prompt opts]
   (println "[SDK query] Querying with prompt:" (subs prompt 0 (min 50 (count prompt))) "...")
   (ensure-initialized!)
   (let [runner (get-session-runner client)
         timeout-s (ms->s (:timeout-ms opts))]
     (println "[SDK query] Got runner:" (boolean runner) "timeout-s:" timeout-s)
     (try
       (let [_ (println "[SDK query] Calling runner.query or coroutine")
             py-messages (if runner
                           ;; Use session runner's query method
                           (py. runner query prompt :timeout timeout-s)
                           ;; Fallback for raw Python clients (legacy)
                           (let [py-client (get-py-client client)
                                 coroutine (make-query-and-receive-coroutine py-client prompt)]
                             (run-async coroutine)))
             _ (println "[SDK query] Got py-messages, parsing")
             messages (mapv parse-message (py/as-list py-messages))
             ;; Extract session-id from the ResultMessage
             session-id (->> messages
                             (filter #(= :result-message (:type %)))
                             first
                             :session-id)]
         (println "[SDK query] Query complete, session-id:" session-id "message-count:" (count messages))
         {:messages messages
          :session-id session-id})
       (catch Exception e
         (println "[SDK query] EXCEPTION:" (type e) (ex-message e))
         (.printStackTrace e)
         (let [msg (ex-message e)]
           (if (and msg (.contains msg "timed out"))
             (throw (ex-info "Query operation timed out"
                             {:type :timeout
                              :operation :query
                              :timeout-ms (:timeout-ms opts)
                              :prompt prompt}
                             e))
             (throw (ex-info "Failed to query client"
                             {:type :query-error
                              :prompt prompt}
                             e)))))))))

;;; Session Management

(defrecord
 ^{:doc "Wraps a client with session ID tracking for multi-query sessions.

   TrackedClient captures and maintains the session-id from query responses,
   enabling session persistence and resumption. Typically wraps a ManagedClient
   created via create-client.

   Client wrapper hierarchy (innermost to outermost):
   1. Python ClaudeSDKClient - raw SDK client
   2. ManagedClient - adds async session runner
   3. TrackedClient - adds session-id tracking

   Use with-session macro for automatic lifecycle management, or manually:
     (let [managed (create-client opts)
           tracked (->TrackedClient managed (atom nil))]
       (connect managed)
       (session-query tracked \"Hello\")
       (get-session-id tracked))

   Fields:
   - client: The underlying client (typically a ManagedClient)
   - session-id-atom: An atom holding the current session ID (string or nil)

   See also: ManagedClient, with-session, session-query."}
 TrackedClient
 [client session-id-atom])

(defn session-query
  "Query using a TrackedClient, updating the session-id atom.

   Like query, but captures the session-id for later retrieval.
   Use within with-session macro.

   Options:
   - :timeout-ms - timeout in milliseconds (nil = no timeout)"
  ([tracked-client prompt]
   (session-query tracked-client prompt nil))
  ([tracked-client prompt opts]
   (println "[SDK session-query] Starting query, prompt:" (subs prompt 0 (min 50 (count prompt))) "...")
   (let [{:keys [client session-id-atom]} tracked-client
         result (query client prompt opts)]
     (println "[SDK session-query] Query returned, session-id:" (:session-id result))
     (when-let [sid (:session-id result)]
       (reset! session-id-atom sid))
     result)))

(defn get-session-id
  "Get the current session-id from a TrackedClient."
  [tracked-client]
  @(:session-id-atom tracked-client))

(defn get-raw-client
  "Get the underlying Python client from a TrackedClient or ManagedClient."
  [tracked-client]
  (let [client (:client tracked-client)]
    (get-py-client client)))

(defmacro with-session
  "Execute body with a session-managed Claude client.

   Binds client-sym to a TrackedClient. Use session-query to send prompts
   and automatically capture the session-id.

   Returns {:result <body-result> :session-id <captured-id>}.
   On exception, throws ex-info with :session-id in ex-data.

   Example:
     (with-session [client {:model \"claude-sonnet-4-5\"}]
       (session-query client \"Hello!\")
       (session-query client \"Continue...\"))
     ;; => {:result {...} :session-id \"abc123\"}"
  [[client-sym opts] & body]
  `(let [session-id-atom# (atom nil)]
     (println "[SDK with-session] Creating client with opts:" (pr-str ~opts))
     (let [raw-client# (create-client ~opts)]
       (try
         (println "[SDK with-session] Connecting...")
         (connect raw-client#)
         (println "[SDK with-session] Connected, executing body")
         (let [~client-sym (->TrackedClient raw-client# session-id-atom#)
               result# (do ~@body)]
           (println "[SDK with-session] Body complete, session-id:" @session-id-atom#)
           {:result result#
            :session-id @session-id-atom#})
         (catch Exception e#
           (println "[SDK with-session] EXCEPTION in body:" (type e#) (ex-message e#))
           (.printStackTrace e#)
           (throw (ex-info "Session error"
                           {:type :session-error
                            :session-id @session-id-atom#}
                           e#)))
         (finally
           (println "[SDK with-session] Disconnecting...")
           (disconnect raw-client#)
           (println "[SDK with-session] Disconnected"))))))

(defn resume-client
  "Create a client configured to resume a previous session.

   Convenience function that sets the :resume option."
  ([session-id]
   (resume-client session-id {}))
  ([session-id opts]
   (create-client (assoc opts :resume session-id))))

(defn fork-client
  "Create a client configured to fork from a previous session.

   Creates a new session branching from the given session-id,
   leaving the original session unchanged."
  ([session-id]
   (fork-client session-id {}))
  ([session-id opts]
   (create-client (assoc opts :resume session-id :fork-session true))))
