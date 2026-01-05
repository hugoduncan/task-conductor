(ns task-conductor.claude-agent-sdk.core
  "Core implementation for Claude Agent SDK integration via libpython-clj.

   Provides functionality to create Claude SDK clients, manage sessions,
   send queries, and handle responses."
  (:require
   [clojure.java.io :as io]
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

(defn initialize!
  "Initialize Python interpreter and import Claude Agent SDK.

   Options:
   - :venv-path - Path to Python venv directory (e.g., \".venv\")
   - :python-executable - Explicit path to Python executable

   Returns true on success, throws on failure."
  ([]
   (initialize! {}))
  ([{:keys [venv-path python-executable]}]
   (when-not (initialized?)
     (let [abs-venv (abs-path venv-path)
           py-exe (or python-executable
                      (when abs-venv
                        (str abs-venv "/bin/python")))]
       (if py-exe
         (py/initialize! :python-executable py-exe)
         (py/initialize!))

       ;; Import required modules
       (require-python 'asyncio)
       (require-python 'claude_agent_sdk)

       ;; Store module references for later use
       (reset! modules
               {:asyncio (py/import-module "asyncio")
                :sdk (py/import-module "claude_agent_sdk")
                :inspect (py/import-module "inspect")
                :dataclasses (py/import-module "dataclasses")})

       ;; Define Python helper classes and coroutines once
       (py/run-simple-string
        "
import asyncio
import threading
import queue

class ClientSessionRunner:
    '''Runs client operations in a single persistent async Task.

    This ensures all async operations run in the same task context,
    which is required by anyio's cancel scope tracking. Commands are
    sent via a thread-safe queue and results returned via futures.
    '''
    def __init__(self, client):
        self.client = client
        self.loop = asyncio.new_event_loop()
        self._command_queue = queue.Queue()
        self._thread = threading.Thread(target=self._run_loop, daemon=True)
        self._running = True
        self._thread.start()

    def _run_loop(self):
        '''Run the event loop in the background thread.'''
        asyncio.set_event_loop(self.loop)
        self.loop.run_until_complete(self._process_commands())

    async def _process_commands(self):
        '''Process commands from the queue in a single async task.'''
        while self._running:
            try:
                # Check for commands with a timeout to allow shutdown
                cmd = await asyncio.get_event_loop().run_in_executor(
                    None, lambda: self._command_queue.get(timeout=0.1))
            except queue.Empty:
                continue

            cmd_type, args, result_future = cmd

            try:
                if cmd_type == 'connect':
                    prompt = args.get('prompt')
                    if prompt:
                        await self.client.connect(prompt)
                    else:
                        await self.client.connect()
                    result_future.set_result(None)

                elif cmd_type == 'query':
                    prompt = args['prompt']
                    await self.client.query(prompt)
                    messages = []
                    async for msg in self.client.receive_response():
                        messages.append(msg)
                    result_future.set_result(messages)

                elif cmd_type == 'disconnect':
                    await self.client.disconnect()
                    self._running = False
                    result_future.set_result(None)

                elif cmd_type == 'shutdown':
                    self._running = False
                    result_future.set_result(None)

            except Exception as e:
                result_future.set_exception(e)

    def _send_command(self, cmd_type, args=None):
        '''Send a command and wait for result.'''
        import concurrent.futures
        result_holder = concurrent.futures.Future()
        self._command_queue.put((cmd_type, args or {}, result_holder))
        return result_holder.result()

    def connect(self, prompt=None):
        '''Connect the client.'''
        return self._send_command('connect', {'prompt': prompt})

    def query(self, prompt):
        '''Send a query and receive response.'''
        return self._send_command('query', {'prompt': prompt})

    def disconnect(self):
        '''Disconnect the client.'''
        return self._send_command('disconnect')

    def close(self):
        '''Shutdown the runner without disconnecting.'''
        self._send_command('shutdown')
        self._thread.join(timeout=5.0)

async def _collect_async_iter(aiter):
    result = []
    async for item in aiter:
        result.append(item)
    return result

async def _query_and_receive(client, prompt):
    await client.query(prompt)
    result = []
    async for msg in client.receive_response():
        result.append(msg)
    return result
")
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
  "Create a ClientSessionRunner for the given Python client."
  [py-client]
  (ensure-initialized!)
  (let [runner-class (:session-runner-class @modules)]
    (runner-class py-client)))

;;; Managed Client
;; Wraps a Python client with its session runner.

(defrecord ManagedClient [py-client session-runner])

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

;; Legacy alias for compatibility
(defn get-loop-runner
  "Get the event loop runner from a ManagedClient (legacy, returns session-runner)."
  [client]
  (when (managed-client? client)
    (:loop-runner client)))

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

   See ClaudeAgentOptions Python class for full documentation.
   Key options:
   - :tools - preset map or tool list
   - :allowed-tools - list of allowed tool name strings
   - :disallowed-tools - list of disallowed tool name strings
   - :permission-mode - \"default\", \"acceptEdits\", \"plan\", or \"bypassPermissions\"
   - :cwd - working directory path"
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

   Returns a Python ClaudeAgentOptions instance."
  [opts]
  (ensure-initialized!)
  (let [sdk (:sdk @modules)
        options-class (py/get-attr sdk "ClaudeAgentOptions")
        ;; Convert Clojure map to keyword args (libpython-clj uses :kw val pairs)
        kw-args (mapcat (fn [[k v]]
                          (when (contains? option-keys k)
                            [(keyword (clj-key->py-key k)) (clj->py v)]))
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
   a session runner for executing async operations in a single task context."
  ([]
   (create-client nil))
  ([opts]
   (ensure-initialized!)
   (let [sdk (:sdk @modules)
         client-class (py/get-attr sdk "ClaudeSDKClient")
         py-client (if opts
                     (client-class :options (make-options opts))
                     (client-class))
         session-runner (create-session-runner py-client)]
     (->ManagedClient py-client session-runner))))

(defn connect
  "Connect the client to establish a session.

   Optionally accepts an initial prompt string. Blocks until connection
   is established.

   Returns the client for chaining."
  ([client]
   (connect client nil))
  ([client prompt]
   (ensure-initialized!)
   (let [runner (get-session-runner client)]
     (try
       (if runner
         ;; Use the session runner (keeps operations in same async task)
         (py. runner connect prompt)
         ;; Fallback for raw Python clients (legacy)
         (let [py-client (get-py-client client)]
           (run-async (if prompt
                        (py. py-client connect prompt)
                        (py. py-client connect)))))
       client
       (catch Exception e
         (throw (ex-info "Failed to connect client"
                         {:type :connection-error
                          :prompt prompt}
                         e)))))))

(defn disconnect
  "Disconnect the client and close the session.

   Blocks until disconnection is complete. Returns nil."
  [client]
  (ensure-initialized!)
  (let [runner (get-session-runner client)]
    (try
      (if runner
        ;; Use the session runner (this also stops its background thread)
        (py. runner disconnect)
        ;; Fallback for raw Python clients (legacy)
        (let [py-client (get-py-client client)]
          (run-async (py. py-client disconnect))))
      nil
      (catch Exception e
        (throw (ex-info "Failed to disconnect client"
                        {:type :disconnection-error}
                        e))))))

;;; ContentBlock Parsing

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

(defn parse-content-block
  "Parse a Python ContentBlock into a Clojure map with :type discriminator.

   Supported types:
   - TextBlock -> {:type :text-block :text \"...\"}
   - ThinkingBlock -> {:type :thinking-block :thinking \"...\" :signature \"...\"}
   - ToolUseBlock -> {:type :tool-use-block :id \"...\" :name \"...\" :input {...}}
   - ToolResultBlock -> {:type :tool-result-block :tool-use-id \"...\" ...}"
  [block]
  (when block
    (let [class-name (get-class-name block)]
      (case class-name
        "TextBlock"
        {:type :text-block
         :text (py.- block text)}

        "ThinkingBlock"
        {:type :thinking-block
         :thinking (py.- block thinking)
         :signature (py.- block signature)}

        "ToolUseBlock"
        {:type :tool-use-block
         :id (py.- block id)
         :name (py.- block name)
         :input (py->clj (py.- block input))}

        "ToolResultBlock"
        (let [content (py.- block content)
              is-error (py.- block is_error)]
          {:type :tool-result-block
           :tool-use-id (py.- block tool_use_id)
           :content (py->clj content)
           :is-error (when-not (py-none? is-error) is-error)})

        ;; Fallback for unknown block types
        {:type :unknown-block
         :class-name class-name
         :data (py->clj block)}))))

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

(defn parse-message
  "Parse a Python Message into a Clojure map with :type discriminator.

   Supported types:
   - UserMessage -> {:type :user-message :content ...}
   - AssistantMessage -> {:type :assistant-message :content [...] :model \"...\"}
   - SystemMessage -> {:type :system-message :subtype \"...\" :data {...}}
   - ResultMessage -> {:type :result-message :session-id \"...\" ...}
   - StreamEvent -> {:type :stream-event ...}"
  [msg]
  (when msg
    (let [class-name (get-class-name msg)]
      (case class-name
        "UserMessage"
        (let [uuid (py.- msg uuid)
              parent-tool-use-id (py.- msg parent_tool_use_id)]
          (cond-> {:type :user-message
                   :content (parse-content (py.- msg content))}
            (and uuid (not (py-none? uuid)))
            (assoc :uuid uuid)
            (and parent-tool-use-id (not (py-none? parent-tool-use-id)))
            (assoc :parent-tool-use-id parent-tool-use-id)))

        "AssistantMessage"
        (let [parent-tool-use-id (py.- msg parent_tool_use_id)
              error (py.- msg error)]
          (cond-> {:type :assistant-message
                   :content (parse-content (py.- msg content))
                   :model (py.- msg model)}
            (and parent-tool-use-id (not (py-none? parent-tool-use-id)))
            (assoc :parent-tool-use-id parent-tool-use-id)
            (and error (not (py-none? error)))
            (assoc :error error)))

        "SystemMessage"
        {:type :system-message
         :subtype (py.- msg subtype)
         :data (py->clj (py.- msg data))}

        "ResultMessage"
        (let [total-cost (py.- msg total_cost_usd)
              usage (py.- msg usage)
              result (py.- msg result)
              structured-output (py.- msg structured_output)]
          (cond-> {:type :result-message
                   :subtype (py.- msg subtype)
                   :duration-ms (py.- msg duration_ms)
                   :duration-api-ms (py.- msg duration_api_ms)
                   :is-error (py.- msg is_error)
                   :num-turns (py.- msg num_turns)
                   :session-id (py.- msg session_id)}
            (and total-cost (not (py-none? total-cost)))
            (assoc :total-cost-usd total-cost)
            (and usage (not (py-none? usage)))
            (assoc :usage (py->clj usage))
            (and result (not (py-none? result)))
            (assoc :result result)
            (and structured-output (not (py-none? structured-output)))
            (assoc :structured-output (py->clj structured-output))))

        "StreamEvent"
        (let [parent-tool-use-id (py.- msg parent_tool_use_id)]
          (cond-> {:type :stream-event
                   :uuid (py.- msg uuid)
                   :session-id (py.- msg session_id)
                   :event (py->clj (py.- msg event))}
            (and parent-tool-use-id (not (py-none? parent-tool-use-id)))
            (assoc :parent-tool-use-id parent-tool-use-id)))

        ;; Fallback for unknown message types
        {:type :unknown-message
         :class-name class-name
         :data (py->clj msg)}))))

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

   The client must be connected before calling query."
  [client prompt]
  (ensure-initialized!)
  (let [runner (get-session-runner client)]
    (try
      (let [py-messages (if runner
                          ;; Use session runner's query method
                          (py. runner query prompt)
                          ;; Fallback for raw Python clients (legacy)
                          (let [py-client (get-py-client client)
                                coroutine (make-query-and-receive-coroutine py-client prompt)]
                            (run-async coroutine)))
            messages (mapv parse-message (py/as-list py-messages))
            ;; Extract session-id from the ResultMessage
            session-id (->> messages
                            (filter #(= :result-message (:type %)))
                            first
                            :session-id)]
        {:messages messages
         :session-id session-id})
      (catch Exception e
        (throw (ex-info "Failed to query client"
                        {:type :query-error
                         :prompt prompt}
                        e))))))

;;; Session Management

(defrecord TrackedClient [client session-id-atom])

(defn session-query
  "Query using a TrackedClient, updating the session-id atom.

   Like query, but captures the session-id for later retrieval.
   Use within with-session macro."
  [tracked-client prompt]
  (let [{:keys [client session-id-atom]} tracked-client
        result (query client prompt)]
    (when-let [sid (:session-id result)]
      (reset! session-id-atom sid))
    result))

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
  `(let [session-id-atom# (atom nil)
         raw-client# (create-client ~opts)]
     (try
       (connect raw-client#)
       (let [~client-sym (->TrackedClient raw-client# session-id-atom#)
             result# (do ~@body)]
         {:result result#
          :session-id @session-id-atom#})
       (catch Exception e#
         (throw (ex-info "Session error"
                         {:type :session-error
                          :session-id @session-id-atom#}
                         e#)))
       (finally
         (disconnect raw-client#)))))

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
