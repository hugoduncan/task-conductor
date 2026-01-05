(ns task-conductor.claude-agent-sdk.interface
  "Public interface for Claude Agent SDK component.

   Provides the API for programmatically orchestrating Claude agents."
  (:require
   [task-conductor.claude-agent-sdk.core :as core]))

;;; Initialization

(defn initialize!
  "Initialize Python interpreter and import Claude Agent SDK.

   Options:
   - :venv-path - Path to Python venv directory (e.g., \".venv\")
   - :python-executable - Explicit path to Python executable

   Returns true on success, throws on failure."
  ([]
   (core/initialize!))
  ([opts]
   (core/initialize! opts)))

(defn initialized?
  "Returns true if Python and the Claude SDK have been initialized."
  []
  (core/initialized?))

;;; Type Conversion

(defn py->clj
  "Recursively convert Python objects to Clojure data structures.

   - dict -> map (with keyword keys)
   - list/tuple -> vector
   - None -> nil
   - dataclass -> map
   - str/int/float/bool -> unchanged (auto-converted)
   - other -> unchanged"
  [obj]
  (core/py->clj obj))

(defn clj->py
  "Convert Clojure data structures to Python objects.

   - map -> dict
   - vector/seq -> list
   - keyword -> string
   - nil -> None
   - other -> unchanged"
  [obj]
  (core/clj->py obj))

;;; Async Bridge

(defn run-async
  "Run a Python coroutine synchronously, blocking until complete.

   Uses asyncio.run() to execute the coroutine in a new event loop.
   Returns the result of the coroutine."
  [coroutine]
  (core/run-async coroutine))

(defn collect-async-iterator
  "Collect all items from a Python AsyncIterator into a Clojure vector.

   Blocks until the iterator is exhausted."
  [async-iter]
  (core/collect-async-iterator async-iter))

;;; Options Construction

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
  (core/make-options opts))

;;; Module Access

(defn get-sdk-module
  "Get the claude_agent_sdk Python module.

   Useful for accessing SDK classes directly."
  []
  (core/get-sdk-module))

(defn get-asyncio-module
  "Get the asyncio Python module."
  []
  (core/get-asyncio-module))

;;; Client Lifecycle

(defn create-client
  "Create a ClaudeSDKClient instance.

   Options map is passed to make-options to create ClaudeAgentOptions.
   Key options include:

   Tool configuration:
   - :tools - preset map or tool list
   - :allowed-tools - vector of allowed tool name strings
   - :disallowed-tools - vector of disallowed tool name strings

   Permission and security:
   - :permission-mode - \"default\", \"acceptEdits\", \"plan\", \"bypassPermissions\"

   Working environment:
   - :cwd - working directory path

   Session:
   - :resume - session ID string to resume a previous session
   - :model - model name

   See make-options for the complete list of supported options.

   Returns the Python ClaudeSDKClient instance."
  ([]
   (core/create-client))
  ([opts]
   (core/create-client opts)))

(defn connect
  "Connect the client to establish a session.

   Optionally accepts an initial prompt string. Blocks until connection
   is established.

   Returns the client for chaining."
  ([client]
   (core/connect client))
  ([client prompt]
   (core/connect client prompt)))

(defn disconnect
  "Disconnect the client and close the session.

   Blocks until disconnection is complete. Returns nil."
  [client]
  (core/disconnect client))

;;; Message and ContentBlock Parsing

(defn parse-content-block
  "Parse a Python ContentBlock into a Clojure map with :type discriminator.

   Supported types:
   - TextBlock -> {:type :text-block :text \"...\"}
   - ThinkingBlock -> {:type :thinking-block :thinking \"...\" :signature \"...\"}
   - ToolUseBlock -> {:type :tool-use-block :id \"...\" :name \"...\" :input {...}}
   - ToolResultBlock -> {:type :tool-result-block :tool-use-id \"...\" ...}"
  [block]
  (core/parse-content-block block))

(defn parse-message
  "Parse a Python Message into a Clojure map with :type discriminator.

   Supported types:
   - UserMessage -> {:type :user-message :content ...}
   - AssistantMessage -> {:type :assistant-message :content [...] :model \"...\"}
   - SystemMessage -> {:type :system-message :subtype \"...\" :data {...}}
   - ResultMessage -> {:type :result-message :session-id \"...\" ...}
   - StreamEvent -> {:type :stream-event ...}"
  [msg]
  (core/parse-message msg))

;;; Query

(defn query
  "Send a prompt to a connected client and collect response messages.

   Returns a map with:
   - :messages - vector of parsed Message maps
   - :session-id - session ID from the ResultMessage (nil if not found)

   The client must be connected before calling query.

   Example response:
   {:messages [{:type :user-message :content \"Hello\"}
               {:type :assistant-message :content [...] :model \"claude-sonnet-4-...\"}
               {:type :result-message :session-id \"abc123\" ...}]
    :session-id \"abc123\"}"
  [client prompt]
  (core/query client prompt))

;;; Session Management

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
  `(core/with-session [~client-sym ~opts] ~@body))

(defn session-query
  "Query using a TrackedClient, updating the session-id atom.

   Like query, but captures the session-id for later retrieval.
   Use within with-session macro."
  [tracked-client prompt]
  (core/session-query tracked-client prompt))

(defn get-session-id
  "Get the current session-id from a TrackedClient."
  [tracked-client]
  (core/get-session-id tracked-client))

(defn get-raw-client
  "Get the underlying Python client from a TrackedClient."
  [tracked-client]
  (core/get-raw-client tracked-client))

(defn managed-client?
  "Returns true if x is a ManagedClient."
  [x]
  (core/managed-client? x))

(defn get-py-client
  "Get the underlying Python client from a ManagedClient or return as-is."
  [client]
  (core/get-py-client client))

(defn make-tracked-client
  "Create a TrackedClient wrapping a raw Python client.

   A TrackedClient maintains session-id state across queries when used
   with session-query.

   Arguments:
   - client - a Python ClaudeSDKClient instance (from create-client)
   - session-id-atom - (optional) an atom to track the session-id;
                       defaults to (atom nil)"
  ([client]
   (core/->TrackedClient client (atom nil)))
  ([client session-id-atom]
   (core/->TrackedClient client session-id-atom)))

(defn resume-client
  "Create a client configured to resume a previous session.

   Convenience function that sets the :resume option."
  ([session-id]
   (core/resume-client session-id))
  ([session-id opts]
   (core/resume-client session-id opts)))

(defn fork-client
  "Create a client configured to fork from a previous session.

   Creates a new session branching from the given session-id,
   leaving the original session unchanged."
  ([session-id]
   (core/fork-client session-id))
  ([session-id opts]
   (core/fork-client session-id opts)))
