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
   - :allowed-tools - vector of tool name strings
   - :system-prompt - string or preset map
   - :permission-mode - \"default\", \"acceptEdits\", \"plan\", \"bypassPermissions\"
   - :cwd - working directory path
   - :max-turns - max conversation turns
   - :model - model name
   - and more (see ClaudeAgentOptions documentation)

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
   Supported options include:
   - :allowed-tools - vector of tool name strings
   - :system-prompt - custom system prompt string
   - :permission-mode - \"default\", \"acceptEdits\", \"plan\", \"bypassPermissions\"
   - :cwd - working directory path
   - :model - model name
   - :resume - session ID string to resume a previous session

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
