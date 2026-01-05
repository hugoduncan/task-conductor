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
   Returns the result of the coroutine."
  [coroutine]
  (ensure-initialized!)
  (let [asyncio (:asyncio @modules)]
    (py. asyncio run coroutine)))

(defn- make-collector-coroutine
  "Create a Python coroutine that collects all items from an async iterator."
  [async-iter]
  ;; We need to run Python code that collects the async iterator
  ;; since we can't directly await in Clojure
  (py/run-simple-string
   "
async def _collect_async_iter(aiter):
    result = []
    async for item in aiter:
        result.append(item)
    return result
")
  (let [collector (py/get-attr (py/import-module "__main__") "_collect_async_iter")]
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
  "Valid keys for ClaudeAgentOptions."
  #{:allowed-tools :system-prompt :mcp-servers :permission-mode
    :continue-conversation :resume :max-turns :disallowed-tools
    :model :output-format :permission-prompt-tool-name :cwd
    :settings :add-dirs :env :extra-args :max-buffer-size
    :stderr :can-use-tool :hooks :user :include-partial-messages
    :fork-session :agents :setting-sources})

(defn- clj-key->py-key
  "Convert Clojure keyword to Python parameter name.

   :allowed-tools -> allowed_tools"
  [k]
  (-> (name k)
      (str/replace "-" "_")))

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
  "Create a ClaudeSDKClient instance.

   Options map is passed to make-options to create ClaudeAgentOptions.
   Returns the Python ClaudeSDKClient instance."
  ([]
   (create-client nil))
  ([opts]
   (ensure-initialized!)
   (let [sdk (:sdk @modules)
         client-class (py/get-attr sdk "ClaudeSDKClient")]
     (if opts
       (client-class :options (make-options opts))
       (client-class)))))

(defn connect
  "Connect the client to establish a session.

   Optionally accepts an initial prompt string. Blocks until connection
   is established.

   Returns the client for chaining."
  ([client]
   (connect client nil))
  ([client prompt]
   (ensure-initialized!)
   (try
     (run-async (if prompt
                  (py. client connect prompt)
                  (py. client connect)))
     client
     (catch Exception e
       (throw (ex-info "Failed to connect client"
                       {:type :connection-error
                        :prompt prompt}
                       e))))))

(defn disconnect
  "Disconnect the client and close the session.

   Blocks until disconnection is complete. Returns nil."
  [client]
  (ensure-initialized!)
  (try
    (run-async (py. client disconnect))
    nil
    (catch Exception e
      (throw (ex-info "Failed to disconnect client"
                      {:type :disconnection-error}
                      e)))))

;;; ContentBlock Parsing

(defn- get-class-name
  "Get the class name of a Python object."
  [obj]
  (when obj
    (py.- (py/python-type obj) __name__)))

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
  (py/run-simple-string
   "
async def _query_and_receive(client, prompt):
    await client.query(prompt)
    result = []
    async for msg in client.receive_response():
        result.append(msg)
    return result
")
  (let [query-fn (py/get-attr (py/import-module "__main__") "_query_and_receive")]
    (query-fn client prompt)))

(defn query
  "Send a prompt to a connected client and collect response messages.

   Returns a map with:
   - :messages - vector of parsed Message maps
   - :session-id - session ID from the ResultMessage (nil if not found)

   The client must be connected before calling query."
  [client prompt]
  (ensure-initialized!)
  (try
    (let [py-messages (run-async (make-query-and-receive-coroutine client prompt))
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
                      e)))))
