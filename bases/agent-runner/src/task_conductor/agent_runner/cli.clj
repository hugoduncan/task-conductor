(ns task-conductor.agent-runner.cli
  "CLI integration for Claude Code session management.

   Provides functions to create Claude sessions via the CLI, which ensures
   sessions have Claude Code's system prompt, CLAUDE.md context, and MCP tools."
  (:require
   [babashka.process :as p]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [task-conductor.agent-runner.events :as events]
   [task-conductor.agent-runner.logging])
  (:import
   [java.io BufferedReader]))

(def ^:private default-timeout-ms
  "Default timeout for CLI operations: 5 minutes."
  300000)

;;; Stream JSON Parsing

(defn extract-session-id
  "Extract session-id from a stream-json message.
   Returns the session-id string if this is a system/init message, nil otherwise."
  [msg]
  (when (and (= "system" (:type msg))
             (= "init" (:subtype msg)))
    (:session_id msg)))

(defn parse-stream-json-line
  "Parse a single line from Claude CLI stream-json output.

   Returns the parsed JSON as a map with keyword keys, or nil if:
   - Line is nil, empty, or blank
   - Line doesn't start with '{'
   - JSON parsing fails (logs warning with line content for debugging)"
  [line]
  (when (and line
             (not (str/blank? line))
             (str/starts-with? (str/triml line) "{"))
    (try
      (json/read-str line :key-fn keyword)
      (catch Exception e
        (log/warn {:line line :error (.getMessage e)}
                  "Failed to parse stream-json line")
        nil))))

(defn- map-content-block
  "Convert a stream-json content block to an event map.
   Returns nil for unrecognized block types."
  [block]
  (case (:type block)
    "text" {:type :text-block :text (:text block)}
    "tool_use" {:type :tool-use-block
                :id (:id block)
                :name (:name block)
                :input (:input block)}
    "tool_result" {:type :tool-result-block
                   :tool_use_id (:tool_use_id block)
                   :content (:content block)}
    "thinking" {:type :thinking-block :thinking (:thinking block)}
    (do
      (log/debug {:block-type (:type block)} "Unrecognized content block type")
      nil)))

(defn map-stream-message-to-events
  "Convert a stream-json message to a vector of event-compatible maps.

   Message type handling:
   - system: Returns [] (session-id extraction handled by extract-session-id)
   - assistant: Returns vector of events, one per content block
   - result: Returns [{:type :result-message :usage <usage>}]
   - other: Returns [] (logs at debug level)

   Content block mappings:
   - {type: \"text\"} -> {:type :text-block :text <text>}
   - {type: \"tool_use\"} -> {:type :tool-use-block :id <id> :name <name> :input <input>}
   - {type: \"tool_result\"} -> {:type :tool-result-block :tool_use_id <id> :content <content>}
   - {type: \"thinking\"} -> {:type :thinking-block :thinking <text>}"
  [msg]
  (case (:type msg)
    "system"
    (do
      (when-not (= "init" (:subtype msg))
        (log/debug {:subtype (:subtype msg)} "Unrecognized system message subtype"))
      [])

    "assistant"
    (into []
          (keep map-content-block)
          (get-in msg [:message :content]))

    "result"
    [{:type :result-message :usage (:usage msg)}]

    (do
      (log/debug {:message-type (:type msg)} "Unrecognized stream message type")
      [])))

(defn- process-stream-line!
  "Process a single line from stream-json output.

   Parses the line, extracts session-id from init messages, tracks result
   messages, and calls event-callback for each event.

   When session-id is discovered from an init message, calls event-callback
   with {:type :session-id-update :session-id <id>} to allow callers to
   synchronize their session-id tracking.

   Arguments:
   - line: The raw line string
   - session-id-atom: Atom to store session-id when found
   - result-atom: Atom to store result message when found
   - event-callback: Optional function to call with each event map"
  [line session-id-atom result-atom event-callback]
  (when-let [parsed (parse-stream-json-line line)]
    ;; Extract session-id if this is an init message
    (when-let [session-id (extract-session-id parsed)]
      (reset! session-id-atom session-id)
      (when event-callback
        (event-callback {:type :session-id-update
                         :session-id session-id})))
    ;; Map to events and process
    (let [events (map-stream-message-to-events parsed)]
      (when (some #(= :result-message (:type %)) events)
        (reset! result-atom parsed))
      (when event-callback
        (doseq [event events]
          (event-callback event))))))

(defn- stream-reader
  "Read lines from an InputStream, printing with prefix.

   When processing stdout with event capture enabled, also parses each line
   as stream-json and emits events via the callback.

   Arguments:
   - input-stream: The InputStream to read from
   - prefix: String prefix for printed output (e.g., \"[claude]\")
   - opts: Map with optional keys:
     - :session-id-atom - Atom to store session-id when found
     - :result-atom - Atom to store result message when found
     - :event-callback - Function to call with each event map"
  [input-stream prefix opts]
  (let [{:keys [session-id-atom result-atom event-callback]} opts]
    (with-open [reader (BufferedReader. (io/reader input-stream))]
      (loop []
        (when-let [line (.readLine reader)]
          (println prefix line)
          ;; Process for events if atoms are provided
          (when session-id-atom
            (process-stream-line! line session-id-atom result-atom event-callback))
          (recur))))))

(defn create-session-via-cli
  "Create a new Claude session via the CLI.

   Spawns `claude --print --verbose --output-format stream-json` to create a
   session with Claude Code's full context (system prompt, CLAUDE.md, MCP tools).

   Uses stream-json format to capture events in real-time during execution.

   Options:
   - :working-dir - directory to run claude from (required)
   - :prompt - initial prompt to send (required)
   - :timeout-ms - timeout in milliseconds (default: 300000)
   - :event-callback - function receiving event maps during execution (optional)

   Returns map on success:
   - :session-id - the session identifier for resumption
   - :result - the final result message from the CLI

   Throws ex-info on failure with :type key:
   - :cli/timeout - process exceeded timeout
   - :cli/non-zero-exit - process exited with non-zero code
   - :cli/missing-session-id - no session_id found in stream output"
  [{:keys [working-dir prompt timeout-ms event-callback]}]
  (let [timeout (or timeout-ms default-timeout-ms)
        cmd ["claude" "--print" "--verbose" "--output-format" "stream-json"
             "-p" prompt]]
    (println "[create-session-via-cli] Launching CLI")
    (println "[create-session-via-cli]   working-dir:" working-dir)
    (println "[create-session-via-cli]   timeout-ms:" timeout)
    (println "[create-session-via-cli]   prompt:" (subs prompt 0 (min 100 (count prompt))))
    (println "[create-session-via-cli]   cmd:" (pr-str cmd))
    (let [session-id-atom (atom nil)
          result-atom (atom nil)
          proc (p/process {:dir working-dir
                           :in ""
                           :out :stream
                           :err :stream}
                          "claude" "--print" "--verbose" "--output-format" "stream-json"
                          "-p" prompt)
          stdout-thread (future
                          (stream-reader (:out proc) "[claude]"
                                         {:session-id-atom session-id-atom
                                          :result-atom result-atom
                                          :event-callback event-callback}))
          stderr-thread (future
                          (stream-reader (:err proc) "[claude:err]" {}))]
      (println "[create-session-via-cli] Process started, pid:" (.pid (:proc proc)))
      (println "[create-session-via-cli] Waiting for completion (timeout:" timeout "ms)...")
      (let [proc-result (deref proc timeout ::timeout)]
        (println "[create-session-via-cli] Deref returned:"
                 (if (= proc-result ::timeout) "TIMEOUT" "completed"))
        (when (= proc-result ::timeout)
          (println "[create-session-via-cli] TIMEOUT - destroying process tree")
          (p/destroy-tree proc)
          ;; Wait briefly for reader threads to finish with remaining output
          (deref stdout-thread 1000 nil)
          (deref stderr-thread 1000 nil)
          (throw (ex-info "CLI process timed out"
                          {:type :cli/timeout
                           :timeout-ms timeout
                           :session-id @session-id-atom
                           :working-dir working-dir})))
        ;; Wait for reader threads to complete
        @stdout-thread
        @stderr-thread
        (let [{:keys [exit]} proc-result
              session-id @session-id-atom
              result @result-atom]
          (println "[create-session-via-cli] Exit code:" exit)
          (println "[create-session-via-cli] Session ID:" session-id)
          (when-not (zero? exit)
            (throw (ex-info "CLI process exited with non-zero code"
                            {:type :cli/non-zero-exit
                             :exit-code exit
                             :session-id session-id
                             :working-dir working-dir})))
          (when-not session-id
            (throw (ex-info "CLI stream missing session_id"
                            {:type :cli/missing-session-id
                             :working-dir working-dir})))
          ;; Flush events to persistent storage
          (when session-id
            (try
              (let [event-count (events/flush-events! session-id)]
                (println "[create-session-via-cli] Flushed" event-count "events"))
              (catch Exception e
                (log/warn e "Failed to flush events" {:session-id session-id}))))
          {:session-id session-id
           :result result})))))
