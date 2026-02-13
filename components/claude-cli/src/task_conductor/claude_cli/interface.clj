(ns task-conductor.claude-cli.interface
  "Public interface for Claude CLI invocation."
  (:require [task-conductor.claude-cli.core :as core]))

;;; Nullable API

(defn make-nullable
  "Create a Nullable claude-cli for testing.

  Returns a nullable instance that can be used with `with-nullable-claude-cli`.
  When bound, `invoke` returns configured responses without spawning processes.

  Config options:
    :exit-code   - Exit code to return (default 0)
    :events      - Events vector to return (default [])
    :session-id  - Session ID to return (default \"test-session\")
    :error       - Error keyword to simulate failure
                   (e.g., :timeout, :interrupted)
                   When set, :exit-code is nil and other fields ignored.

  Access tracked invocations via `invocations` function.

  Example:
    (let [nullable (make-nullable {:exit-code 0
                                   :events [{:type \"result\"}]
                                   :session-id \"abc-123\"})]
      (with-nullable-claude-cli nullable
        (invoke {:prompt \"test\"}))
      (invocations nullable))
    ;=> [{:opts {:prompt \"test\"} :timestamp #inst \"...\"}]"
  ([]
   (make-nullable {}))
  ([config]
   {:config config
    :invocations (atom [])}))

(defmacro with-nullable-claude-cli
  "Execute body with nullable claude-cli bound.

  All calls to `invoke` within body use the nullable instead of
  spawning real processes. Invocations are tracked and can be
  retrieved via `invocations`."
  [nullable & body]
  `(binding [core/*nullable* ~nullable]
     ~@body))

(defn invocations
  "Get invocations recorded by a nullable.

  Returns vector of maps, each with:
    :opts      - The options map passed to invoke
    :timestamp - java.time.Instant when invocation occurred"
  [nullable]
  @(:invocations nullable))

(defn invoke
  "Invoke Claude CLI with the given options.
  Returns {:process p :result-promise promise}.

  Options:
    :prompt            - The prompt to send to Claude (required)
    :dir               - Working directory for the process
    :timeout           - Timeout in milliseconds
    :model             - Model to use (e.g. \"sonnet\")
    :allowed-tools     - Vector of allowed tool names
    :disallowed-tools  - Vector of disallowed tool names
    :max-turns         - Maximum number of turns
    :mcp-config        - Path to MCP config file
    :on-line           - Callback for each raw line (fn [line] ...)
    :on-event          - Callback for each parsed event (fn [event] ...)

  Always includes these CLI flags:
    --verbose --output-format stream-json --print conversation-summary

  The result-promise delivers:
    {:exit-code n :events [...] :session-id \"uuid\"} on success
    {:exit-code nil :error :timeout} on timeout
    {:exit-code nil :error :interrupted :exception e} on interruption

  The :session-id is extracted from Claude CLI's output events. It may be nil
  if the session ended before emitting a session-id (e.g., early error)."
  [opts]
  (core/invoke-process opts))

(defn cancel!
  "Cancel a running invocation by destroying the process.
  Returns true if the process was running and is now destroyed,
  false if already terminated."
  [handle]
  (core/cancel! handle))
