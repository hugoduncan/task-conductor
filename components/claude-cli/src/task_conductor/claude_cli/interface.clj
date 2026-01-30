(ns task-conductor.claude-cli.interface
  "Public interface for Claude CLI invocation."
  (:require [task-conductor.claude-cli.core :as core]))

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
    {:exit-code n :events [...]} on success
    {:exit-code nil :error :timeout} on timeout
    {:exit-code nil :error :interrupted :exception e} on interruption"
  [opts]
  (core/invoke-process opts))

(defn cancel!
  "Cancel a running invocation by destroying the process.
  Returns true if the process was running and is now destroyed,
  false if already terminated."
  [handle]
  (core/cancel! handle))
