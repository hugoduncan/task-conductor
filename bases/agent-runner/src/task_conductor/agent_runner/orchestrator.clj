(ns task-conductor.agent-runner.orchestrator
  "Orchestrates story task execution with Claude agent sessions.

   This namespace provides the main entry point for executing stories.
   It coordinates task selection from mcp-tasks, creates fresh Claude
   sessions for each task, and manages state transitions.

   Key responsibilities:
   - Query mcp-tasks for the next unblocked child task
   - Build session configuration with MCP server settings
   - Execute tasks in isolated Claude sessions
   - Handle pause/resume and error recovery"
  (:require
   [clojure.edn :as edn]
   [clojure.java.shell :as shell]))

;; NOTE: console and handoff namespaces will be added when implemented
;; (see stories #53 and #54/55)

;;; CLI Integration

(defn run-mcp-tasks
  "Execute mcp-tasks CLI command and return parsed EDN result.

   Runs the mcp-tasks CLI with the given arguments, always requesting
   EDN format output. Returns the parsed result on success.

   Args is a sequence of command arguments (strings).

   Returns parsed EDN data from mcp-tasks output.
   Throws ex-info on non-zero exit or parse failure with details."
  [& args]
  (let [cmd (into ["mcp-tasks"] (concat args ["--format" "edn"]))
        {:keys [exit out err]} (apply shell/sh cmd)]
    (if (zero? exit)
      (try
        (edn/read-string out)
        (catch Exception e
          (throw (ex-info "Failed to parse mcp-tasks output"
                          {:cmd cmd
                           :output out
                           :error (ex-message e)}))))
      (throw (ex-info "mcp-tasks command failed"
                      {:cmd cmd
                       :exit-code exit
                       :stderr err
                       :stdout out})))))

(comment
  ;; Example: Query unblocked children of story 57
  (run-mcp-tasks "list" "--parent-id" "57" "--blocked" "false" "--limit" "1")

  ;; Example: Show a specific task
  (run-mcp-tasks "show" "108"))
