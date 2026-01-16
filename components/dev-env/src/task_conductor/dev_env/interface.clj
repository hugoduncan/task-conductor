(ns task-conductor.dev-env.interface
  "Protocol for development environment integrations.

   Provides a pluggable abstraction for opening interactive Claude CLI
   sessions in various development environments (Emacs, terminal, etc.).
   Implementations handle environment-specific session management while
   the orchestrator uses a consistent async callback pattern.")

(defprotocol DevEnv
  "Protocol for development environment integrations.

   Implementations manage the lifecycle of interactive Claude CLI sessions
   in a specific environment."

  (open-cli-session [this opts callback]
    "Request opening an interactive CLI session.

     opts map:
       :session-id  - Claude session ID for --resume
       :prompt      - Optional prompt to send after session opens
       :working-dir - Directory to cd before launching

     callback: (fn [result] ...) invoked when session completes.
       result map contains:
         :session-id  - The session ID
         :status      - :completed, :cancelled, or :error
         :hook-status - Hook status map from handoff.edn (if available)
         :exit-code   - CLI exit code

     Returns {:status :requested} immediately (non-blocking).")

  (close-session [this session-id]
    "Request closing a CLI session.

     Signals the environment to close the session identified by session-id.
     Returns {:status :requested}."))
