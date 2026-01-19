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

     callback: Optional (fn [result] ...) for reporting open errors.
       Only invoked if session fails to open.
       result map: {:status :error :message \"...\"}

     Session status (idle, completed) is reported via handoff file
     (.task-conductor/handoff.edn), not via callback. The orchestrator
     uses watch-hook-status-file to monitor session state.

     Returns {:status :requested} immediately (non-blocking).")

  (close-session [this session-id]
    "Request closing a CLI session.

     Signals the environment to close the session identified by session-id.
     Returns {:status :requested}.")

  (notify [this message]
    "Send a notification to the user.

     message - string to display

     Returns {:status :requested}."))
