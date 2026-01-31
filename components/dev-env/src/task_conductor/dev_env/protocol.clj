(ns task-conductor.dev-env.protocol
  "Protocol for hosting interactive Claude CLI sessions.

  When a non-interactive Claude CLI session returns needing human input,
  the orchestrator can launch an interactive session via a dev-env
  implementation to continue the conversation.

  Implementations (e.g., emacs, terminal) spawn the Claude CLI process
  directly. The JVM is notified of lifecycle events but does not own
  the process.")

(defprotocol DevEnv
  "Protocol for dev environment implementations that host interactive Claude sessions.

  Implementations must handle:
  - Starting interactive sessions with a given session-id
  - Lifecycle hooks for session events
  - Querying session state (transcript, events)
  - Closing sessions

  Callback context maps include:
    :session-id - The session identifier (string)
    :timestamp  - When the event occurred (java.time.Instant)
    :reason     - Why the event occurred (keyword)

  Reason keywords:
    :user-exit - User explicitly closed the session
    :error     - Session ended due to an error
    :timeout   - Session timed out
    :idle      - Session is waiting for user input"

  (start-session [dev-env session-id opts]
    "Resume a Claude session interactively in this dev environment.

    Parameters:
      session-id - The Claude session ID to resume (string)
      opts       - Options map:
                   :dir          - Working directory for the session
                   :on-started   - Callback when session starts (fn [context])

    Returns a session handle (implementation-defined).
    The dev-env implementation spawns the Claude CLI process directly.")

  (register-hook [dev-env hook-type callback]
    "Register a lifecycle callback for session events.

    Parameters:
      hook-type - Keyword indicating the event type:
                  :on-close - Session closed (by user, error, or timeout)
                  :on-idle  - Session waiting for user input
      callback  - Function (fn [context]) called when event occurs

    Returns a hook registration (opaque, for potential deregistration).
    Callbacks receive a context map with :session-id, :timestamp, :reason.")

  (query-transcript [dev-env session-id]
    "Get the conversation transcript for a session.

    Parameters:
      session-id - The session to query (string)

    Returns the transcript (implementation-defined format, typically string).
    Returns nil if session not found or transcript unavailable.")

  (query-events [dev-env session-id]
    "Get raw events for a session.

    Parameters:
      session-id - The session to query (string)

    Returns a vector of event maps.
    Returns nil if session not found or events unavailable.")

  (close-session [dev-env session-id]
    "Request that a session be closed.

    Parameters:
      session-id - The session to close (string)

    Returns true if close was initiated, false if session not found.
    The actual close may be asynchronous; use :on-close hook for notification."))

;;; NoOp Implementation

(defrecord NoOpDevEnv [calls hooks]
  ;; A no-op implementation for testing and development.
  ;; Tracks all calls in the `calls` atom for verification.
  ;; Hooks are stored but never fired (no real sessions).

  DevEnv
  (start-session [_ session-id opts]
    (swap! calls conj {:op :start-session
                       :session-id session-id
                       :opts opts
                       :timestamp (java.time.Instant/now)})
    {:session-id session-id :handle :noop})

  (register-hook [_ hook-type callback]
    (let [hook-id (java.util.UUID/randomUUID)]
      (swap! hooks assoc hook-id {:type hook-type :callback callback})
      (swap! calls conj {:op :register-hook
                         :hook-type hook-type
                         :hook-id hook-id
                         :timestamp (java.time.Instant/now)})
      hook-id))

  (query-transcript [_ session-id]
    (swap! calls conj {:op :query-transcript
                       :session-id session-id
                       :timestamp (java.time.Instant/now)})
    nil)

  (query-events [_ session-id]
    (swap! calls conj {:op :query-events
                       :session-id session-id
                       :timestamp (java.time.Instant/now)})
    nil)

  (close-session [_ session-id]
    (swap! calls conj {:op :close-session
                       :session-id session-id
                       :timestamp (java.time.Instant/now)})
    false))

(defn make-noop-dev-env
  "Create a NoOpDevEnv for testing.
  Returns the dev-env instance. Access atoms via :calls and :hooks keys."
  []
  (->NoOpDevEnv (atom []) (atom {})))
