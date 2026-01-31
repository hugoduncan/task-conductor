(ns task-conductor.dev-env.interface
  "Public API for dev-env interactions.

  Consumers should use this namespace instead of the protocol directly.
  Provides wrapper functions and helper utilities for common operations."
  (:require
   [task-conductor.dev-env.protocol :as protocol]))

(def ^:private valid-hook-types
  "The set of supported hook types for register-hook."
  #{:on-close :on-idle})

(defn start-session
  "Resume a Claude session interactively in the dev environment.

  Parameters:
    dev-env    - A DevEnv implementation
    session-id - The Claude session ID to resume (string)
    opts       - Options map:
                 :dir        - Working directory for the session
                 :on-started - Callback when session starts (fn [context])

  Returns a session handle (implementation-defined)."
  [dev-env session-id opts]
  (protocol/start-session dev-env session-id opts))

(defn register-hook
  "Register a lifecycle callback for session events.

  Parameters:
    dev-env   - A DevEnv implementation
    hook-type - Keyword indicating the event type:
                :on-close - Session closed (by user, error, or timeout)
                :on-idle  - Session waiting for user input
    callback  - Function (fn [context]) called when event occurs

  Returns a hook registration ID.

  Throws ex-info if hook-type is not supported."
  [dev-env hook-type callback]
  (when-not (contains? valid-hook-types hook-type)
    (throw (ex-info (str "Unsupported hook-type: " hook-type
                         ". Supported types are: " (pr-str valid-hook-types))
                    {:hook-type hook-type
                     :valid-hook-types valid-hook-types})))
  (protocol/register-hook dev-env hook-type callback))

(defn query-transcript
  "Get the conversation transcript for a session.

  Parameters:
    dev-env    - A DevEnv implementation
    session-id - The session to query (string)

  Returns the transcript or nil if unavailable."
  [dev-env session-id]
  (protocol/query-transcript dev-env session-id))

(defn query-events
  "Get raw events for a session.

  Parameters:
    dev-env    - A DevEnv implementation
    session-id - The session to query (string)

  Returns a vector of event maps or nil if unavailable."
  [dev-env session-id]
  (protocol/query-events dev-env session-id))

(defn close-session
  "Request that a session be closed.

  Parameters:
    dev-env    - A DevEnv implementation
    session-id - The session to close (string)

  Returns true if close was initiated, false if session not found."
  [dev-env session-id]
  (protocol/close-session dev-env session-id))

(defn connected?
  "Check if the dev-env is connected and operational.

  Parameters:
    dev-env - A DevEnv implementation

  Returns true if the dev-env can accept commands, false otherwise."
  [dev-env]
  (protocol/connected? dev-env))

;;; Helpers

(defn register-hooks
  "Register multiple lifecycle hooks at once.

  Parameters:
    dev-env  - A DevEnv implementation
    hook-map - Map of hook-type to callback function
               e.g., {:on-close fn1 :on-idle fn2}

  Returns a map of hook-type to hook registration."
  [dev-env hook-map]
  (reduce-kv
   (fn [acc hook-type callback]
     (assoc acc hook-type (register-hook dev-env hook-type callback)))
   {}
   hook-map))

(defmacro with-session
  "Execute body with a session, ensuring close-session on exit.

  Starts a session, binds the session handle, executes body, and
  guarantees close-session is called when exiting (normal or exception).

  Usage:
    (with-session [session dev-env session-id opts]
      (do-something-with session))

  Parameters:
    binding    - [name dev-env session-id opts]
    body       - Forms to execute with the session

  Returns the result of body."
  [[name dev-env session-id opts] & body]
  `(let [dev-env# ~dev-env
         session-id# ~session-id
         ~name (start-session dev-env# session-id# ~opts)]
     (try
       ~@body
       (finally
         (close-session dev-env# session-id#)))))
