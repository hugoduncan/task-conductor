(ns task-conductor.emacs-dev-env.interface
  "Public API for the emacs-dev-env component.

  This component provides a DevEnv implementation that delegates to Emacs
  via nREPL. Emacs polls for commands and sends responses back.

  Typical usage from Emacs (via nREPL eval):
    ;; Register and get dev-env-id
    (def dev-env-id (register-emacs-dev-env))

    ;; Command loop
    (loop []
      (let [result (await-command-by-id dev-env-id)]
        (when (= :ok (:status result))
          (let [cmd (:command result)
                response (handle-command cmd)]
            (send-response-by-id dev-env-id (:command-id cmd) response))
          (recur))))

    ;; Cleanup
    (unregister-emacs-dev-env dev-env-id)"
  (:require
   [task-conductor.emacs-dev-env.core :as core]))

;;; Lifecycle

(defn make-emacs-dev-env
  "Create an EmacsDevEnv instance.

  Returns an unconnected dev-env implementing the DevEnv protocol.
  Call `register-emacs-dev-env` from Emacs via nREPL to connect."
  []
  (core/make-emacs-dev-env))

(defn connected?
  "Check if Emacs has registered with this dev-env."
  [dev-env]
  (core/connected? dev-env))

(defn shutdown
  "Shutdown the dev-env, closing the command channel.

  Any blocked `await-command` calls will return nil."
  [dev-env]
  (core/shutdown dev-env))

;;; nREPL-callable functions
;; These are called by Emacs via nREPL eval

(defn register-emacs-dev-env
  "Called by Emacs to register itself as a dev-env.

  Arity:
    ()        - Create new dev-env, register in global registry, return dev-env-id
    (dev-env) - Mark existing dev-env as connected (for internal use)

  Returns dev-env-id (string) when called with no args.
  Returns true when called with dev-env instance."
  ([]
   (core/register-emacs-dev-env))
  ([dev-env]
   (core/register-emacs-dev-env dev-env)))

(defn unregister-emacs-dev-env
  "Called by Emacs to unregister and shutdown a dev-env.

  Removes the dev-env from the registry and shuts it down.
  Returns true if found and removed, false if not found."
  [dev-env-id]
  (core/unregister-emacs-dev-env dev-env-id))

(defn get-dev-env
  "Look up a dev-env by ID from the registry.
  Returns the EmacsDevEnv instance or nil if not found."
  [dev-env-id]
  (core/get-dev-env dev-env-id))

(defn await-command
  "Called by Emacs to poll for the next command.

  Blocks until a command is available or the channel is closed.
  Returns a command map or nil if closed.

  Command map format:
    {:command-id <uuid>
     :command    :start-session|:query-transcript|:query-events|:close-session
     :params     {...}}"
  [dev-env]
  (core/await-command dev-env))

(defn send-response
  "Called by Emacs to send a response for a command.

  Parameters:
    dev-env    - The EmacsDevEnv instance
    command-id - UUID of the command being responded to
    response   - The response value

  Returns true if response was delivered, false if command not found."
  [dev-env command-id response]
  (core/send-response dev-env command-id response))

(defn send-hook-event
  "Called by Emacs to notify of session events.

  Parameters:
    dev-env    - The EmacsDevEnv instance
    hook-type  - :on-idle or :on-close
    session-id - The session that triggered the event
    reason     - Why the event occurred (:user-exit, :error, :timeout, :idle)

  Returns true after invoking hooks."
  [dev-env hook-type session-id reason]
  (core/send-hook-event dev-env hook-type session-id reason))

;;; ID-based nREPL functions
;; These are the primary API for Emacs to interact with the dev-env.
;; They take a dev-env-id string and look up the dev-env in the registry.

(defn await-command-by-id
  "Called by Emacs to poll for the next command using dev-env-id.

  Parameters:
    dev-env-id - String ID returned from register-emacs-dev-env
    timeout-ms - Optional timeout in ms (default 30000)

  Returns:
    {:status :ok :command cmd}        - command received
    {:status :timeout}                - timeout expired
    {:status :closed}                 - channel was closed
    {:status :error :message \"...\"}   - dev-env not found"
  ([dev-env-id]
   (core/await-command-by-id dev-env-id))
  ([dev-env-id timeout-ms]
   (core/await-command-by-id dev-env-id timeout-ms)))

(defn send-response-by-id
  "Called by Emacs to send a response for a command using dev-env-id.

  Parameters:
    dev-env-id - String ID returned from register-emacs-dev-env
    command-id - UUID of the command being responded to
    response   - The response value

  Returns true if response was delivered, false if command not found,
  or {:error ...} if dev-env not found."
  [dev-env-id command-id response]
  (core/send-response-by-id dev-env-id command-id response))

(defn send-hook-event-by-id
  "Called by Emacs to notify of session events using dev-env-id.

  Parameters:
    dev-env-id - String ID returned from register-emacs-dev-env
    hook-type  - :on-idle or :on-close
    session-id - The session that triggered the event
    reason     - Why the event occurred (:user-exit, :error, :timeout, :idle)

  Returns true after invoking hooks, or {:error ...} if dev-env not found."
  [dev-env-id hook-type session-id reason]
  (core/send-hook-event-by-id dev-env-id hook-type session-id reason))

;;; Health Check

(defn ping
  "Send a ping command to verify Emacs is responsive.

  Parameters:
    dev-env    - The EmacsDevEnv instance
    timeout-ms - Optional timeout in ms (default 5000)

  Returns:
    {:status :ok}       - Emacs responded
    {:status :timeout}  - No response within timeout
    {:status :error :message \"...\"}  - Error occurred"
  ([dev-env]
   (core/ping dev-env))
  ([dev-env timeout-ms]
   (core/ping dev-env timeout-ms)))

(defn ping-by-id
  "Send a ping command using dev-env-id.

  Parameters:
    dev-env-id - String ID returned from register-emacs-dev-env
    timeout-ms - Optional timeout in ms (default 5000)

  Returns:
    {:status :ok}       - Emacs responded
    {:status :timeout}  - No response within timeout
    {:status :error :message \"...\"}  - Error or dev-env not found"
  ([dev-env-id]
   (core/ping-by-id dev-env-id))
  ([dev-env-id timeout-ms]
   (core/ping-by-id dev-env-id timeout-ms)))

;;; Dev-Env Selection

(defn list-dev-envs
  "List all registered dev-envs with their connection status.

  Returns a vector of maps:
    [{:dev-env-id \"...\"
      :type :emacs
      :connected? true/false}]"
  []
  (core/list-dev-envs))

(defn list-healthy-dev-envs
  "List all registered dev-envs that respond to ping.

  Pings each connected dev-env and returns only those that respond.

  Parameters:
    timeout-ms - Optional ping timeout per dev-env (default 5000)

  Returns a vector of maps:
    [{:dev-env-id \"...\"
      :type :emacs
      :connected? true}]"
  ([]
   (core/list-healthy-dev-envs))
  ([timeout-ms]
   (core/list-healthy-dev-envs timeout-ms)))

(defn select-dev-env
  "Select the best available dev-env.

  Currently only Emacs dev-envs exist. Returns the first healthy one,
  or nil if none available.

  Parameters:
    timeout-ms - Optional ping timeout per dev-env (default 5000)

  Returns:
    {:dev-env-id \"...\" :type :emacs :dev-env <instance>} or nil"
  ([]
   (core/select-dev-env))
  ([timeout-ms]
   (core/select-dev-env timeout-ms)))
