(ns task-conductor.emacs-dev-env.interface
  "Public API for the emacs-dev-env component.

  This component provides a DevEnv implementation that delegates to Emacs
  via nREPL. Emacs polls for commands and sends responses back.

  Typical usage:
    ;; JVM side - create and use the dev-env
    (def dev-env (make-emacs-dev-env))

    ;; Emacs side - register and start command loop
    (register-emacs-dev-env dev-env)
    (loop []
      (when-let [cmd (await-command dev-env)]
        (let [result (handle-command cmd)]
          (send-response dev-env (:command-id cmd) result))
        (recur)))"
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
  "Called by Emacs to register itself with this dev-env.

  Marks the dev-env as connected and ready to receive commands.
  Returns true on success."
  [dev-env]
  (core/register-emacs-dev-env dev-env))

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
