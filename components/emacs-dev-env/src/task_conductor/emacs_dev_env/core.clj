(ns task-conductor.emacs-dev-env.core
  "Emacs-based DevEnv implementation.

  Commands are queued for Emacs to poll via nREPL. The JVM blocks waiting
  for responses from Emacs, which handles the actual Claude CLI process."
  (:require
   [clojure.core.async :as async]
   [task-conductor.dev-env.protocol :as protocol])
  (:import
   [java.util UUID]))

(defrecord EmacsDevEnv [state]
  ;; state is an atom containing:
  ;;   :command-chan - core.async channel for command queue
  ;;   :hooks        - map of hook-id -> {:type :callback}
  ;;   :sessions     - map of session-id -> session-state
  ;;   :connected?   - whether Emacs has registered

  protocol/DevEnv

  (start-session [_ session-id opts]
    (let [{:keys [command-chan]} @state
          response-promise (promise)
          command {:command-id (UUID/randomUUID)
                   :command :start-session
                   :params {:session-id session-id
                            :opts opts}
                   :response-promise response-promise}]
      (async/>!! command-chan command)
      (swap! state assoc-in [:sessions session-id] {:status :starting})
      @response-promise))

  (register-hook [_ hook-type callback]
    (let [hook-id (UUID/randomUUID)]
      (swap! state assoc-in [:hooks hook-id] {:type hook-type
                                              :callback callback})
      hook-id))

  (query-transcript [_ session-id]
    (let [{:keys [command-chan]} @state
          response-promise (promise)
          command {:command-id (UUID/randomUUID)
                   :command :query-transcript
                   :params {:session-id session-id}
                   :response-promise response-promise}]
      (async/>!! command-chan command)
      @response-promise))

  (query-events [_ session-id]
    (let [{:keys [command-chan]} @state
          response-promise (promise)
          command {:command-id (UUID/randomUUID)
                   :command :query-events
                   :params {:session-id session-id}
                   :response-promise response-promise}]
      (async/>!! command-chan command)
      @response-promise))

  (close-session [_ session-id]
    (let [{:keys [command-chan]} @state
          response-promise (promise)
          command {:command-id (UUID/randomUUID)
                   :command :close-session
                   :params {:session-id session-id}
                   :response-promise response-promise}]
      (async/>!! command-chan command)
      (let [result @response-promise]
        (when result
          (swap! state update :sessions dissoc session-id))
        result))))

;;; Lifecycle

(defn make-emacs-dev-env
  "Create an EmacsDevEnv instance.

  Returns an unconnected dev-env. Call `register-emacs-dev-env` from Emacs
  via nREPL to mark it as connected and ready to receive commands."
  []
  (->EmacsDevEnv
   (atom {:command-chan (async/chan 32)
          :hooks {}
          :sessions {}
          :connected? false})))

(defn connected?
  "Check if Emacs has registered with this dev-env."
  [dev-env]
  (:connected? @(:state dev-env)))

(defn shutdown
  "Shutdown the dev-env, closing the command channel."
  [dev-env]
  (let [{:keys [command-chan]} @(:state dev-env)]
    (async/close! command-chan)
    (reset! (:state dev-env) {:command-chan nil
                              :hooks {}
                              :sessions {}
                              :connected? false})))

;;; nREPL-callable functions

(defn register-emacs-dev-env
  "Called by Emacs to register itself with this dev-env.

  Marks the dev-env as connected and ready to receive commands.
  Returns true on success."
  [dev-env]
  (swap! (:state dev-env) assoc :connected? true)
  true)

(defn await-command
  "Called by Emacs to poll for the next command.

  Blocks until a command is available or the channel is closed.
  Returns a command map (without the response-promise) or nil if closed.

  Command map format:
    {:command-id <uuid>
     :command    :start-session|:query-transcript|:query-events|:close-session
     :params     {...}}"
  [dev-env]
  (let [{:keys [command-chan]} @(:state dev-env)]
    (when command-chan
      (when-let [command (async/<!! command-chan)]
        (swap! (:state dev-env) assoc-in [:pending-commands (:command-id command)]
               (:response-promise command))
        (dissoc command :response-promise)))))

(defn send-response
  "Called by Emacs to send a response for a command.

  Parameters:
    dev-env    - The EmacsDevEnv instance
    command-id - UUID of the command being responded to
    response   - The response value

  Returns true if response was delivered, false if command not found."
  [dev-env command-id response]
  (let [pending (get-in @(:state dev-env) [:pending-commands command-id])]
    (if pending
      (do
        (deliver pending response)
        (swap! (:state dev-env) update :pending-commands dissoc command-id)
        true)
      false)))

;;; Hook invocation

(defn invoke-hook
  "Invoke all registered hooks of the given type with context.

  Called by Emacs (via send-hook-event) when session events occur.

  Parameters:
    dev-env   - The EmacsDevEnv instance
    hook-type - :on-idle or :on-close
    context   - Map with :session-id, :timestamp, :reason"
  [dev-env hook-type context]
  (let [hooks (get @(:state dev-env) :hooks)]
    (doseq [[_hook-id {:keys [type callback]}] hooks
            :when (= type hook-type)]
      (try
        (callback context)
        (catch Exception e
          (println "Hook callback error:" (.getMessage e)))))))

(defn send-hook-event
  "Called by Emacs to notify of session events.

  Parameters:
    dev-env    - The EmacsDevEnv instance
    hook-type  - :on-idle or :on-close
    session-id - The session that triggered the event
    reason     - Why the event occurred (:user-exit, :error, :timeout, :idle)

  Returns true after invoking hooks."
  [dev-env hook-type session-id reason]
  (let [context {:session-id session-id
                 :timestamp (java.time.Instant/now)
                 :reason reason}]
    (invoke-hook dev-env hook-type context)
    true))
