(ns task-conductor.emacs-dev-env.core
  "Emacs-based DevEnv implementation.

  Commands are queued for Emacs to poll via nREPL. The JVM blocks waiting
  for responses from Emacs, which handles the actual Claude CLI process."
  (:require
   [clojure.core.async :as async]
   [taoensso.timbre :as log]
   [task-conductor.dev-env.protocol :as protocol])
  (:import
   [java.util UUID]))

;;; Registry

(defonce ^{:doc "Registry of dev-env-id -> EmacsDevEnv instances.
  Allows Emacs to reference dev-envs by ID via nREPL."}
  registry
  (atom {}))

(def ^:const default-await-timeout-ms
  "Default timeout for await-command in milliseconds."
  30000)

(def ^:const default-response-timeout-ms
  "Default timeout for waiting on command responses in milliseconds."
  30000)

(def ^:private response-timeout-sentinel
  "Sentinel value returned when response times out."
  ::response-timeout)

(defn- send-command-and-wait
  "Send a command to Emacs and wait for response.

  Parameters:
    command-chan - The command channel to send on
    command-kw   - Keyword for the command (e.g., :start-session)
    params       - Map of parameters for the command
    timeout-ms   - Timeout for response in milliseconds

  Returns the response from Emacs, or {:error :timeout :message \"...\"} on timeout."
  [command-chan command-kw params timeout-ms]
  (let [response-promise (promise)
        command {:command-id (UUID/randomUUID)
                 :command command-kw
                 :params params
                 :response-promise response-promise}]
    (async/>!! command-chan command)
    (let [result (deref response-promise timeout-ms response-timeout-sentinel)]
      (if (= result response-timeout-sentinel)
        {:error :timeout :message (str "Response timeout waiting for " (name command-kw))}
        result))))

(defrecord EmacsDevEnv [state]
  ;; state is an atom containing:
  ;;   :command-chan     - core.async channel for command queue
  ;;   :hooks            - map of hook-id -> {:type :callback}
  ;;   :sessions         - map of session-id -> session-state
  ;;   :pending-commands - map of command-id -> response promise
  ;;   :connected?       - whether Emacs has registered

  protocol/DevEnv

  (start-session [_ session-id opts]
    (let [{:keys [command-chan]} @state]
      (swap! state assoc-in [:sessions session-id] {:status :starting})
      (send-command-and-wait command-chan :start-session
                             {:session-id session-id :opts opts}
                             default-response-timeout-ms)))

  (register-hook [_ hook-type callback]
    (let [hook-id (UUID/randomUUID)]
      (swap! state assoc-in [:hooks hook-id] {:type hook-type
                                              :callback callback})
      hook-id))

  (query-transcript [_ session-id]
    (let [{:keys [command-chan]} @state]
      (send-command-and-wait command-chan :query-transcript
                             {:session-id session-id}
                             default-response-timeout-ms)))

  (query-events [_ session-id]
    (let [{:keys [command-chan]} @state]
      (send-command-and-wait command-chan :query-events
                             {:session-id session-id}
                             default-response-timeout-ms)))

  (close-session [_ session-id]
    (let [{:keys [command-chan]} @state
          result (send-command-and-wait command-chan :close-session
                                        {:session-id session-id}
                                        default-response-timeout-ms)]
      (when (and result (not (:error result)))
        (swap! state update :sessions dissoc session-id))
      result)))

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
          :pending-commands {}
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
                              :pending-commands {}
                              :connected? false})))

;;; Registry functions

(defn get-dev-env
  "Look up a dev-env by ID from the registry.
  Returns nil if not found."
  [dev-env-id]
  (get @registry dev-env-id))

;;; nREPL-callable functions

(defn register-emacs-dev-env
  "Called by Emacs to register itself as a dev-env.

  Creates a new EmacsDevEnv, stores it in the registry, and marks it connected.
  Returns the dev-env-id (UUID string) for use in subsequent calls.

  Arity:
    ()        - Create new dev-env, register, return ID (for Emacs)
    (dev-env) - Mark existing dev-env as connected (for internal use)"
  ([]
   (let [dev-env (make-emacs-dev-env)
         dev-env-id (str (UUID/randomUUID))]
     (swap! (:state dev-env) assoc :connected? true)
     (swap! registry assoc dev-env-id dev-env)
     dev-env-id))
  ([dev-env]
   (swap! (:state dev-env) assoc :connected? true)
   true))

(defn unregister-emacs-dev-env
  "Called by Emacs to unregister and shutdown a dev-env.

  Removes the dev-env from the registry and shuts it down.
  Returns true if found and removed, false if not found."
  [dev-env-id]
  (if-let [dev-env (get-dev-env dev-env-id)]
    (do
      (shutdown dev-env)
      (swap! registry dissoc dev-env-id)
      true)
    false))

(defn await-command
  "Called by Emacs to poll for the next command.

  Blocks until a command is available, timeout expires, or channel closes.
  Returns:
    {:status :ok :command cmd}      - command received
    {:status :timeout}              - timeout expired
    {:status :closed}               - channel was closed

  Command map format:
    {:command-id <uuid>
     :command    :start-session|:query-transcript|:query-events|:close-session
     :params     {...}}

  Parameters:
    dev-env    - The EmacsDevEnv instance
    timeout-ms - Optional timeout in ms (default 30000)"
  ([dev-env]
   (await-command dev-env default-await-timeout-ms))
  ([dev-env timeout-ms]
   (let [{:keys [command-chan]} @(:state dev-env)]
     (if-not command-chan
       {:status :closed}
       (let [timeout-chan (async/timeout timeout-ms)
             [value port] (async/alts!! [command-chan timeout-chan])]
         (cond
           (= port timeout-chan)
           {:status :timeout}

           (nil? value)
           {:status :closed}

           :else
           (do
             (swap! (:state dev-env) assoc-in [:pending-commands (:command-id value)]
                    (:response-promise value))
             {:status :ok
              :command (dissoc value :response-promise)})))))))

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
          (log/error e "Hook callback error"
                     {:hook-type hook-type
                      :context context}))))))

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

;;; ID-based nREPL functions
;; These functions take a dev-env-id string and look up the dev-env in the registry.
;; They are the primary API for Emacs to interact with the dev-env.

(defn await-command-by-id
  "Called by Emacs to poll for the next command using dev-env-id.

  Parameters:
    dev-env-id - String ID returned from register-emacs-dev-env
    timeout-ms - Optional timeout in ms (default 30000)

  Returns:
    {:status :ok :command cmd}      - command received
    {:status :timeout}              - timeout expired
    {:status :closed}               - channel was closed
    {:status :error :message \"...\"}  - dev-env not found"
  ([dev-env-id]
   (await-command-by-id dev-env-id default-await-timeout-ms))
  ([dev-env-id timeout-ms]
   (if-let [dev-env (get-dev-env dev-env-id)]
     (await-command dev-env timeout-ms)
     {:status :error :message (str "Dev-env not found: " dev-env-id)})))

(defn send-response-by-id
  "Called by Emacs to send a response for a command using dev-env-id.

  Parameters:
    dev-env-id - String ID returned from register-emacs-dev-env
    command-id - UUID of the command being responded to
    response   - The response value

  Returns true if response was delivered, false if command not found,
  or {:error ...} if dev-env not found."
  [dev-env-id command-id response]
  (if-let [dev-env (get-dev-env dev-env-id)]
    (send-response dev-env command-id response)
    {:error :not-found :message (str "Dev-env not found: " dev-env-id)}))

(defn send-hook-event-by-id
  "Called by Emacs to notify of session events using dev-env-id.

  Parameters:
    dev-env-id - String ID returned from register-emacs-dev-env
    hook-type  - :on-idle or :on-close
    session-id - The session that triggered the event
    reason     - Why the event occurred (:user-exit, :error, :timeout, :idle)

  Returns true after invoking hooks, or {:error ...} if dev-env not found."
  [dev-env-id hook-type session-id reason]
  (if-let [dev-env (get-dev-env dev-env-id)]
    (send-hook-event dev-env hook-type session-id reason)
    {:error :not-found :message (str "Dev-env not found: " dev-env-id)}))

;;; Health Check

(def ^:const default-ping-timeout-ms
  "Default timeout for ping in milliseconds (shorter than command timeout)."
  5000)

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
   (ping dev-env default-ping-timeout-ms))
  ([dev-env timeout-ms]
   (let [{:keys [command-chan connected?]} @(:state dev-env)]
     (cond
       (not connected?)
       {:status :error :message "Dev-env not connected"}

       (not command-chan)
       {:status :error :message "Command channel closed"}

       :else
       (let [result (send-command-and-wait command-chan :ping {} timeout-ms)]
         (cond
           (:error result)
           {:status :timeout}

           (= :ok (:status result))
           {:status :ok}

           :else
           {:status :error :message (or (:message result) "Ping failed")}))))))

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
   (ping-by-id dev-env-id default-ping-timeout-ms))
  ([dev-env-id timeout-ms]
   (if-let [dev-env (get-dev-env dev-env-id)]
     (ping dev-env timeout-ms)
     {:status :error :message (str "Dev-env not found: " dev-env-id)})))

;;; Dev-Env Selection

(defn list-dev-envs
  "List all registered dev-envs with their connection status.

  Returns a vector of maps:
    [{:dev-env-id \"...\"
      :type :emacs
      :connected? true/false}]"
  []
  (vec
   (for [[dev-env-id dev-env] @registry]
     {:dev-env-id dev-env-id
      :type :emacs
      :connected? (connected? dev-env)})))

(defn list-healthy-dev-envs
  "List all registered dev-envs that respond to ping.

  Pings each connected dev-env and returns only those that respond.
  Uses a short timeout to avoid long waits.

  Parameters:
    timeout-ms - Optional ping timeout per dev-env (default 5000)

  Returns a vector of maps:
    [{:dev-env-id \"...\"
      :type :emacs
      :connected? true}]"
  ([]
   (list-healthy-dev-envs default-ping-timeout-ms))
  ([timeout-ms]
   (vec
    (for [[dev-env-id dev-env] @registry
          :when (connected? dev-env)
          :let [ping-result (ping dev-env timeout-ms)]
          :when (= :ok (:status ping-result))]
      {:dev-env-id dev-env-id
       :type :emacs
       :connected? true}))))

(defn select-dev-env
  "Select the best available dev-env.

  Currently only Emacs dev-envs exist. Returns the first healthy one,
  or nil if none available.

  Parameters:
    timeout-ms - Optional ping timeout per dev-env (default 5000)

  Returns:
    {:dev-env-id \"...\" :type :emacs :dev-env <instance>} or nil"
  ([]
   (select-dev-env default-ping-timeout-ms))
  ([timeout-ms]
   (let [healthy (list-healthy-dev-envs timeout-ms)]
     (when-let [{:keys [dev-env-id]} (first healthy)]
       {:dev-env-id dev-env-id
        :type :emacs
        :dev-env (get-dev-env dev-env-id)}))))
