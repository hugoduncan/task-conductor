(ns task-conductor.emacs-dev-env.core
  "Emacs-based DevEnv implementation.

  Commands are queued for Emacs to poll via nREPL. The JVM blocks waiting
  for responses from Emacs, which handles the actual Claude CLI process."
  (:require
   [clojure.core.async :as async]
   [taoensso.timbre :as log]
   [task-conductor.dev-env.protocol :as protocol]
   [task-conductor.dev-env.registry :as generic-registry]
   [task-conductor.pathom-graph.interface :as graph]
   [task-conductor.project.registry :as project-registry]
   [task-conductor.statechart-engine.interface :as sc])
  (:import
   [java.util UUID]))

(declare install-project-registry-watch!)

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
    state        - The dev-env state atom
    command-chan - The command channel to send on
    command-kw   - Keyword for the command (e.g., :start-session)
    params       - Map of parameters for the command
    timeout-ms   - Timeout for response in milliseconds

  Returns the response from Emacs,
  or {:error :timeout :message \"...\"} on timeout."
  [state command-chan command-kw params timeout-ms]
  (let [command-id (UUID/randomUUID)
        response-promise (promise)
        command {:command-id command-id
                 :command command-kw
                 :params params
                 :response-promise response-promise}]
    (async/>!! command-chan command)
    (let [result (deref response-promise timeout-ms response-timeout-sentinel)]
      (if (= result response-timeout-sentinel)
        (do
          ;; Clean up pending command on timeout
          ;; to prevent late response delivery
          (swap! state update :pending-commands dissoc command-id)
          {:error :timeout
           :message (str "Response timeout waiting for " (name command-kw))})
        result))))

(defn- send-notification
  "Send a one-way notification to Emacs (no response expected).
  Returns true if put on channel, false if channel is nil."
  [command-chan command-kw params]
  (if command-chan
    (do
      (async/>!! command-chan {:command-id (UUID/randomUUID)
                               :command command-kw
                               :params params
                               :notification true})
      true)
    (do
      (log/debug "Notification dropped, no command channel"
                 {:command command-kw})
      false)))

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
      (send-command-and-wait state command-chan :start-session
                             {:session-id session-id :opts opts}
                             default-response-timeout-ms)))

  (register-hook [_ session-id hook-type callback]
    (let [{:keys [command-chan]} @state
          hook-id (UUID/randomUUID)]
      ;; Store callback locally for when emacs sends hook events
      (swap! state assoc-in [:hooks hook-id] {:type hook-type
                                              :callback callback
                                              :session-id session-id})
      ;; Send command to emacs to set up hook detection
      (send-command-and-wait state command-chan :register-hook
                             {:session-id session-id
                              :hook-type hook-type}
                             default-response-timeout-ms)
      hook-id))

  (query-transcript [_ session-id]
    (let [{:keys [command-chan]} @state]
      (send-command-and-wait state command-chan :query-transcript
                             {:session-id session-id}
                             default-response-timeout-ms)))

  (query-events [_ session-id]
    (let [{:keys [command-chan]} @state]
      (send-command-and-wait state command-chan :query-events
                             {:session-id session-id}
                             default-response-timeout-ms)))

  (close-session [_ session-id]
    (let [{:keys [command-chan]} @state
          result (send-command-and-wait state command-chan :close-session
                                        {:session-id session-id}
                                        default-response-timeout-ms)]
      (when (and result (not (:error result)))
        (swap! state update :sessions dissoc session-id))
      result))

  (connected? [_]
    (:connected? @state)))

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

(defn get-dev-env
  "Look up a dev-env by ID from the registry.
  Returns nil if not found."
  [dev-env-id]
  (generic-registry/get-dev-env dev-env-id))

;;; nREPL-callable functions

(defn register-emacs-dev-env
  "Called by Emacs to register itself as a dev-env.

  Creates a new EmacsDevEnv, stores it in the generic dev-env registry,
  then marks it connected. Also installs the project registry watch
  (idempotent) so notifications are only active when a dev-env is connected.
  Returns the dev-env-id for use in subsequent calls.

  Arity:
    ()        - Create new dev-env, register, return ID (for Emacs)
    (dev-env) - Mark existing dev-env as connected (for internal use)"
  ([]
   (install-project-registry-watch!)
   (let [dev-env (make-emacs-dev-env)
         dev-env-id (generic-registry/register! dev-env :emacs {})]
     (swap! (:state dev-env) assoc :connected? true)
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
      (generic-registry/unregister! dev-env-id)
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
             (when-let [rp (:response-promise value)]
               (swap!
                (:state dev-env)
                assoc-in
                [:pending-commands (:command-id value)]
                rp))
             {:status :ok
              :command (dissoc value :response-promise)})))))))

(defn send-response
  "Called by Emacs to send a response for a command.

  Parameters:
    dev-env    - The EmacsDevEnv instance
    command-id - UUID of the command being responded to
    response   - The response value

  Returns true if response was delivered, false if command not found
  (e.g., due to timeout cleanup)."
  [dev-env command-id response]
  (let [pending (get-in @(:state dev-env) [:pending-commands command-id])]
    (if pending
      (do
        (deliver pending response)
        (swap! (:state dev-env) update :pending-commands dissoc command-id)
        true)
      (do
        (log/debug "Late response for unknown command" {:command-id command-id})
        false))))

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
;; These functions take a dev-env-id string and look up
;; the dev-env in the registry.
;; They are the primary API for Emacs to interact with the dev-env.

(defmacro with-dev-env
  "Look up a dev-env by ID and bind it, or return a standard error map.
  Evaluates body with binding bound to the dev-env instance.
  Returns {:status :error :message ...} if the dev-env is not found."
  [binding dev-env-id & body]
  `(if-let [~binding (get-dev-env ~dev-env-id)]
     (do ~@body)
     {:status :error :message (str "Dev-env not found: " ~dev-env-id)}))

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
   (with-dev-env dev-env dev-env-id
     (await-command dev-env timeout-ms))))

(defn send-response-by-id
  "Called by Emacs to send a response for a command using dev-env-id.

  Parameters:
    dev-env-id - String ID returned from register-emacs-dev-env
    command-id - UUID of the command being responded to
    response   - The response value

  Returns true if response was delivered, false if command not found,
  or {:error ...} if dev-env not found."
  [dev-env-id command-id response]
  (with-dev-env dev-env dev-env-id
    (send-response dev-env command-id response)))

(defn send-hook-event-by-id
  "Called by Emacs to notify of session events using dev-env-id.

  Parameters:
    dev-env-id - String ID returned from register-emacs-dev-env
    hook-type  - :on-idle or :on-close
    session-id - The session that triggered the event
    reason     - Why the event occurred (:user-exit, :error, :timeout, :idle)

  Returns true after invoking hooks, or {:error ...} if dev-env not found."
  [dev-env-id hook-type session-id reason]
  (with-dev-env dev-env dev-env-id
    (send-hook-event dev-env hook-type session-id reason)))

;;; Session Query

(defn query-sessions-by-id
  "Query sessions in escalated/idle states. Called by Emacs via nREPL.
  Returns {:status :ok :sessions [...]} or {:status :error ...}.

  Validates that the caller is a registered dev-env."
  [dev-env-id]
  (with-dev-env _dev-env dev-env-id
    {:status :ok
     :sessions (:engine/active-sessions
                (graph/query [:engine/active-sessions]))}))

(defn notify-sessions-changed!
  "Push session data to a specific dev-env via notification.
  Sends :notify-sessions-changed command with current session data."
  [dev-env]
  (let [{:keys [command-chan connected?]} @(:state dev-env)]
    (when (and connected? command-chan)
      (let [sessions (:engine/active-sessions
                      (graph/query
                       [:engine/active-sessions]))]
        (send-notification command-chan
                           :notify-sessions-changed
                           {:sessions sessions})))))

(defn notify-all-sessions-changed!
  "Push session data to all connected dev-envs."
  []
  (doseq [{:keys [dev-env/id type]} (generic-registry/list-dev-envs)
          :when (= :emacs type)
          :let [dev-env (generic-registry/get-dev-env id)]
          :when (connected? dev-env)]
    (notify-sessions-changed! dev-env)))

;;; Project Query

(defn- derive-project-status
  "Derive project status from its active sessions.
  Returns :escalated, :idle, :running, or nil."
  [sessions]
  (when (seq sessions)
    (let [states (into #{} (map :state) sessions)]
      (cond
        (contains? states :escalated) :escalated
        (contains? states :idle) :idle
        :else :running))))

(defn- enrich-project
  "Enrich a project map with execution status from active sessions."
  [project all-sessions]
  (let [project-path (:project/path project)
        matching (filterv #(= project-path (:project-dir %)) all-sessions)
        status (derive-project-status matching)]
    (cond-> project
      status (assoc :project/status status)
      (seq matching) (assoc :project/active-sessions
                            (mapv #(select-keys % [:session-id :state :task-id])
                                  matching)))))

(defn- enriched-projects
  "Fetch all projects enriched with session status."
  []
  (let [projects (:project/all (graph/query [:project/all]))
        all-sessions (sc/all-session-summaries)]
    (mapv #(enrich-project % all-sessions) projects)))

(defn notify-projects-changed!
  "Push pre-computed project data to a specific dev-env via notification."
  [dev-env projects]
  (let [{:keys [command-chan connected?]} @(:state dev-env)]
    (when (and connected? command-chan)
      (send-notification command-chan
                         :notify-projects-changed
                         {:projects projects}))))

(defn notify-all-projects-changed!
  "Push enriched project data to all connected dev-envs.
  Computes enriched projects once and sends to all."
  []
  (let [projects (enriched-projects)]
    (doseq [{:keys [dev-env/id type]} (generic-registry/list-dev-envs)
            :when (= :emacs type)
            :let [dev-env (generic-registry/get-dev-env id)]
            :when (connected? dev-env)]
      (notify-projects-changed! dev-env projects))))

(defn query-projects-by-id
  "Query all registered projects enriched with execution status.
  Returns {:status :ok :projects [...]} or {:status :error ...}."
  [dev-env-id]
  (with-dev-env _dev-env dev-env-id
    {:status :ok
     :projects (enriched-projects)}))

(defn- invoke-project-mutation!
  "Invoke a project mutation via graph/query.
  Returns {:status :ok :project result} or {:status :error ...}."
  [mutation-kw params]
  (let [mutation-sym (symbol "task-conductor.project.resolvers"
                             (str "project-" (name mutation-kw) "!"))
        query-result (graph/query [(list mutation-sym params)])
        result (:project/result (get query-result mutation-sym))]
    (cond
      (:error result)
      {:status :error :message (:message result) :error (:error result)}

      (nil? result)
      {:status :error :message "Mutation returned no result"}

      :else
      {:status :ok :project result})))

(defn create-project-by-id
  "Create a project. Called by Emacs via nREPL.
  Returns {:status :ok :project {...}} or {:status :error ...}."
  [dev-env-id path name]
  (with-dev-env _dev-env dev-env-id
    (invoke-project-mutation!
     :create (cond-> {:project/path path}
               name (assoc :project/name name)))))

(defn update-project-by-id
  "Update project name. Called by Emacs via nREPL.
  Returns {:status :ok :project {...}} or {:status :error ...}."
  [dev-env-id path name]
  (with-dev-env _dev-env dev-env-id
    (invoke-project-mutation!
     :update {:project/path path :project/name name})))

(defn delete-project-by-id
  "Delete project by path. Called by Emacs via nREPL.
  Returns {:status :ok :project {...}} or {:status :error ...}."
  [dev-env-id path]
  (with-dev-env _dev-env dev-env-id
    (invoke-project-mutation! :delete {:project/path path})))

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
   (let [state (:state dev-env)
         {:keys [command-chan connected?]} @state]
     (cond
       (not connected?)
       {:status :error :message "Dev-env not connected"}

       (not command-chan)
       {:status :error :message "Command channel closed"}

       :else
       (let [result
             (send-command-and-wait state command-chan :ping {} timeout-ms)]
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
   (with-dev-env dev-env dev-env-id
     (ping dev-env timeout-ms))))

;;; Dev-Env Selection

(defn list-dev-envs
  "List all registered emacs dev-envs with their connection status.

  Returns a vector of maps:
    [{:dev-env-id \"...\"
      :type :emacs
      :connected? true/false}]"
  []
  (vec
   (for [{:keys [dev-env/id type]} (generic-registry/list-dev-envs)
         :when (= :emacs type)
         :let [dev-env (generic-registry/get-dev-env id)]]
     {:dev-env-id id
      :type :emacs
      :connected? (connected? dev-env)})))

(defn list-healthy-dev-envs
  "List all registered emacs dev-envs that respond to ping.

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
    (for [{:keys [dev-env/id type]} (generic-registry/list-dev-envs)
          :when (= :emacs type)
          :let [dev-env (generic-registry/get-dev-env id)]
          :when (connected? dev-env)
          :let [ping-result (ping dev-env timeout-ms)]
          :when (= :ok (:status ping-result))]
      {:dev-env-id id
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

;;; Registry Watch

(defn install-project-registry-watch!
  "Install a watch on the project registry that pushes notifications
  to all connected dev-envs when projects change."
  []
  (project-registry/watch!
   ::project-notify
   (fn [_key _ref _old _new]
     (try
       (notify-all-projects-changed!)
       (catch Exception e
         (log/warn "Failed to notify projects changed"
                   {:error (.getMessage e)}))))))

(defn remove-project-registry-watch!
  "Remove the project registry watch."
  []
  (project-registry/unwatch! ::project-notify))
