(ns task-conductor.agent-runner.repl
  "User-facing REPL control functions for story execution.

   Provides interactive functions for controlling the console state machine
   from the REPL. Each function prints human-readable output and returns
   data for programmatic use."
  (:require
   [task-conductor.agent-runner.console :as console]
   [task-conductor.agent-runner.flow :as flow]
   [task-conductor.agent-runner.orchestrator :as orchestrator]
   [task-conductor.dev-env.emacs :as emacs]
   [task-conductor.dev-env.interface :as dev-env]))

;;; Control Functions

(defn start-story
  "Begin executing a story's tasks for a workspace.

   Validates that the console is in :idle state, then transitions to
   :selecting-task with the given story-id.
   Workspace can be: nil (focused), keyword alias, or string path.

   Returns the new state map on success.
   Throws if not in :idle state."
  ([story-id]
   (start-story nil story-id))
  ([workspace story-id]
   (let [current (console/current-state workspace)]
     (when (not= :idle current)
       (throw (ex-info (str "Cannot start story: console is " current ", expected :idle")
                       {:type :invalid-state
                        :current-state current
                        :required-state :idle})))
     (let [new-state (console/transition! workspace :selecting-task {:story-id story-id})]
       (println (str "Started story " story-id))
       new-state))))

(defn status
  "Return current console status for a workspace.

   Returns a map with :state, :story-id, :current-task-id, and :paused.
   Prints a human-readable summary.

   Workspace can be: nil (focused), keyword alias, or string path."
  ([]
   (status nil))
  ([workspace]
   (let [state (console/get-workspace-state workspace)
         result {:state (:state state)
                 :story-id (:story-id state)
                 :current-task-id (:current-task-id state)
                 :paused (:paused state)}]
     (println (str "State: " (:state result)))
     (when (:story-id result)
       (println (str "Story: " (:story-id result))))
     (when (:current-task-id result)
       (println (str "Task: " (:current-task-id result))))
     (when (:paused result)
       (println "PAUSED"))
     result)))

(defn pause
  "Set the pause flag for a workspace.

   The outer loop checks this flag before starting the next task.
   Workspace can be: nil (focused), keyword alias, or string path.
   Returns the current state map."
  ([]
   (pause nil))
  ([workspace]
   (console/set-paused! workspace)
   (println "Paused - execution will stop after current task completes")
   (console/get-workspace-state workspace)))

(defn continue
  "Clear the pause flag for a workspace.

   Allows the outer loop to resume on the next iteration.
   Workspace can be: nil (focused), keyword alias, or string path.
   Returns the current state map."
  ([]
   (continue nil))
  ([workspace]
   (console/clear-paused! workspace)
   (println "Resumed - execution will continue")
   (console/get-workspace-state workspace)))

(defn abort
  "Cancel execution and return to :idle state for a workspace.

   Transitions through :error-recovery to :idle, preserving history.
   Workspace can be: nil (focused), keyword alias, or string path.
   Returns the new state map.

   If already :idle, prints a message and returns the current state.
   If in :story-complete, transitions directly to :idle."
  ([]
   (abort nil))
  ([workspace]
   (let [current (console/current-state workspace)]
     (cond
       (= :idle current)
       (do
         (println "Already idle")
         (console/get-workspace-state workspace))

       (= :story-complete current)
       (do
         (console/clear-paused! workspace)
         (let [new-state (console/transition! workspace :idle)]
           (println "Aborted - returned to idle")
           new-state))

       :else
       (do
         (console/clear-paused! workspace)
         (console/transition! workspace :error-recovery {:error {:type :user-abort}})
         (let [new-state (console/transition! workspace :idle)]
           (println "Aborted - returned to idle")
           new-state))))))

;;; Error Recovery Functions

(defn retry
  "Re-attempt the failed task for a workspace.

   Transitions from :error-recovery back to :running-sdk with the same task.
   Workspace can be: nil (focused), keyword alias, or string path.
   Throws if not in :error-recovery state.

   Returns the new state map."
  ([]
   (retry nil))
  ([workspace]
   (let [current (console/current-state workspace)]
     (when (not= :error-recovery current)
       (throw (ex-info (str "Cannot retry: console is " current ", expected :error-recovery")
                       {:type :invalid-state
                        :current-state current
                        :required-state :error-recovery})))
     (let [state (console/get-workspace-state workspace)
           task-id (:current-task-id state)
           session-id (:session-id state)
           new-state (console/transition! workspace :running-sdk {:session-id session-id
                                                                  :current-task-id task-id})]
       (println (str "Retrying task " task-id))
       new-state))))

(defn skip
  "Skip the failed task and move to the next for a workspace.

   Transitions from :error-recovery to :selecting-task.
   Workspace can be: nil (focused), keyword alias, or string path.
   Throws if not in :error-recovery state.

   Returns the new state map."
  ([]
   (skip nil))
  ([workspace]
   (let [current (console/current-state workspace)]
     (when (not= :error-recovery current)
       (throw (ex-info (str "Cannot skip: console is " current ", expected :error-recovery")
                       {:type :invalid-state
                        :current-state current
                        :required-state :error-recovery})))
     (let [state (console/get-workspace-state workspace)
           task-id (:current-task-id state)
           new-state (console/transition! workspace :selecting-task)]
       (println (str "Skipped task " task-id))
       new-state))))

;;; Context Management Functions

(defn- validate-story-id
  "Returns the current story-id for a workspace, or throws if not in a story."
  [workspace]
  (let [story-id (:story-id (console/get-workspace-state workspace))]
    (when-not story-id
      (throw (ex-info "No active story"
                      {:type :no-active-story
                       :current-state (console/current-state workspace)})))
    story-id))

(defn add-context
  "Append text to the current story's shared-context.

   Validates that a story is active, then shells out to mcp-tasks CLI
   to update the story's shared-context field.
   Workspace can be: nil (focused), keyword alias, or string path.

   Returns the updated task on success.
   Throws if no active story or CLI call fails."
  ([text]
   (add-context nil text))
  ([workspace text]
   (let [story-id (validate-story-id workspace)
         result (orchestrator/run-mcp-tasks "update"
                                            "--task-id" (str story-id)
                                            "--shared-context" text)]
     (println (str "Added context to story " story-id))
     result)))

(defn view-context
  "Display the current story's shared-context.

   Retrieves the story and prints its shared-context in a readable format.
   Workspace can be: nil (focused), keyword alias, or string path.

   Returns the shared-context vector.
   Throws if no active story or CLI call fails."
  ([]
   (view-context nil))
  ([workspace]
   (let [story-id (validate-story-id workspace)
         result (orchestrator/run-mcp-tasks "show"
                                            "--task-id" (str story-id))
         shared-context (get-in result [:task :shared-context])]
     (println "Shared Context:")
     (if (seq shared-context)
       (doseq [[idx entry] (map-indexed vector shared-context)]
         (println (str "  " (inc idx) ". " entry)))
       (println "  (none)"))
     shared-context)))

;;; Session Tracking

(defn list-sessions
  "List sessions for the current story in a workspace.

   Retrieves session entries from console state filtered by the current
   story-id. Sessions are recorded with their associated story-id.
   Workspace can be: nil (focused), keyword alias, or string path.

   Prints a formatted list with session-id, task-id, and timestamp.

   Returns the filtered sessions vector.
   Throws if no active story."
  ([]
   (list-sessions nil))
  ([workspace]
   (let [story-id (validate-story-id workspace)
         sessions (->> (:sessions (console/get-workspace-state workspace))
                       (filter #(= story-id (:story-id %))))]
     (println (str "Sessions for story " story-id ":"))
     (if (seq sessions)
       (doseq [[idx {:keys [session-id task-id timestamp]}]
               (map-indexed vector sessions)]
         (println (str "  " (inc idx) ". " session-id
                       " (task " task-id ", " timestamp ")")))
       (println "  (none)"))
     (vec sessions))))

;;; Story Execution

(defonce ^:private dev-env-atom (atom nil))

(defn- get-or-create-dev-env
  "Get existing dev-env or create new one. Returns nil if Emacs not available."
  []
  (or @dev-env-atom
      (when-let [e (emacs/create-emacs-dev-env)]
        (reset! dev-env-atom e)
        e)))

(defn run-story
  "Execute all tasks in a story with automated orchestration.

   Uses the StoryFlowModel by default, which auto-refines unrefined stories
   and creates child tasks as needed. Creates fresh Claude SDK sessions for
   each task, running them sequentially until the story is complete, paused,
   or blocked.

   Validates that the console is in :idle state before starting.
   Prints progress information and final outcome.

   Workspace can be: nil (focused), keyword alias, or string path.

   Arguments:
   - workspace: Optional workspace (nil uses focused)
   - story-id: The ID of the story to execute
   - opts: Optional map of session configuration overrides
     - :flow-model - Custom flow model (defaults to StoryFlowModel)

   Returns the result map from orchestrator/execute-story with:
   - :outcome - One of :complete, :paused, :blocked, :no-tasks, :error
   - :progress - {:completed N :total M} (when applicable)
   - :state - Final console state map

   Throws if not in :idle state."
  ([story-id]
   (run-story nil story-id {}))
  ([arg1 arg2]
   ;; Disambiguate: (story-id opts) vs (workspace story-id)
   (if (map? arg2)
     (run-story nil arg1 arg2)
     (run-story arg1 arg2 {})))
  ([workspace story-id opts]
   (let [current (console/current-state workspace)]
     (when (not= :idle current)
       (throw (ex-info (str "Cannot run story: console is " current ", expected :idle")
                       {:type :invalid-state
                        :current-state current
                        :required-state :idle})))
     (println (str "Running story " story-id "..."))
     (let [dev-env (get-or-create-dev-env)
           _ (when dev-env (println "[run-story] Using Emacs dev-env for CLI handoff"))
           opts (cond-> opts
                  (not (:flow-model opts))
                  (assoc :flow-model (flow/story-flow-model orchestrator/run-mcp-tasks story-id))
                  dev-env
                  (assoc :dev-env dev-env))
           ;; Pass workspace directly - orchestrator/console functions resolve internally
           result (orchestrator/execute-story story-id workspace opts)]
       (case (:outcome result)
         :complete
         (println (str "Story complete! ("
                       (get-in result [:progress :completed])
                       " tasks)"))

         :paused
         (println "Story paused - use (continue) then (run-story ...) to resume")

         :blocked
         (do
           (println "Story blocked - all remaining tasks have dependencies:")
           (doseq [{:keys [id title blocking-task-ids]}
                   (:blocked-tasks result)]
             (println (str "  Task " id ": " title))
             (println (str "    Blocked by: " blocking-task-ids))))

         :no-tasks
         (println "Story has no child tasks - create tasks or refine the story")

         :handed-to-cli
         (do
           (println "Handed off to CLI for user interaction")
           (println (str "Reason: " (:reason result)))
           (println (str "Session ID: " (:session-id (:state result)))))

         :error
         (do
           (println "Story execution failed:")
           (println (str "  " (get-in result [:error :message])))))
       result))))

;;; Dev-Env CLI Session Management

(defn open-cli
  "Open a new Claude CLI session in Emacs via the dev-env socket.

   Connects to the Emacs socket server (task-conductor-dev-env) and sends
   an open-session request. Emacs will start a new Claude CLI buffer.

   Arguments:
   - opts: Optional map with:
     - :session-id - Session ID for --resume (omit to start fresh session)
     - :prompt - Initial prompt to send after session starts
     - :working-dir - Directory for the CLI session (defaults to cwd)
     - :callback - Function called when session completes
                   Receives {:tracking-id :status :hook-status :exit-code}

   Returns {:status :requested, :tracking-id <id>} on success.
   Returns {:status :error, :message <msg>} on failure.

   Example:
     (open-cli)                              ; new session
     (open-cli {:prompt \"Hello\"})          ; with initial prompt
     (open-cli {:session-id \"abc-123\"})    ; resume existing session"
  ([]
   (open-cli {}))
  ([opts]
   (let [env (or @dev-env-atom
                 (when-let [e (emacs/create-emacs-dev-env)]
                   (reset! dev-env-atom e)
                   e))]
     (if-not env
       (do
         (println "Failed to connect to Emacs socket server")
         (println "Ensure task-conductor-dev-env-start has been called in Emacs")
         {:status :error
          :message "Could not connect to Emacs socket"})
       (let [working-dir (or (:working-dir opts) (System/getProperty "user.dir"))
             callback (or (:callback opts)
                          (fn [result]
                            (println "Session completed:" result)))
             result (dev-env/open-cli-session
                     env
                     {:session-id (:session-id opts)  ; nil for new session
                      :prompt (:prompt opts)
                      :working-dir working-dir}
                     callback)]
         (println (str "Opening CLI session: " (:tracking-id result)))
         (when (:session-id opts)
           (println (str "Resuming session: " (:session-id opts))))
         (println (str "Working dir: " working-dir))
         (when (:prompt opts)
           (println (str "Prompt: " (subs (:prompt opts) 0 (min 50 (count (:prompt opts)))) "...")))
         result)))))

(defn close-cli
  "Close a CLI session by tracking-id.

   Sends a close-session request to Emacs, which will kill the CLI buffer.
   Any pending callback for this session will receive {:status :cancelled}.

   Returns {:status :requested}."
  [tracking-id]
  (if-let [env @dev-env-atom]
    (do
      (dev-env/close-session env tracking-id)
      (println (str "Closing session: " tracking-id))
      {:status :requested})
    (do
      (println "No dev-env connection")
      {:status :error :message "Not connected"})))

(defn disconnect-dev-env
  "Disconnect from the Emacs dev-env socket server.

   Closes the connection and clears the cached EmacsDevEnv."
  []
  (when-let [env @dev-env-atom]
    (emacs/stop! env)
    (reset! dev-env-atom nil)
    (println "Disconnected from Emacs dev-env"))
  nil)
