(ns task-conductor.agent-runner.orchestrator
  "Orchestrates story task execution with Claude agent sessions.

   This namespace provides the main entry point for executing stories.
   It coordinates task selection from mcp-tasks, creates fresh Claude
   sessions for each task, and manages state transitions.

   Key responsibilities:
   - Query mcp-tasks for the next unblocked child task
   - Build session configuration with MCP server settings
   - Execute tasks in isolated Claude sessions
   - Handle pause/resume and error recovery"
  (:require
   [clojure.edn :as edn]
   [clojure.java.shell :as shell]
   [task-conductor.agent-runner.console :as console]
   [task-conductor.claude-agent-sdk.interface :as sdk]))

;;; CLI Integration

(defn run-mcp-tasks
  "Execute mcp-tasks CLI command and return parsed EDN result.

   Runs the mcp-tasks CLI with the given arguments, always requesting
   EDN format output. Returns the parsed result on success.

   Args is a sequence of command arguments (strings).

   Returns parsed EDN data from mcp-tasks output.
   Throws ex-info on non-zero exit or parse failure with details."
  [& args]
  (let [cmd (into ["mcp-tasks"] (concat args ["--format" "edn"]))
        {:keys [exit out err]} (apply shell/sh cmd)]
    (if (zero? exit)
      (try
        (edn/read-string out)
        (catch Exception e
          (throw (ex-info "Failed to parse mcp-tasks output"
                          {:cmd cmd
                           :output out
                           :error (ex-message e)}))))
      (throw (ex-info "mcp-tasks command failed"
                      {:cmd cmd
                       :exit-code exit
                       :stderr err
                       :stdout out})))))

;;; Task Selection

(defn- query-unblocked-child
  "Query mcp-tasks for first unblocked child of story-id."
  [story-id]
  (run-mcp-tasks "list"
                 "--parent-id" (str story-id)
                 "--blocked" "false"
                 "--limit" "1"))

(defn- query-all-children
  "Query mcp-tasks for all children of story-id."
  [story-id]
  (run-mcp-tasks "list" "--parent-id" (str story-id)))

(defn- build-progress
  "Build progress map from metadata."
  [{:keys [open-task-count completed-task-count]}]
  {:completed completed-task-count
   :total (+ open-task-count completed-task-count)})

(defn- build-blocked-task-info
  "Extract blocking info from a task for reporting."
  [{:keys [id title blocking-task-ids]}]
  {:id id
   :title title
   :blocking-task-ids blocking-task-ids})

(defn select-next-task
  "Select the next unblocked task from a story's children.

   Returns a map with :status and additional keys based on status:

   {:status :task-available :task <task-map> :progress {:completed N :total M}}
   - A task is ready to execute

   {:status :all-blocked :blocked-tasks [...] :progress {...}}
   - All remaining tasks are blocked. :blocked-tasks contains
     [{:id N :title \"...\" :blocking-task-ids [...]}, ...]

   {:status :all-complete :progress {:completed N :total N}}
   - All tasks have been completed

   {:status :no-tasks}
   - Story has no child tasks"
  [story-id]
  (println "[select-next-task] Querying for story-id:" story-id)
  (let [{:keys [tasks metadata]} (query-unblocked-child story-id)]
    (println "[select-next-task] Got tasks:" (count tasks) "metadata:" metadata)
    (if (seq tasks)
      ;; Found an unblocked task
      {:status :task-available
       :task (first tasks)
       :progress (build-progress metadata)}
      ;; No unblocked tasks - determine why
      (let [{all-tasks :tasks
             all-meta :metadata} (query-all-children story-id)
            {:keys [open-task-count completed-task-count]} all-meta]
        (cond
          ;; Has open tasks but none unblocked = all blocked
          (pos? open-task-count)
          {:status :all-blocked
           :blocked-tasks (mapv build-blocked-task-info
                                (filter :is-blocked all-tasks))
           :progress (build-progress all-meta)}

          ;; No open tasks but some completed = all complete
          (pos? completed-task-count)
          {:status :all-complete
           :progress (build-progress all-meta)}

          ;; No tasks at all
          :else
          {:status :no-tasks})))))

;;; Session Configuration

(defn build-task-session-config
  "Build SDK session options for executing a task.

   Takes task-info and optional opts map.
   Returns a config map suitable for claude-agent-sdk/create-client.

   MCP server configuration:
   - If :mcp-servers provided in opts → uses it directly
   - Otherwise → enables auto-discovery via :setting-sources [\"project\"]
     so SDK reads .mcp.json from :cwd

   Default options (can be overridden via opts):
   - :permission-mode \"bypassPermissions\" - for automated execution
   - :cwd - from opts, task-info :worktree-path, or current directory

   Note: Tasks from select-next-task don't include :worktree-path. When
   not provided, falls back to the current working directory. The agent
   session typically calls mcp-tasks work-on which sets up the worktree.

   Example:
     (build-task-session-config
       {:worktree-path \"/path/to/worktree\" :task-id 110}
       {:max-turns 50})"
  ([task-info]
   (build-task-session-config task-info {}))
  ([task-info opts]
   (let [cwd (or (:cwd opts)
                 (:worktree-path task-info)
                 (System/getProperty "user.dir"))
         ;; Defaults for automated task execution
         defaults {:permission-mode "bypassPermissions"
                   :cwd cwd}
         ;; Configure MCP servers - explicit config or auto-discovery
         mcp-config (if (contains? opts :mcp-servers)
                      {}
                      {:mcp-servers {"mcp-tasks" {:command "mcp-tasks-server"}}})]
     (merge defaults mcp-config opts))))

;;; Task Execution

(defn- build-task-prompt
  "Build the prompt string for executing a task.

   Constructs a prompt that invokes the execute-story-child workflow
   with the task's parent story ID. The agent will then use mcp-tasks
   to find and execute the specific task."
  [task-info]
  (let [parent-id (:parent-id task-info)]
    (format "/mcp-tasks:execute-story-child (MCP) %d" parent-id)))

(defn run-sdk-session
  "Run an SDK session with the given config and prompt.

   Extracted to enable testing via with-redefs. Creates a session,
   sends the prompt, and returns the result with session-id.

   Returns {:result <query-result> :session-id <string>}."
  [session-config prompt]
  (println "[run-sdk-session] Creating session with config:" (pr-str session-config))
  (println "[run-sdk-session] Prompt:" prompt)
  (let [result (sdk/with-session [client session-config]
                 (println "[run-sdk-session] Session created, sending query")
                 (sdk/session-query client prompt))]
    (println "[run-sdk-session] Session query complete, session-id:" (:session-id result))
    result))

(defn execute-task
  "Execute a task in a fresh Claude SDK session.

   Creates an isolated session, injects the execute-story-child prompt
   with the task's parent story ID, and runs until completion or handoff.

   Arguments:
   - task-info: Map from work-on tool with :task-id, :parent-id, :worktree-path
   - opts: Optional session configuration overrides

   Returns a map with:
   - :session-id - Session identifier for later resumption
   - :messages - Vector of parsed response messages
   - :result - Final result message
   - :handoff-requested? - True if CLI handoff was requested (not yet implemented)

   The session uses MCP auto-discovery by default, so the agent has
   access to mcp-tasks and other configured MCP servers.

   Example:
     (execute-task {:task-id 111
                    :parent-id 57
                    :worktree-path \"/path/to/worktree\"})"
  ([task-info]
   (execute-task task-info {}))
  ([task-info opts]
   (println "[execute-task] task-info:" task-info)
   (let [session-config (build-task-session-config task-info opts)
         _ (println "[execute-task] Built session-config:" (pr-str session-config))
         prompt (build-task-prompt task-info)
         _ (println "[execute-task] Built prompt:" prompt)
         {:keys [result session-id]} (run-sdk-session session-config prompt)]
     (println "[execute-task] Got result, session-id:" session-id)
     (println "[execute-task] Messages:" (count (:messages result)))
     (doseq [msg (:messages result)]
       (println "[execute-task] Message type:" (:type msg))
       (when (= :assistant-message (:type msg))
         (println "[execute-task] Assistant content:"
                  (pr-str (take 3 (:content msg)))))
       (when (= :result-message (:type msg))
         (println "[execute-task] Result: is-error:" (:is-error msg)
                  "num-turns:" (:num-turns msg)
                  "result:" (:result msg))))
     {:session-id session-id
      :messages (:messages result)
      :result result
      :handoff-requested? false})))

;;; Story Execution Loop

(defn- handle-task-execution
  "Execute a single task and handle handoff if needed.
   Returns the execution result from execute-task."
  [task opts]
  (println "[handle-task-execution] Task:" (:id task) "parent:" (:parent-id task))
  (let [task-info {:task-id (:id task)
                   :parent-id (:parent-id task)
                   :worktree-path (:worktree-path task)}
        _ (println "[handle-task-execution] Calling execute-task with:" task-info)
        result (execute-task task-info opts)]
    (println "[handle-task-execution] execute-task returned session-id:" (:session-id result))
    (console/record-session! (:session-id result) (:id task))
    ;; TODO: Implement handoff detection in execute-task once SDK supports
    ;; detecting when agent requests CLI handoff (Story #54).
    ;; Currently :handoff-requested? is always false.
    (when (:handoff-requested? result)
      (console/hand-to-cli))
    result))

(defn- task-loop
  "Main task selection and execution loop.
   Runs until story complete, paused, blocked, or error.
   Returns outcome map."
  [story-id opts]
  (println "[task-loop] Starting loop for story-id:" story-id)
  (loop [iteration 0]
    (println "[task-loop] Iteration:" iteration "paused?:" (console/paused?))
    (if (console/paused?)
      {:outcome :paused
       :state @console/console-state}
      (let [{:keys [status task progress blocked-tasks]} (select-next-task story-id)]
        (println "[task-loop] select-next-task returned status:" status)
        (case status
          :task-available
          (do
            (println "[task-loop] Running task:" (:id task) (:title task))
            (console/transition! :running-sdk
                                 {:session-id nil
                                  :current-task-id (:id task)})
            (let [result (handle-task-execution task opts)]
              (println "[task-loop] Task execution complete, session-id:" (:session-id result))
              ;; Update session-id directly after SDK returns
              (swap! console/console-state assoc :session-id (:session-id result))
              (console/transition! :task-complete)
              (console/transition! :selecting-task))
            (recur (inc iteration)))

          :all-blocked
          {:outcome :blocked
           :blocked-tasks blocked-tasks
           :progress progress
           :state @console/console-state}

          :all-complete
          (do
            (console/transition! :story-complete)
            {:outcome :complete
             :progress progress
             :state @console/console-state})

          :no-tasks
          {:outcome :no-tasks
           :state @console/console-state})))))

(defn execute-story
  "Execute all tasks in a story sequentially.

   Drives the main task execution loop: selects tasks, runs them in
   isolated SDK sessions, and handles state transitions.

   Arguments:
   - story-id: The ID of the story to execute
   - opts: Optional map of session configuration overrides

   Returns a map with:
   - :outcome - One of :complete, :paused, :blocked, :no-tasks, :error
   - :progress - {:completed N :total M} (when applicable)
   - :blocked-tasks - List of blocked task info (when :blocked)
   - :error - Error details (when :error)
   - :state - Final console state map

   The function transitions the console state machine through:
   :idle → :selecting-task → :running-sdk → :task-complete → loop

   Each task runs in a fresh SDK session with MCP auto-discovery.
   The loop checks the paused flag between iterations.

   Example:
     (execute-story 57)
     ;; => {:outcome :complete
     ;;     :progress {:completed 5 :total 5}
     ;;     :state {...}}"
  ([story-id]
   (execute-story story-id {}))
  ([story-id opts]
   (println "[execute-story] Starting story-id:" story-id)
   (try
     (console/transition! :selecting-task {:story-id story-id})
     (println "[execute-story] Transitioned to :selecting-task, calling task-loop")
     (let [result (task-loop story-id opts)]
       (println "[execute-story] task-loop returned:" (:outcome result))
       result)
     (catch Exception e
       (println "[execute-story] EXCEPTION caught:" (type e))
       (println "[execute-story] ex-message:" (ex-message e))
       (println "[execute-story] ex-data:" (ex-data e))
       (println "[execute-story] stacktrace:")
       (.printStackTrace e)
       (try
         (console/transition! :error-recovery
                              {:error {:type :exception
                                       :message (ex-message e)
                                       :data (ex-data e)}})
         (catch Exception e2
           (println "[execute-story] Failed to transition to error-recovery:" (ex-message e2))))
       {:outcome :error
        :error {:type :exception
                :message (ex-message e)
                :data (ex-data e)}
        :state @console/console-state}))))

(comment
  ;; Example: Query unblocked children of story 57
  (run-mcp-tasks "list" "--parent-id" "57" "--blocked" "false" "--limit" "1")

  ;; Example: Show a specific task
  (run-mcp-tasks "show" "108")

  ;; Example: Select next task from story
  (select-next-task 57)

  ;; Example: Build session config with auto-discovery
  (build-task-session-config {:worktree-path "/path/to/project"})
  ;; => {:permission-mode "bypassPermissions"
  ;;     :cwd "/path/to/project"
  ;;     :setting-sources ["project"]}

  ;; Example: Build session config with explicit MCP servers
  (build-task-session-config
   {:worktree-path "/path/to/project"}
   {:mcp-servers {"mcp-tasks" {:command "mcp-tasks"
                               :args ["serve"]}}})
  ;; => {:permission-mode "bypassPermissions"
  ;;     :cwd "/path/to/project"
  ;;     :mcp-servers {"mcp-tasks" {...}}}

  ;; Example: Execute a task (requires SDK initialization)
  ;; (sdk/initialize! {:venv-path ".venv"})
  (execute-task {:task-id 111
                 :parent-id 57
                 :worktree-path "/path/to/worktree"})
  ;; => {:session-id "abc-123"
  ;;     :messages [...]
  ;;     :result {...}
  ;;     :handoff-requested? false}

  ;; Example: Execute all tasks in a story
  (execute-story 57)
  ;; => {:outcome :complete
  ;;     :progress {:completed 5 :total 5}
  ;;     :state {...}}
  )
