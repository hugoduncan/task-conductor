(ns task-conductor.agent-runner.orchestrator
  "Orchestrates story task execution with Claude agent sessions.

   This namespace provides the main entry point for executing stories.
   It coordinates with a FlowModel to determine state transitions and
   prompts, creates fresh Claude sessions for each task, and manages
   console state.

   Key responsibilities:
   - Delegate state decisions to FlowModel
   - Build session configuration with MCP server settings
   - Execute tasks in isolated Claude sessions
   - Handle pause/resume and error recovery
   - Respond to FlowDecision actions"
  (:require
   [clojure.edn :as edn]
   [clojure.java.shell :as shell]
   [clojure.string :as str]
   [task-conductor.agent-runner.console :as console]
   [task-conductor.agent-runner.flow :as flow]
   [task-conductor.agent-runner.session :as session]
   [task-conductor.dev-env.interface :as dev-env]))

;;; CLI Integration

(defn- normalize-auto-resolved-keywords
  "Replace auto-resolved keywords (::foo) with namespaced keywords (:mcp-tasks/foo).
   EDN reader can't parse :: without namespace context."
  [s]
  (str/replace s #"::(\w+)" ":mcp-tasks/$1"))

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
        (edn/read-string (normalize-auto-resolved-keywords out))
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

;;; Worktree Discovery

(defn- parse-git-worktree-list
  "Parse output of `git worktree list` into a sequence of maps.
   Each map has :path, :commit, and :branch keys.
   Returns empty sequence if parsing fails."
  [output]
  (for [line (str/split-lines output)
        :when (not (str/blank? line))
        :let [[path commit branch-part] (str/split line #"\s+" 3)]
        :when (and path commit)]
    {:path path
     :commit commit
     :branch (when branch-part
               (second (re-find #"\[(.+)\]" branch-part)))}))

(defn- list-git-worktrees
  "List all git worktrees for the current repository.
   Returns {:ok worktrees} or {:error message}."
  []
  (let [{:keys [exit out err]} (shell/sh "git" "worktree" "list")]
    (if (zero? exit)
      {:ok (parse-git-worktree-list out)}
      {:error (str "git worktree list failed: " err)})))

(defn- find-worktree-for-task
  "Find the worktree path for a given task ID.
   Worktrees are named <task-id>-<slugified-title>.
   Returns the path string if found, nil otherwise."
  [task-id]
  (let [{:keys [ok error]} (list-git-worktrees)]
    (when-not error
      (some (fn [{:keys [path branch]}]
              (when (and branch
                         (re-matches (re-pattern (str task-id "-.*")) branch))
                path))
            ok))))

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
  "Build session options for executing a task via CLI.

   Takes task-info and optional opts map. Returns a config map used by
   run-cli-session. CLI sessions read CLAUDE.md and .mcp.json from :cwd.

   Options used by CLI:
   - :cwd - from opts, task-info :worktree-path, or current directory
   - :timeout-ms - CLI timeout (default: 120000ms)

   Note: Tasks from select-next-task don't include :worktree-path. When
   not provided, falls back to the current working directory. The agent
   session typically calls mcp-tasks work-on which sets up the worktree.

   Example:
     (build-task-session-config
       {:worktree-path \"/path/to/worktree\" :task-id 110}
       {:timeout-ms 180000})"
  ([task-info]
   (build-task-session-config task-info {}))
  ([task-info opts]
   (let [cwd (or (:cwd opts)
                 (:worktree-path task-info)
                 (System/getProperty "user.dir"))]
     (merge {:cwd cwd} opts))))

;;; Task Execution

(defn- build-task-prompt
  "Build the prompt string for executing a task.

   Constructs a prompt that invokes the execute-story-child workflow
   with the task's parent story ID. The agent will then use mcp-tasks
   to find and execute the specific task."
  [task-info]
  (let [parent-id (:parent-id task-info)]
    (format "/mcp-tasks:execute-story-child (MCP) %d" parent-id)))

(defn run-cli-session
  "Run a CLI session with the given config and prompt.

   Creates a new session via CLI (ensuring Claude Code's system prompt,
   CLAUDE.md context, and MCP tools), sends the prompt, and returns
   the result with session-id.

   Extracted to enable testing via with-redefs.

   Returns {:session-id <string> :messages [] :result <cli-response>}."
  [session-config prompt]
  (println "[run-cli-session] Creating CLI session with config:" (pr-str session-config))
  (println "[run-cli-session] Prompt:" prompt)
  (let [result (session/run-cli-session session-config prompt)]
    (println "[run-cli-session] Session complete, session-id:" (:session-id result))
    result))

(defn execute-task
  "Execute a task in a fresh Claude CLI session.

   Creates an isolated session via CLI (ensuring Claude Code's system prompt,
   CLAUDE.md context, and MCP tools), injects the execute-story-child prompt
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
         {:keys [messages result session-id]} (run-cli-session session-config prompt)]
     (println "[execute-task] Got result, session-id:" session-id)
     (println "[execute-task] Messages:" (count messages))
     (doseq [msg messages]
       (println "[execute-task] Message type:" (:type msg))
       (when (= :assistant-message (:type msg))
         (println "[execute-task] Assistant content:"
                  (pr-str (take 3 (:content msg)))))
       (when (= :result-message (:type msg))
         (println "[execute-task] Result: is-error:" (:is-error msg)
                  "num-turns:" (:num-turns msg)
                  "result:" (:result msg))))
     {:session-id session-id
      :messages messages
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

;;; Flow Model Story Execution

(defn- derive-flow-decision
  "Process on-sdk-complete result and return next action for the loop.
   Handles transition to CLI, story completion, errors, and continuation.
   Returns {:next-prompt prompt} or {:outcome ...}.
   opts may contain :dev-env for async CLI handoff."
  [decision opts]
  (case (:action decision)
    :continue-sdk
    (do
      (console/transition! :task-complete)
      (console/transition! :selecting-task)
      {:next-prompt (:prompt decision)})

    :hand-to-cli
    (do
      (println "[derive-flow-decision] Hand-to-CLI requested:" (:reason decision))
      (println "[derive-flow-decision] Session ID:" (:session-id @console/console-state))
      (if-let [dev-env-instance (:dev-env opts)]
        (let [completion-promise (promise)]
          (println "[derive-flow-decision] Using async mode with dev-env, waiting for CLI completion")
          (console/hand-to-cli {:dev-env dev-env-instance
                                :idle-callback
                                (fn []
                                  (dev-env/notify
                                   dev-env-instance
                                   "CLI is idle - continue here or exit to resume automated flow"))
                                :callback (fn [result]
                                            (println "[derive-flow-decision] CLI completed, delivering to promise")
                                            (println "[derive-flow-decision] Result state:" (:state result))
                                            (deliver completion-promise result))})
          (println "[derive-flow-decision] Blocking on CLI completion...")
          (let [_result @completion-promise]
            (println "[derive-flow-decision] CLI completion received, continuing flow")
            ;; CLI completed - continue the flow by returning next-prompt nil
            ;; This will cause the loop to ask flow-model for next action
            {:next-prompt nil}))
        (do
          (println "[derive-flow-decision] No dev-env - returning without launching CLI")
          {:outcome :handed-to-cli
           :reason (:reason decision)
           :state @console/console-state})))

    :story-done
    (do
      (console/transition! :story-complete)
      {:outcome :complete
       :reason (:reason decision)
       :state @console/console-state})

    :error
    (do
      (console/transition! :error-recovery
                           {:error {:type :flow-error
                                    :message (:reason decision)}})
      {:outcome :error
       :error {:type :flow-error
               :message (:reason decision)}
       :state @console/console-state})

    ;; Default: treat as task-done, ask on-task-complete for next action
    (do
      (console/transition! :task-complete)
      (console/transition! :selecting-task)
      {:next-prompt nil})))

(defn- run-cli-with-prompt
  "Run CLI session with the given prompt and config.
   Returns {:session-id string :messages vector :result map}.

   Uses :cwd from opts, falling back to console-state :cwd or current directory."
  [prompt opts]
  (let [cwd (or (:cwd opts)
                (:cwd @console/console-state)
                (System/getProperty "user.dir"))
        session-config (build-task-session-config {:worktree-path cwd} opts)]
    (println "[run-cli-with-prompt] Running CLI with prompt:" prompt)
    (println "[run-cli-with-prompt] Using cwd:" cwd)
    (let [{:keys [result session-id]} (run-cli-session session-config prompt)]
      (println "[run-cli-with-prompt] CLI complete, session-id:" session-id)
      (println "[run-cli-with-prompt] Result text:" (subs (str (:result result)) 0
                                                          (min 200 (count (str (:result result))))))
      (when-let [denials (seq (:permission_denials result))]
        (println "[run-cli-with-prompt] Permission denials:" (count denials))
        (doseq [{:keys [tool_name]} denials]
          (println "  -" tool_name)))
      {:session-id session-id
       :messages (:messages result)
       :result result})))

(defn- handle-flow-decision
  "Process a FlowDecision and return outcome map or :next-prompt to continue.

   Returns:
   - {:next-prompt prompt} if loop should continue with that prompt
   - {:outcome ...} map if loop should exit"
  [decision flow-model opts]
  (println "[handle-flow-decision] Decision:" (pr-str decision))
  (case (:action decision)
    :continue-sdk
    (do
      (console/transition! :running-sdk {:session-id nil})
      (let [sdk-result (run-cli-with-prompt (:prompt decision) opts)]
        ;; Store session-id for later handoff (cwd is set at story start)
        (swap! console/console-state assoc :session-id (:session-id sdk-result))
        (let [next-decision (flow/on-sdk-complete flow-model sdk-result @console/console-state)]
          (println "[handle-flow-decision] on-sdk-complete returned:" (pr-str next-decision))
          (derive-flow-decision next-decision opts))))

    :hand-to-cli
    ;; Don't call console/hand-to-cli here - we haven't run SDK yet.
    ;; This happens when initial-prompt returns hand-to-cli (e.g., manual-review state)
    {:outcome :handed-to-cli
     :reason (:reason decision)
     :state @console/console-state}

    :story-done
    ;; Story is already complete (e.g., PR merged) - no SDK needed
    ;; Transition :selecting-task → :story-complete is valid
    (do
      (console/transition! :story-complete)
      {:outcome :complete
       :reason (:reason decision)
       :state @console/console-state})

    :error
    (do
      (console/transition! :error-recovery
                           {:error {:type :flow-error
                                    :message (:reason decision)}})
      {:outcome :error
       :error {:type :flow-error
               :message (:reason decision)}
       :state @console/console-state})

    ;; Unknown action - treat as error
    (do
      (console/transition! :error-recovery
                           {:error {:type :unknown-action
                                    :action (:action decision)}})
      {:outcome :error
       :error {:type :unknown-action
               :action (:action decision)}
       :state @console/console-state})))

(defn- flow-model-loop
  "Main execution loop driven by FlowModel.
   Runs until story complete, handed to CLI, or error.
   Returns outcome map."
  [flow-model opts]
  (println "[flow-model-loop] Starting flow model loop")
  (loop [iteration 0
         pending-prompt nil]
    (println "[flow-model-loop] Iteration:" iteration "paused?:" (console/paused?)
             "pending-prompt:" (some? pending-prompt))
    (if (console/paused?)
      {:outcome :paused
       :state @console/console-state}
      (let [decision (cond
                       ;; Use pending prompt from previous SDK completion
                       pending-prompt
                       {:action :continue-sdk :prompt pending-prompt}

                       ;; First iteration - get initial prompt
                       (zero? iteration)
                       (flow/initial-prompt flow-model {} @console/console-state)

                       ;; Subsequent iterations - ask flow model for next action
                       :else
                       (flow/on-task-complete flow-model @console/console-state))
            _ (println "[flow-model-loop] Got decision:" (pr-str decision))
            result (handle-flow-decision decision flow-model opts)]
        (if (:outcome result)
          ;; Terminal result - return it
          result
          ;; Continue loop - use next-prompt if provided, otherwise ask flow model
          (recur (inc iteration) (:next-prompt result)))))))

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

(defn execute-story-with-flow-model
  "Execute a story using a FlowModel for state-driven execution.

   The flow model determines state transitions, prompts, and actions.
   This enables pluggable execution strategies including the StoryFlowModel
   which derives state from task fields.

   Arguments:
   - flow-model: A FlowModel implementation (e.g., StoryFlowModel)
   - story-id: The ID of the story to execute
   - opts: Optional map of session configuration overrides

   Returns a map with:
   - :outcome - One of :complete, :paused, :handed-to-cli, :error
   - :reason - Explanation for the outcome (when applicable)
   - :error - Error details (when :error)
   - :state - Final console state map

   The flow model's initial-prompt is called first, then on-sdk-complete
   after each SDK turn, and on-task-complete between tasks.

   Example:
     (let [fm (flow/story-flow-model run-mcp-tasks 57)]
       (execute-story-with-flow-model fm 57))
     ;; => {:outcome :complete
     ;;     :reason \"Story complete - PR merged\"
     ;;     :state {...}}"
  ([flow-model story-id]
   (execute-story-with-flow-model flow-model story-id {}))
  ([flow-model story-id opts]
   (println "[execute-story-with-flow-model] Starting story-id:" story-id)
   (try
     ;; Look up worktree for story and store in console-state
     (let [worktree-cwd (find-worktree-for-task story-id)]
       (println "[execute-story-with-flow-model] Story worktree:" worktree-cwd)
       (console/transition! :selecting-task {:story-id story-id})
       (when worktree-cwd
         (swap! console/console-state assoc :cwd worktree-cwd)))
     (println "[execute-story-with-flow-model] Calling flow-model-loop")
     (let [result (flow-model-loop flow-model opts)]
       (println "[execute-story-with-flow-model] Returned:" (:outcome result))
       result)
     (catch Exception e
       (println "[execute-story-with-flow-model] EXCEPTION:" (type e))
       (println "[execute-story-with-flow-model] ex-message:" (ex-message e))
       (println "[execute-story-with-flow-model] ex-data:" (ex-data e))
       (.printStackTrace e)
       (try
         (console/transition! :error-recovery
                              {:error {:type :exception
                                       :message (ex-message e)
                                       :data (ex-data e)}})
         (catch Exception e2
           (println "[execute-story-with-flow-model] Failed transition:" (ex-message e2))))
       {:outcome :error
        :error {:type :exception
                :message (ex-message e)
                :data (ex-data e)}
        :state @console/console-state}))))

(defn execute-story
  "Execute all tasks in a story sequentially.

   Drives the main task execution loop: selects tasks, runs them in
   isolated SDK sessions, and handles state transitions.

   Arguments:
   - story-id: The ID of the story to execute
   - opts: Optional map of session configuration overrides
     - :flow-model - If provided, uses flow-model-driven execution
                     via execute-story-with-flow-model

   Returns a map with:
   - :outcome - One of :complete, :paused, :blocked, :no-tasks, :error,
                :handed-to-cli (when using flow model)
   - :progress - {:completed N :total M} (when applicable, legacy mode only)
   - :blocked-tasks - List of blocked task info (when :blocked, legacy mode)
   - :reason - Explanation for outcome (flow model mode)
   - :error - Error details (when :error)
   - :state - Final console state map

   The function transitions the console state machine through:
   :idle → :selecting-task → :running-sdk → :task-complete → loop

   Each task runs in a fresh SDK session with MCP auto-discovery.
   The loop checks the paused flag between iterations.

   Example (legacy mode):
     (execute-story 57)
     ;; => {:outcome :complete
     ;;     :progress {:completed 5 :total 5}
     ;;     :state {...}}

   Example (flow model mode):
     (execute-story 57 {:flow-model (flow/story-flow-model run-mcp-tasks 57)})
     ;; => {:outcome :complete
     ;;     :reason \"Story complete - PR merged\"
     ;;     :state {...}}"
  ([story-id]
   (execute-story story-id {}))
  ([story-id opts]
   (if-let [flow-model (:flow-model opts)]
     ;; Flow model mode - delegate to execute-story-with-flow-model
     (execute-story-with-flow-model flow-model story-id (dissoc opts :flow-model))
     ;; Legacy mode - use task-loop with select-next-task
     (do
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
            :state @console/console-state}))))))

(comment
  ;; Example: Query unblocked children of story 57
  (run-mcp-tasks "list" "--parent-id" "57" "--blocked" "false" "--limit" "1")

  ;; Example: Show a specific task
  (run-mcp-tasks "show" "108")

  ;; Example: Select next task from story
  (select-next-task 57)

  ;; Example: Build CLI session config
  (build-task-session-config {:worktree-path "/path/to/project"})
  ;; => {:cwd "/path/to/project"}

  ;; Example: Build CLI session config with custom timeout
  (build-task-session-config
   {:worktree-path "/path/to/project"}
   {:timeout-ms 180000})
  ;; => {:cwd "/path/to/project"
  ;;     :timeout-ms 180000}

  ;; Example: Execute a task (uses CLI for session creation)
  (execute-task {:task-id 111
                 :parent-id 57
                 :worktree-path "/path/to/worktree"})
  ;; => {:session-id "abc-123"
  ;;     :result {:response <cli-json>}
  ;;     :handoff-requested? false}

  ;; Example: Execute all tasks in a story (legacy mode)
  (execute-story 57)
  ;; => {:outcome :complete
  ;;     :progress {:completed 5 :total 5}
  ;;     :state {...}}

  ;; Example: Execute story with StoryFlowModel (state-driven)
  (let [fm (flow/story-flow-model run-mcp-tasks 57)]
    (execute-story 57 {:flow-model fm}))
  ;; => {:outcome :complete
  ;;     :reason "Story complete - PR merged"
  ;;     :state {...}}

  ;; Example: Execute story directly with flow model
  (let [fm (flow/story-flow-model run-mcp-tasks 57)]
    (execute-story-with-flow-model fm 57))
  ;; => {:outcome :handed-to-cli
  ;;     :reason "PR created and awaiting manual review"
  ;;     :state {...}}
  )
