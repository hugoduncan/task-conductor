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
   [task-conductor.agent-runner.events :as events]
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

(defn- maybe-switch-to-worktree!
  "Check if a worktree exists for the story and switch cwd if needed.
   Called after SDK turns to detect newly created worktrees.
   Returns the new cwd if switched, nil otherwise."
  [workspace]
  (let [{:keys [story-id cwd]} (console/get-workspace-state workspace)
        worktree-path (when story-id (find-worktree-for-task story-id))]
    (when (and worktree-path (not= worktree-path cwd))
      (println "[maybe-switch-to-worktree!] Detected worktree:" worktree-path)
      (println "[maybe-switch-to-worktree!] Switching from cwd:" cwd)
      (console/update-workspace! workspace {:cwd worktree-path})
      worktree-path)))

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

   Takes task-info, optional workspace path, and optional opts map.
   Returns a config map used by run-cli-session.
   CLI sessions read CLAUDE.md and .mcp.json from :cwd.

   Options used by CLI:
   - :cwd - from opts, workspace, task-info :worktree-path, or current directory
   - :timeout-ms - CLI timeout (default: 120000ms)

   Note: Tasks from select-next-task don't include :worktree-path. When
   not provided, falls back to workspace path or current working directory.
   The agent session typically calls mcp-tasks work-on which sets up the worktree.

   Example:
     (build-task-session-config
       {:worktree-path \"/path/to/worktree\" :task-id 110}
       \"/workspace/path\"
       {:timeout-ms 180000})"
  ([task-info]
   (build-task-session-config task-info nil {}))
  ([task-info workspace]
   (build-task-session-config task-info workspace {}))
  ([task-info workspace opts]
   (let [cwd (or (:cwd opts)
                 (:worktree-path task-info)
                 workspace
                 (System/getProperty "user.dir"))]
     (merge {:cwd cwd} opts))))

;;; Task Execution

(defn- make-cli-event-callback
  "Create an event callback for CLI session event capture.

   Unlike SDK callbacks which receive raw Python messages, CLI callbacks
   receive already-mapped event maps from stream-json parsing. This function
   creates a callback that adds the required context fields and stores events
   in the event buffer.

   Handles special event types:
   - :session-id-update - Updates the session-id-atom (not stored as event)
   - All other types - Stored in the event buffer with context fields

   Arguments:
   - context: Map with :story-id (required) and optional :task-id
   - opts: Optional map with :session-id (initial session-id, will be updated)

   Returns a map with:
   - :callback - Function to pass to CLI :event-callback option
   - :session-id-atom - Atom updated when session-id is discovered"
  ([context]
   (make-cli-event-callback context {}))
  ([context opts]
   (let [session-id-atom (atom (or (:session-id opts)
                                   (str (java.util.UUID/randomUUID))))
         story-id (:story-id context)
         task-id (:task-id context)
         callback (fn [event-map]
                    ;; Handle session-id synchronization from CLI
                    (if (= :session-id-update (:type event-map))
                      (reset! session-id-atom (:session-id event-map))
                      ;; Regular event - add to buffer with context
                      (let [session-id @session-id-atom
                            event (cond-> {:timestamp (java.time.Instant/now)
                                           :session-id session-id
                                           :story-id story-id
                                           :type (:type event-map)
                                           :content (dissoc event-map :type)}
                                    task-id (assoc :task-id task-id))]
                        (try
                          (events/add-event! event)
                          (catch Exception e
                            (println "[make-cli-event-callback] Failed to add event:"
                                     (.getMessage e)))))))]
     {:callback callback
      :session-id-atom session-id-atom})))

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
   - workspace: Workspace path for session working directory (optional)
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
   (execute-task task-info nil {}))
  ([task-info workspace]
   (execute-task task-info workspace {}))
  ([task-info workspace opts]
   (println "[execute-task] task-info:" task-info)
   (let [;; Create event callback with story-id and task-id context
         event-context {:story-id (:parent-id task-info)
                        :task-id (:task-id task-info)}
         {:keys [callback]} (make-cli-event-callback event-context)
         ;; Build session config with event callback
         session-config (build-task-session-config
                         task-info workspace
                         (assoc opts :event-callback callback))
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
  [task workspace opts]
  (println "[handle-task-execution] Task:" (:id task) "parent:" (:parent-id task))
  (let [task-info {:task-id (:id task)
                   :parent-id (:parent-id task)
                   :worktree-path (:worktree-path task)}
        _ (println "[handle-task-execution] Calling execute-task with:" task-info)
        result (execute-task task-info workspace opts)]
    (println "[handle-task-execution] execute-task returned session-id:" (:session-id result))
    (console/record-session! workspace (:session-id result) (:id task) (java.time.Instant/now))
    ;; TODO: Implement handoff detection in execute-task once SDK supports
    ;; detecting when agent requests CLI handoff (Story #54).
    ;; Currently :handoff-requested? is always false.
    (when (:handoff-requested? result)
      (console/hand-to-cli {:workspace workspace}))
    result))

;;; Flow Model Story Execution

(defn- derive-flow-decision
  "Process on-sdk-complete result and return next action for the loop.
   Handles transition to CLI, story completion, errors, and continuation.
   Returns {:next-prompt prompt} or {:outcome ...}.
   opts may contain :dev-env for async CLI handoff."
  [decision workspace opts]
  (case (:action decision)
    :continue-sdk
    (do
      (console/transition! workspace :task-complete)
      (console/transition! workspace :selecting-task)
      {:next-prompt (:prompt decision)})

    :hand-to-cli
    (if-let [dev-env-instance (:dev-env opts)]
      (let [completion-promise (promise)]
        (console/hand-to-cli {:workspace workspace
                              :dev-env dev-env-instance
                              :idle-callback
                              (fn [_hook-status]
                                (dev-env/notify
                                 dev-env-instance
                                 "CLI is idle - continue here or exit to resume automated flow"))
                              :callback (fn [result]
                                          (deliver completion-promise result))})
        (let [_result @completion-promise]
          ;; CLI completed - continue the flow by returning next-prompt nil
          ;; This will cause the loop to ask flow-model for next action
          {:next-prompt nil}))
      {:outcome :handed-to-cli
       :reason (:reason decision)
       :state (console/get-workspace-state workspace)})

    :story-done
    (do
      (console/transition! workspace :story-complete)
      {:outcome :complete
       :reason (:reason decision)
       :state (console/get-workspace-state workspace)})

    :error
    (do
      (console/transition! workspace :error-recovery
                           {:error {:type :flow-error
                                    :message (:reason decision)}})
      {:outcome :error
       :error {:type :flow-error
               :message (:reason decision)}
       :state (console/get-workspace-state workspace)})

    ;; Default: treat as task-done, ask on-task-complete for next action
    (do
      (console/transition! workspace :task-complete)
      (console/transition! workspace :selecting-task)
      {:next-prompt nil})))

(defn- run-cli-with-prompt
  "Run CLI session with the given prompt and config.
   Returns {:session-id string :messages vector :result map}.

   Uses :cwd from opts, workspace, console-state :cwd, or current directory."
  [prompt workspace opts]
  (let [cwd (or (:cwd opts)
                (:cwd (console/get-workspace-state workspace))
                workspace
                (System/getProperty "user.dir"))
        session-config (build-task-session-config {:worktree-path cwd} workspace opts)]
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
  [decision flow-model workspace opts]
  (println "[handle-flow-decision] Decision:" (pr-str decision))
  (case (:action decision)
    :continue-sdk
    (do
      (console/transition! workspace :running-sdk {:session-id nil})
      (let [sdk-result (run-cli-with-prompt (:prompt decision) workspace opts)]
        ;; Store session-id for later handoff
        (console/update-workspace! workspace {:session-id (:session-id sdk-result)})
        ;; Check if SDK created a worktree and switch to it
        (maybe-switch-to-worktree! workspace)
        (let [next-decision (flow/on-sdk-complete flow-model sdk-result (console/get-workspace-state workspace))]
          (println "[handle-flow-decision] on-sdk-complete returned:" (pr-str next-decision))
          (derive-flow-decision next-decision workspace opts))))

    :hand-to-cli
    ;; Don't call console/hand-to-cli here - we haven't run SDK yet.
    ;; This happens when initial-prompt returns hand-to-cli (e.g., manual-review state)
    {:outcome :handed-to-cli
     :reason (:reason decision)
     :state (console/get-workspace-state workspace)}

    :story-done
    ;; Story is already complete (e.g., PR merged) - no SDK needed
    ;; Transition :selecting-task → :story-complete is valid
    (do
      (console/transition! workspace :story-complete)
      {:outcome :complete
       :reason (:reason decision)
       :state (console/get-workspace-state workspace)})

    :error
    (do
      (console/transition! workspace :error-recovery
                           {:error {:type :flow-error
                                    :message (:reason decision)}})
      {:outcome :error
       :error {:type :flow-error
               :message (:reason decision)}
       :state (console/get-workspace-state workspace)})

    ;; Unknown action - treat as error
    (do
      (console/transition! workspace :error-recovery
                           {:error {:type :unknown-action
                                    :action (:action decision)}})
      {:outcome :error
       :error {:type :unknown-action
               :action (:action decision)}
       :state (console/get-workspace-state workspace)})))

(defn- flow-model-loop
  "Main execution loop driven by FlowModel.
   Runs until story complete, handed to CLI, or error.
   Returns outcome map."
  [flow-model workspace opts]
  (println "[flow-model-loop] Starting flow model loop")
  (loop [iteration 0
         pending-prompt nil]
    (println "[flow-model-loop] Iteration:" iteration "paused?:" (console/paused? workspace)
             "pending-prompt:" (some? pending-prompt))
    (if (console/paused? workspace)
      {:outcome :paused
       :state (console/get-workspace-state workspace)}
      (let [decision (cond
                       ;; Use pending prompt from previous SDK completion
                       pending-prompt
                       {:action :continue-sdk :prompt pending-prompt}

                       ;; First iteration - get initial prompt
                       (zero? iteration)
                       (flow/initial-prompt flow-model {} (console/get-workspace-state workspace))

                       ;; Subsequent iterations - ask flow model for next action
                       :else
                       (flow/on-task-complete flow-model (console/get-workspace-state workspace)))
            _ (println "[flow-model-loop] Got decision:" (pr-str decision))
            result (handle-flow-decision decision flow-model workspace opts)]
        (if (:outcome result)
          ;; Terminal result - return it
          result
          ;; Continue loop - use next-prompt if provided, otherwise ask flow model
          (recur (inc iteration) (:next-prompt result)))))))

(defn- task-loop
  "Main task selection and execution loop.
   Runs until story complete, paused, blocked, or error.
   Returns outcome map."
  [story-id workspace opts]
  (println "[task-loop] Starting loop for story-id:" story-id)
  (loop [iteration 0]
    (println "[task-loop] Iteration:" iteration "paused?:" (console/paused? workspace))
    (if (console/paused? workspace)
      {:outcome :paused
       :state (console/get-workspace-state workspace)}
      (let [{:keys [status task progress blocked-tasks]} (select-next-task story-id)]
        (println "[task-loop] select-next-task returned status:" status)
        (case status
          :task-available
          (do
            (println "[task-loop] Running task:" (:id task) (:title task))
            (console/transition! workspace :running-sdk
                                 {:session-id nil
                                  :current-task-id (:id task)})
            (let [result (handle-task-execution task workspace opts)]
              (println "[task-loop] Task execution complete, session-id:" (:session-id result))
              ;; Update session-id directly after SDK returns
              (console/update-workspace! workspace {:session-id (:session-id result)})
              (console/transition! workspace :task-complete)
              (console/transition! workspace :selecting-task))
            (recur (inc iteration)))

          :all-blocked
          {:outcome :blocked
           :blocked-tasks blocked-tasks
           :progress progress
           :state (console/get-workspace-state workspace)}

          :all-complete
          (do
            (console/transition! workspace :story-complete)
            {:outcome :complete
             :progress progress
             :state (console/get-workspace-state workspace)})

          :no-tasks
          {:outcome :no-tasks
           :state (console/get-workspace-state workspace)})))))

(defn execute-story-with-flow-model
  "Execute a story using a FlowModel for state-driven execution.

   The flow model determines state transitions, prompts, and actions.
   This enables pluggable execution strategies including the StoryFlowModel
   which derives state from task fields.

   Arguments:
   - flow-model: A FlowModel implementation (e.g., StoryFlowModel)
   - story-id: The ID of the story to execute
   - workspace: Workspace path (optional, nil uses focused)
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
   (execute-story-with-flow-model flow-model story-id nil {}))
  ([flow-model story-id workspace]
   (execute-story-with-flow-model flow-model story-id workspace {}))
  ([flow-model story-id workspace opts]
   (println "[execute-story-with-flow-model] Starting story-id:" story-id "workspace:" workspace)
   (try
     ;; Look up worktree for story and store in console-state
     (let [worktree-cwd (find-worktree-for-task story-id)]
       (println "[execute-story-with-flow-model] Story worktree:" worktree-cwd)
       (console/transition! workspace :selecting-task {:story-id story-id})
       (when worktree-cwd
         (console/update-workspace! workspace {:cwd worktree-cwd})))
     (println "[execute-story-with-flow-model] Calling flow-model-loop")
     (let [result (flow-model-loop flow-model workspace opts)]
       (println "[execute-story-with-flow-model] Returned:" (:outcome result))
       result)
     (catch Exception e
       (println "[execute-story-with-flow-model] EXCEPTION:" (type e))
       (println "[execute-story-with-flow-model] ex-message:" (ex-message e))
       (println "[execute-story-with-flow-model] ex-data:" (ex-data e))
       (.printStackTrace e)
       (try
         (console/transition! workspace :error-recovery
                              {:error {:type :exception
                                       :message (ex-message e)
                                       :data (ex-data e)}})
         (catch Exception e2
           (println "[execute-story-with-flow-model] Failed transition:" (ex-message e2))))
       {:outcome :error
        :error {:type :exception
                :message (ex-message e)
                :data (ex-data e)}
        :state (console/get-workspace-state workspace)}))))

(defn execute-story
  "Execute all tasks in a story sequentially.

   Drives the main task execution loop: selects tasks, runs them in
   isolated SDK sessions, and handles state transitions.

   Arguments:
   - story-id: The ID of the story to execute
   - workspace: Workspace path (optional, nil uses focused)
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
   (execute-story story-id nil {}))
  ([story-id workspace]
   (execute-story story-id workspace {}))
  ([story-id workspace opts]
   (if-let [flow-model (:flow-model opts)]
     ;; Flow model mode - delegate to execute-story-with-flow-model
     (execute-story-with-flow-model flow-model story-id workspace (dissoc opts :flow-model))
     ;; Legacy mode - use task-loop with select-next-task
     (do
       (println "[execute-story] Starting story-id:" story-id "workspace:" workspace)
       (try
         (console/transition! workspace :selecting-task {:story-id story-id})
         (println "[execute-story] Transitioned to :selecting-task, calling task-loop")
         (let [result (task-loop story-id workspace opts)]
           (println "[execute-story] task-loop returned:" (:outcome result))
           result)
         (catch Exception e
           (println "[execute-story] EXCEPTION caught:" (type e))
           (println "[execute-story] ex-message:" (ex-message e))
           (println "[execute-story] ex-data:" (ex-data e))
           (println "[execute-story] stacktrace:")
           (.printStackTrace e)
           (try
             (console/transition! workspace :error-recovery
                                  {:error {:type :exception
                                           :message (ex-message e)
                                           :data (ex-data e)}})
             (catch Exception e2
               (println "[execute-story] Failed to transition to error-recovery:" (ex-message e2))))
           {:outcome :error
            :error {:type :exception
                    :message (ex-message e)
                    :data (ex-data e)}
            :state (console/get-workspace-state workspace)}))))))

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
