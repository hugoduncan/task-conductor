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
   [clojure.java.shell :as shell]))

;; NOTE: console and handoff namespaces will be added when implemented
;; (see stories #53 and #54/55)

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
  (let [{:keys [tasks metadata]} (query-unblocked-child story-id)]
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

   Takes task-info (from work-on tool) and optional opts map.
   Returns a config map suitable for claude-agent-sdk/create-client.

   MCP server configuration:
   - If :mcp-servers provided in opts → uses it directly
   - Otherwise → enables auto-discovery via :setting-sources [\"project\"]
     so SDK reads .mcp.json from :cwd

   Default options (can be overridden via opts):
   - :permission-mode \"bypassPermissions\" - for automated execution
   - :cwd - from task-info :worktree-path

   Example:
     (build-task-session-config
       {:worktree-path \"/path/to/worktree\" :task-id 110}
       {:max-turns 50})"
  ([task-info]
   (build-task-session-config task-info {}))
  ([task-info opts]
   (let [cwd (or (:cwd opts)
                 (:worktree-path task-info))
         ;; Defaults for automated task execution
         defaults {:permission-mode "bypassPermissions"
                   :cwd cwd}
         ;; Enable MCP auto-discovery unless explicit :mcp-servers provided
         mcp-config (if (contains? opts :mcp-servers)
                      {}
                      {:setting-sources ["project"]})]
     (merge defaults mcp-config opts))))

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
  )
