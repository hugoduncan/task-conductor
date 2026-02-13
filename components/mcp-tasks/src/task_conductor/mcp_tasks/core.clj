(ns task-conductor.mcp-tasks.core
  "Core CLI wrapper for mcp-tasks operations.
  Provides synchronous execution of mcp-tasks commands with EDN parsing."
  (:require [babashka.process :as p]
            [cheshire.core :as json]))

;;; Nullable Support

(def ^:dynamic *nullable*
  "When bound to a nullable config map, run-cli returns configured
  responses instead of spawning real processes. Used for testing."
  nil)

(defn- classify-operation
  "Classify CLI args as :query or :mutation based on command."
  [args]
  (let [cmd (first args)]
    (if (#{"list" "show" "why-blocked"} cmd)
      :query
      :mutation)))

(defn- get-command
  "Extract CLI command from args."
  [args]
  (keyword (first args)))

(defn- nullable-run-cli
  "Handle run-cli when *nullable* is bound.
  Records the operation and returns configured response.

  Supports two response formats:
  1. Simple queue: {:responses [r1 r2 ...]} - responses consumed in order
  2. Command-keyed: {:responses {:show [r1] :list [r2] :why-blocked [r3]}}
     - responses consumed per-command"
  [nullable opts]
  (let [{:keys [operations responses debug?]} nullable
        op-type (classify-operation (:args opts))
        cmd (get-command (:args opts))
        response
        (let [resps @responses]
          (cond
            ;; Command-keyed responses (map with keyword keys)
            (and (map? resps) (keyword? (first (keys resps))))
            (let [cmd-queue (get resps cmd)]
              (if (seq cmd-queue)
                (do (swap! responses update cmd rest)
                    (first cmd-queue))
                {:error :no-more-responses
                 :message (str
                           "Nullable has no more responses for command: "
                           cmd)
                 :command cmd
                 :available-commands (keys resps)}))

            ;; Simple queue (vector)
            (sequential? resps)
            (if (seq resps)
              (do (swap! responses rest)
                  (first resps))
              {:error :no-more-responses
               :message "Nullable has no more configured responses"})

            ;; Invalid format
            :else
            {:error :invalid-nullable-config
             :message (str "Nullable :responses must be"
                           " a vector or command-keyed map")}))]
    (when debug?
      (println "[mcp-tasks-nullable]" cmd (:args opts) "->" response))
    (swap!
     operations
     update
     (if (= op-type :query) :queries :mutations)
     conj
     {:opts opts :timestamp (java.time.Instant/now) :response response})
    response))

(defn- safe-read-string
  "Read a string as Clojure data with *read-eval* disabled.
  Uses clojure.core/read-string to support ::keyword syntax which
  clojure.edn/read-string does not handle."
  [s]
  (binding [*read-eval* false]
    (read-string s)))

(defn run-cli
  "Execute mcp-tasks CLI with args in the given project directory.
  Always uses --format edn for native Clojure parsing.

  Returns parsed EDN on success, or error map on failure:
  {:error :cli-error :exit-code N :stderr \"...\"}
  {:error :io-error :message \"...\"}

  Options:
    :project-dir - Working directory for .mcp-tasks.edn discovery (required)
    :args        - Vector of command arguments

  When *nullable* is bound, returns configured response without subprocess."
  [{:keys [project-dir args] :as opts}]
  (if *nullable*
    (nullable-run-cli *nullable* opts)
    (try
      (let [full-args (into ["mcp-tasks"] (conj (vec args) "--format" "edn"))
            result (apply p/shell {:dir project-dir
                                   :out :string
                                   :err :string
                                   :continue true}
                          full-args)]
        (if (zero? (:exit result))
          (safe-read-string (:out result))
          {:error :cli-error
           :exit-code (:exit result)
           :stderr (:err result)}))
      (catch java.io.IOException e
        {:error :io-error
         :message (.getMessage e)}))))

(defn build-list-args
  "Construct CLI arguments for the list command from options map.

  Supported options:
    :status        - Filter by status (open, closed, in-progress, blocked, any)
    :category      - Filter by category name
    :type          - Filter by type (task, bug, feature, story, chore)
    :parent-id     - Filter by parent task ID
    :task-id       - Filter by specific task ID
    :title-pattern - Filter by title pattern (regex or substring)
    :blocked       - Filter by blocked status (true/false)
    :show-blocking - Include blocking details
    :limit         - Maximum tasks to return
    :unique        - Enforce 0 or 1 match"
  [opts]
  (let [{:keys [status category type parent-id task-id title-pattern
                blocked show-blocking limit unique]} opts]
    (cond-> ["list"]
      status (conj "--status" (name status))
      category (conj "--category" category)
      type (conj "--type" (name type))
      parent-id (conj "--parent-id" (str parent-id))
      task-id (conj "--task-id" (str task-id))
      title-pattern (conj "--title-pattern" title-pattern)
      (some? blocked) (conj "--blocked" (str blocked))
      show-blocking (conj "--show-blocking")
      limit (conj "--limit" (str limit))
      unique (conj "--unique"))))

(defn list-tasks
  "List tasks with optional filters.

  Returns {:tasks [...] :metadata {...}} on success, or error map on failure.

  Options:
    :project-dir   - Working directory (required)
    :status        - Filter by status
    :category      - Filter by category name
    :type          - Filter by type
    :parent-id     - Filter by parent task ID
    :task-id       - Filter by specific task ID
    :title-pattern - Filter by title pattern
    :blocked       - Filter by blocked status
    :show-blocking - Include blocking details
    :limit         - Maximum tasks to return
    :unique        - Enforce 0 or 1 match"
  [{:keys [project-dir] :as opts}]
  (run-cli {:project-dir project-dir
            :args (build-list-args (dissoc opts :project-dir))}))

(defn show-task
  "Get a single task by ID.

  Returns {:task {...} :metadata {...}} on success, or error map on failure.

  Options:
    :project-dir - Working directory (required)
    :task-id     - Task ID to retrieve (required)"
  [{:keys [project-dir task-id]}]
  (run-cli {:project-dir project-dir
            :args ["show" "--task-id" (str task-id)]}))

;;; Mutation operations

(defn build-add-args
  "Construct CLI arguments for the add command from options map.

  Required options:
    :category - Task category (e.g., simple, medium, large)
    :title    - Task title

  Optional options:
    :description - Task description
    :type        - Task type (task, bug, feature, story, chore)
    :parent-id   - Parent task ID
    :prepend     - Add task at beginning instead of end"
  [opts]
  (let [{:keys [category title description type parent-id prepend]} opts]
    (cond-> ["add" "--category" category "--title" title]
      description (conj "--description" description)
      type (conj "--type" (name type))
      parent-id (conj "--parent-id" (str parent-id))
      prepend (conj "--prepend"))))

(defn add-task
  "Create a new task.

  Returns created task on success, or error map on failure.

  Required options:
    :project-dir - Working directory (required)
    :category    - Task category (required)
    :title       - Task title (required)

  Optional options:
    :description - Task description
    :type        - Task type (task, bug, feature, story, chore)
    :parent-id   - Parent task ID
    :prepend     - Add task at beginning instead of end"
  [{:keys [project-dir] :as opts}]
  (run-cli {:project-dir project-dir
            :args (build-add-args (dissoc opts :project-dir))}))

(defn build-complete-args
  "Construct CLI arguments for the complete command from options map.

  Options (at least one of :task-id or :title required):
    :task-id  - Task ID to complete
    :title    - Task title pattern
    :category - Category for verification
    :comment  - Completion comment"
  [opts]
  (let [{:keys [task-id title category comment]} opts]
    (cond-> ["complete"]
      task-id (conj "--task-id" (str task-id))
      title (conj "--title" title)
      category (conj "--category" category)
      comment (conj "--comment" comment))))

(defn complete-task
  "Mark a task as complete.

  Returns updated task on success, or error map on failure.

  Options:
    :project-dir - Working directory (required)
    :task-id     - Task ID to complete (or :title)
    :title       - Task title pattern (or :task-id)
    :category    - Category for verification (optional)
    :comment     - Completion comment (optional)"
  [{:keys [project-dir] :as opts}]
  (run-cli {:project-dir project-dir
            :args (build-complete-args (dissoc opts :project-dir))}))

(defn build-update-args
  "Construct CLI arguments for the update command from options map.

  Required options:
    :task-id - Task ID to update

  Optional options:
    :title          - New task title
    :description    - New task description
    :design         - New design notes
    :status         - New status (open, closed, in-progress, blocked)
    :category       - New category
    :type           - New type (task, bug, feature, story, chore)
    :parent-id      - New parent task ID (nil to remove)
    :meta           - Metadata map (serialized as JSON)
    :relations      - Relations vector (serialized as JSON)
    :session-events - Session events (serialized as JSON)
    :shared-context - String to append to shared context
    :code-reviewed  - ISO-8601 timestamp
    :pr-num         - GitHub PR number"
  [opts]
  (let [{:keys [task-id title description design status category type
                parent-id meta relations session-events shared-context
                code-reviewed pr-num]} opts]
    (cond-> ["update" "--task-id" (str task-id)]
      title (conj "--title" title)
      description (conj "--description" description)
      design (conj "--design" design)
      status (conj "--status" (name status))
      category (conj "--category" category)
      type (conj "--type" (name type))
      (some?
       parent-id) (conj "--parent-id" (if (nil? parent-id) "" (str parent-id)))
      meta (conj "--meta" (json/generate-string meta))
      relations (conj "--relations" (json/generate-string relations))
      session-events (conj
                      "--session-events"
                      (json/generate-string session-events))
      shared-context (conj "--shared-context" shared-context)
      code-reviewed (conj "--code-reviewed" code-reviewed)
      pr-num (conj "--pr-num" (str pr-num)))))

(defn update-task
  "Update task fields.

  Returns updated task on success, or error map on failure.

  Options:
    :project-dir    - Working directory (required)
    :task-id        - Task ID to update (required)
    :title          - New task title
    :description    - New task description
    :design         - New design notes
    :status         - New status (open, closed, in-progress, blocked)
    :category       - New category
    :type           - New type (task, bug, feature, story, chore)
    :parent-id      - New parent task ID (nil to remove)
    :meta           - Metadata map
    :relations      - Relations vector
    :session-events - Session events
    :shared-context - String to append to shared context
    :code-reviewed  - ISO-8601 timestamp
    :pr-num         - GitHub PR number"
  [{:keys [project-dir] :as opts}]
  (run-cli {:project-dir project-dir
            :args (build-update-args (dissoc opts :project-dir))}))

(defn build-delete-args
  "Construct CLI arguments for the delete command from options map.

  Options (at least one required):
    :task-id       - Task ID to delete
    :title-pattern - Title pattern to match"
  [opts]
  (let [{:keys [task-id title-pattern]} opts]
    (cond-> ["delete"]
      task-id (conj "--task-id" (str task-id))
      title-pattern (conj "--title-pattern" title-pattern))))

(defn delete-task
  "Delete a task.

  Returns deletion result on success, or error map on failure.

  Options:
    :project-dir   - Working directory (required)
    :task-id       - Task ID to delete (or :title-pattern)
    :title-pattern - Title pattern to match (or :task-id)"
  [{:keys [project-dir] :as opts}]
  (run-cli {:project-dir project-dir
            :args (build-delete-args (dissoc opts :project-dir))}))

(defn build-reopen-args
  "Construct CLI arguments for the reopen command from options map.

  Options (at least one required):
    :task-id - Task ID to reopen
    :title   - Exact task title to match"
  [opts]
  (let [{:keys [task-id title]} opts]
    (cond-> ["reopen"]
      task-id (conj "--task-id" (str task-id))
      title (conj "--title" title))))

(defn reopen-task
  "Reopen a closed task.

  Returns reopened task on success, or error map on failure.

  Options:
    :project-dir - Working directory (required)
    :task-id     - Task ID to reopen (or :title)
    :title       - Exact task title to match (or :task-id)"
  [{:keys [project-dir] :as opts}]
  (run-cli {:project-dir project-dir
            :args (build-reopen-args (dissoc opts :project-dir))}))

(defn why-blocked
  "Show why a task is blocked.

  Returns blocking information on success, or error map on failure.

  Options:
    :project-dir - Working directory (required)
    :task-id     - Task ID to check (required)"
  [{:keys [project-dir task-id]}]
  (run-cli {:project-dir project-dir
            :args ["why-blocked" "--task-id" (str task-id)]}))

(defn work-on
  "Set up environment for working on a task.

  Returns worktree information on success, or error map on failure.
  The :worktree-path in the result is the directory where work should happen.

  Options:
    :project-dir - Working directory (required)
    :task-id     - Task ID to work on (required)

  Returns map with keys including:
    :worktree-path - Absolute path to the worktree directory
    :branch-name   - Git branch name for the task
    :task-id       - The task ID
    :title         - Task title"
  [{:keys [project-dir task-id]}]
  (run-cli {:project-dir project-dir
            :args ["work-on" "--task-id" (str task-id)]}))
