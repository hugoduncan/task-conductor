(ns task-conductor.mcp-tasks.interface
  "Public interface for mcp-tasks CLI operations.

  All functions in this namespace require a `:project-dir` key specifying
  the working directory where .mcp-tasks.edn is located.

  ## Error Handling

  All functions return error maps on failure:
  - `{:error :cli-error :exit-code N :stderr \"...\"}` - CLI command failed
  - `{:error :io-error :message \"...\"}` - IO/process error

  ## Example Usage

  ```clojure
  (require '[task-conductor.mcp-tasks.interface :as tasks])

  ;; List all open tasks
  (tasks/list-tasks {:project-dir \"/path/to/project\"
                     :status :open})

  ;; Get a specific task
  (tasks/show-task {:project-dir \"/path/to/project\"
                    :task-id 42})

  ;; Create a new task
  (tasks/add-task {:project-dir \"/path/to/project\"
                   :category \"simple\"
                   :title \"Fix bug\"})
  ```"
  (:require [task-conductor.mcp-tasks.core :as core]))

(def run-cli
  "Execute mcp-tasks CLI with args in the given project directory.
  Always uses --format edn for native Clojure parsing.

  Returns parsed EDN on success, or error map on failure.

  Options:
    :project-dir - Working directory for .mcp-tasks.edn discovery (required)
    :args        - Vector of command arguments

  Error maps:
    {:error :cli-error :exit-code N :stderr \"...\"}
    {:error :io-error :message \"...\"}"
  core/run-cli)

(def list-tasks
  "List tasks with optional filters.

  Returns {:tasks [...] :metadata {...}} on success, or error map on failure.

  Required:
    :project-dir   - Working directory for .mcp-tasks.edn discovery

  Filter options:
    :status        - Filter by status: :open, :closed, :in-progress, :blocked, :any
    :category      - Filter by category name (string)
    :type          - Filter by type: :task, :bug, :feature, :story, :chore
    :parent-id     - Filter by parent task ID (integer)
    :task-id       - Filter by specific task ID (integer)
    :title-pattern - Filter by title pattern (regex or substring)
    :blocked       - Filter by blocked status (boolean)
    :show-blocking - Include blocking details in results (boolean)
    :limit         - Maximum tasks to return (integer)
    :unique        - Enforce 0 or 1 match, error if multiple (boolean)

  Example:
    (list-tasks {:project-dir \"/path/to/project\"
                 :status :open
                 :type :task
                 :limit 10})"
  core/list-tasks)

(def show-task
  "Get a single task by ID.

  Returns {:task {...} :metadata {...}} on success, or error map on failure.

  Required:
    :project-dir - Working directory for .mcp-tasks.edn discovery
    :task-id     - Task ID to retrieve (integer)

  Example:
    (show-task {:project-dir \"/path/to/project\"
                :task-id 42})"
  core/show-task)

(def add-task
  "Create a new task.

  Returns created task on success, or error map on failure.

  Required:
    :project-dir - Working directory for .mcp-tasks.edn discovery
    :category    - Task category: \"simple\", \"medium\", \"large\", etc.
    :title       - Task title (string)

  Optional:
    :description - Task description (string)
    :type        - Task type: :task, :bug, :feature, :story, :chore
    :parent-id   - Parent task ID (integer)
    :prepend     - Add task at beginning instead of end (boolean)

  Example:
    (add-task {:project-dir \"/path/to/project\"
               :category \"simple\"
               :title \"Fix login button\"
               :description \"Button not responding on mobile\"})"
  core/add-task)

(def complete-task
  "Mark a task as complete.

  Returns updated task on success, or error map on failure.

  Required:
    :project-dir - Working directory for .mcp-tasks.edn discovery

  Identification (at least one required):
    :task-id     - Task ID to complete (integer)
    :title       - Exact task title to match (string)

  Optional:
    :category    - Category for verification (string)
    :comment     - Completion comment (string)

  Example:
    (complete-task {:project-dir \"/path/to/project\"
                    :task-id 42
                    :comment \"Fixed and verified\"})"
  core/complete-task)

(def update-task
  "Update task fields.

  Returns updated task on success, or error map on failure.

  Required:
    :project-dir    - Working directory for .mcp-tasks.edn discovery
    :task-id        - Task ID to update (integer)

  Optional field updates:
    :title          - New task title (string)
    :description    - New task description (string)
    :design         - New design notes (string)
    :status         - New status: :open, :closed, :in-progress, :blocked
    :category       - New category (string)
    :type           - New type: :task, :bug, :feature, :story, :chore
    :parent-id      - New parent task ID (integer, nil to remove)
    :meta           - Metadata map (map, serialized as JSON)
    :relations      - Relations vector (vector, serialized as JSON)
    :session-events - Session events (serialized as JSON)
    :shared-context - String to append to shared context
    :code-reviewed  - ISO-8601 timestamp (string)
    :pr-num         - GitHub PR number (integer)

  Example:
    (update-task {:project-dir \"/path/to/project\"
                  :task-id 42
                  :status :in-progress
                  :description \"Updated requirements\"})"
  core/update-task)

(def delete-task
  "Delete a task.

  Returns deletion result on success, or error map on failure.

  Required:
    :project-dir   - Working directory for .mcp-tasks.edn discovery

  Identification (at least one required):
    :task-id       - Task ID to delete (integer)
    :title-pattern - Title pattern to match (string)

  Example:
    (delete-task {:project-dir \"/path/to/project\"
                  :task-id 42})"
  core/delete-task)

(def reopen-task
  "Reopen a closed task.

  Returns reopened task on success, or error map on failure.

  Required:
    :project-dir - Working directory for .mcp-tasks.edn discovery

  Identification (at least one required):
    :task-id     - Task ID to reopen (integer)
    :title       - Exact task title to match (string)

  Example:
    (reopen-task {:project-dir \"/path/to/project\"
                  :task-id 42})"
  core/reopen-task)

(def why-blocked
  "Show why a task is blocked.

  Returns blocking information on success, or error map on failure.

  Required:
    :project-dir - Working directory for .mcp-tasks.edn discovery
    :task-id     - Task ID to check (integer)

  Example:
    (why-blocked {:project-dir \"/path/to/project\"
                  :task-id 42})"
  core/why-blocked)
