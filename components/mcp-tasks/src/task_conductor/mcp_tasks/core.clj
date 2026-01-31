(ns task-conductor.mcp-tasks.core
  "Core CLI wrapper for mcp-tasks operations.
  Provides synchronous execution of mcp-tasks commands with EDN parsing."
  (:require [babashka.process :as p]))

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
    :args        - Vector of command arguments"
  [{:keys [project-dir args]}]
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
       :message (.getMessage e)})))

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
